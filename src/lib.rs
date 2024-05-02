//! A library for turning mhtml webpages into summarized epub articles
//!
//! This is primarily intended for use in [repub](https://github.com/hafaio/repub) for reMarkable
//! devices, but can be generally used for webpage summarization.
//!
//! # Examples
//!
//! ```
//! use repub::Repub;
//!
//! let mhtml = // ...
//! # r#"From: <Saved by Blink>
//! # Snapshot-Content-Location: http://test
//! # Subject: title
//! # MIME-Version: 1.0
//! # Content-Type: multipart/related;
//! #    type="text/html";
//! #    boundary="boundary"
//! #
//! # --boundary
//! # Content-Type: text/html
//! # Content-ID: <frame-0@mhtml.blink>
//! # Content-Transfer-Encoding: quoted-printable
//! # Content-Location: http://test
//! #
//! # <html></html>
//! # --boundary--
//! # "#;
//! let mut buff = Vec::new();
//! Repub::default().mhtml_to_epub(mhtml, &mut buff).unwrap();
//! ```
#![warn(missing_docs)]
#[cfg(feature = "image")]
mod img;

pub use epub_builder::EpubVersion;
use epub_builder::{EpubBuilder, EpubContent, ReferenceType, ZipLibrary};
use eyre::Report;
#[cfg(feature = "image")]
pub use img::{FilterType, ImgTransform};
use kuchiki::{Attribute, ExpandedName, NodeRef};
use log::{trace, warn};
use mail_parser::{Header, HeaderName, HeaderValue, MessageParser, PartType};
use markup5ever::{namespace_url, ns, Namespace, Prefix, QualName};
use readable_readability::Readability;
use std::cmp::Reverse;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::error::Error as StdError;
use std::fmt::{Display, Error as FmtError, Formatter};
use std::io::{Read, Write};

/// The image format of the transformed image
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ImageFormat {
    /// a jpeg image
    #[default]
    Jpeg,
    /// a png image
    Png,
}

impl ImageFormat {
    /// get the extension of the image format
    fn ext(&self) -> &'static str {
        match self {
            ImageFormat::Jpeg => "jpg",
            ImageFormat::Png => "png",
        }
    }

    /// get the mimetype of the image format
    fn mime(&self) -> &'static str {
        match self {
            ImageFormat::Jpeg => "image/jpeg",
            ImageFormat::Png => "image/png",
        }
    }
}

/// How to handle images in the summarized article.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ImageHandling {
    /// remove all images
    #[default]
    Strip,
    /// keep only the first copy of every image
    Filter,
    /// keep all images
    Keep,
}

/// The main class for converting mhtml
#[derive(Debug)]
pub struct Repub<Css, Trans> {
    /// if true, inclue the origin url at the top of the article
    pub include_url: bool,
    /// if true, add the article as an `h1` tag preceeding the content
    pub include_title: bool,
    /// if true, add the byline preceeding the content
    pub include_byline: bool,
    /// if true, add the cover image found preceeding the content
    pub include_cover: bool,
    /// if true, strip all links from the epub
    pub strip_links: bool,
    /// threshold for approximate url matching
    ///
    /// Due to some bugs with chromiums renderer, some urls will be stripped from the final mhtml,
    /// and as a fallback this can be set to a value less then one to allow for approximate
    /// matching. 1.0 will accept any reasonable image, 0.0 only accepts complete matches and is
    /// significantly faster than any other setting.
    pub href_sim_thresh: f64,
    /// how to handle images
    pub image_handling: ImageHandling,
    /// optional css content to render to the final epub
    pub css: Css,
    /// function to transform images
    pub transform: Trans,
    /// the version of epub to write
    pub epub_version: EpubVersion,
}

/// a method for altering images for inclusion
pub trait ImageTransform {
    /// The result of transforming the image
    type Output<'a>: Read + 'a;

    /// alter an image buffer with mime
    ///
    /// Return None to remove the image, otherwise include the format of the processed image.
    fn transform<'a, S: AsRef<str>>(
        &self,
        buff: &'a [u8],
        mime: S,
    ) -> Option<(Self::Output<'a>, ImageFormat)>;
}

/// a transform that preserves valid iamge types, and ignores all others
pub struct NoopTransform;

impl ImageTransform for NoopTransform {
    type Output<'a> = &'a [u8];

    fn transform<'a, S: AsRef<str>>(
        &self,
        buff: &'a [u8],
        mime: S,
    ) -> Option<(Self::Output<'a>, ImageFormat)> {
        let fmt = match mime.as_ref() {
            "image/jpeg" => Some(ImageFormat::Jpeg),
            "image/png" => Some(ImageFormat::Png),
            _ => None,
        }?;
        Some((buff, fmt))
    }
}

impl Default for Repub<&'static str, NoopTransform> {
    /// creates minimalist settings that have the least "impact" but probably aren't desired
    fn default() -> Self {
        Self {
            include_url: false,
            include_title: false,
            include_byline: false,
            include_cover: false,
            strip_links: false,
            href_sim_thresh: 0.0,
            image_handling: ImageHandling::default(),
            css: "",
            transform: NoopTransform,
            epub_version: EpubVersion::V20,
        }
    }
}

/// Possible errors during epub creation.
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// image format wasn't valid
    InvalidImageFormat,
    /// an error happened when parsing the mhtml
    MhtmlParseError,
    /// the mhtml didn't conform to the format expected from a chrome page export
    MhtmlFormatError,
    /// an error occured when trying to convert images
    ImageConversionError,
    /// an error occured when creating the epub
    EpubCreationError,
    /// an error occured when writing the epub
    EpubWritingError,
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), FmtError> {
        write!(fmt, "{self:?}")
    }
}

impl StdError for Error {}

impl From<Report> for Error {
    fn from(_: Report) -> Self {
        Error::EpubCreationError
    }
}

/// convenience for getting a header
fn get_header<'a, 'b>(
    headers: &'a [Header<'b>],
    header: HeaderName,
) -> Option<&'a HeaderValue<'b>> {
    headers
        .iter()
        .find(|head| head.name == header)
        .map(|head| &head.value)
}

/// shortcut for creating elements
fn new_elem(
    name: &str,
    attributes: impl IntoIterator<Item = (Namespace, Option<Prefix>, impl AsRef<str>, impl AsRef<str>)>,
    children: impl IntoIterator<Item = NodeRef>,
) -> NodeRef {
    let node = NodeRef::new_element(
        // NOTE the svg namespace here is so that img tags get closed like xml
        QualName::new(None, ns!(svg), name.into()),
        attributes.into_iter().map(|(ns, prefix, attr, value)| {
            (
                ExpandedName::new(ns, attr.as_ref()),
                Attribute {
                    prefix,
                    value: value.as_ref().into(),
                },
            )
        }),
    );
    for child in children {
        node.append(child);
    }
    node
}

/// shortcut for creating elements
fn new_attrless_elem(name: &str, children: impl IntoIterator<Item = NodeRef>) -> NodeRef {
    let attrs: [(Namespace, Option<Prefix>, &str, &str); 0] = [];
    new_elem(name, attrs, children)
}

/// next node in traversal order
fn next_node(node: &NodeRef) -> Option<NodeRef> {
    node.first_child().or_else(|| next_node_skip(node))
}

/// next node in traversal order, skipping descendants
fn next_node_skip(node: &NodeRef) -> Option<NodeRef> {
    node.next_sibling()
        .or_else(|| node.ancestors().find_map(|n| n.next_sibling()))
}

impl<C, T> Repub<C, T>
where
    C: AsRef<str>,
    T: ImageTransform,
{
    /// find a close match to an image url
    fn find_url<'a>(
        &self,
        data: &'a BTreeMap<&'a str, (&'a str, &'a [u8])>,
        src: &str,
    ) -> Option<(Reverse<usize>, String, &'a str, &'a [u8])> {
        let decoded = percent_encoding::percent_decode_str(src)
            .decode_utf8()
            .ok()?;
        if let Some((mime, data)) = data.get(decoded.as_ref()) {
            Some((Reverse(0), decoded.to_string(), mime, data))
        } else if self.href_sim_thresh > 0.0 {
            let thresh: usize =
                f64::trunc(decoded.chars().count() as f64 * self.href_sim_thresh) as usize;
            let (dist, href, mime, data) = data
                .iter()
                .map(|(href, (mime, data))| (strsim::levenshtein(href, &decoded), href, mime, data))
                .min()?;
            if dist < thresh {
                Some((Reverse(dist), href.to_string(), mime, data))
            } else {
                warn!("didn't find approximate match for image: {decoded}");
                None
            }
        } else {
            warn!("didn't find exact match for image: {decoded}");
            None
        }
    }

    /// convert an mhtml string to an epub with current options
    pub fn mhtml_to_epub(
        &self,
        mhtml: impl AsRef<str>,
        out: &mut impl Write,
    ) -> Result<Option<String>, Error> {
        // parse mhtml and get get header values
        let msg = MessageParser::default()
            .parse(mhtml.as_ref().as_bytes())
            .ok_or(Error::MhtmlParseError)?;
        let (first, rest) = msg.parts.split_first().ok_or(Error::MhtmlFormatError)?;
        let subject = get_header(&first.headers, HeaderName::Subject).and_then(|val| match val {
            HeaderValue::Text(title) => Some(title.as_ref()),
            _ => None,
        });
        let (main, resources) = rest.split_first().ok_or(Error::MhtmlFormatError)?;
        let loc =
            get_header(&main.headers, HeaderName::ContentLocation).and_then(|val| match val {
                HeaderValue::Text(loc) => Some(loc),
                _ => None,
            });
        let html = if let PartType::Html(content) = &main.body {
            Ok(content)
        } else {
            Err(Error::MhtmlFormatError)
        }?;
        let (node, meta) = Readability::new().parse(html);
        let title = meta
            .article_title
            .as_ref()
            .map(String::as_ref)
            .or_else(|| meta.page_title.as_ref().map(String::as_ref))
            .or(subject);

        // create epub
        let mut epub = EpubBuilder::new(ZipLibrary::new()?)?;
        if let Some(title) = title {
            epub.metadata("title", title)?;
        }
        if let Some(author) = &meta.byline {
            epub.metadata("author", author)?;
        }

        // fetch images from resources
        let mut image_data = BTreeMap::new();
        for attach in resources {
            let ctype = get_header(&attach.headers, HeaderName::ContentType);
            let loc = get_header(&attach.headers, HeaderName::ContentLocation);
            if let (
                Some(HeaderValue::ContentType(ctype)),
                Some(HeaderValue::Text(loc)),
                PartType::Binary(body),
            ) = (ctype, loc, &attach.body)
            {
                if let ("image", Some(mime)) = (ctype.ctype(), ctype.subtype()) {
                    match image_data.entry(loc.as_ref()) {
                        Entry::Vacant(ent) => {
                            ent.insert((mime, body.as_ref()));
                        }
                        Entry::Occupied(mut ent) => {
                            let (_, old) = ent.get();
                            // use the larger image on collision
                            // NOTE we probably want the smallest image that's larger than the max size
                            if old.len() < body.len() {
                                ent.insert((mime, body.as_ref()));
                            }
                        }
                    }
                }
            }
        }

        // fetch and save cover image if requested and valid
        let cover_img = if self.include_cover {
            if let Some((image, fmt)) = meta
                .image_url
                .as_ref()
                // NOTE we use fuzzy matching, but don't sync with other images
                .and_then(|cover| self.find_url(&image_data, cover))
                // TODO make cover image a little smaller so it fits with title?
                .and_then(|(_, _, mime, img)| {
                    self.transform.transform(img, format!("image/{}", mime))
                })
            {
                let file_name = format!("image_cover.{}", fmt.ext());
                epub.add_cover_image(&file_name, image, fmt.mime())?;
                Some(file_name)
            } else {
                None
            }
        } else {
            None
        };

        // find images in html
        let mut images = BTreeMap::new();
        // We need to iterate that's sentitive to our mutations so we have to implement this a little
        // manually
        let mut current = node.first_child();
        while let Some(node) = current {
            if let Some(data) = node.as_element() {
                match &*data.name.local {
                    "a" if self.strip_links => {
                        while let Some(child) = node.last_child() {
                            node.insert_after(child);
                        }
                        current = next_node_skip(&node);
                        node.detach();
                    }
                    "img" | "picture" => {
                        if self.image_handling != ImageHandling::Strip {
                            // find best image match
                            let mut matched = None;
                            for dec in node.inclusive_descendants() {
                                if let Some(dec_dat) = dec.as_element() {
                                    let attrs = dec_dat.attributes.borrow();
                                    if let Some(src) = attrs.get("src") {
                                        matched =
                                            std::cmp::max(matched, self.find_url(&image_data, src));
                                    }
                                    if let Some(srcset) = attrs.get("srcset") {
                                        for src in srcset.split(',') {
                                            matched = std::cmp::max(
                                                matched,
                                                self.find_url(&image_data, src.trim()),
                                            );
                                        }
                                    }
                                }
                            }
                            // if match, save it to epub and replace
                            if let Some((_, url, mime, img)) = matched {
                                let num = images.len();
                                let path = match (images.entry(url), self.image_handling) {
                                    (Entry::Vacant(ent), _) => {
                                        let trans = self
                                            .transform
                                            .transform(img, format!("image/{}", mime));
                                        let name = match trans {
                                            Some((image, fmt)) => {
                                                let name = format!("image_{num}.{}", fmt.ext());
                                                epub.add_resource(&name, image, fmt.mime())?;
                                                Some(name)
                                            }
                                            None => None,
                                        };
                                        ent.insert(name).as_ref()
                                    }
                                    (_, ImageHandling::Filter) => None, // already rendered, so filter
                                    (Entry::Occupied(ent), _) => ent.into_mut().as_ref(),
                                };
                                // if path is good insert simple image
                                if let Some(image_path) = path {
                                    node.insert_before(new_elem(
                                        "img",
                                        // TODO preserve alt
                                        [(ns!(), None, "src", image_path)],
                                        [],
                                    ));
                                }
                            }
                        }
                        // always remove original node
                        current = next_node_skip(&node);
                        node.detach();
                    }
                    _ => {
                        // other element
                        current = next_node(&node);
                    }
                }
            } else {
                // not element
                current = next_node(&node);
            }
        }

        // create content
        let body_node = new_attrless_elem("body", []);
        // add url heading
        if self.include_url {
            if let Some(url) = loc {
                body_node.append(new_elem(
                    "a",
                    [(ns!(), None, "href", url.as_ref())],
                    [NodeRef::new_text(url.as_ref())],
                ));
            }
        }
        // add title
        if self.include_title {
            if let Some(title) = title {
                body_node.append(new_attrless_elem("h1", [NodeRef::new_text(title)]));
            }
        }
        // add byline
        if self.include_byline {
            if let Some(byline) = &meta.byline {
                body_node.append(new_elem(
                    "address",
                    [(ns!(), None, "style", "font-style: italic")],
                    [NodeRef::new_text(byline)],
                ));
            }
        }
        // add cover image, only Some if requested
        if let Some(src) = cover_img {
            body_node.append(new_elem(
                "div",
                [(ns!(), None, "style", "margin-top: 1em")],
                [new_elem("img", [(ns!(), None, "src", src)], [])],
            ));
        }
        // append content stripping body tag
        if node
            .as_element()
            .map(|data| &*data.name.local == "body")
            .unwrap_or(true)
        {
            while let Some(child) = node.first_child() {
                body_node.append(child);
            }
        } else {
            body_node.append(node);
        }
        // add head
        let head_node = new_attrless_elem(
            "head",
            [
                new_elem(
                    "meta",
                    [
                        (ns!(), None, "http-equiv", "Content-Type"),
                        (
                            ns!(),
                            None,
                            "content",
                            "application/xhtml+xml; charset=utf-8",
                        ),
                    ],
                    [],
                ),
                new_elem(
                    "link",
                    [
                        (ns!(), None, "type", "text/css"),
                        (ns!(), None, "rel", "stylesheet"),
                        (ns!(), None, "href", "stylesheet.css"),
                    ],
                    [],
                ),
            ],
        );
        if let Some(title) = title {
            head_node.insert_after(new_attrless_elem("title", [NodeRef::new_text(title)]))
        }
        // create html body
        let html_node = new_elem(
            "html",
            [
                (ns!(xmlns), None, "xmlns", "http://www.w3.org/1999/xhtml"),
                (
                    ns!(xmlns),
                    Some("xmlns".into()),
                    "epub",
                    "http://www.w3.org/1999/xhtml",
                ),
            ],
            [head_node, body_node],
        );
        // create document
        let document = NodeRef::new_document();
        document.append(NodeRef::new_doctype(r#"html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd""#, "", ""));
        document.append(html_node);

        // actually append content
        // NOTE kuchiki doesn't appent the trailing xml ?> properly so this is encoded manually
        let mut content: Vec<_> = r#"<?xml version="1.0" encoding="UTF-8"?>"#.as_bytes().into();
        document.serialize(&mut content).unwrap();
        trace!("full html: {}", std::str::from_utf8(&content).unwrap());

        epub.add_content(
            EpubContent::new("article.xhtml", &*content)
                .title(title.unwrap_or("[missing title]"))
                .reftype(ReferenceType::Text),
        )?;

        // add reamining options and serialize
        epub.stylesheet(self.css.as_ref().as_bytes())?;
        epub.epub_version(self.epub_version);
        epub.generate(out).or(Err(Error::EpubWritingError))?;
        Ok(title.map(str::to_string))
    }
}

#[cfg(test)]
#[cfg(feature = "image")]
mod tests {
    use super::{EpubVersion, FilterType, ImageHandling, ImageFormat, ImgTransform, Repub};
    use base64::engine::general_purpose::STANDARD;
    use base64::Engine;
    use epub::doc::EpubDoc;
    use image::DynamicImage;
    use std::io::{Cursor, Seek, Write};

    fn create_mhtml(
        doc: impl AsRef<str>,
        loc: impl AsRef<str>,
        title: impl AsRef<str>,
        images: impl IntoIterator<Item = impl AsRef<str>>,
    ) -> String {
        let mut img = Cursor::new(Vec::new());
        DynamicImage::new_rgb8(1, 1)
            .write_to(&mut img, image::ImageFormat::Png)
            .unwrap();
        let img_str = STANDARD.encode(img.into_inner());

        let mut res = Vec::new();
        writeln!(
            res,
            r#"From: <Saved by Blink>
Snapshot-Content-Location: {loc}
Subject: {title}
Date: Sat, 7 Jan 2023 20:59:18 -0000
MIME-Version: 1.0
Content-Type: multipart/related;
	type="text/html";
	boundary="----multipart-boundary----"


------multipart-boundary----
Content-Type: text/html
Content-ID: <frame-0@mhtml.blink>
Content-Transfer-Encoding: quoted-printable
Content-Location: {loc}
"#,
            loc = loc.as_ref(),
            title = title.as_ref(),
        )
        .unwrap();
        res.write(&quoted_printable::encode(doc.as_ref().as_bytes()))
            .unwrap();

        for img in images {
            writeln!(
                res,
                "------multipart-boundary----
Content-Type: image/png
Content-Transfer-Encoding: base64
Content-Location: {}
",
                img.as_ref(),
            )
            .unwrap();
            for line in img_str.as_bytes().chunks(76) {
                res.write(line).unwrap();
                writeln!(res).unwrap();
            }
        }

        writeln!(res, "------multipart-boundary------").unwrap();
        String::from_utf8(res).unwrap()
    }

    #[test]
    fn no_images() {
        let images: [&'static str; 0] = [];
        let mhtml = create_mhtml(
            r#"<!doctype html><html><head></head><body><div><p>text</p><img src="img.png" alt="info"><p>more text</p></body></html>"#,
            "https://test.html",
            "a fake doc",
            images,
        );
        let mut buff = Cursor::new(Vec::new());
        Repub::default().mhtml_to_epub(&mhtml, &mut buff).unwrap();
        buff.rewind().unwrap();
        let mut doc = EpubDoc::from_reader(&mut buff).unwrap();
        assert_eq!(*doc.metadata.get("title").unwrap(), ["a fake doc"]);
        let (contents, _) = doc.get_current_str().unwrap();
        assert!(contents.contains("<p>text</p><p>more text</p>"),);
    }

    #[test]
    #[cfg(feature = "image")]
    fn options() {
        let mhtml = create_mhtml(
            r#"<!doctype html><html><head></head><body><div><p>text</p><img src="close_img.png" alt="info"><p>more text</p></body></html>"#,
            "https://test.html",
            "a fake doc",
            ["img.png"],
        );
        let mut buff = Cursor::new(Vec::new());
        Repub {
            include_url: true,
            include_title: true,
            include_byline: true,
            include_cover: true,
            strip_links: true,
            href_sim_thresh: 1.0,
            image_handling: ImageHandling::Keep,
            css: "div { margin: 1em }",
            transform: ImgTransform {
                brightness: 1.2,
                max_width: 100,
                max_height: 100,
                filter_type: FilterType::CatmullRom,
                output_format: ImageFormat::Jpeg,
            },
            epub_version: EpubVersion::V20,
        }
        .mhtml_to_epub(&mhtml, &mut buff)
        .unwrap();
        buff.rewind().unwrap();
        let mut doc = EpubDoc::from_reader(&mut buff).unwrap();
        assert_eq!(*doc.metadata.get("title").unwrap(), ["a fake doc"]);
        assert_eq!(
            doc.resources.get("stylesheet.css"),
            Some(&("OEBPS/stylesheet.css".into(), "text/css".into()))
        );
        let (css, _) = doc.get_resource_str("stylesheet.css").unwrap();
        assert_eq!(css, "div { margin: 1em }");
        let (contents, _) = doc.get_current_str().unwrap();
        eprintln!("{}", contents);
        assert!(contents.contains(r#"<?xml version="1.0" encoding="UTF-8"?>"#));
        assert!(contents.contains(r#"<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">"#));
        assert!(contents.contains(r#"<html xmlns:epub="http://www.w3.org/1999/xhtml" xmlns="http://www.w3.org/1999/xhtml">"#));
        assert!(contents
            .contains(r#"<a href="https://test.html">https://test.html</a><h1>a fake doc</h1>"#));
        assert!(contents.contains(r#"<p>text</p><img src="image_0.jpg"></img><p>more text</p>"#));
        assert!(contents
            .contains(r#"<link href="stylesheet.css" rel="stylesheet" type="text/css"></link>"#));
    }
}
