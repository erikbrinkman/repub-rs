use super::{ImageFormat, ImageTransform};
pub use image::imageops::FilterType;
use image::io::Reader;
pub use image::ImageFormat as ImgFormat;
use image::{DynamicImage, Pixel};
use std::io::{Cursor, Seek};

/// brighten a dynamic image
fn brighten(img: DynamicImage, factor: f32) -> DynamicImage {
    let mut rgba = img.into_rgba8();
    let inv = ((1 << 8) as f32 / factor) as u16;
    for y in 0..rgba.height() {
        for x in 0..rgba.width() {
            let &(mut val) = rgba.get_pixel(x, y);
            val.apply_without_alpha(|c| u8::MAX - (((u8::MAX - c) as u16 * inv) >> 8) as u8);
            rgba.put_pixel(x, y, val);
        }
    }
    DynamicImage::ImageRgba8(rgba)
}

/// create a transform using the image library
///
/// This transform supports tweaking the brightness, rescaling, and outputing in an arbitrary
/// format.
pub struct ImgTransform {
    /// how much to brighten the image
    pub brightness: f32,
    /// the maximum width post transform
    pub max_width: u32,
    /// the maximum height post transform
    pub max_height: u32,
    /// how to fillter when resizing the image
    pub filter_type: FilterType,
    /// what format to output the image in
    pub output_format: ImageFormat,
}

impl ImageTransform for ImgTransform {
    type Output<'a> = Cursor<Vec<u8>>;

    // FIXME update this to return errors too
    fn transform<'a, S: AsRef<str>>(
        &self,
        bytes: &'a [u8],
        mime: S,
    ) -> Option<(Self::Output<'a>, ImageFormat)> {
        // FIXME make this name more general, and add an option to filter out all black or all
        // white images. Will want an example of when this happens.
        let cursor = Cursor::new(bytes);
        // use mime, but still try to guess from data
        let img = match image::ImageFormat::from_mime_type(mime) {
            Some(fmt) => Reader::with_format(cursor, fmt),
            None => Reader::new(cursor),
        };
        let mut img = img.with_guessed_format().ok()?.decode().ok()?;
        if img.width() > self.max_width || img.height() > self.max_height {
            img = img.resize(self.max_width, self.max_height, self.filter_type)
        };
        let img = if self.brightness == 1.0 {
            img
        } else {
            brighten(img, self.brightness)
        };
        let mut buff = Cursor::new(Vec::new());
        let out_fmt = match &self.output_format {
            ImageFormat::Jpeg => ImgFormat::Jpeg,
            ImageFormat::Png => ImgFormat::Png,
        };
        // NOTE we need to convert this into an image without alpha before saving as jpeg
        img.into_rgb8().write_to(&mut buff, out_fmt).unwrap();
        // img.write_to(&mut buff, out_fmt).ok()?;
        buff.rewind().ok()?;
        Some((buff, self.output_format))
    }
}
