use clap::{Parser, Subcommand, ValueEnum};
use repub::{EpubVersion, FilterType, ImageHandling, Repub};
use std::io;

#[derive(Debug, ValueEnum, Clone, Copy, PartialEq, Eq)]
enum Images {
    /// Remove all images
    Strip,
    /// Keep first occurance of each image
    Filter,
    /// Keep all images
    Keep,
}

impl From<Images> for ImageHandling {
    fn from(inp: Images) -> Self {
        match inp {
            Images::Strip => ImageHandling::Strip,
            Images::Filter => ImageHandling::Filter,
            Images::Keep => ImageHandling::Keep,
        }
    }
}

/// convert a full mhtml into a summarized epub article
#[derive(Debug, Parser)]
struct Cli {
    #[command(subcommand)]
    style: Option<Style>,
}

#[derive(Debug, Subcommand)]
enum Style {
    /// Use default reMarkable configuration
    Remarkable,
    /// Customize all aspects of configuration
    Custom {
        /// Attach the url to the start of the article
        #[arg(long)]
        include_url: bool,

        /// Attach the title to the start of the article
        #[arg(long)]
        include_title: bool,

        /// Attach the byline to the start of the article
        #[arg(long)]
        include_byline: bool,

        /// Attach the coverage image to the start of the article
        #[arg(long)]
        include_cover: bool,

        /// Remove links, preserving the underlying text
        #[arg(long)]
        strip_links: bool,

        /// Remove nested divs, flattening the structure
        #[arg(long)]
        flatten_divs: bool,

        /// Match image urls that are at least this similar
        ///
        /// 1.0 indicates only exact matches count. Lower numbers will be more lenient but still
        /// select the closet url.
        #[arg(long, default_value_t = 1.0)]
        href_sim_thresh: f64,

        /// Brighten images
        ///
        /// 1.0 indicates no brightening. Slightly larger will result in mild brightening, slightly
        /// lower will result in some darkening.
        #[arg(long)]
        brighten: f32,

        /// Create a V3.0 epub
        #[arg(long)]
        epub_v3: bool,

        /// Attach custom css
        #[arg(short, long)]
        css: Option<String>,

        /// Specify how to handle images
        #[arg(short, long, value_enum, default_value_t = Images::Strip)]
        images: Images,

        /// Images wider than this will be resized
        #[arg(long)]
        max_width: Option<u32>,

        /// Images longer than this will be resized
        #[arg(long)]
        max_height: Option<u32>,
    },
}

pub fn main() {
    let args = Cli::parse();
    let mhtml = io::read_to_string(io::stdin()).unwrap();
    match args.style {
        None | Some(Style::Remarkable) => {
            Repub::remarkable().mhtml_to_epub(mhtml, &mut io::stdout().lock())
        }
        Some(Style::Custom {
            include_url,
            include_title,
            include_byline,
            include_cover,
            strip_links,
            flatten_divs,
            href_sim_thresh,
            brighten,
            epub_v3,
            images,
            max_width,
            max_height,
            css,
        }) => Repub {
            include_url,
            include_title,
            include_byline,
            include_cover,
            strip_links,
            flatten_divs,
            href_sim_thresh,
            image_handling: images.into(),
            css: css.unwrap_or_else(|| "".into()),
            max_width: max_width.unwrap_or(u32::MAX),
            max_height: max_height.unwrap_or(u32::MAX),
            filter_type: FilterType::Triangle,
            brighten,
            epub_version: if epub_v3 {
                EpubVersion::V30
            } else {
                EpubVersion::V20
            },
        }
        .mhtml_to_epub(mhtml, &mut io::stdout().lock()),
    }
    .unwrap();
}
