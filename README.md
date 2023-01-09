repub-rs
========

A rust library for converting mhtml webpages to epub articles.

To Do
-----

- [ ] The current version of epub builder is a pinned githash which is not
  ideal, but we need version 0.6 for wasm to work. Therefore this can't be
  published to crates.io until epub-builder is also updated.
- [ ] Handling of byline and title is a bit manual, and it'd be better to
  expose more of those properties.
