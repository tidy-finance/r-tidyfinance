# Repository guidelines for Claude

## README

- **Never edit `README.md` directly.** It is generated from `README.Rmd`
  (the file header says so: *"README.md is generated from README.Rmd"*).
- If a change should appear in the README, edit `README.Rmd` and regenerate
  `README.md` (e.g. `devtools::build_readme()`), then commit **both** files
  together. A `README.md` change must never be committed without the
  corresponding `README.Rmd` change.

## Versioning

- Bump the development version **once per pull request, not per commit.**
- The development version lives in the `Version:` field of `DESCRIPTION`; it is
  the fourth, `.9000`-style component (e.g. `0.5.0.9005` -> `0.5.0.9006`).
  Increment that component by one for the PR.
