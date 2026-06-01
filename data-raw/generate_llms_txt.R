# Generate `llms.txt` and `llms-full.txt` from the package documentation.
#
# These files expose the tidyfinance API to LLM coding agents following the
# llms.txt convention (https://llmstxt.org). They are plain text and live on
# their own -- they do NOT require a pkgdown/Quarto site. The convention is to
# serve them at a site root (e.g. https://www.tidy-finance.org/r/llms.txt), but
# they work equally well from the repository root or shipped inside the package.
#
# `llms.txt`       -- a compact, grouped index of every exported function with a
#                     one-line description and a link to its annotated source.
# `llms-full.txt`  -- full inline reference (title, description, usage,
#                     arguments, value, examples) for every exported function.
#
# Run from the package root with:  Rscript data-raw/generate_llms_txt.R
# Re-run whenever the documentation in man/*.Rd changes.

repo   <- "tidy-finance/r-tidyfinance"
branch <- "main"

# ---- small Rd parsing helpers (base R only) --------------------------------

# Return the content of the first `\tag{...}` block, NULL if absent. Handles
# nested braces.
find_block <- function(txt, tag) {
  m <- regexpr(paste0("\\\\", tag, "\\{"), txt)
  if (m == -1L) return(NULL)
  brace_after(txt, m + attr(m, "match.length") - 1L)$content
}

# Return the content of every `\tag{...}` block as a character vector.
find_all <- function(txt, tag) {
  out <- character(0)
  pat <- paste0("\\\\", tag, "\\{")
  pos <- 1L
  repeat {
    sub <- substring(txt, pos)
    m <- regexpr(pat, sub)
    if (m == -1L) break
    open <- pos + m + attr(m, "match.length") - 2L
    res <- brace_after(txt, open)
    out <- c(out, res$content)
    pos <- res$end
  }
  out
}

# Given the index of an opening `{`, return list(content, end) where `end` is
# the index just after the matching `}`.
brace_after <- function(txt, open_idx) {
  n <- nchar(txt)
  i <- open_idx + 1L
  depth <- 1L
  start <- i
  while (i <= n && depth > 0L) {
    c <- substr(txt, i, i)
    if (c == "{") depth <- depth + 1L
    else if (c == "}") {
      depth <- depth - 1L
      if (depth == 0L) break
    }
    i <- i + 1L
  }
  list(content = substr(txt, start, i - 1L), end = i + 1L)
}

# Resolve `\ifelse{html}{a}{b}` -> b, then `\href{url}{text}` -> text,
# drop `\figure{...}`, unwrap single-argument formatting macros, and unescape
# Rd specials. Leaves plain text suitable for Markdown.
strip_inline <- function(s) {
  if (is.null(s) || !nzchar(s)) return("")

  repl_two <- function(t, tag, keep) {
    pat <- paste0("\\\\", tag, "\\{")
    repeat {
      m <- regexpr(pat, t)
      if (m == -1L) break
      a <- brace_after(t, m + attr(m, "match.length") - 2L)
      if (a$end - 1L <= nchar(t) && substr(t, a$end - 1L, a$end - 1L) == "{") {
        b <- brace_after(t, a$end - 1L)
        rep <- switch(keep, first = a$content, second = b$content, "")
        t <- paste0(substr(t, 1L, m - 1L), rep, substring(t, b$end))
      } else {
        rep <- switch(keep, first = a$content, "")
        t <- paste0(substr(t, 1L, m - 1L), rep, substring(t, a$end))
      }
    }
    t
  }
  # `\ifelse{html}{html-branch}{text-branch}` -> text-branch
  repeat {
    m <- regexpr("\\\\ifelse\\{", s)
    if (m == -1L) break
    cond <- brace_after(s, m + attr(m, "match.length") - 2L)
    yes  <- brace_after(s, cond$end - 1L)
    no   <- brace_after(s, yes$end - 1L)
    s <- paste0(substr(s, 1L, m - 1L), no$content, substring(s, no$end))
  }
  s <- repl_two(s, "href", "second")
  s <- repl_two(s, "figure", "none")

  single <- c("code", "link", "emph", "strong", "verb", "pkg", "dQuote",
              "sQuote", "eqn", "samp", "file", "env", "option", "var",
              "command", "url", "cite", "email", "acronym", "donttest",
              "dontrun", "R")
  for (tag in single) {
    pat <- paste0("\\\\", tag, "\\{")
    repeat {
      m <- regexpr(pat, s)
      if (m == -1L) break
      inner <- brace_after(s, m + attr(m, "match.length") - 2L)
      s <- paste0(substr(s, 1L, m - 1L), inner$content, substring(s, inner$end))
    }
  }
  s <- gsub("\\\\link\\[[^]]*\\]", "", s)
  s <- gsub("\\\\dots|\\\\ldots", "...", s)
  s <- gsub("(?<!\\\\)%.*", "", s, perl = TRUE)       # Rd comments, not \%
  s <- gsub("\\\\%", "%", s)
  s <- gsub("\\\\\\{", "{", s); s <- gsub("\\\\\\}", "}", s)
  s <- gsub("\\\\&", "&", s)
  s <- gsub("[ \t]+", " ", s)
  trimws(s)
}

# Parse `\item{name}{description}` pairs out of an \arguments block.
parse_items <- function(block) {
  items <- list()
  pos <- 1L
  repeat {
    sub <- substring(block, pos)
    m <- regexpr("\\\\item\\{", sub)
    if (m == -1L) break
    open <- pos + m + attr(m, "match.length") - 2L
    nm <- brace_after(block, open)
    j <- nm$end - 1L
    while (j <= nchar(block) && substr(block, j, j) %in% c(" ", "\t", "\n")) j <- j + 1L
    if (j <= nchar(block) && substr(block, j, j) == "{") {
      ds <- brace_after(block, j)
    } else {
      ds <- list(content = "", end = nm$end)
    }
    items[[length(items) + 1L]] <- list(
      name = strip_inline(nm$content), desc = strip_inline(ds$content)
    )
    pos <- ds$end
  }
  items
}

strip_comments_code <- function(s) {
  s <- gsub("(?<!\\\\)%.*", "", s, perl = TRUE)
  gsub("\\\\%", "%", s)
}

# ---- read package metadata --------------------------------------------------

d <- read.dcf("DESCRIPTION")
pkg     <- d[1, "Package"]
title   <- gsub("\\s+", " ", d[1, "Title"])
pkgdesc <- gsub("\\s+", " ", d[1, "Description"])

# ---- parse all man/*.Rd -----------------------------------------------------

rd_files <- sort(list.files("man", pattern = "\\.Rd$", full.names = TRUE))
funcs <- lapply(rd_files, function(f) {
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  name <- find_block(txt, "name")
  if (is.null(name)) return(NULL)
  src <- regmatches(txt, regexpr("edit documentation in (\\S+)", txt))
  src <- if (length(src)) sub("edit documentation in ", "", src) else NA_character_
  concept <- find_all(txt, "concept")
  list(
    name     = trimws(name),
    src      = src,
    concept  = if (length(concept)) concept[1] else NA_character_,
    title    = strip_inline(find_block(txt, "title")),
    desc     = find_block(txt, "description"),
    usage    = find_block(txt, "usage"),
    args     = find_block(txt, "arguments"),
    value    = find_block(txt, "value"),
    examples = find_block(txt, "examples")
  )
})
funcs <- Filter(Negate(is.null), funcs)
funcs <- Filter(function(x) x$name != paste0(pkg, "-package"), funcs)

# ---- section ordering / titles ---------------------------------------------

section_titles <- c(
  "download functions"            = "Download functions",
  "WRDS functions"                = "WRDS functions",
  "pseudo functions"              = "Pseudo-data functions (no credentials required)",
  "estimation functions"          = "Estimation functions",
  "portfolio functions"           = "Portfolio sorting functions",
  "rolling and lagging functions" = "Rolling and lagging functions",
  "utility functions"             = "Utility and helper functions"
)

url_for <- function(fn) {
  if (!is.na(fn$src)) paste0("https://github.com/", repo, "/blob/", branch, "/", fn$src)
  else paste0("https://github.com/", repo)
}

by_name <- function(fns) fns[order(vapply(fns, function(x) x$name, ""))]

groups <- setNames(vector("list", length(section_titles)), names(section_titles))
other <- list()
for (fn in funcs) {
  c <- fn$concept
  if (!is.na(c) && c %in% names(groups)) groups[[c]] <- c(groups[[c]], list(fn))
  else other <- c(other, list(fn))
}

# ---- write llms.txt ---------------------------------------------------------

out <- c(
  paste0("# ", pkg), "",
  paste0("> ", title, ". ", pkgdesc), "",
  paste0(pkg, " is an R package of helper functions for empirical research in ",
         "financial economics, accompanying the book Tidy Finance with R ",
         "(Scheuch, Voigt, and Weiss, 2023). Each link points to the function's ",
         "annotated source on GitHub. For complete inline reference (signatures, ",
         "arguments, return values, and runnable examples) see llms-full.txt in ",
         "the same location."), ""
)
for (key in names(section_titles)) {
  fns <- groups[[key]]
  if (length(fns) == 0) next
  out <- c(out, paste0("## ", section_titles[[key]]), "")
  for (fn in by_name(fns)) {
    out <- c(out, paste0("- [", fn$name, "](", url_for(fn), "): ", fn$title))
  }
  out <- c(out, "")
}
if (length(other)) {
  out <- c(out, "## Other", "")
  for (fn in by_name(other)) {
    out <- c(out, paste0("- [", fn$name, "](", url_for(fn), "): ", fn$title))
  }
  out <- c(out, "")
}
out <- c(out,
  "## Optional", "",
  paste0("- [Package overview](https://github.com/", repo,
         "): README, installation, and contribution guide"),
  "- [Tidy Finance with R (book)](https://www.tidy-finance.org/r/): full methodology and chapter-by-chapter explanations",
  "- [llms-full.txt](llms-full.txt): full inline documentation for every exported function"
)
writeLines(out, "llms.txt")

# ---- write llms-full.txt ----------------------------------------------------

full <- c(
  paste0("# ", pkg, " — full function reference"), "",
  paste0("> ", title, ". ", pkgdesc), "",
  "Generated from the package documentation (man/*.Rd). One section per exported function.", ""
)
ordered <- unlist(lapply(names(section_titles), function(k) by_name(groups[[k]])), recursive = FALSE)
ordered <- c(ordered, by_name(other))
for (fn in ordered) {
  full <- c(full, paste0("## ", fn$name), "", fn$title, "")
  dd <- strip_inline(fn$desc)
  if (nzchar(dd)) full <- c(full, dd, "")
  if (!is.null(fn$usage)) {
    u <- trimws(strip_comments_code(fn$usage))
    full <- c(full, "Usage:", "```r", u, "```", "")
  }
  if (!is.null(fn$args)) {
    items <- parse_items(fn$args)
    if (length(items)) {
      full <- c(full, "Arguments:")
      for (it in items) full <- c(full, paste0("- `", it$name, "`: ", it$desc))
      full <- c(full, "")
    }
  }
  if (!is.null(fn$value)) {
    v <- strip_inline(fn$value)
    if (nzchar(v)) full <- c(full, paste0("Value: ", v), "")
  }
  if (!is.null(fn$examples)) {
    ex <- strip_comments_code(fn$examples)
    for (tag in c("donttest", "dontrun")) {
      pat <- paste0("\\\\", tag, "\\{")
      repeat {
        m <- regexpr(pat, ex)
        if (m == -1L) break
        inner <- brace_after(ex, m + attr(m, "match.length") - 2L)
        ex <- paste0(substr(ex, 1L, m - 1L), inner$content, substring(ex, inner$end))
      }
    }
    ex <- trimws(ex)
    if (nzchar(ex)) full <- c(full, "Examples:", "```r", ex, "```", "")
  }
  full <- c(full, paste0("Source: ", url_for(fn)), "")
}
writeLines(full, "llms-full.txt")

message("Wrote llms.txt and llms-full.txt for ", length(ordered), " functions.")
