# ============================================================================
# Reproducible Academic CV pipeline
# ----------------------------------------------------------------------------
# Purpose : Turn three plain source files (cv_data.xlsx, profile.yml,
#           my_publications.bib) into a fully formatted Typst document that
#           uses the `pro-academic-cv` template (Typst Universe), rendered
#           through Quarto (see cv.qmd).
#
# Flow    : cv_load()    -> reads + tidies the three sources
#           section_*()  -> each CV section as a Typst markup string
#           cat_typst()  -> prints a section wrapped in a raw ```{=typst}```
#                           block so that Quarto/pandoc passes it through
#                           verbatim to the Typst compiler
#
# Layout  : Every CV entry is emitted as an unbreakable Typst block, and each
#           section heading is grouped with its first entry, so entries never
#           split across pages and headings are never orphaned at a page
#           bottom.
#
# R deps  : readxl, yaml (everything else is base R)
# ============================================================================

## 0. Configuration ----------------------------------------------------------
cv_config <- function(...) {
  cfg <- list(
    font_family      = "Corbel",
    font_size        = "10pt",
    author_font_size = "25pt",
    prio_experience     = 10L, 
    prio_courses        = 7L,  
    prio_supervision    = 7L,  
    prio_services       = 7L,  
    prio_qualifications = 10L, 
    prio_ce             = 10L, 
    show_supervisees = TRUE,   
    pub_status_override = c(),
    pub_legend = "P = Published, U = Unpublished"
  )
  utils::modifyList(cfg, list(...))
}

## 1. Small helpers ----------------------------------------------------------
as_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # Strip hidden Excel newlines/carriage returns to prevent premature line breaks 
  # and guarantee perfect right-edge justification
  x <- gsub("[\r\n]+", " ", x)
  trimws(x)
}

as_prio <- function(x) suppressWarnings(as.numeric(as_chr(x)))

prio_eq <- function(df, threshold) {
  p <- as_prio(df$priority); df[!is.na(p) & p == threshold, , drop = FALSE]
}
prio_gt <- function(df, threshold) {
  p <- as_prio(df$priority); df[!is.na(p) & p > threshold, , drop = FALSE]
}
prio_gte <- function(df, threshold) {
  p <- as_prio(df$priority); df[!is.na(p) & p >= threshold, , drop = FALSE]
}

esc_typst <- function(x) {
  x <- as_chr(x)
  from <- c("\\", "#", "*", "_", "`", "$", "[", "]", "~", "@", "<", ">")
  to   <- c("\\\\", "\\#", "\\*", "\\_", "\\`", "\\$", "\\[", "\\]", "\\~", "\\@", "\\<", "\\>")
  for (i in seq_along(from)) x <- gsub(from[i], to[i], x, fixed = TRUE)
  x
}

esc_code_str <- function(x) {
  x <- gsub("\\", "\\\\", as_chr(x), fixed = TRUE)
  gsub('"', '\\"', x, fixed = TRUE)
}

ct <- function(x) sprintf("[%s]", x)

# Force the hyperlink text color here to ensure it applies to headers and DOIs alike
lnk <- function(url, label, icon = FALSE) {
  lab <- if (icon) sprintf("#link-icon() %s", label) else label
  sprintf('#link("%s")[#text(fill: rgb("005cb2"))[%s]]', esc_code_str(url), lab)
}

MONTH_ABBR <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

parse_date_robust <- function(x) {
  x <- as_chr(x)
  out <- as.Date(rep(NA_character_, length(x)))
  y  <- grepl("^\\d{4}$", x)
  ym <- grepl("^\\d{4}-\\d{2}$", x)
  ymd <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(y))   out[y]   <- as.Date(paste0(x[y], "-01-01"))
  if (any(ym))  out[ym]  <- as.Date(paste0(x[ym], "-01"))
  if (any(ymd)) out[ymd] <- as.Date(x[ymd])
  out
}

fmt_month_year <- function(x, x_parsed = NULL) {
  if (length(x) == 0 || !nzchar(as_chr(x))) return("")
  p <- if (is.null(x_parsed)) parse_date_robust(x) else x_parsed
  if (is.na(p)) return(as_chr(x))
  if (nchar(as_chr(x)) <= 4) return(format(p, "%Y"))
  sprintf("%s %d", MONTH_ABBR[as.integer(format(p, "%m"))], as.integer(format(p, "%Y")))
}

fmt_range <- function(start, end) {
  s <- fmt_month_year(start)
  e <- fmt_month_year(end)
  if (!nzchar(e)) e <- "Present"
  if (nzchar(s) && nzchar(e)) paste(s, e, sep = " \u2013 ") else paste0(s, e)
}

plural <- function(n, word) sprintf("%d %s", n, if (n == 1) word else paste0(word, "s"))

## 2. Source readers ---------------------------------------------------------
cv_load <- function(data_dir, cfg = cv_config()) {
  stopifnot(dir.exists(data_dir))
  xl_path <- file.path(data_dir, "cv_data.xlsx")
  if (!file.exists(xl_path))
    stop("cv_data.xlsx not found under '", data_dir, "'.")
  
  sheets <- c("organizations", "appointments", "courses", "teaching",
              "research_supervision", "academic_services",
              "academic_qualifications", "professional_development",
              "continuous_education")
  xl <- lapply(sheets, function(s) as.data.frame(readxl::read_excel(xl_path, sheet = s)))
  names(xl) <- sheets
  
  list(
    profile = yaml::read_yaml(file.path(data_dir, "profile.yml")),
    xl      = xl,
    pubs    = parse_bib(file.path(data_dir, "my_publications.bib"))
  )
}

## -- Minimal BibTeX reader (base R only) ------------------------------------
split_top_level <- function(s) {
  chars <- strsplit(s, "", useBytes = FALSE)[[1]]
  depth <- 0L; in_q <- FALSE; start <- 1L
  out <- character(0)
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "{") depth <- depth + 1L
    else if (ch == "}") depth <- depth - 1L
    else if (ch == '"') in_q <- !in_q
    if (ch == "," && depth == 0L && !in_q) {
      out <- c(out, substring(s, start, i - 1L))
      start <- i + 1L
    }
  }
  c(out, substring(s, start, length(chars)))
}

clean_bib_value <- function(v) {
  v <- trimws(v)
  v <- sub("^\\{(.*)\\}$", "\\1", v)
  v <- sub('^"(.*)"$', "\\1", v)
  v <- gsub("\\\\&", "&", v)
  repeat {
    nv <- gsub("\\{([^{}]*)\\}", "\\1", v)
    if (identical(nv, v)) break
    v <- nv
  }
  trimws(v)
}

parse_bib <- function(path) {
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  starts <- gregexpr("@[A-Za-z]+[ \t\r\n]*\\{", txt)[[1]]
  if (starts[1] < 0) stop("No BibTeX entries found in ", path)
  n <- length(starts)
  
  fields <- c("author", "title", "year", "journal", "volume", "number",
              "pages", "doi", "url", "school", "publisher", "type", "month")
  res <- as.data.frame(matrix("", nrow = n, ncol = 2 + length(fields)),
                       stringsAsFactors = FALSE)
  names(res) <- c("key", "category", fields)
  
  for (i in seq_len(n)) {
    s <- starts[i]
    e <- if (i < n) starts[i + 1] - 1L else nchar(txt)
    body <- substr(txt, s, e)
    m <- regmatches(body, regexpr("^@[A-Za-z]+[ \t\r\n]*\\{[^,]+,", body))[[1]]
    res$category[i] <- tolower(sub("^@([A-Za-z]+).*", "\\1", m))
    res$key[i]       <- trimws(sub("^@[A-Za-z]+[ \t\r\n]*\\{([^,]+),.*", "\\1", m))
    inner <- substr(body, nchar(m) + 1L, nchar(body))
    inner <- trimws(sub("\\}[\\s,]*$", "", inner))
    chunks <- split_top_level(inner)
    for (ch in chunks) {
      fm <- regmatches(ch, regexpr("^[^=]+=", ch))
      if (!length(fm)) next
      name <- tolower(trimws(sub("=$", "", fm[[1]])))
      if (name %in% fields)
        res[i, name] <- clean_bib_value(substr(ch, nchar(fm[[1]]) + 1L, nchar(ch)))
    }
  }
  res$year <- suppressWarnings(as.integer(res$year))
  res
}

format_authors <- function(x) {
  x <- as_chr(x)
  if (!nzchar(x)) return("")
  people <- trimws(strsplit(x, " and ", fixed = TRUE)[[1]])
  fmt_one <- function(p) {
    if (grepl(",", p, fixed = TRUE)) {
      lp <- trimws(strsplit(p, ",", fixed = TRUE)[[1]])
      last <- lp[1]; given <- if (length(lp) > 1) lp[2] else ""
    } else {
      ws <- trimws(strsplit(p, "\\s+")[[1]])
      last <- if (length(ws)) ws[length(ws)] else p
      given <- if (length(ws) > 1) paste(ws[-length(ws)], collapse = " ") else ""
    }
    if (!nzchar(given)) return(last)
    gv <- strsplit(given, "[\\s-]+")[[1]]
    inits <- paste0(substr(gv, 1L, 1L), ".", collapse = " ")
    paste0(last, ", ", inits)
  }
  out <- vapply(people, fmt_one, character(1), USE.NAMES = FALSE)
  n <- length(out)
  if (n == 1) out
  else if (n == 2) paste(out, collapse = " & ")
  else paste0(paste(out[-n], collapse = ", "), ", & ", out[n])
}

## 3. Typst emission helpers -------------------------------------------------
cat_typst <- function(x) {
  cat("```{=typst}\n", x, "\n```\n", sep = "")
}

h2 <- function(title, note = NULL) {
  if (!is.null(note)) sprintf("== %s (note:%s)", title, note) else sprintf("== %s", title)
}
h3 <- function(title) sprintf("=== %s", title)

# Applies inset natively to the block to ensure flawless, edge-to-edge right justification
desc_block <- function(text, above = "0.6em") {
  paste0('#block(width: 100%, above: ', above, ', inset: (left: 1.5em))[\n#set par(justify: false)\n#text(size: 0.95em, fill: rgb("2d3748"))[', esc_typst(text), ']]')
}
# Bullet version for overviews: splits on ";" and renders each piece as a round bullet.
# Inset 2.8em is deliberately larger than the 1.5em used for running text,
# so these inner bullets sit slightly to the right of the outer organization bullet.
bullet_desc_block <- function(text, above = "0.6em") {
  parts <- trimws(strsplit(as_chr(text), ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  # Use Typst markup list "- item" - simpler and more reliable than #list() nesting.
  items <- paste(
    paste0("- #text(size: 0.95em, fill: rgb(\"2d3748\"))[", esc_typst(parts), "]"),
    collapse = "\n"
  )
  paste0(
    '#block(width: 100%, above: ', above, ', inset: (left: 0.8em))[\n#set par(justify: false)\n', items, '\n]'
  )
}


bl <- function(x) sprintf("#block(breakable: false)[\n%s\n]", x)
bullet_entry <- function(inner) bl(sprintf("#list[\n%s\n]", inner))

compose_section <- function(heading, entries, gap = "1.2em", before = "2.2em", after_heading = "0.5em") {
  if (!length(entries)) return(heading)
  first <- bl(paste0(heading, "\n\n#v(", after_heading, ")\n", entries[[1]]))
  sep <- paste0("\n\n#v(", gap, ", weak: true)\n\n")
  res <- if (length(entries) == 1) first else paste0(first, sep, paste(entries[-1], collapse = sep))
  paste0("#v(", before, ", weak: true)\n", res, "\n")
}

## 4. Section emitters -------------------------------------------------------
header_block <- function(cv, cfg) {
  p <- cv$profile
  g <- function(k) as_chr(p[[k]])
  nm  <- g("name")
  ttl <- g("professional_title")
  
  orcid <- g("orcid")
  id <- regmatches(orcid, regexpr("[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9Xx]{4}", orcid))
  if (length(id) == 1 && nzchar(id)) orcid <- paste0("https://orcid.org/", id)
  
  contacts <- character(0)
  if (nzchar(g("linkedin"))) contacts <- c(contacts, lnk(g("linkedin"), "LinkedIn", icon = TRUE))
  if (nzchar(orcid))         contacts <- c(contacts, lnk(orcid, "ORCID", icon = TRUE))
  if (nzchar(g("email")))    contacts <- c(contacts, lnk(paste0("mailto:", g("email")), esc_typst(g("email")), icon = TRUE))
  if (nzchar(g("phone")))    contacts <- c(contacts, lnk(paste0("tel:", gsub("[^+0-9]", "", g("phone"))), esc_typst(g("phone")), icon = TRUE))
  
  paste0(
    '#import "@preview/pro-academic-cv:0.1.0": *\n',
    '#import "@preview/cuti:0.4.0": fakesc\n',
    '\n',
    '// Footer - set BEFORE resume.with so it does not force a page break\n',
    '#set page(footer: context [\n',
    '  #line(length: 100%, stroke: 0.5pt + rgb("cbd5e0"))\n',
    '  #v(0.35em)\n',
    '  #grid(columns: (1fr, auto), gutter: 1em, [\n',
    sprintf('    #text(size: 7.5pt, tracking: 0.05em, fill: rgb("718096"))[#fakesc[%s -- Curriculum Vitae]]\n', esc_typst(nm)),
    '  ], [\n',
    '    #text(size: 7.5pt, fill: rgb("718096"))[Page ]\n',
    '    #text(size: 7.5pt, weight: "semibold", fill: rgb("2b6cb0"))[#counter(page).display()]\n',
    '    #text(size: 7.5pt, fill: rgb("718096"))[ of ]\n',
    '    #text(size: 7.5pt, fill: rgb("718096"))[#counter(page).final().first()]\n',
    '  ])\n',
    '])\n',
    '\n',
    '#show: resume.with(\n',
    '  margin: (left: 0.65in, right: 0.65in, top: 0.65in, bottom: 0.65in),\n',
    sprintf('  font-settings: (font-family: "%s", font-size: %s, author-font-size: %s, lang: "en"),\n',
            esc_code_str(cfg$font_family), cfg$font_size, cfg$author_font_size),
    '  author-info: (\n',
    sprintf('    name: "%s",\n', esc_code_str(nm)),
    sprintf('    primary-info: [%s],\n', esc_typst(ttl)),
    sprintf('    secondary-info: [%s],\n', if (length(contacts)) paste(contacts, collapse = " | ") else ""),
    sprintf('    tertiary-info: "%s",\n', esc_code_str(g("location"))),
    '  ),\n',
    '  author-position: left,\n',
    ')\n',
    '\n',
    '// Global Link Styling (Professional Blue)\n',
    '#show link: set text(fill: rgb("005cb2"))\n',
    '\n',
    '// Global alignment - left-aligned (ragged right) for whole CV, no forced justification\n',
    '#set par(justify: false)\n',
    '\n',
    '// Major headings (level 2) - Dark Navy + Enforced Spacing\n',
    '#show heading.where(level: 2): set text(fill: rgb("1a365d"))\n',
    '#show heading.where(level: 2): set block(below: 1.2em)\n',
    '\n',
    '// Subsection headings (level 3) - Small caps with subtle royal blue tint\n',
    '#show heading.where(level: 3): it => [\n',
    '  #block(breakable: false, above: 1.0em, below: 0.35em)[\n',
    '    #fakesc[#text(size: 0.95em, weight: "semibold", tracking: 0.04em, fill: rgb("2b6cb0"))[#it.body]]\n',
    '  ]\n',
    ']\n'
  )
}

section_profile <- function(cv, cfg) {
  p <- cv$profile
  join_items <- function(k, sep = ", ") {
    v <- vapply(p[[k]], as_chr, character(1), USE.NAMES = FALSE)
    paste(esc_typst(v[nzchar(v)]), collapse = sep)
  }
  # Helper for left-column labels: small caps, semibold, royal blue, tracked
  label_cell <- function(label) {
    paste0('#text(size: 1em, weight: "semibold", tracking: 0.04em, fill: rgb("2b6cb0"))[#fakesc[', esc_typst(label), ']]')
  }
  # Software pills: light blue-grey background, rounded, small text
  soft <- vapply(p[["software_package"]], as_chr, character(1), USE.NAMES = FALSE)
  soft <- soft[nzchar(soft)]
  pills <- paste(
    vapply(soft, function(s) {
      paste0('#box(fill: rgb("edf2f7"), radius: 0.35em, inset: (x: 0.6em, y: 0.25em))[#text(size: 0.85em, fill: rgb("2d3748"))[', esc_typst(s), ']]')
    }, character(1)),
    collapse = " #h(0.35em) "
  )
  # Hybrid grid: label column left, content right. No change to margins.
  # Row gutter keeps breathing room between rows.
  grid_body <- paste0(
    "#grid(\n",
    "  columns: (1.7in, 1fr),\n",
    "  gutter: 0.8em,\n",
    "  row-gutter: 0.7em,\n",
    "  [", label_cell("Research Interests"), "],\n",
    "  [#par(justify: false)[", join_items("research_interests"), "]],\n",
    "  [", label_cell("Methodological Expertise"), "],\n",
    "  [#par(justify: false)[", join_items("methodological_expertise"), "]],\n",
    "  [", label_cell("Software"), "],\n",
    "  [#par(justify: false)[", pills, "]]\n",
    ")\n"
  )
  paste0(
    "#v(2.2em, weak: true)\n",
    bl(paste0(h2("Professional Profile"), "\n\n", esc_typst(as_chr(p$academic_profile)))),
    "\n\n#v(1.0em)\n",
    "#block(inset: (left: 1.5em))[\n", grid_body, "]\n"
  )
}

section_experience <- function(cv, cfg) {
  ap <- prio_gte(cv$xl$appointments, cfg$prio_experience)
  org <- cv$xl$organizations
  org_ids <- unique(as_chr(ap$organization_id))
  org_key <- vapply(org_ids, function(o) {
    d <- ap[as_chr(ap$organization_id) == o, , drop = FALSE]
    ends <- parse_date_robust(as_chr(d$end))
    if (any(is.na(ends))) Inf else max(as.numeric(ends), na.rm = TRUE)
  }, numeric(1))
  org_ids <- org_ids[order(-org_key)]
  org_row <- function(o) org[as_chr(org$id) == o, , drop = FALSE][1, ]
  
  entries <- vapply(org_ids, function(o) {
    d <- ap[as_chr(ap$organization_id) == o, , drop = FALSE]
    end_k <- parse_date_robust(as_chr(d$end))
    end_k <- ifelse(is.na(end_k), Inf, as.numeric(end_k))
    start_k <- parse_date_robust(as_chr(d$start))
    start_k <- ifelse(is.na(start_k), -Inf, as.numeric(start_k))
    d <- d[order(-end_k, -start_k), , drop = FALSE]
    starts <- as_chr(d$start); ends <- as_chr(d$end)
    r <- org_row(o)
    org_range <- fmt_range(min(starts), if (all(nzchar(ends))) max(ends) else "")
    
    if (nrow(d) == 1) {
      sub <- esc_typst(as_chr(d$designation[1]))
      dep <- esc_typst(as_chr(d$department[1]))
      if (nzchar(dep)) sub <- paste(sub, dep, sep = " \u00b7 ")
    } else {
      sub <- if (length(unique(as_chr(d$department))) == 1)
        esc_typst(as_chr(d$department[1])) else ""
    }
    
    head <- paste0(
      "  #r2c2-entry-header(\n",
      sprintf("    top-left: %s,\n",     ct(esc_typst(as_chr(r$organization)))),
      sprintf("    top-right: %s,\n",    ct(esc_typst(org_range))),
      if (nzchar(sub))      sprintf("    bottom-left: %s,\n",  ct(sub)) else NULL,
      sprintf("    bottom-right: %s,\n", ct(esc_typst(as_chr(r$location)))),
      if (nrow(d) > 1) "  ) \\" else "  )"
    )
    
    body <- character(0)
    if (nrow(d) > 1) {
      body <- vapply(seq_len(nrow(d)), function(i) {
        sprintf("  #row2col(%s, %s) \\",
                ct(esc_typst(as_chr(d$designation[i]))),
                ct(esc_typst(fmt_range(starts[i], ends[i]))))
      }, character(1))
    }
    ovw <- if (nzchar(as_chr(r$overview))) bullet_desc_block(as_chr(r$overview)) else NULL
    bullet_entry(paste(c(head, body, ovw), collapse = "\n"))
  }, character(1), USE.NAMES = FALSE)
  
  compose_section(h2("Academic Experience"), entries)
}

section_courses <- function(cv, cfg) {
  co <- prio_gte(cv$xl$courses, cfg$prio_courses)
  te <- cv$xl$teaching
  
  max_yrs <- vapply(seq_len(nrow(co)), function(i) {
    t <- te[as_chr(te$course_id) == as_chr(co$course_id[i]), , drop = FALSE]
    if (nrow(t) == 0) return(-Inf)
    max(suppressWarnings(as.integer(as_chr(t$year))), na.rm = TRUE)
  }, numeric(1))
  co <- co[order(-max_yrs), , drop = FALSE]
  
  entries <- vapply(seq_len(nrow(co)), function(i) {
    cid <- as_chr(co$course_id[i])
    t <- te[as_chr(te$course_id) == cid, , drop = FALSE]
    yrs <- ""
    sub <- esc_typst(as_chr(co$level[i]))
    if (nrow(t) > 0) {
      ty <- sort(as.integer(as_chr(t$year)))
      yrs <- if (ty[1] == ty[length(ty)]) sprintf("%d", ty[1])
      else sprintf("%d \u2013 %d", ty[1], ty[length(ty)])
      ns <- sum(suppressWarnings(as.integer(as_chr(t$student_count))))
      sub <- paste(sub, esc_typst(sprintf("%s \u00b7 %s",
                                          plural(nrow(t), "section"), plural(ns, "student"))), sep = " \u00b7 ")
    }
    inner <- paste0(
      "  #r2c2-entry-header(\n",
      sprintf("    top-left: %s,\n",  ct(esc_typst(as_chr(co$course_title[i])))),
      if (nzchar(yrs)) sprintf("    top-right: %s,\n", ct(esc_typst(yrs))) else NULL,
      sprintf("    bottom-left: %s,\n", ct(sub)),
      "  )\n",
      desc_block(as_chr(co$description[i]))
    )
    bullet_entry(inner)
  }, character(1), USE.NAMES = FALSE)
  
  compose_section(h2("Courses Delivered"), entries)
}

section_supervision <- function(cv, cfg) {
  rs <- prio_gte(cv$xl$research_supervision, cfg$prio_supervision)
  rs <- rs[order(-as.integer(as_chr(rs$year))), , drop = FALSE]
  entries <- vapply(seq_len(nrow(rs)), function(i) {
    
    title_text <- esc_typst(as_chr(rs$research_topic[i]))
    
    # Students are appended directly to the title line in italics, level column is skipped entirely
    if (isTRUE(cfg$show_supervisees) && nzchar(as_chr(rs$students[i]))) {
      title_text <- paste0(title_text, " \u00b7 _", esc_typst(as_chr(rs$students[i])), "_")
    }
    
    # Bypassing the template's macro to use a custom grid. The 0.5in gutter strictly enforces the gap
    bullet_entry(paste0(
      "  #grid(\n",
      "    columns: (1fr, auto),\n",
      "    gutter: 0.5in,\n",
      sprintf("    align(left)[#par(justify: false, hanging-indent: 1.5em)[%s]],\n", title_text),
      sprintf("    align(right)[%s]\n", esc_typst(as_chr(rs$year[i]))),
      "  )"
    ))
  }, character(1), USE.NAMES = FALSE)
  
  compose_section(h2("Research Supervision"), entries)
}

section_services <- function(cv, cfg) {
  sv <- prio_gte(cv$xl$academic_services, cfg$prio_services)
  org <- cv$xl$organizations
  appt <- cv$xl$appointments
  
  # Match end dates to the filtered services. 
  # Missing/empty dates (ongoing roles) are temporarily assigned "9999" so they sort to the very top.
  end_dates <- as_chr(appt$end[match(as_chr(sv$role_id), as_chr(appt$role_id))])
  end_dates[is.na(end_dates) | !nzchar(end_dates)] <- "9999"
  
  # Sort the dataframe in descending order by the end dates
  sv <- sv[order(end_dates, decreasing = TRUE), , drop = FALSE]
  
  entries <- vapply(seq_len(nrow(sv)), function(i) {
    o <- org[as_chr(org$id) == as_chr(sv$organization_id[i]), , drop = FALSE][1, ]
    
    # 1. Match the appointment to grab start and end dates
    a <- appt[as_chr(appt$role_id) == as_chr(sv$role_id[i]), , drop = FALSE]
    
    date_str <- "[]"
    if (nrow(a) > 0) {
      st <- as_chr(a$start[1])
      en <- as_chr(a$end[1])
      
      # 2. Route dates through your native formatter and wrap in Typst italics
      formatted_date <- fmt_range(st, en)
      date_text <- sprintf("_%s_", esc_typst(formatted_date))
      date_str <- ct(date_text)
    }
    
    # Injects weight: "regular" to unbold the institution while keeping the role title bold
    title_org <- sprintf('#text(weight: "bold")[%s], #text(weight: "regular")[%s]', 
                         esc_typst(as_chr(sv$service_title[i])), 
                         esc_typst(as_chr(o$organization)))
    
    # 3. Inject date_str into the top-right parameter for right-alignment on the first line
    head <- paste0(
      "  #row2col(\n",
      sprintf("    %s,\n", ct(title_org)),
      sprintf("    %s\n", date_str),
      "  )\n"
    )
    
    # Overrides the default 0.6em gap with 0.2em to pull the description closer to the title
    ovw <- if (nzchar(as_chr(sv$service_description[i]))) {
      bullet_desc_block(as_chr(sv$service_description[i]), above = "0.6em")
    } else ""
    
    bullet_entry(paste0(head, ovw))
  }, character(1), USE.NAMES = FALSE)
  
  compose_section(h2("Academic Services"), entries)
}

section_qualifications <- function(cv, cfg) {
  q <- prio_gte(cv$xl$academic_qualifications, cfg$prio_qualifications)
  comp <- as_chr(q$completion)
  key <- ifelse(!nzchar(comp), Inf, as.numeric(parse_date_robust(comp)))
  q <- q[order(-key), , drop = FALSE]
  entries <- vapply(seq_len(nrow(q)), function(i) {
    when <- as_chr(q$completion[i])
    when <- if (!nzchar(when)) esc_typst(as_chr(q$status[i])) else esc_typst(when)
    head <- paste0(
      "  #r2c2-entry-header(\n",
      sprintf("    top-left: %s,\n",     ct(esc_typst(as_chr(q$degree[i])))),
      sprintf("    top-right: %s,\n",    ct(when)),
      sprintf("    bottom-left: %s,\n",  ct(esc_typst(paste(
        Filter(nzchar, c(as_chr(q$department[i]), as_chr(q$institution[i]))),
        collapse = ", ")))),
      sprintf("    bottom-right: %s,\n", ct(esc_typst(as_chr(q$location[i])))),
      "  )"
    )
    # Same bullet formatting as Academic Experience overview: splits on ; , inset 2.8em, size 0.95em, justify false
    desc <- if ("description" %in% names(q) && nzchar(as_chr(q$description[i]))) {
      bullet_desc_block(as_chr(q$description[i]))
    } else ""
    bullet_entry(paste0(head, "\n", desc))
  }, character(1), USE.NAMES = FALSE)
  
  compose_section(h2("Academic Qualifications"), entries)
}

section_publications <- function(cv, cfg) {
  pb <- cv$pubs
  status <- ifelse(
    pb$category == "unpublished" |
      grepl("under review|submitted", pb$type, ignore.case = TRUE),
    "S", "P")
  if (length(cfg$pub_status_override)) {
    ov <- cfg$pub_status_override
    hit <- pb$key %in% names(ov)
    status[hit] <- ov[pb$key[hit]]
  }
  ord <- order(match(status, c("P", "UPR")), -ifelse(is.na(pb$year), 0L, pb$year))
  pb <- pb[ord, , drop = FALSE]
  status <- status[ord]
  
  entry_value <- function(i) {
    a <- format_authors(pb$author[i])
    yr <- if (is.na(pb$year[i])) "" else sprintf("(%d).", pb$year[i])
    head_auth <- paste(Filter(nzchar, c(a, yr)), collapse = " ")
    
    title <- sprintf("%s.", esc_typst(pb$title[i]))
    
    tail <- switch(
      pb$category[i],
      article = {
        vol <- paste(Filter(nzchar, c(
          if (nzchar(pb$volume[i]))
            paste0(pb$volume[i],
                   if (nzchar(pb$number[i])) sprintf("(%s)", pb$number[i]) else "") else "",
          if (nzchar(pb$pages[i])) gsub("--", "\u2013", pb$pages[i]) else ""
        )), collapse = ", ")
        paste(Filter(nzchar, c(
          sprintf("_%s_", esc_typst(pb$journal[i])),
          vol
        )), collapse = ", ")
      },
      misc = sprintf("_%s_.", esc_typst(pb$publisher[i])),
      mastersthesis = sprintf("[MPhil thesis, %s].", esc_typst(pb$school[i])),
      paste0(esc_typst(pb$type[i]), ".")
    )
    
    href <- if (nzchar(pb$doi[i])) paste0("https://doi.org/", pb$doi[i]) else pb$url[i]
    link_str <- if (nzchar(href)) lnk(href, href) else ""
    
    paste(Filter(nzchar, c(head_auth, title, tail, link_str)), collapse = " ")
  }
  
  items <- vapply(seq_len(nrow(pb)), function(i) {
    # Wrapped natively in a par block to guarantee the hanging indent applies unconditionally
    sprintf('    align(left)[%s],\n    align(left)[#par(hanging-indent: 1.5em)[%s]]', status[i], entry_value(i))
  }, character(1), USE.NAMES = FALSE)
  
  paste0(
    "#v(2.2em, weak: true)\n",
    bl(paste0(
      h2("Publications", note = cfg$pub_legend), "\n\n",
      # Bypassing the template's macro to build a custom grid immune to paragraph overrides
      "#grid(\n",
      "  columns: (auto, 1fr),\n",
      "  column-gutter: 1.2em,\n",
      "  row-gutter: 0.8em,\n",
      paste(items, collapse = ",\n"), "\n",
      ")\n"
    ))
  )
}

section_profdev <- function(cv, cfg) {
  pd <- cv$xl$professional_development
  if (!is.null(cfg$prio_profdev))
    pd <- prio_gte(pd, cfg$prio_profdev)
  if (nrow(pd)) {
    pd <- pd[order(-as.numeric(parse_date_robust(as_chr(pd$start)))), , drop = FALSE]
  }
  
  pd_entries <- vapply(seq_len(nrow(pd)), function(i) {
    top <- sprintf('%s + text(weight: "regular")[#h(0.35em)\u00b7 %s]',
                   ct(esc_typst(as_chr(pd$activity_title[i]))),
                   esc_typst(as_chr(pd$activity_type[i])))
    dates <- fmt_range(as_chr(pd$start[i]), as_chr(pd$end[i]))
    if (!nzchar(dates)) dates <- esc_typst(as_chr(pd$status[i]))
    bullet_entry(paste0(
      "  #r2c2-entry-header(\n",
      sprintf("    top-left: %s,\n",  top),
      sprintf("    top-right: %s,\n", ct(esc_typst(dates))),
      "  )\n",
      desc_block(as_chr(pd$description[i]))
    ))
  }, character(1), USE.NAMES = FALSE)
  
  ce <- prio_gte(cv$xl$continuous_education, cfg$prio_ce)
  # Category filter - AND with priority, so both must pass
  if (!is.null(cfg$ce_types) && length(cfg$ce_types) > 0 && "type" %in% names(ce)) {
    ce <- ce[as_chr(ce$type) %in% cfg$ce_types, , drop = FALSE]
  }
  ce <- ce[order(-as.numeric(parse_date_robust(as_chr(ce$completion_date)))), , drop = FALSE]
  
  ce_entries <- vapply(seq_len(nrow(ce)), function(i) {
    org_txt <- esc_typst(as_chr(ce$issuing_organization[i]))
    url <- as_chr(ce$credential_url[i])
    if (nzchar(url)) org_txt <- paste0(org_txt, " ", lnk(url, "#link-icon()"))
    
    # Restored title and organization to the same line
    title_text <- paste0(esc_typst(as_chr(ce$ce_title[i])), ", ", org_txt)
    
    bullet_entry(paste0(
      "  #grid(\n",
      "    columns: (1fr, auto),\n",
      "    gutter: 0.5in,\n",
      sprintf("    align(left)[#par(hanging-indent: 1.5em)[%s]],\n", title_text),
      sprintf("    align(right)[%s]\n", esc_typst(fmt_month_year(as_chr(ce$completion_date[i])))),
      "  )"
    ))
  }, character(1), USE.NAMES = FALSE)
  
  out <- character(0)
  if (length(pd_entries)) out <- compose_section(
    h2("Professional Development"), pd_entries)
  
  if (length(ce_entries)) out <- c(out, compose_section(
    h3("Continuing Education"), ce_entries, before = "1.8em", after_heading = "1.2em"))
  
  paste(out, collapse = "\n\n")
}

## 5. Whole document ---------------------------------------------------------
render_cv <- function(cv, cfg) {
  parts <- c(
    section_profile(cv, cfg),
    section_experience(cv, cfg),
    section_courses(cv, cfg),
    section_supervision(cv, cfg),
    section_services(cv, cfg),
    section_qualifications(cv, cfg),
    section_publications(cv, cfg),
    section_profdev(cv, cfg)
  )
  parts <- Filter(nzchar, parts)
  
  paste(
    header_block(cv, cfg),
    paste(parts, collapse = "\n"),
    sep = "\n\n"
  )
}