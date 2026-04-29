# ============================================================
# briefing_to_pdf.R
#
# PDF Renderer — Veterinary E-commerce Intelligence Brief
#
# PURPOSE:
#   Converts the Writing Agent's markdown-formatted brief text
#   into a branded, multi-page PDF using the Cairo graphics
#   device and the grid drawing system.
#
# CALLED BY:
#   HomeDelivery_WritingAgent.R — Save Outputs section
#
# USAGE:
#   source(file.path(myWD, "briefing_to_pdf.R"))
#   briefing_to_pdf(final_summary, pdf_filename, outputFolder)
#
# ARGUMENTS:
#   briefing_text — character string returned by writing_agent()
#   pdf_filename  — output filename only, e.g. "2026-01-01 Brief.pdf"
#   outputFolder  — full path to output directory (trailing slash required)
#   release_date  — defaults to today's date in "Month DD, YYYY" format
#
# DEPENDENCIES:
#   Cairo, png, grid, grDevices, stringr
#   All loaded in HomeDelivery_Brief.Rmd setup chunk.
#   logo_path and stripe_path must be set in the global environment
#   by the calling script.
#
# NOTES:
#   - PDF is saved to outputFolder, not the working directory.
#   - Page layout constants are defined at the top of this script.
#     Adjust font sizes, margins, and spacing there if needed.
#   - Brand colors are defined in the NVA Brand Colors section.
# ============================================================


# ── NVA Brand Colors ──────────────────────────────────────────────────────────
NVA_DARK_TEAL  <- "#044F59"
NVA_TEAL       <- "#3FA0AB"
NVA_LIGHT_TEAL <- "#AFE3DF"
NVA_CREAM      <- "#F8F4F1"
NVA_Purple     <- "#D672CC"
NVA_Orange     <- "#FFAC64"
NVA_Green      <- "#3F884D"

# Section header accent colors — cycles through sections in order
header_colors <- c(NVA_Orange, NVA_Green, NVA_Purple)

# Load stripe image once at script level — reused on every page
stripe_img <- png::readPNG(stripe_path)

# ── Layout constants ──────────────────────────────────────────────────────────

# Page geometry (inches)
PAGE_W <- 8.5; PAGE_H <- 11
MARGIN_L <- 0.65; MARGIN_R <- 0.65
CONTENT_W <- PAGE_W - MARGIN_L - MARGIN_R

# Font sizes (points)
FS_TITLE   <- 20; FS_SUB  <- 10
FS_SECTION <- 11
FS_BULLET  <- 7.5
FS_ACTION  <- 6.5
FS_PLAIN   <- 8.5
FS_FOOTER  <- 7.5

# Vertical spacing (inches)
LINE_SECTION <- 0.30
LINE_BULLET  <- 0.145
LINE_ACTION  <- 0.145 * 1.35
LINE_PLAIN   <- 0.135

PAD_SECTION_BEFORE <- 0.18
PAD_SECTION_AFTER  <- 0.06
PAD_BULLET_AFTER   <- 0.03
PAD_ACTION_TOP     <- 0.08
PAD_ACTION_BOTTOM  <- 0.14
LINE_ACTION_GAP    <- LINE_ACTION * 0.4

HEADER_H    <- 0.75
FOOTER_H    <- 0.35
TOP_START   <- HEADER_H + 0.18
BOTTOM_STOP <- PAGE_H - FOOTER_H - 0.07  # furthest point cursor can reach before page break

TEXT_LEFT_PAD  <- 0.15
TEXT_RIGHT_PAD <- 0.10
ACCENT_BAR_W   <- 0.04

ACTION_TEXT_X   <- MARGIN_L
SECTION_BADGE_W <- CONTENT_W

# ── Text wrapping ─────────────────────────────────────────────────────────────
# Estimates character capacity per line based on usable content width.
# Bullet prefix chars are subtracted to account for the bullet symbol indent.
usable_width_in   <- CONTENT_W - TEXT_LEFT_PAD - TEXT_RIGHT_PAD
CHARS_PER_IN      <- 18
wrap_width        <- floor(usable_width_in * CHARS_PER_IN)

BULLET_PREFIX_CHARS <- 3
wrap_width_bullet   <- wrap_width - BULLET_PREFIX_CHARS

# Wraps a string to fit within char_width characters per line.
# Strips markdown bold and italic markers before wrapping.
wrap_text <- function(text, char_width = wrap_width) {
  text  <- gsub("\\*\\*(.+?)\\*\\*", "\\1", text)
  text  <- gsub("\\*(.+?)\\*",       "\\1", text)
  words <- strsplit(text, " ")[[1]]
  lines <- character(0)
  current <- ""
  for (w in words) {
    test <- if (nchar(current) == 0) w else paste(current, w)
    if (nchar(test) <= char_width) {
      current <- test
    } else {
      if (nchar(current) > 0) lines <- c(lines, current)
      current <- w
    }
  }
  if (nchar(current) > 0) lines <- c(lines, current)
  lines
}

# Splits a single action string into individual sentences.
# Handles cases where the Writing Agent outputs all actions as one paragraph
# separated by period-space rather than newlines.
split_action_sentences <- function(txt) {
  sentences <- unlist(strsplit(txt, "(?<=\\.)\\s+(?=[A-Z])", perl = TRUE))
  sentences <- trimws(sentences)
  sentences <- sentences[nzchar(sentences)]
  sentences
}

# ── Text cleanup and block parsing ───────────────────────────────────────────

# Normalizes unicode characters that may arrive as escape sequences
# depending on how the API response was encoded.
normalize_text <- function(text) {
  text <- enc2utf8(text)
  text <- gsub("\\\\<e2\\\\><80\\\\><94>", "\u2014", text)
  text <- gsub("\\\\<e2\\\\><80\\\\><93>", "\u2013", text)
  text <- gsub("\u201c|\u201d", "\"", text)
  text <- gsub("\u2018|\u2019", "'",  text)
  text
}

# Strips all known prefixes from Recommended Action lines so only
# the plain action text is passed to the drawing functions.
clean_action_text <- function(txt) {
  txt <- sub("^\U0001F4CC\\s*", "", txt)
  txt <- sub("^>\\s*\\*\\*\\s*Recommended Actions?\\s*:?\\s*\\*\\*\\s*", "", txt, ignore.case = TRUE)
  txt <- sub("^\\*\\*\\s*Recommended Actions?\\s*:?\\s*\\*\\*\\s*",       "", txt, ignore.case = TRUE)
  txt <- sub("^\\s*[-*]\\s*Recommended Actions?\\s*:?\\s*",               "", txt, ignore.case = TRUE)
  txt <- sub("^Recommended Actions?\\s*:?\\s*",                           "", txt, ignore.case = TRUE)
  txt <- sub("^>\\s*", "", txt)
  txt <- sub("^\\s*[\u2022\\-\\*]\\s*", "", txt)
  txt <- gsub("\\*\\*(.+?)\\*\\*", "\\1", txt)
  txt <- gsub("\\*(.+?)\\*",       "\\1", txt)
  trimws(txt)
}

# Classifies a single line of briefing text into a typed block.
# Returns NULL for empty lines, which are silently skipped.
classify_line <- function(ln) {
  ln <- trimws(ln)
  if (nchar(ln) == 0) return(NULL)
  
  if (grepl("^#{2,3}\\s+", ln) | grepl("^..\\d{1}\\.", ln)) {
    txt <- sub("^#{2,3}\\s+", "", ln)
    txt <- gsub("\\*{2,3}", "", txt)
    return(list(type = "section", text = txt))
  }
  if (grepl("^# ", ln)) {
    return(list(type = "section", text = sub("^# ", "", ln)))
  }
  if (grepl("Recommended", ln, ignore.case = TRUE)) {
    txt <- clean_action_text(ln)
    if (!nzchar(txt)) return(NULL)
    return(list(type = "action", text = txt))
  }
  if (grepl("^\\s*[-*]\\s+", ln)) {
    txt <- sub("^\\s*[-*]\\s+", "", ln)
    txt <- gsub("\\*\\*(.+?)\\*\\*", "\\1", txt)
    txt <- gsub("\\*(.+?)\\*",       "\\1", txt)
    return(list(type = "bullet", text = txt))
  }
  if (grepl("^---", ln)) {
    return(list(type = "divider", text = ""))
  }
  ln <- gsub("\\*\\*(.+?)\\*\\*", "\\1", ln)
  return(list(type = "plain", text = ln))
}

# Parses the full briefing text into a list of typed blocks
# ready for the rendering loop.
parse_briefing <- function(text) {
  text  <- gsub("# Veterinary E-Commerce Executive Briefing\n\n---\n\n", "", text, fixed = TRUE)
  text  <- normalize_text(text)
  lines <- unlist(strsplit(text, "\n"))
  blocks <- list()
  for (ln in lines) {
    blk <- classify_line(ln)
    if (!is.null(blk)) blocks <- c(blocks, list(blk))
  }
  blocks
}

# ── Lookahead height estimators ───────────────────────────────────────────────
# Used before drawing to check whether content fits on the current page.
# If not, a new page is started before drawing begins.

# Estimates total height of all bullet blocks following block i
estimate_bullet_block_height <- function(blocks, i) {
  total <- 0
  j <- i + 1
  while (j <= length(blocks) && blocks[[j]]$type == "bullet") {
    total <- total + estimate_single_bullet_height(blocks[[j]]$text)
    j <- j + 1
  }
  total
}

# Estimates height of a single bullet including its wrapped lead and body text
estimate_single_bullet_height <- function(btext) {
  colon_pos <- regexpr(":", btext, fixed = TRUE)
  if (colon_pos > 0) {
    lead <- substr(btext, 1, colon_pos)
    rest <- trimws(substr(btext, colon_pos + 1, nchar(btext)))
  } else {
    lead <- NA_character_
    rest <- btext
  }
  n_lead <- if (!is.na(lead) && nchar(trimws(lead)) > 0)
    length(wrap_text(lead, wrap_width_bullet)) else 0
  n_rest <- if (!is.na(rest) && nchar(trimws(rest)) > 0)
    length(wrap_text(rest, wrap_width_bullet)) else 0
  (n_lead + n_rest) * LINE_BULLET + PAD_BULLET_AFTER
}

# Estimates total height of an action box given a vector of sentence strings
estimate_action_box_height <- function(sentences) {
  top_pad    <- 0.10
  bottom_pad <- 0.08
  wrapped    <- lapply(sentences, wrap_text, char_width = wrap_width)
  n_gaps     <- max(length(sentences) - 1, 0)
  total_lines <- sum(sapply(wrapped, length))
  top_pad +
    LINE_ACTION +
    (total_lines * LINE_ACTION * 0.5) +
    (n_gaps * LINE_ACTION_GAP) +
    bottom_pad
}

# ── Drawing helpers ───────────────────────────────────────────────────────────

# Draws a filled rectangle with no border at the given position and size
fill_rect <- function(x, y, w, h, fill) {
  grid.rect(
    x = unit(x, "inches"), y = unit(y, "inches"),
    width = unit(w, "inches"), height = unit(h, "inches"),
    hjust = 0, vjust = 0,
    gp = gpar(fill = fill, col = NA, lwd = 0)
  )
}

# Draws a completed action box from a vector of sentence strings.
# Each sentence is wrapped and separated by LINE_ACTION_GAP spacing.
# Page overflow check happens in flush_actions() before this is called.
draw_action_box <- function(sentences, cursor, y_grid, advance) {
  
  top_pad    <- 0.10
  bottom_pad <- 0.08
  
  wrapped_items <- lapply(sentences, wrap_text, char_width = wrap_width * 1.2)
  n_gaps        <- max(length(sentences) - 1, 0)
  
  # Draw "Recommended Action(s)" label
  grid.text(
    "Recommended Action(s)",
    x     = unit(ACTION_TEXT_X, "inches"),
    y     = unit(y_grid(cursor + LINE_ACTION * 0.4), "inches"),
    hjust = 0, vjust = 0.5,
    gp    = gpar(fontsize = FS_ACTION, fontface = "bold",
                 col = NVA_DARK_TEAL, fontfamily = "sans")
  )
  
  cursor <- advance(cursor, LINE_ACTION)
  
  # Draw each sentence on its own wrapped line(s)
  for (idx in seq_along(wrapped_items)) {
    for (ln in wrapped_items[[idx]]) {
      grid.text(
        ln,
        x     = unit(ACTION_TEXT_X, "inches"),
        y     = unit(y_grid(cursor), "inches"),
        hjust = 0, vjust = 0.5,
        gp    = gpar(fontsize = FS_ACTION, fontface = "italic",
                     col = NVA_DARK_TEAL, fontfamily = "sans")
      )
      cursor <- advance(cursor, LINE_ACTION * 0.4)
    }
    if (idx < length(wrapped_items)) {
      cursor <- advance(cursor, LINE_ACTION_GAP)
    }
  }
  
  cursor <- advance(cursor, PAD_ACTION_BOTTOM + PAD_ACTION_BOTTOM + PAD_ACTION_BOTTOM)
  cursor
}

# ── Main PDF rendering function ───────────────────────────────────────────────

briefing_to_pdf <- function(briefing_text,
                            pdf_filename,
                            outputFolder  = outputDir,
                            release_date  = format(Sys.Date(), "%B %d, %Y")) {
  
  blocks <- parse_briefing(briefing_text)
  
  # Open Cairo PDF device — file saved to outputFolder, not working directory
  cairo_pdf(paste0(outputFolder, pdf_filename), width = PAGE_W, height = PAGE_H, onefile = TRUE)
  
  logo_img <- png::readPNG(logo_path)
  
  # ── Page chrome ──────────────────────────────────────────────────────────────
  # Draws the header, footer, logo, and stripe on each new page.
  # Returns TOP_START so the cursor is correctly positioned after the header.
  new_page <- function() {
    grid.newpage()
    fill_rect(0, 0, PAGE_W, PAGE_H, "white")
    
    # Header bar
    fill_rect(0, PAGE_H - HEADER_H, PAGE_W, HEADER_H, NVA_DARK_TEAL)
    grid.text(
      "Home Delivery",
      x = unit(MARGIN_L + 1.5, "inches"), y = unit(PAGE_H - 0.30, "inches"),
      hjust = 0,
      gp = gpar(fontface = "bold", fontsize = FS_TITLE, col = "white")
    )
    grid.text(
      paste0("Weekly Industry Briefing  \u00b7  ", release_date),
      x = unit(MARGIN_L + 1.5, "inches"), y = unit(PAGE_H - 0.55, "inches"),
      hjust = 0,
      gp = gpar(fontsize = FS_SUB, col = NVA_LIGHT_TEAL)
    )
    
    # Logo
    grid.raster(
      logo_img,
      x = unit(MARGIN_L, "inches"), y = unit(PAGE_H - HEADER_H / 2, "inches"),
      width = unit(1.1, "inches"), height = unit(0.6, "inches"),
      just = c("left", "center")
    )
    
    # Accent stripe below header
    grid.raster(
      stripe_img,
      x      = unit(0, "inches"),
      y      = unit(PAGE_H - HEADER_H, "inches"),
      width  = unit(PAGE_W, "inches"),
      height = unit(0.06, "inches"),
      just   = c("left", "top")
    )
    
    # Footer bar
    fill_rect(0, 0, PAGE_W, FOOTER_H, NVA_DARK_TEAL)
    grid.text(
      "NVA Home Delivery  \u00b7  Confidential  \u00b7  For Internal Use Only  \u00b7  Contact: Sam Bell - Samantha.Bell@nva.com",
      x = unit(PAGE_W / 2, "inches"), y = unit(0.18, "inches"),
      gp = gpar(fontsize = FS_FOOTER, col = NVA_LIGHT_TEAL, fontfamily = "sans")
    )
    
    TOP_START
  }
  
  # ── Cursor helpers ────────────────────────────────────────────────────────────
  # y_grid converts "distance from top" cursor position to grid inches from bottom
  # advance moves the cursor down by d inches
  # check_page triggers a new page if the next block won't fit
  y_grid  <- function(from_top) PAGE_H - from_top
  advance <- function(cur, d) cur + d
  
  check_page <- function(cur, needed) {
    if ((cur + needed) > BOTTOM_STOP) {
      cursor <<- new_page()
    }
  }
  
  cursor        <- new_page()
  header_count  <- 0
  accent_color  <- header_colors[1]
  action_buffer <- character(0)
  
  # ── Flush action buffer ───────────────────────────────────────────────────────
  # Action lines are buffered as they are encountered so all sentences in a
  # Recommended Actions block can be drawn together in a single box.
  # flush_actions() is called before each new section header and at end of document.
  flush_actions <- function() {
    if (length(action_buffer) == 0) return()
    
    # Split each buffered string into individual sentences
    sentences <- unlist(lapply(action_buffer, split_action_sentences))
    sentences <- sentences[nzchar(trimws(sentences))]
    
    if (length(sentences) == 0) {
      action_buffer <<- character(0)
      return()
    }
    
    # Pre-check full box height before drawing anything
    box_h  <- estimate_action_box_height(sentences)
    cursor <<- advance(cursor, PAD_ACTION_TOP + PAD_ACTION_TOP)
    check_page(cursor, box_h + PAD_ACTION_BOTTOM)
    
    cursor        <<- draw_action_box(sentences, cursor, y_grid, advance)
    action_buffer <<- character(0)
  }
  
  # ── Render blocks ─────────────────────────────────────────────────────────────
  for (i in seq_along(blocks)) {
    
    block <- blocks[[i]]
    btype <- block$type
    btext <- block$text
    
    # Action lines are buffered — not drawn immediately
    if (btype == "action") {
      action_buffer <- c(action_buffer, btext)
      next
    }
    
    # Flush any buffered actions before drawing the next non-action block
    flush_actions()
    
    # ── Section header ──────────────────────────────────────────────────────────
    if (btype == "section") {
      
      header_count <- header_count + 1
      accent_color <- header_colors[((header_count - 1) %% length(header_colors)) + 1]
      
      # Lookahead: estimate full section height including bullets and actions
      # to decide whether to start a new page before drawing the header
      bullet_h <- estimate_bullet_block_height(blocks, i)
      
      j <- i + 1
      while (j <= length(blocks) && blocks[[j]]$type == "bullet") j <- j + 1
      action_sentences <- character(0)
      while (j <= length(blocks) && blocks[[j]]$type == "action") {
        action_sentences <- c(action_sentences,
                              split_action_sentences(blocks[[j]]$text))
        j <- j + 1
      }
      action_h <- if (length(action_sentences) > 0)
        PAD_ACTION_TOP + estimate_action_box_height(action_sentences) + PAD_ACTION_BOTTOM
      else 0
      
      section_h <- LINE_SECTION + 0.03 + bullet_h + action_h
      
      check_page(cursor, section_h)
      
      # Cap accent bar height so it never extends past the bottom content boundary
      bar_h_full <- LINE_SECTION + 0.05 + bullet_h
      bar_h      <- min(bar_h_full, BOTTOM_STOP - cursor - PAD_SECTION_BEFORE * 2)
      
      # Draw accent bar and section header badge
      fill_rect(
        MARGIN_L, y_grid(cursor + bar_h),
        ACCENT_BAR_W, bar_h,
        accent_color
      )
      fill_rect(
        MARGIN_L + ACCENT_BAR_W, y_grid(cursor + LINE_SECTION),
        SECTION_BADGE_W - ACCENT_BAR_W, LINE_SECTION,
        NVA_LIGHT_TEAL
      )
      grid.text(
        btext,
        x = unit(MARGIN_L + ACCENT_BAR_W + 0.12, "inches"),
        y = unit(y_grid(cursor + LINE_SECTION / 2), "inches"),
        hjust = 0, vjust = 0.5,
        gp = gpar(fontface = "bold", fontsize = FS_SECTION,
                  col = NVA_DARK_TEAL, fontfamily = "sans")
      )
      
      next_is_text <- i < length(blocks) &&
        blocks[[i + 1]]$type %in% c("plain", "bullet", "action")
      
      cursor <- advance(
        cursor,
        LINE_SECTION + if (next_is_text) 0.05 else PAD_SECTION_AFTER
      )
      
      # ── Bullet ───────────────────────────────────────────────────────────────
    } else if (btype == "bullet") {
      
      # Bullets with a colon split into a bold lead and normal body text
      colon_pos <- regexpr(":", btext, fixed = TRUE)
      
      if (colon_pos > 0) {
        lead <- substr(btext, 1, colon_pos)
        rest <- trimws(substr(btext, colon_pos + 1, nchar(btext)))
      } else {
        lead <- NA_character_
        rest <- btext
      }
      
      # Check full bullet height before drawing to prevent orphaned lead lines
      check_page(cursor, estimate_single_bullet_height(btext))
      
      if (!is.na(lead) && nchar(trimws(lead)) > 0) {
        lead_wrapped <- wrap_text(lead, char_width = wrap_width_bullet)
        for (j in seq_along(lead_wrapped)) {
          prefix <- if (j == 1) "\u2022  " else "    "
          grid.text(
            paste0(prefix, lead_wrapped[j]),
            x = unit(MARGIN_L + 0.15, "inches"),
            y = unit(y_grid(cursor + LINE_BULLET * 0.7), "inches"),
            hjust = 0, vjust = 0.5,
            gp = gpar(fontsize = FS_BULLET, fontface = "bold",
                      col = "#1A1A1A", fontfamily = "sans")
          )
          cursor <- advance(cursor, LINE_BULLET)
        }
      }
      
      if (!is.na(rest) && nchar(trimws(rest)) > 0) {
        rest_wrapped <- wrap_text(rest, char_width = wrap_width_bullet)
        for (k in seq_along(rest_wrapped)) {
          prefix <- if (is.na(lead) && k == 1) "\u2022  " else "    "
          grid.text(
            paste0(prefix, rest_wrapped[k]),
            x = unit(MARGIN_L + 0.15, "inches"),
            y = unit(y_grid(cursor + LINE_BULLET * 0.7), "inches"),
            hjust = 0, vjust = 0.5,
            gp = gpar(fontsize = FS_BULLET, col = "#1A1A1A", fontfamily = "sans")
          )
          cursor <- advance(cursor, LINE_BULLET)
        }
      }
      
      cursor <- advance(cursor, PAD_BULLET_AFTER)
      
      # ── Plain text ────────────────────────────────────────────────────────────
    } else if (btype == "plain") {
      
      wrapped <- wrap_text(btext, char_width = wrap_width)
      check_page(cursor, length(wrapped) * LINE_PLAIN + 0.03)
      
      for (ln in wrapped) {
        grid.text(
          ln,
          x = unit(MARGIN_L + 0.15, "inches"),
          y = unit(y_grid(cursor + LINE_PLAIN * 0.7), "inches"),
          hjust = 0, vjust = 0.5,
          gp = gpar(fontsize = FS_PLAIN, col = "#444444", fontfamily = "sans")
        )
        cursor <- advance(cursor, LINE_PLAIN)
      }
      
      cursor <- advance(cursor, 0.03)
    }
  }
  
  # Flush any remaining actions at end of document
  flush_actions()
  
  dev.off()
  message("✅ PDF saved: ", paste0(outputFolder, pdf_filename))
  invisible(pdf_filename)
}