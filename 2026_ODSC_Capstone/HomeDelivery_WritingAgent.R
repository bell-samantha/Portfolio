# ============================================================
# HomeDelivery_WritingAgent.R
#
# Writing Agent — Veterinary E-commerce Intelligence Brief
#
# PURPOSE:
#   Accepts the cleaned research output table from the Research
#   Agent, synthesizes it into a structured VP-level executive
#   brief using Claude with extended thinking enabled, and saves
#   the result to both Excel and a branded PDF.
#
# CALLED BY:
#   HomeDelivery_Brief.Rmd — Section 3, Writing Agent
#
# RETURNS:
#   A named list containing:
#     - brief             : the finished brief text
#     - input_tokens      : raw input tokens used
#     - output_tokens     : output tokens used (includes thinking)
#     - call_duration_sec : total API call time in seconds
#     - retry_count       : always 0L (no retry logic in this agent)
#     - status            : "success" or "error"
#     - error_message     : NA if successful
#
# OUTPUT FILES (saved to outputFolder):
#     - YYYY-MM-DD Writing Agent Results.xlsx
#     - YYYY-MM-DD Home Delivery Industry Briefing.pdf
#
# DEPENDENCIES:
#   httr, jsonlite, dplyr, openxlsx
#   All loaded in HomeDelivery_Brief.Rmd setup chunk.
#   ANTHROPIC_API_KEY must be set as an environment variable.
#   briefing_to_pdf.R must exist in myWD.
#   myWD and outputDir must be set in the global environment
#   by the calling script.
#
# NOTES:
#   - Extended thinking is enabled. budget_tokens controls how
#     many tokens Claude can use for internal reasoning before
#     writing the brief. max_tokens must be high enough to cover
#     both thinking and output tokens combined.
#   - api_timeout_sec controls how long R waits for the full API
#     response. Extended thinking significantly increases response
#     time — 600 seconds is recommended.
#   - temperature is intentionally omitted — it conflicts with
#     extended thinking and will cause an API error if included.
#   - The %||% operator must be available in the calling environment.
# ============================================================


writing_agent <- function(research_table,
                          model           = CLAUDE_MODEL_WRITE,
                          max_tokens      = 16000,  # Output tokens only — must cover
                          # thinking + written brief combined
                          budget_tokens   = 8000,   # Thinking token allocation —
                          # leaves ~8000 tokens for brief output
                          api_timeout_sec = 600,    # How long R waits for full API response —
                          # extended thinking requires extra time
                          pdf_filename    = "Home Delivery Industry Briefing.pdf",
                          outputFolder    = outputDir) {
  
  # ── API key check ────────────────────────────────────────────────────────────
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("ANTHROPIC_API_KEY environment variable not set.")
  
  # ── Validate research table has rows before calling API ──────────────────────
  if (!dim(research_table)[1] >= 1) {
    stop("No rows in Research Results table. Nothing to write in brief.")
  }
  
  # ── Select required columns and convert to plain text ────────────────────────
  # Reduces input token count by dropping unused columns before building prompt.
  # Rows are collapsed to pipe-delimited strings for inclusion in the prompt.
  required_cols <- c("summary", "quotes", "source_description")
  
  short_table <- research_table %>%
    select(all_of(required_cols))
  
  table_content <- apply(short_table, 1, function(row) {
    paste(row, collapse = " | ")
  })
  
  table_text <- paste(table_content, collapse = "\n")
  
  # ── Build prompt ─────────────────────────────────────────────────────────────
  # Instructs the Writing Agent on output format, grounding rules, section
  # definitions, and validation steps. The research table text is appended
  # at the end as the sole source of information for the brief.
  prompt_text <- paste(
    "You are a Veterinary e-commerce business analyst. Turn the input table into an executive briefing.",
    "Turn this into executive briefing. Heading and contact/footer will be added later.",
    "\n\nFORMAT:",
    "- Use concise bullet points and include bullet formatting.",
    "- Start with and include these sections:",
    "  1. Risks to Our Business",
    "  2. Growth Opportunities",
    "  3. Market Insights on Our Radar",
    
    "\n\nGROUNDING RULES - most important:",
    "- Only use information present in the input table. Do not supplement with your own knowledge of the pet industry, competitors, or market trends, even if you are confident it is accurate.",
    "- Only include a conclusion if it is directly supported by text in the input table. Do not let reasoning introduce claims not in the source data.",
    "- If the table lacks enough information to fill a section, write fewer bullets. Quality over quantity.",
    "- If content is missing or unclear, write 'More research needed'.",
    
    "\n\nREQUIREMENTS:",
    "- Highlight trending topics or themes exclusively from the research data and choose the most actionable items. If a topic appears in Risks or Opportunities, DO NOT repeat it in Market Insights. Include short quotes from the source content in the bullets if relevant",
    "- Suggest Recommended Actions at the bottom of each section 1-4.", "Suggest more than one action, YOU MUST PREFACE EVERY RECOMMENDED ACTION WITH 'Recommended Actions:'",
    "Prioritize accuracy & strong reasoning. Each should be supported by the industry data and goal of advancing the ecommerce business.",
    "Each action must be directly triggered by a specific finding in the input table and reference that finding. Do not suggest actions from general business knowledge.",
    "- Prioritize clarity and brevity, each section <= 6 bullets, each bullet <= 25 words and 1-2 sentences with 1-4 word intro that must be formatted as 'Intro: sentences.'",
    "- End each bullet with the short name of the source_description field in parentheses. End each bullet with the short source name in parentheses. The cited source must be the specific row in the input table the information came from. Do not cite a source for information that did not come from that row.",
    "STRICT FIELD LIMITS:", "Recommended Actions <=3 sentences. \n\n",
    
    "\n\nSECTION DEFINITIONS:",
    "- Risks: competitor actions (Chewy, Amazon), recalls, manufacturer or logistics issues.",
    "- Opportunities: pet owner behavior shifts, household demographic trends, pet medical needs.",
    "- Market Insights: ecommerce news, executive-relevant highlights that do not fit Risks or Opportunities.",
    
    "\n\nVALIDATION - do this before finalizing:",
    "- For each bullet, find the exact input table row that supports it. If none exists, remove the bullet and any related Recommended Actions.",
    "- For each source citation, confirm it matches the row the information came from. Correct misattributions.",
    "- For each Recommended Action, confirm it references a specific table finding. Remove any that do not.\n\n",
    
    table_text
  )
  
  # ── Build request body ───────────────────────────────────────────────────────
  # temperature is intentionally omitted — it conflicts with extended thinking
  # and will cause an API error if included alongside the thinking parameter.
  body_list <- list(
    model      = model,
    max_tokens = max_tokens,
    thinking   = list(
      type          = "enabled",
      budget_tokens = budget_tokens
    ),
    messages = list(
      list(role = "user", content = prompt_text)
    )
  )
  
  # ── Call the Claude API ──────────────────────────────────────────────────────
  call_start <- Sys.time()
  
  response <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json",
      "anthropic-beta"    = "interleaved-thinking-2025-05-14"
    ),
    body   = jsonlite::toJSON(body_list, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(api_timeout_sec)
  )
  
  call_end <- Sys.time()
  
  # ── Error handling ───────────────────────────────────────────────────────────
  # Catches non-200 responses and surfaces the full API error message
  status <- httr::status_code(response)
  if (status != 200) {
    error_content <- tryCatch(
      httr::content(response, as = "text", encoding = "UTF-8"),
      error = function(e) "Unable to parse error response"
    )
    stop(paste0("API request failed\nStatus: ", status, "\nResponse: ", error_content))
  }
  
  # ── Parse response ───────────────────────────────────────────────────────────
  parsed <- httr::content(response, as = "parsed", encoding = "UTF-8")
  
  if (is.null(parsed$content) || length(parsed$content) == 0) {
    stop("Unexpected API response format — content block is empty or missing.")
  }
  
  # Filter to text blocks only — excludes thinking blocks which appear first
  # in the content array when extended thinking is enabled
  text_parts    <- Filter(function(b) b$type == "text", parsed$content)
  final_summary <- paste(sapply(text_parts, `[[`, "text"), collapse = "\n")
  
  # ── Extract token usage ──────────────────────────────────────────────────────
  input_tokens  <- as.integer(parsed$usage$input_tokens  %||% NA_integer_)
  output_tokens <- as.integer(parsed$usage$output_tokens %||% NA_integer_)
  
  # ── Save outputs ─────────────────────────────────────────────────────────────
  # Excel file saves raw brief text for audit and spot-check purposes
  # PDF is the branded deliverable distributed to stakeholders via Power Automate
  openxlsx::write.xlsx(
    data.frame(brief = final_summary),
    paste0(outputFolder, "/", format(Sys.Date(), "%Y-%m-%d "), "Writing Agent Results.xlsx")
  )
  
  source(file.path(myWD, "briefing_to_pdf.R"))
  briefing_to_pdf(
    briefing_text = final_summary,
    pdf_filename  = paste0(format(Sys.Date(), "%Y-%m-%d "), pdf_filename),
    outputFolder  = outputFolder
  )
  
  # ── Return results to calling script ─────────────────────────────────────────
  return(list(
    brief             = final_summary,
    input_tokens      = input_tokens,
    output_tokens     = output_tokens,
    call_duration_sec = as.numeric(difftime(call_end, call_start, units = "secs")),
    retry_count       = 0L,       # No retry logic in this agent — failures surface immediately
    status            = "success",
    error_message     = NA_character_
  ))
}