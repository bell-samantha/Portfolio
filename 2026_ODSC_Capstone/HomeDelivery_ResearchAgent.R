# ============================================================
# HomeDelivery_ResearchAgent.R
#
# Research Agent — Veterinary E-commerce Intelligence Brief
#
# PURPOSE:
#   Accepts a source URL and fetches recent content from it
#   using the Claude API with the web_fetch server tool.
#   Returns a structured list of findings as parsed JSON,
#   along with token usage metrics for cost monitoring.
#
# CALLED BY:
#   HomeDelivery_Brief.Rmd — Section 2, Research Agent loop
#
# RETURNS:
#   A named list containing:
#     - data               : parsed JSON data frame of findings
#     - input_tokens       : raw input tokens used
#     - output_tokens      : output tokens used
#     - cache_write_tokens : tokens written to cache (first call)
#     - cache_read_tokens  : tokens read from cache (subsequent calls)
#     - cited_URL          : the URL actually fetched by the agent
#
# DEPENDENCIES:
#   httr, jsonlite
#   ANTHROPIC_API_KEY must be set as an environment variable
#
# NOTES:
#   - api_timeout_sec controls how long R waits for the full API
#     response, not the web fetch itself (which is server-side).
#     600 seconds is recommended to avoid false timeouts on slow sources.
#   - max_tokens controls output tokens only. Input, cache write,
#     and cache read tokens are tracked separately in the calling script.
#   - The system prompt is cached on the first call and reused on
#     subsequent calls in the loop, reducing input token costs.
#   - The %||% operator must be available in the calling environment.
# ============================================================


research_agent <- function(query,
                           model,
                           dateLimit,
                           api_timeout_sec = 600,  # How long R waits for the full API response
                           max_tokens = 3000) {    # Output tokens only
  
  # ── API key check ────────────────────────────────────────────────────────
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("ANTHROPIC_API_KEY environment variable not set.")
  
  # ── Build request body ───────────────────────────────────────────────────
  # system prompt is marked ephemeral so it is cached after the first call,
  # reducing input token costs on subsequent iterations of the source loop.
  body <- list(
    model       = model,
    max_tokens  = max_tokens,
    temperature = 0.1,        # Low temperature — minimizes creativity, keeps output true to source
    tools = list(
      list(
        type               = "web_fetch_20260209",
        name               = "web_fetch",
        max_uses           = 1,     # Max fetches per source call — controls cost
        max_content_tokens = 8000   # Max tokens returned per page fetch
      )
    ),
    system = list(
      list(
        type = "text",
        text = paste(
          "OUTPUT RULE: Respond with a raw JSON array only. Start with [ and end with ]. No explanation, no prose, no preamble, no markdown, no code fences. No thinking before or after the JSON. Violating this rule causes a system failure.",
          "You are a veterinary ecommerce researcher. When given a URL, fetch and extract content from that exact URL only. Do not search for other pages.",
          "When using web_fetch, read only enough content to find qualifying results. Stop fetching once you have up to 6 results. If no qualifying content is found, return [] immediately.",
          
          "Today's date is provided in the user message. This is authoritative. Never use your training data to estimate the current date.",
          "Only include articles published within 10 days before today's date. Discard anything outside that window silently. If nothing qualifies, return [] immediately.",
          "date_updated must be the explicit publication date as printed on the page. If not visible, return ''. Never infer or estimate a date.",
          
          "Only use information from the URL you were given. Never supplement with training knowledge or fill gaps.",
          "Focus on: Chewy and competitors, pet Rx, pet food, flea/tick/heartworm, recalls, pet owner shopping behavior, ecommerce.",
          "Extract specific claims, data points, and direct quotes. Every object MUST include all fields. If a field is unavailable, return as ''.",
          
          "quotes must be verbatim from the source. Use ... only within a single continuous passage to trim words — never to bridge separate sentences. Max 25 words excluding ellipsis.",
          "If no single passage can be quoted cleanly under 25 words, leave quotes as '' and capture the information in summary instead.",
          "Do NOT paraphrase, reconstruct, or join fragments from different parts of the article to form a quote.",
          
          "If web_fetch fails, returns nothing, or is rate limited, return [] immediately.",
          "STRICT FIELD LIMITS: summarize in 1-2 sentences only. Quotes <= 25 words excluding ellipsis.",
          "Max 5 results.",
          "Fields to return: 'source_description', 'summary', 'quotes', 'date_updated'"
        ),
        cache_control = list(type = "ephemeral")  # Cache system prompt — saves input tokens on calls 2+
      )
    ),
    messages = list(
      list(role = "user", content = paste(query))
    )
  )
  
  # ── Call the Claude API ──────────────────────────────────────────────────
  response <- httr::POST(
    url    = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "anthropic-beta"    = "prompt-caching-2024-07-31",
      "Content-Type"      = "application/json"
    ),
    body   = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(api_timeout_sec)
  )
  httr::stop_for_status(response)
  result <- httr::content(response, as = "parsed", simplifyVector = FALSE)
  
  # ── Extract token usage ──────────────────────────────────────────────────
  input_tokens       <- as.integer(result$usage$input_tokens                    %||% NA_integer_)
  output_tokens      <- as.integer(result$usage$output_tokens                   %||% NA_integer_)
  cache_write_tokens <- as.integer(result$usage$cache_creation_input_tokens     %||% NA_integer_)
  cache_read_tokens  <- as.integer(result$usage$cache_read_input_tokens         %||% NA_integer_)
  
  # ── Extract and preview raw response text ───────────────────────────────
  # Filters to text-type blocks only — excludes tool use and fetch result blocks
  # Preview printed to console for debugging unexpected output or JSON issues
  text_parts        <- Filter(function(b) b$type == "text", result$content)
  raw_response_text <- paste(sapply(text_parts, `[[`, "text"), collapse = "\n")
  
  cat("\n--- RAW RESPONSE PREVIEW ---\n")
  cat(substr(raw_response_text, 1, 1000), "\n")
  cat("--- END PREVIEW ---\n")
  
  # ── Extract fetched URL from response ────────────────────────────────────
  # Walks response content blocks to find the web_fetch_tool_result block.
  # Guards against error responses where content type is web_fetch_tool_error
  # rather than web_fetch_result, which would not contain a URL field.
  fetched_url <- ""
  for (block in result$content) {
    if (!is.null(block$type) && block$type == "web_fetch_tool_result") {
      if (!is.null(block$content) &&
          !is.null(block$content$type) &&
          block$content$type == "web_fetch_result" &&
          !is.null(block$content$url)) {
        fetched_url <- block$content$url
      }
      break  # Stop after first fetch result regardless of whether URL was found
    }
  }
  
  cat("\n--- URL PREVIEW ---\n")
  cat(if (nchar(fetched_url) > 0) fetched_url else "⚠️ No fetch URL found in response", "\n")
  cat("--- END PREVIEW ---\n")
  
  # ── Clean and parse JSON response ────────────────────────────────────────
  # Strip markdown code fences if the model wrapped output despite instructions
  no_fences <- gsub("```json|```", "", raw_response_text, perl = TRUE)
  
  # Extract content between first [ and last ] only
  # (?s) flag enables dot to match newlines for multi-line JSON arrays
  clean_response_text <- sub("(?s).*?(\\[.*\\]).*", "\\1", no_fences, perl = TRUE)
  
  # If no JSON array found, treat as empty result rather than throwing an error
  if (!startsWith(trimws(clean_response_text), "[")) {
    clean_response_text <- "[]"
  }
  
  # Parse JSON — stop with a descriptive error if parsing fails so the
  # calling loop can log the failure cleanly rather than crashing silently
  parsed_json <- tryCatch({
    jsonlite::fromJSON(clean_response_text)
  }, error = function(e) {
    stop(paste(
      "Anthropic API did not return valid JSON despite prompt instructions.",
      "Raw response:", raw_response_text,
      "Cleaned response:", clean_response_text,
      "Error:", e$message
    ))
  })
  
  # ── Return results to calling script ─────────────────────────────────────
  return(list(
    data               = parsed_json,
    input_tokens       = input_tokens,
    output_tokens      = output_tokens,
    cache_write_tokens = cache_write_tokens,
    cache_read_tokens  = cache_read_tokens,
    cited_URL          = fetched_url
  ))
}