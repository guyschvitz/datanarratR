#' Generate GPT Summary for Long Text with Automatic Chunking
#'
#' This function summarizes large volumes of text using a two-stage GPT approach:
#' (1) splits long text into chunks that fit within token limits and summarizes
#' each chunk, (2) combines chunk summaries into a final consolidated summary.
#' Handles token estimation and automatic chunking to prevent API limits from
#' being exceeded.
#'
#' @param text.vec Character vector containing text entries to summarize
#' @param gpt.token Character string containing the API authentication token
#' @param gpt.base.url Character string for the GPT API base URL
#'   (default: "https://api.openai.com")
#' @param gpt.model Character string specifying the GPT model to use
#'   (default: "gpt-4o-mini")
#' @param system.prompt.stage1 Character string containing the system prompt
#'   for chunk summarization
#' @param user.prompt.stage1 Character string containing the user prompt
#'   template for chunk summarization (default: "Please summarize the following
#'   events: {text}")
#' @param system.prompt.stage2 Character string containing the system prompt
#'   for final consolidation
#' @param user.prompt.stage2 Character string containing the user prompt
#'   template for final consolidation
#' @param period.label Character string providing temporal context for the
#'   summary (optional, default: NULL)
#' @param max.input.tokens Numeric value for maximum input tokens per API call
#'   (default: 120000)
#' @param gpt.temperature Numeric value for GPT temperature parameter
#'   (default: 0)
#' @param gpt.max.output.tokens Numeric value for maximum output tokens from
#'   GPT (default: 2048)
#' @param token.multiplier Numeric multiplier for converting word count to
#'   estimated tokens (default: 1.3)
#' @param timeout Numeric. Request timeout in seconds (default: 120)
#' @param max.retries Integer. Maximum retry attempts (default: 3)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{final.summary} - Character string with the final consolidated summary
#'     \item \code{chunk.summaries} - Character vector with individual chunk summaries
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage with event text
#' event.texts <- c("Event 1 description...", "Event 2 description...", ...)
#'
#' summary.result <- getGptLongTextSummary(
#'   text.vec = event.texts,
#'   gpt.token = my_api_token,
#'   system.prompt.stage1 = "You are an expert analyst summarizing conflict events.",
#'   system.prompt.stage2 = "You are an expert analyst creating consolidated summaries."
#' )
#'
#' # Access results
#' final.summary <- summary.result$final.summary
#' chunk.summaries <- summary.result$chunk.summaries
#'
#' # Custom prompts and parameters
#' summary.result <- getGptLongTextSummary(
#'   text.vec = event.texts,
#'   gpt.token = my_api_token,
#'   system.prompt.stage1 = "Analyze these security events for key patterns.",
#'   user.prompt.stage1 = "Identify main actors and event types in:{text}",
#'   system.prompt.stage2 = "Synthesize these analyses into coherent overview.",
#'   user.prompt.stage2 = "Create final summary from:{text}",
#'   max.input.tokens = 100000,
#'   gpt.temperature = 0.1
#' )
#' }
#'
#' @importFrom tokenizers tokenize_words
#' @importFrom glue glue
#' @importFrom purrr map_dbl map_chr
#' @importFrom jrcgpt startGptChat addGptMessage getGptApiResponse
#' @export
getGptLongTextSummary <- function(text.vec,
                                  gpt.token,
                                  gpt.base.url = "https://api.openai.com",
                                  gpt.model = "gpt-4o-mini",
                                  system.prompt.stage1,
                                  user.prompt.stage1 = "Please summarize the following events{period_context}: {text}",
                                  system.prompt.stage2,
                                  user.prompt.stage2 = "Please provide a consolidated summary of these summaries{period_context}: {text}",
                                  period.label = NULL,
                                  max.input.tokens = 120000,
                                  gpt.temperature = 0,
                                  gpt.max.output.tokens = 2048,
                                  token.multiplier = 1.3,
                                  timeout = 120,
                                  max.retries = 3) {

  # Input validation
  if (missing(text.vec) || !is.character(text.vec) || length(text.vec) == 0) {
    stop(
      "Argument 'text.vec' must be a non-empty character vector, got ",
      if (missing(text.vec)) "missing" else paste(class(text.vec)[1], "of length", length(text.vec))
    )
  }

  if (missing(gpt.token) || !is.character(gpt.token) || nchar(gpt.token) == 0) {
    stop(
      "Argument 'gpt.token' must be a valid non-empty character string, got ",
      if (missing(gpt.token)) "missing" else paste(class(gpt.token)[1], "with", nchar(gpt.token), "characters")
    )
  }

  if (missing(system.prompt.stage1) || !is.character(system.prompt.stage1)) {
    stop(
      "Argument 'system.prompt.stage1' must be provided as a character string, got ",
      if (missing(system.prompt.stage1)) "missing" else class(system.prompt.stage1)[1]
    )
  }

  if (missing(system.prompt.stage2) || !is.character(system.prompt.stage2)) {
    stop(
      "Argument 'system.prompt.stage2' must be provided as a character string, got ",
      if (missing(system.prompt.stage2)) "missing" else class(system.prompt.stage2)[1]
    )
  }

  if (!is.numeric(max.input.tokens) || length(max.input.tokens) != 1 || max.input.tokens <= 0) {
    stop(
      "Argument 'max.input.tokens' must be a positive numeric value, got ",
      class(max.input.tokens)[1], " with value ", max.input.tokens
    )
  }

  if (!is.numeric(gpt.temperature) || length(gpt.temperature) != 1 ||
      gpt.temperature < 0 || gpt.temperature > 1) {
    stop(
      "Argument 'gpt.temperature' must be numeric between 0 and 1, got ",
      class(gpt.temperature)[1], " with value ", gpt.temperature
    )
  }

  if (!is.numeric(token.multiplier) || length(token.multiplier) != 1 || token.multiplier <= 0) {
    stop(
      "Argument 'token.multiplier' must be a positive numeric value, got ",
      class(token.multiplier)[1], " with value ", token.multiplier
    )
  }

  # Remove empty entries
  text.vec <- text.vec[nchar(text.vec) > 0]

  if (length(text.vec) == 0) {
    empty.result <- list(
      final.summary = "No text content to summarize.",
      chunk.summaries = character(0)
    )
    return(empty.result)
  }

  message("Starting long text summarization process")
  message("  - Input entries: ", length(text.vec))
  message("  - Max input tokens per call: ", max.input.tokens)

  # Main processing with error handling
  result.ls <- tryCatch(
    {
      # Handle period context for prompts
      if (is.null(period.label) || nchar(period.label) == 0) {
        period.context <- ""
      } else {
        period.context <- paste0(" from ", period.label)
      }

      # Estimate token counts for each entry
      message("  - Estimating token counts for chunking")
      entry.token.counts <- purrr::map_dbl(
        .x = text.vec,
        .f = function(entry) {
          word.count <- length(
            tokenizers::tokenize_words(x = entry)[[1]]
          )
          token.estimate <- ceiling(word.count * token.multiplier)
          return(token.estimate)
        }
      )

      # Calculate prompt overhead for stage 1
      stage1.user.prompt.base <- gsub(
        pattern = "\\{text\\}",
        replacement = "",
        x = user.prompt.stage1
      )
      prompt.overhead <- nchar(system.prompt.stage1) + nchar(stage1.user.prompt.base)
      effective.max.tokens <- max.input.tokens - prompt.overhead - 500  # Buffer for formatting

      message("  - Prompt overhead: ", prompt.overhead, " characters")
      message("  - Effective max tokens per chunk: ", effective.max.tokens)

      # Create chunks that fit within token limits
      message("  - Creating text chunks")
      chunks.ls <- list()
      current.chunk <- character(0)
      current.chunk.tokens <- 0
      chunk.count <- 0

      for (i in seq_along(text.vec)) {
        entry.tokens <- entry.token.counts[i]

        # If adding this entry would exceed limit, finalize current chunk
        if (current.chunk.tokens + entry.tokens > effective.max.tokens &&
            length(current.chunk) > 0) {
          chunk.count <- chunk.count + 1
          chunks.ls[[chunk.count]] <- list(
            text = current.chunk,
            token.count = current.chunk.tokens,
            entry.count = length(current.chunk)
          )
          current.chunk <- character(0)
          current.chunk.tokens <- 0
        }

        # Add entry to current chunk
        current.chunk <- c(current.chunk, text.vec[i])
        current.chunk.tokens <- current.chunk.tokens + entry.tokens
      }

      # Add final chunk if it has content
      if (length(current.chunk) > 0) {
        chunk.count <- chunk.count + 1
        chunks.ls[[chunk.count]] <- list(
          text = current.chunk,
          token.count = current.chunk.tokens,
          entry.count = length(current.chunk)
        )
      }

      message("  - Created ", length(chunks.ls), " chunks for processing")

      # Stage 1: Summarize each chunk
      message("  - Stage 1: Summarizing individual chunks")
      chunk.summaries.vec <- purrr::map_chr(
        .x = seq_along(chunks.ls),
        .f = function(chunk.idx) {

          chunk.data <- chunks.ls[[chunk.idx]]
          chunk.text <- paste(chunk.data$text, collapse = "\n\n")

          message(
            "    - Processing chunk ", chunk.idx, "/", length(chunks.ls),
            " (", chunk.data$entry.count, " entries, ~", chunk.data$token.count, " tokens)"
          )

          # Create messages for this chunk
          user.prompt.filled <- glue::glue(
            .x = user.prompt.stage1,
            text = chunk.text,
            period_context = period.context
          )

          messages <- jrcgpt::startGptChat(
            message = system.prompt.stage1,
            role = "system"
          )

          messages <- jrcgpt::addGptMessage(
            messages = messages,
            message = user.prompt.filled,
            role = "user"
          )

          # Get GPT response for chunk
          response <- jrcgpt::getGptApiResponse(
            token = gpt.token,
            base.url = gpt.base.url,
            model = gpt.model,
            api.type = "completions",
            messages = messages,
            max.tokens = gpt.max.output.tokens,
            temperature = gpt.temperature,
            timeout = timeout,
            max.retries = max.retries
          )

          # Extract summary from response
          chunk.summary <- response$choices[[1]]$message$content
          return(chunk.summary)
        }
      )

      message("  - Completed stage 1: ", length(chunk.summaries.vec), " chunk summaries generated")

      # Stage 2: Consolidate chunk summaries into final summary
      message("  - Stage 2: Creating consolidated final summary")

      if (length(chunk.summaries.vec) == 1) {
        # If only one chunk, use its summary as final summary
        final.summary <- chunk.summaries.vec[1]
        message("  - Single chunk processed, using chunk summary as final summary")

      } else {
        # Multiple chunks - consolidate summaries
        combined.summaries <- paste(chunk.summaries.vec, collapse = "\n\n")

        user.prompt.stage2.filled <- glue::glue(
          .x = user.prompt.stage2,
          text = combined.summaries,
          period_context = period.context
        )

        messages.stage2 <- jrcgpt::startGptChat(
          message = system.prompt.stage2,
          role = "system"
        )

        messages.stage2 <- jrcgpt::addGptMessage(
          messages = messages.stage2,
          message = user.prompt.stage2.filled,
          role = "user"
        )

        response.stage2 <- jrcgpt::getGptApiResponse(
          token = gpt.token,
          base.url = gpt.base.url,
          model = gpt.model,
          api.type = "completions",
          messages = messages.stage2,
          max.tokens = gpt.max.output.tokens,
          temperature = gpt.temperature,
          timeout = timeout,
          max.retries = max.retries
        )

        final.summary <- response.stage2$choices[[1]]$message$content
      }

      # Create result list
      final.result <- list(
        final.summary = final.summary,
        chunk.summaries = chunk.summaries.vec
      )

      message("  * Successfully completed long text summarization")
      message("  * Processed ", length(text.vec), " entries in ", length(chunks.ls), " chunks")
      message("  * Final summary length: ", nchar(final.result$final.summary), " characters")

      return(final.result)

    },
    error = function(e) {
      error.msg <- paste("Error in getGptLongTextSummary:", e$message)
      message(error.msg)
      stop(error.msg, call. = FALSE)
    }
  )

  return(result.ls)
}
