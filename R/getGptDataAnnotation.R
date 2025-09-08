#' Get GPT Data Annotation for a Single Row
#'
#' Processes a single row of data through GPT to add annotations, classifications,
#' or other derived information. Useful for enhancing event datasets with
#' custom categories before performing narrative analysis.
#'
#' @param data.row A single row data frame with data to process
#' @param sys.prompt System prompt for the GPT chat
#' @param user.prompt.template User prompt template with {{field}} placeholders.
#'   Uses glue package syntax for string interpolation. See \code{?glue::glue}
#'   for template formatting details and advanced features.
#' @param prompt.fields Character vector of field names to use in the prompt.
#'   Must match the placeholder names in user.prompt.template (without braces).
#' @param gpt.model Model identifier string
#' @param api.token API token for authentication
#' @param api.base.url Base URL for the API endpoint
#' @param temperature Temperature parameter for the model (default: 0)
#'
#' @return The input row with added llm_response, model, api_call_timestamp,
#'   and error columns. If an error occurs, llm_response will be NA and error
#'   will be TRUE with details in error_message column.
#'
#' @details
#' The user.prompt.template parameter uses the glue package for string
#' interpolation. Field names should be enclosed in double braces: {{field_name}}.
#' The glue package supports advanced formatting options including conditional
#' text, loops, and custom formatting functions. For complete documentation
#' on template syntax, see the glue package manual: \code{vignette("glue")}.
#'
#' @examples
#' \dontrun{
#' # Example: Classify conflict events by cooperation/conflict type
#' event.df <- data.frame(
#'   event_date = "2024-01-15",
#'   actor1 = "France",
#'   actor2 = "Russia",
#'   description = "France's foreign ministry said Tuesday that Russia's military
#'   intelligence has long orchestrated cyberattacks on French entities."
#' )
#'
#' sys.prompt <- "You are an expert in conflict analysis. Classify events based on
#' the nature of interaction between actors using these categories: 'verbal cooperation',
#' 'material cooperation', 'verbal conflict', or 'material conflict'."
#'
#' user.template <- "Event date: {{event_date}}
#' Actors: {{actor1}} and {{actor2}}
#' Description: {{description}}
#'
#' Classify this interaction as one of: verbal cooperation, material cooperation,
#' verbal conflict, or material conflict. Respond with only the classification."
#'
#' getGptDataAnnotation(
#'   data.row = event.df,
#'   sys.prompt = sys.prompt,
#'   user.prompt.template = user.template,
#'   prompt.fields = c("event_date", "actor1", "actor2", "description"),
#'   gpt.model = "gpt-4o-mini",
#'   api.token = "your_token"
#' )
#' }
#'
#' @importFrom glue glue
#' @importFrom jrcgpt startGptChat addGptMessage getGptApiResponse getGptResponseText
#' @export
getGptDataAnnotation <- function(data.row,
                                 sys.prompt,
                                 user.prompt.template,
                                 prompt.fields,
                                 gpt.model,
                                 api.token,
                                 api.base.url,
                                 temperature = 0) {

  # Input validation
  if (missing(data.row) || !is.data.frame(data.row)) {
    stop("Argument 'data.row' must be a valid data.frame")
  }

  if (nrow(data.row) != 1) {
    stop(
      "Argument 'data.row' must contain exactly one row, got ",
      nrow(data.row), " rows"
    )
  }

  if (missing(sys.prompt) || !is.character(sys.prompt) || length(sys.prompt) != 1) {
    stop(
      "Argument 'sys.prompt' must be a single character string, got ",
      if (missing(sys.prompt)) "missing" else paste(class(sys.prompt)[1], "of length", length(sys.prompt))
    )
  }

  if (missing(user.prompt.template) || !is.character(user.prompt.template) ||
      length(user.prompt.template) != 1) {
    stop(
      "Argument 'user.prompt.template' must be a single character string, got ",
      if (missing(user.prompt.template)) "missing" else paste(class(user.prompt.template)[1], "of length", length(user.prompt.template))
    )
  }

  if (missing(prompt.fields) || !is.character(prompt.fields) || length(prompt.fields) == 0) {
    stop(
      "Argument 'prompt.fields' must be a non-empty character vector, got ",
      if (missing(prompt.fields)) "missing" else paste(class(prompt.fields)[1], "of length", length(prompt.fields))
    )
  }

  if (missing(gpt.model) || !is.character(gpt.model) || length(gpt.model) != 1) {
    stop(
      "Argument 'gpt.model' must be a single character string, got ",
      if (missing(gpt.model)) "missing" else paste(class(gpt.model)[1], "of length", length(gpt.model))
    )
  }

  if (missing(api.token) || !is.character(api.token) || length(api.token) != 1) {
    stop(
      "Argument 'api.token' must be a single character string, got ",
      if (missing(api.token)) "missing" else paste(class(api.token)[1], "of length", length(api.token))
    )
  }

  if (!is.numeric(temperature) || length(temperature) != 1 ||
      temperature < 0 || temperature > 1) {
    stop(
      "Argument 'temperature' must be numeric between 0 and 1, got ",
      class(temperature)[1], " with value ", temperature
    )
  }

  # Check if all required fields exist in the data
  missing.fields <- setdiff(prompt.fields, names(data.row))
  if (length(missing.fields) > 0) {
    stop(
      "Missing required fields in data.row: ",
      paste(missing.fields, collapse = ", ")
    )
  }

  # Extract placeholders from the template to validate
  template.placeholders <- unlist(
    regmatches(
      x = user.prompt.template,
      m = gregexpr(pattern = "\\{([^}]+)\\}", text = user.prompt.template)
    )
  )

  template.placeholders <- gsub(
    pattern = "[{}]",
    replacement = "",
    x = template.placeholders
  )

  # Check if prompt.fields match the placeholders in the template
  missing.in.template <- setdiff(prompt.fields, template.placeholders)
  if (length(missing.in.template) > 0) {
    stop(
      "Fields specified in 'prompt.fields' not found in template: ",
      paste(missing.in.template, collapse = ", ")
    )
  }

  unused.in.template <- setdiff(template.placeholders, prompt.fields)
  if (length(unused.in.template) > 0) {
    stop(
      "Template contains placeholders not specified in 'prompt.fields': ",
      paste(unused.in.template, collapse = ", ")
    )
  }

  # Check if any prompt fields have NA values
  if (any(sapply(data.row[, prompt.fields], is.na))){
    stop(
      "NA value detected in one of the following prompt fields: ",
      paste(sQuote(prompt.fields), collapse = ", "), "."
    )
  }

  # Main processing with error handling
  result.row <- tryCatch(
    {
      # Create a named list for glue substitution
      prompt.values.ls <- as.list(data.row[, prompt.fields, drop = FALSE])

      usr.prompt <- glue::glue(
        user.prompt.template,
        .envir = list2env(prompt.values.ls)
      )

      # Initialize chat with system prompt
      message.ls <- jrcgpt::startGptChat(
        content = sys.prompt,
        role = "system"
      )

      # Append user prompt
      message.ls <- jrcgpt::addGptMessage(
        messages = message.ls,
        content = usr.prompt,
        role = "user"
      )

      # Send API query (jrcgpt::getGptApiResponse handles retries internally)
      resp.ls <- jrcgpt::getGptApiResponse(
        token = api.token,
        base.url = api.base.url,
        model = gpt.model,
        api.type = "completions",
        messages = message.ls,
        temperature = temperature
      ) |>
        suppressMessages()

      # Extract response as text
      resp.txt <- jrcgpt::getGptResponseText(
        gpt.response = resp.ls,
        endpoint.type = "completions"
      )

      # Add LLM response and metadata to input df
      enhanced.row <- data.row
      enhanced.row$llm_response <- resp.txt
      enhanced.row$model <- gpt.model
      enhanced.row$api_call_timestamp <- as.character(Sys.time())
      enhanced.row$error <- FALSE

      return(enhanced.row)

    },
    error = function(e) {
      error.msg <- paste("Error in getGptDataAnnotation:", e$message)
      message(error.msg)

      # Return row with error information
      error.row <- data.row
      error.row$llm_response <- NA_character_
      error.row$model <- gpt.model
      error.row$api_call_timestamp <- as.character(Sys.time())
      error.row$error <- TRUE
      error.row$error_message <- e$message

      return(error.row)
    }
  )

  return(result.row)
}
