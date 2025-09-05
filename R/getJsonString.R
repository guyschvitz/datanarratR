#' Get JSON String from Text
#'
#' Extracts a JSON string from text that may contain additional content
#' before or after the JSON. Useful for handling LLM responses that don't follow
#' strict formatting instructions.
#'
#' @param text A character string containing a JSON object
#'
#' @return A character string containing the extracted JSON, or NULL if no
#'   valid JSON structure found
#'
#' @examples
#' \dontrun{
#' text <- "Here's the result: {\"type\": \"success\", \"value\": 42} Done!"
#' getJsonString(text = text)
#' }
#'
#' @importFrom stringr str_trim str_locate str_locate_all str_sub
#' @export
getJsonString <- function(text) {

  # Input validation
  if (!is.character(text) || length(text) != 1) {
    stop(
      "Argument 'text' must be a single character string, got ",
      class(text)[1], " of length ", length(text)
    )
  }

  if (is.na(text) || nchar(text) == 0) {
    warning("Input text is empty or NA. Cannot extract JSON.")
    null.result <- NULL
    return(null.result)
  }

  # Remove extra whitespace and newlines
  text.clean <- stringr::str_trim(string = text)

  # Find the first opening brace and last closing brace
  first.brace.location <- stringr::str_locate(
    string = text.clean,
    pattern = "\\{"
  )

  first.brace <- first.brace.location[1, "start"]

  last.brace.matches <- stringr::str_locate_all(
    string = text.clean,
    pattern = "\\}"
  )[[1]]

  if (nrow(last.brace.matches) == 0) {
    warning("No closing brace '}' found in text. Cannot extract JSON.")
    null.result <- NULL
    return(null.result)
  }

  last.brace <- max(last.brace.matches[, "start"])

  # Check if we found both braces
  if (is.na(first.brace) || is.na(last.brace) || first.brace >= last.brace) {
    warning(
      "No valid JSON object structure found in text. Missing or mismatched braces."
    )
    null.result <- NULL
    return(null.result)
  }

  # Extract the JSON substring
  json.str <- stringr::str_sub(
    string = text.clean,
    start = first.brace,
    end = last.brace
  )

  return(json.str)
}
