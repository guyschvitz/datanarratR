#' Extract and Format Text from Event Data
#'
#' This function extracts and formats narrative descriptions from event data,
#' with customizable column selection, deduplication, and text formatting.
#' Designed to work with output from getEventDataByPeriods() or any event data.frame.
#'
#' @param event.df A data.frame containing event data
#' @param text.cols Character vector specifying columns to include in text
#'   output (default: c("event_date", "event_type", "notes"))
#' @param dedup.cols Character vector specifying columns to use for
#'   deduplication (default: same as text.cols)
#' @param text.format Character string with glue-style formatting template
#'   (default: ACLED format)
#' @param sort.col Character string specifying column to sort by
#'   (default: "event_date")
#' @param sort.ascending Logical indicating sort direction (default: TRUE)
#'
#' @return Character vector containing formatted text descriptions, one per
#'   unique event
#'
#' @examples
#' \dontrun{
#' # Example 1: Default ACLED formatting
#' text.output <- getEventDetailsText(event.df = period.data)
#'
#' # Example 2: Custom ACLED format with more details
#' text.output <- getEventDetailsText(
#'   event.df = period.data,
#'   text.cols = c("event_date", "event_type", "actor1", "actor2", "fatalities", "notes"),
#'   text.format = "Date: {event_date}\nType: {event_type}\nActors: {actor1} vs {actor2}\nFatalities: {fatalities}\nDescription: {notes}"
#' )
#'
#' # Example 3: UCDP-GED format
#' text.output <- getEventDetailsText(
#'   event.df = ucdp.data,
#'   text.cols = c("date_start", "conflict_name", "deaths_total", "adm_1"),
#'   dedup.cols = c("date_start", "conflict_name", "adm_1"),
#'   text.format = "Date: {date_start}\nConflict: {conflict_name}\nDeaths: {deaths_total}\nLocation: {adm_1}",
#'   sort.col = "date_start"
#' )
#'
#' # Example 4: Simple format for quick overview
#' text.output <- getEventDetailsText(
#'   event.df = event.data,
#'   text.cols = c("event_date", "event_type"),
#'   text.format = "{event_date}: {event_type}"
#' )
#'
#' # Usage with getEventDataByPeriods output
#' period.result <- getEventDataByPeriods(...)
#' period.texts.ls <- purrr::map(period.result$period.data, getEventDetailsText)
#' }
#'
#' @importFrom data.table as.data.table
#' @importFrom glue glue_data
#' @export
getEventDetailsText <- function(event.df,
                                text.cols = c("event_date", "event_type", "notes"),
                                dedup.cols = NULL,
                                text.format = "Date: {event_date}\nType: {event_type}\nDescription: {notes}",
                                sort.col = "event_date",
                                sort.ascending = TRUE) {

  # Input validation
  if (missing(event.df) || !is.data.frame(event.df)) {
    stop("Argument 'event.df' must be a valid data.frame")
  }

  if (nrow(event.df) == 0) {
    empty.result <- character(0)
    return(empty.result)
  }

  if (!is.character(text.cols) || length(text.cols) == 0) {
    stop(
      "Argument 'text.cols' must be a non-empty character vector, got ",
      class(text.cols)[1], " of length ", length(text.cols)
    )
  }

  if (!is.character(text.format) || length(text.format) != 1) {
    stop(
      "Argument 'text.format' must be a single character string, got ",
      class(text.format)[1], " of length ", length(text.format)
    )
  }

  if (!is.character(sort.col) || length(sort.col) != 1) {
    stop(
      "Argument 'sort.col' must be a single character string, got ",
      class(sort.col)[1], " of length ", length(sort.col)
    )
  }

  if (!is.logical(sort.ascending) || length(sort.ascending) != 1) {
    stop(
      "Argument 'sort.ascending' must be a single logical value, got ",
      class(sort.ascending)[1], " of length ", length(sort.ascending)
    )
  }

  # Use text.cols for deduplication if dedup.cols not specified
  if (is.null(dedup.cols)) {
    dedup.cols <- text.cols
  }

  # Check that required columns exist
  all.required.cols <- unique(c(text.cols, dedup.cols, sort.col))
  missing.cols <- all.required.cols[!all.required.cols %in% names(event.df)]
  if (length(missing.cols) > 0) {
    stop(
      "Missing required columns in event.df: ",
      paste(missing.cols, collapse = ", ")
    )
  }

  message("Processing ", nrow(event.df), " events for text extraction")
  message("  - Using columns for text: ", paste(text.cols, collapse = ", "))
  message("  - Using columns for deduplication: ", paste(dedup.cols, collapse = ", "))

  # Main processing with error handling
  result.vec <- tryCatch(
    {
      # Convert to data.table for efficient processing
      if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("Package 'data.table' is required but not available")
      }

      dt <- data.table::as.data.table(event.df)

      # Ensure sort column is appropriate type for sorting
      if (sort.col %in% names(dt) && is.character(dt[[sort.col]])) {
        # Try to convert date-like strings to Date objects
        if (grepl(pattern = "date", x = sort.col, ignore.case = TRUE)) {
          dt[[sort.col]] <- tryCatch(
            {
              converted.dates <- as.Date(dt[[sort.col]])
              message("  - Converted ", sort.col, " to Date for sorting")
              return(converted.dates)
            },
            error = function(e) {
              message(
                "  - Warning: Could not convert ", sort.col,
                " to Date, using as character"
              )
              return(dt[[sort.col]])
            }
          )
        }
      }

      # Remove duplicates based on specified columns
      dt.unique <- unique(x = dt, by = dedup.cols)

      if (nrow(dt.unique) < nrow(dt)) {
        duplicates.removed <- nrow(dt) - nrow(dt.unique)
        message("  - Removed ", duplicates.removed, " duplicate events")
      }

      # Sort by specified column
      if (sort.ascending) {
        data.table::setorder(x = dt.unique, cols = sort.col)
      } else {
        data.table::setorder(x = dt.unique, cols = sort.col, order = -1L)
      }

      # Create formatted text output
      message("  - Formatting text using glue template")

      # Convert data.table back to data.frame for glue compatibility
      df.unique <- as.data.frame(dt.unique)

      # Use glue to format text with the data.frame
      formatted.text.vec <- glue::glue_data(.x = df.unique, .sep = text.format)

      # Convert glue output to character vector
      final.result <- as.character(formatted.text.vec)

      message("  * Successfully extracted and formatted ", length(final.result), " unique events")

      return(final.result)

    },
    error = function(e) {
      error.msg <- paste("Error in getEventDetailsText:", e$message)
      message(error.msg)
      stop(error.msg, call. = FALSE)
    }
  )

  return(result.vec)
}
