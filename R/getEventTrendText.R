#' Analyze Conflict Event Trends for LLM analysis
#'
#' Computes recent change (reference -> last observed, normalized by interval)
#' and long-term trend (slope over the overall period). Returns either a
#' structured data.frame or a single text block summarizing results per country.
#'
#' @param event.df data.frame. Conflict event data.
#' @param event.date.col character. Column name with event dates.
#' @param ctry.id.col character. Column name with country identifiers.
#' @param ctry.id character vector. Country identifier(s) to filter by.
#' @param event.type.col character. Column name with event type.
#' @param start.date Date or YYYY-MM-DD. Start of overall analysis period.
#' @param end.date Date or YYYY-MM-DD. End of overall analysis period.
#' @param period.length integer. Length of the last observed period. Default 1.
#' @param period.unit character. Units for last observed period ("month",
#'   "quarter", "year"). Default "month".
#' @param use.ctry.regex logical. If TRUE, regex-match countries; else exact
#'   match. Default TRUE.
#' @param by.event.type logical. If TRUE, compute by event type; else totals.
#'   Default TRUE.
#' @param output.as.text logical. If TRUE (default) return single character
#'   string; else data.frame.
#'
#' @return If output.as.text = FALSE, a data.frame with trend analysis results.
#'   If output.as.text = TRUE, a single character string with one block per
#'   country.
#'
#' @examples
#' \dontrun{
#' result <- getEventTrendText(
#'   event.df = acled.df,
#'   event.date.col = "event_date",
#'   ctry.id.col = "country",
#'   ctry.id = "Nigeria",
#'   event.type.col = "event_type",
#'   start.date = "2023-01-01",
#'   end.date = "2023-12-31",
#'   period.length = 6,
#'   period.unit = "month"
#' )
#' }
#'
#' @importFrom dplyr filter group_by summarise rename mutate left_join n across all_of arrange row_number case_when rowwise ungroup
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom rlang sym
#' @importFrom lubridate add_with_rollback period floor_date
#' @importFrom stats coef lm
#' @export
getEventTrendText <- function(event.df,
                              event.date.col,
                              ctry.id.col,
                              ctry.id,
                              event.type.col,
                              start.date,
                              end.date,
                              period.length = 1,
                              period.unit = "month",
                              use.ctry.regex = TRUE,
                              by.event.type = TRUE,
                              output.as.text = TRUE) {

  # Input validation
  if (!is.data.frame(event.df)) {
    stop("Argument 'event.df' must be a data.frame, got ", class(event.df)[1])
  }

  if (nrow(event.df) == 0) {
    stop("Input data is empty. Cannot proceed with analysis.")
  }

  required.cols <- c(event.date.col, ctry.id.col, event.type.col)
  missing.cols <- setdiff(required.cols, names(event.df))
  if (length(missing.cols) > 0) {
    stop(
      glue::glue(
        "Missing required columns: {paste(missing.cols, collapse = ', ')}"
      )
    )
  }

  if (!period.unit %in% c("month", "quarter", "year")) {
    stop(
      "Argument 'period.unit' must be one of: 'month', 'quarter', 'year', got '",
      period.unit, "'"
    )
  }

  if (length(ctry.id) == 0 || any(is.na(ctry.id)) || any(ctry.id == "")) {
    stop("Argument 'ctry.id' cannot be empty, NA, or contain empty strings")
  }

  if (!is.numeric(period.length) || period.length <= 0) {
    stop(
      "Argument 'period.length' must be positive numeric, got ",
      class(period.length)[1], " with value ", period.length
    )
  }

  # Coerce dates and validate
  start.date <- tryCatch(
    {
      as.Date(start.date)
    },
    error = function(e) {
      stop(
        "Invalid 'start.date' format: ", start.date, ". Use YYYY-MM-DD format."
      )
    }
  )

  end.date <- tryCatch(
    {
      as.Date(end.date)
    },
    error = function(e) {
      stop(
        "Invalid 'end.date' format: ", end.date, ". Use YYYY-MM-DD format."
      )
    }
  )

  if (end.date < start.date) {
    stop("Argument 'end.date' cannot be earlier than 'start.date'")
  }

  # Calculate derived dates properly
  overall.start.date <- start.date
  overall.end.date <- end.date

  # Calculate last observed period (end of the overall period)
  last.obs.end.date <- end.date
  last.obs.start.date <- lubridate::add_with_rollback(
    time = last.obs.end.date,
    period = -lubridate::period(num = period.length, units = period.unit)
  ) + 1

  # Calculate reference period (entire period minus the last observed period)
  reference.start.date <- overall.start.date
  reference.end.date <- last.obs.start.date - 1

  # Coerce event date column
  if (!inherits(event.df[[event.date.col]], "Date")) {
    event.df[[event.date.col]] <- tryCatch(
      {
        as.Date(event.df[[event.date.col]])
      },
      error = function(e) {
        stop(
          glue::glue(
            "Cannot convert '{event.date.col}' to Date format."
          )
        )
      }
    )
  }

  # Filter by countries
  if (use.ctry.regex) {
    regex.pattern <- paste(ctry.id, collapse = "|")
    event.sub.df <- tryCatch(
      {
        event.df |>
          dplyr::filter(
            grepl(
              pattern = regex.pattern,
              x = !!rlang::sym(ctry.id.col),
              ignore.case = TRUE
            )
          )
      },
      error = function(e) {
        stop(
          glue::glue(
            "Error in regex filtering: {e$message}"
          )
        )
      }
    )
  } else {
    event.sub.df <- event.df |>
      dplyr::filter(!!rlang::sym(ctry.id.col) %in% ctry.id)
  }

  if (nrow(event.sub.df) == 0) {
    warning(
      glue::glue(
        "No events found for country identifier(s): ",
        "{paste(sQuote(ctry.id), collapse = ', ')}"
      )
    )

    empty.result <- if (output.as.text) "" else data.frame()
    return(empty.result)
  }

  # Helper functions
  countEventsInPeriod <- function(df, start.date, end.date) {
    period.df <- df |>
      dplyr::filter(
        !!rlang::sym(event.date.col) >= start.date &
          !!rlang::sym(event.date.col) <= end.date
      )

    if (nrow(period.df) == 0) {
      if (by.event.type) {
        empty.result <- data.frame(
          country = character(0),
          event_type = character(0),
          count = numeric(0)
        )
        return(empty.result)
      }

      empty.result <- data.frame(
        country = character(0),
        count = numeric(0)
      )
      return(empty.result)
    }

    if (by.event.type) {
      result <- period.df |>
        dplyr::group_by(
          !!rlang::sym(ctry.id.col),
          !!rlang::sym(event.type.col)
        ) |>
        dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
        dplyr::rename(
          country = !!rlang::sym(ctry.id.col),
          event_type = !!rlang::sym(event.type.col)
        )
      return(result)
    } else {
      result <- period.df |>
        dplyr::group_by(!!rlang::sym(ctry.id.col)) |>
        dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
        dplyr::rename(country = !!rlang::sym(ctry.id.col))
      return(result)
    }
  }

  nIntervals <- function(start.date, end.date, unit) {
    s <- lubridate::floor_date(time = as.Date(start.date), unit = unit)
    e <- lubridate::floor_date(time = as.Date(end.date), unit = unit)
    if (e < s) return(0L)

    interval.count <- length(
      seq(from = s, to = e, by = unit)
    )
    return(interval.count)
  }

  labelTrendFromSlope <- function(slope) {
    if (is.na(slope)) return("Insufficient data")

    abs.slope <- abs(slope)
    if (abs.slope < 0.1) return("Stable")
    if (abs.slope < 0.5) {
      result <- if (slope > 0) "Minor increase" else "Minor decrease"
      return(result)
    }
    if (abs.slope < 1.5) {
      result <- if (slope > 0) "Moderate increase" else "Moderate decrease"
      return(result)
    }

    result <- if (slope > 0) "Major increase" else "Major decrease"
    return(result)
  }

  labelChangeFromPct <- function(pct) {
    # Handle NA values first
    if (is.na(pct)) return("Insufficient data")
    if (is.infinite(pct)) return("Major increase")

    abs.pct <- abs(pct)
    if (abs.pct < 5) return("No change")
    if (abs.pct < 20) {
      result <- if (pct > 0) "Minor increase" else "Minor decrease"
      return(result)
    }
    if (abs.pct < 50) {
      result <- if (pct > 0) "Moderate increase" else "Moderate decrease"
      return(result)
    }

    result <- if (pct > 0) "Major increase" else "Major decrease"
    return(result)
  }

  # Calculate reference & last observed counts using correct dates
  last.obs.counts.df <- countEventsInPeriod(
    df = event.sub.df,
    start.date = last.obs.start.date,
    end.date = last.obs.end.date
  )

  reference.counts.df <- countEventsInPeriod(
    df = event.sub.df,
    start.date = reference.start.date,
    end.date = reference.end.date
  )

  # Trend analysis (for slope calculation over overall period)
  trend.df <- event.sub.df |>
    dplyr::filter(
      !!rlang::sym(event.date.col) >= overall.start.date &
        !!rlang::sym(event.date.col) <= overall.end.date
    )

  if (nrow(trend.df) == 0) {
    warning("No events found in the overall analysis period")
    empty.result <- if (output.as.text) "" else data.frame()
    return(empty.result)
  }

  trend.df <- trend.df |>
    dplyr::mutate(
      time_period = dplyr::case_when(
        period.unit == "month" ~ lubridate::floor_date(
          time = !!rlang::sym(event.date.col),
          unit = "month"
        ),
        period.unit == "quarter" ~ lubridate::floor_date(
          time = !!rlang::sym(event.date.col),
          unit = "quarter"
        ),
        period.unit == "year" ~ lubridate::floor_date(
          time = !!rlang::sym(event.date.col),
          unit = "year"
        )
      )
    )

  if (by.event.type) {
    trend.agg.df <- trend.df |>
      dplyr::group_by(
        !!rlang::sym(ctry.id.col),
        !!rlang::sym(event.type.col),
        time_period
      ) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::rename(
        country = !!rlang::sym(ctry.id.col),
        event_type = !!rlang::sym(event.type.col)
      )
    group.vars <- c("country", "event_type")
  } else {
    trend.agg.df <- trend.df |>
      dplyr::group_by(!!rlang::sym(ctry.id.col), time_period) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::rename(country = !!rlang::sym(ctry.id.col))
    group.vars <- "country"
  }

  trend.results.df <- trend.agg.df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group.vars))) |>
    dplyr::arrange(time_period) |>
    dplyr::mutate(time_index = dplyr::row_number()) |>
    dplyr::summarise(
      trend_slope = if (dplyr::n() >= 3) {
        tryCatch(
          {
            model.result <- stats::lm(formula = count ~ time_index)
            slope.value <- stats::coef(model.result)[2]
            return(slope.value)
          },
          error = function(e) NA_real_
        )
      } else {
        NA_real_
      },
      .groups = "drop"
    )

  # Merge all components
  if (by.event.type) {
    all.combinations.df <- base::expand.grid(
      country = unique(event.sub.df[[ctry.id.col]]),
      event_type = unique(event.sub.df[[event.type.col]]),
      stringsAsFactors = FALSE
    )

    results.df <- all.combinations.df |>
      dplyr::left_join(
        y = last.obs.counts.df,
        by = c("country", "event_type")
      ) |>
      dplyr::rename(last_obs_count = count) |>
      dplyr::left_join(
        y = reference.counts.df,
        by = c("country", "event_type")
      ) |>
      dplyr::rename(reference_count = count) |>
      dplyr::left_join(
        y = trend.results.df,
        by = c("country", "event_type")
      )
  } else {
    all.combinations.df <- data.frame(
      country = unique(event.sub.df[[ctry.id.col]])
    )

    results.df <- all.combinations.df |>
      dplyr::left_join(y = last.obs.counts.df, by = "country") |>
      dplyr::rename(last_obs_count = count) |>
      dplyr::left_join(y = reference.counts.df, by = "country") |>
      dplyr::rename(reference_count = count) |>
      dplyr::left_join(y = trend.results.df, by = "country")
  }

  # Replace missing counts with 0
  results.df$last_obs_count[is.na(results.df$last_obs_count)] <- 0
  results.df$reference_count[is.na(results.df$reference_count)] <- 0

  # Add period metadata columns using corrected dates
  results.df <- results.df |>
    dplyr::mutate(
      reference_start = reference.start.date,
      reference_end = reference.end.date,
      last_obs_start = last.obs.start.date,
      last_obs_end = last.obs.end.date,
      overall_start = overall.start.date,
      overall_end = overall.end.date
    )

  # Calculate interval-normalized percentage change
  results.df <- results.df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      interval_unit = period.unit,
      reference_intervals = nIntervals(
        start.date = reference_start,
        end.date = reference_end,
        unit = period.unit
      ),
      last_obs_intervals = nIntervals(
        start.date = last_obs_start,
        end.date = last_obs_end,
        unit = period.unit
      ),
      reference_avg = ifelse(
        test = reference_intervals > 0,
        yes = reference_count / reference_intervals,
        no = NA_real_
      ),
      last_obs_avg = ifelse(
        test = last_obs_intervals > 0,
        yes = last_obs_count / last_obs_intervals,
        no = NA_real_
      )
    ) |>
    dplyr::ungroup()

  results.df <- results.df |>
    dplyr::mutate(
      pct_change = dplyr::case_when(
        is.na(reference_avg) | is.na(last_obs_avg) ~ NA_real_,
        reference_avg == 0 & last_obs_avg == 0 ~ 0,
        reference_avg == 0 & last_obs_avg > 0 ~ Inf,
        TRUE ~ (last_obs_avg - reference_avg) / reference_avg * 100
      ),
      pct_change_label = vapply(
        X = pct_change,
        FUN = labelChangeFromPct,
        FUN.VALUE = character(1)
      ),
      trend_label = vapply(
        X = trend_slope,
        FUN = labelTrendFromSlope,
        FUN.VALUE = character(1)
      ),
      interpretation_trend = dplyr::case_when(
        trend_label == "Insufficient data" ~ "Long-term trend - Insufficient data.",
        TRUE ~ glue::glue(
          "Long-term trend - {trend_label} ",
          "(slope {ifelse(is.na(trend_slope), 'NA', sprintf('%+.2f', trend_slope))} ",
          "events/period; {overall_start} - {overall_end})."
        )
      ),
      interpretation_pct = dplyr::case_when(
        is.na(pct_change) ~ "Recent change - Insufficient data to compute normalized change.",
        is.infinite(pct_change) ~ glue::glue(
          "Recent change - Major increase from reference period ",
          "({reference_start} - {reference_end}) to last observed period ",
          "({last_obs_start} - {last_obs_end}): from 0 to ",
          "{round(last_obs_avg, 1)} events per {interval_unit}."
        ),
        TRUE ~ glue::glue(
          "Recent change - {pct_change_label} from reference period ",
          "({reference_start} - {reference_end}) to last observed period ",
          "({last_obs_start} - {last_obs_end}): ",
          "{sprintf('%+.1f', pct_change)}% (reference {round(reference_avg, 1)} -> ",
          "last observed {round(last_obs_avg, 1)} per {interval_unit})."
        )
      )
    )

  # Round for readability
  results.df$pct_change <- round(results.df$pct_change, digits = 2)
  results.df$trend_slope <- round(results.df$trend_slope, digits = 4)

  # Data quality validation before proceeding
  if (output.as.text) {
    # Count how many event types have sufficient data
    sufficient.data.count <- results.df |>
      dplyr::summarise(
        sufficient_trend = sum(
          !is.na(trend_slope) & trend_label != "Insufficient data",
          na.rm = TRUE
        ),
        sufficient_change = sum(
          !is.na(pct_change) & pct_change_label != "Insufficient data",
          na.rm = TRUE
        )
      )

    # If no event types have sufficient data for either trend or change analysis
    if (sufficient.data.count$sufficient_trend == 0 &&
        sufficient.data.count$sufficient_change == 0) {
      stop(
        glue::glue(
          "Insufficient data for meaningful trend analysis for ",
          "{paste(ctry.id, collapse = ', ')}. ",
          "Period analyzed: {overall.start.date} to {overall.end.date}. ",
          "Consider using a longer time period or checking if events exist ",
          "for this country."
        )
      )
    }

    # If less than 25% of event types have sufficient data, issue a warning
    total.event.types <- nrow(results.df)
    sufficient.proportion <- (sufficient.data.count$sufficient_trend +
                                sufficient.data.count$sufficient_change) /
      (2 * total.event.types)

    if (sufficient.proportion < 0.25) {
      warning(
        glue::glue(
          "Limited data quality for trend analysis for ",
          "{paste(ctry.id, collapse = ', ')}. ",
          "Only {round(sufficient.proportion * 100, 1)}% of analyses have ",
          "sufficient data. Consider using a longer time period for more ",
          "robust results."
        )
      )
    }
  }

  if (!output.as.text) {
    return(results.df)
  }

  # Generate text output: single block per country
  country.names <- unique(results.df$country)

  blocks <- purrr::map_chr(
    .x = country.names,
    .f = function(country) {
      country.results <- results.df |>
        dplyr::filter(country == !!country)

      header <- glue::glue(
        "Country: {country}\n",
        "Overall period: {country.results$overall_start[1]} - {country.results$overall_end[1]}\n",
        "Reference period: {country.results$reference_start[1]} - {country.results$reference_end[1]}\n",
        "Last observed period: {country.results$last_obs_start[1]} - {country.results$last_obs_end[1]}"
      )

      # Generate body lines
      if (by.event.type && "event_type" %in% names(country.results)) {
        body.lines <- apply(
          X = country.results,
          MARGIN = 1,
          FUN = function(row) {
            glue::glue(
              "{row[['event_type']]}:\n {row[['interpretation_trend']]}\n {row[['interpretation_pct']]}"
            )
          }
        )
      } else {
        body.lines <- apply(
          X = country.results,
          MARGIN = 1,
          FUN = function(row) {
            glue::glue(
              "All events:\n {row[['interpretation_trend']]}\n {row[['interpretation_pct']]}"
            )
          }
        )
      }

      text.block <- glue::glue(
        "{header}\n\n{paste(body.lines, collapse = '\n\n')}"
      )
      return(text.block)
    }
  )

  # Return single string with blocks separated by blank lines
  final.result <- paste(blocks, collapse = "\n\n")
  return(final.result)
}
