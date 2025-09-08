# datanarratR

Conflict Event Data Processing and Analysis with LLM Summarisation

## Overview

`datanarratR` provides tools for processing, analysing, and summarising conflict event datasets such as ACLED and UCDP-GED. The package helps to extract key insights on events, trends and actors and converts them into narrative summaries using LLMs.

The goal is to automate parts of the data analysis workflow and make data insights more easily accessible for non-technical audiences.

## Key Features

- **Event Data Processing**: Filter and aggregate conflict events by countries, time periods, and event types
- **Trend Analysis**: Calculate recent changes and long-term trends
- **Text Extraction**: Generate formatted narrative descriptions from structured event data
- **LLM Summarisation**: Automatically summarise large volumes of text using GPT APIs with smart chunking
- **Data Annotation**: Enhance datasets with AI-generated classifications and labels

## Installation

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install datanarratR from GitHub
devtools::install_github("guyschvitz/datanarratR")
```

## Dependencies

The package requires several dependencies that will be installed automatically:

- `data.table` - Fast data processing
- `dplyr` - Data manipulation 
- `glue` - String interpolation
- `lubridate` - Date handling
- `purrr` - Functional programming, iterations
- `rlang` - Tidy evaluation
- `stringr` - String operations
- `tokenizers` - Text tokenisation
- `jrcgpt` - GPT API integration

## Quick Start

```r
library(datanarratR)

# 1. Process event counts by time period
event_summary <- getEventCountText(
  event.df = cled.df,
  start.date = "2023-01-01",
  end.date = "2023-12-31",
  event.date.col = "event_date",
  ctry.id.col = "country",
  ctry.id = "Nigeria"
)

# 2. Analyse trends over time
trend_analysis <- getEventTrendText(
  event.df = acled.df,
  event.date.col = "event_date", 
  ctry.id.col = "country",
  ctry.id = "Nigeria",
  event.type.col = "event_type",
  start.date = "2023-01-01",
  end.date = "2023-12-31"
)

# 3. Extract narrative details
event_narratives <- getEventDetailsText(
  event.df = acled.df,
  text.cols = c("event_date", "event_type", "notes"),
  text.format = "Date: {event_date}\nType: {event_type}\nDescription: {notes}"
)

# 4. Generate AI summary
summary_result <- getGptLongTextSummary(
  text.vec = event_narratives,
  gpt.token = your_api_token,
  system.prompt.stage1 = "Summarise these conflict events",
  system.prompt.stage2 = "Create a consolidated analysis"
)
```

## Use Cases

- Generate automated summaries of recent conflict developments
- Analyse long-term trends in conflict patterns
- Add AI-generated classifications to event datasets

## License

MIT License - see LICENSE file for details.

## Support

For questions or issues, please open an issue on the GitHub repository: https://github.com/guyschvitz/datanarratR/issues
