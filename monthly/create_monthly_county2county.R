# Convert weekly county-to-county mobility CSV files to monthly CSV files.
#
# Monthly assignment:
#   Each weekly file is assigned to the calendar month containing the week's
#   start date (the date embedded in its file name).
#
# Flow aggregation:
#   visitor_flows and pop_flows are summed for each origin-destination pair and
#   divided by the total number of weekly files in that month (4 or 5). Thus,
#   an OD pair that is absent from a weekly file contributes zero for that week.
#
# Other variables:
#   - GEOIDs are read as character data so leading zeroes are retained.
#   - Coordinate and identifier-like fields use the first non-missing value.
#   - number_devices_primary_daytime, when present, is averaged by origin over
#     the weeks for which that column is available.
#   - date_range reports the actual first weekly start through final weekly end
#     represented in the monthly file.
#
# Run from the project folder:
#   Rscript monthly/create_monthly_county2county.R
#
# Optional arguments:
#   1. Input directory  (default: county2county)
#   2. Output directory (default: monthly)
#   3. Comma-separated months for a partial run, e.g. 2020-04,2020-05

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop(
    "This script requires the data.table package. ",
    "Install it with install.packages('data.table') and rerun the script.",
    call. = FALSE
  )
}

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1L) args[[1L]] else "county2county"
output_dir <- if (length(args) >= 2L) args[[2L]] else "monthly"
selected_months <- if (length(args) >= 3L && nzchar(args[[3L]])) {
  trimws(strsplit(args[[3L]], ",", fixed = TRUE)[[1L]])
} else {
  character()
}

if (!dir.exists(input_dir)) {
  stop("Input directory does not exist: ", input_dir, call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

input_dir <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

file_pattern <- "^weekly_county2county_[0-9]{4}_[0-9]{2}_[0-9]{2}\\.csv$"
weekly_files <- list.files(
  input_dir,
  pattern = file_pattern,
  full.names = TRUE
)

if (length(weekly_files) == 0L) {
  stop("No weekly county-to-county CSV files found in: ", input_dir, call. = FALSE)
}

file_names <- basename(weekly_files)
date_text <- sub(
  "^weekly_county2county_([0-9]{4}_[0-9]{2}_[0-9]{2})\\.csv$",
  "\\1",
  file_names
)
week_start <- as.Date(date_text, format = "%Y_%m_%d")

if (anyNA(week_start)) {
  stop(
    "Could not parse the week-start date from: ",
    paste(file_names[is.na(week_start)], collapse = ", "),
    call. = FALSE
  )
}

file_index <- data.table(
  path = weekly_files,
  week_start = week_start,
  month = format(week_start, "%Y-%m")
)
setorder(file_index, week_start)

if (length(selected_months) > 0L) {
  invalid_months <- setdiff(selected_months, unique(file_index$month))
  if (length(invalid_months) > 0L) {
    stop(
      "Requested month(s) not found in the input files: ",
      paste(invalid_months, collapse = ", "),
      call. = FALSE
    )
  }
  file_index <- file_index[month %in% selected_months]
}

id_vars <- c("geoid_o", "geoid_d")
date_var <- "date_range"
flow_vars <- c("visitor_flows", "pop_flows")
device_var <- "number_devices_primary_daytime"
required_vars <- c(id_vars, date_var, flow_vars)
id_like_vars <- c(id_vars, "census_block_group")

first_non_missing <- function(x) {
  keep <- !is.na(x)
  if (is.character(x)) {
    keep <- keep & nzchar(x)
  }
  position <- which(keep)[1L]
  if (is.na(position)) {
    return(x[NA_integer_])
  }
  x[[position]]
}

month_ids <- unique(file_index$month)
cat(
  sprintf(
    "Converting %d weekly files into %d monthly files.\n",
    nrow(file_index),
    length(month_ids)
  )
)

for (month_id in month_ids) {
  month_files <- file_index[month == month_id]
  paths <- month_files$path
  n_weeks <- nrow(month_files)

  cat(sprintf("\n[%s] Reading %d weekly files...\n", month_id, n_weeks))

  headers <- lapply(
    paths,
    function(path) names(fread(path, nrows = 0L, showProgress = FALSE))
  )

  missing_required <- vapply(
    headers,
    function(header) length(setdiff(required_vars, header)) > 0L,
    logical(1L)
  )
  if (any(missing_required)) {
    details <- vapply(
      which(missing_required),
      function(i) {
        sprintf(
          "%s (missing: %s)",
          basename(paths[[i]]),
          paste(setdiff(required_vars, headers[[i]]), collapse = ", ")
        )
      },
      character(1L)
    )
    stop(
      "Required columns are missing from input files: ",
      paste(details, collapse = "; "),
      call. = FALSE
    )
  }

  month_columns <- unique(unlist(headers, use.names = FALSE))

  weekly_tables <- vector("list", n_weeks)
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    header <- headers[[i]]
    character_columns <- intersect(id_like_vars, header)

    weekly <- fread(
      path,
      colClasses = list(character = character_columns),
      na.strings = c("", "NA", "NaN"),
      showProgress = FALSE
    )

    if (!all(vapply(weekly[, ..flow_vars], is.numeric, logical(1L)))) {
      stop(
        "Flow variables are not numeric in: ",
        basename(path),
        call. = FALSE
      )
    }

    weekly[, (date_var) := NULL]
    weekly[, .week_number := i]
    weekly_tables[[i]] <- weekly

    cat(
      sprintf(
        "  %s: %s rows\n",
        basename(path),
        format(nrow(weekly), big.mark = ",", scientific = FALSE)
      )
    )
  }

  combined <- rbindlist(weekly_tables, use.names = TRUE, fill = TRUE)
  rm(weekly_tables)

  metadata_vars <- setdiff(
    month_columns,
    c(id_vars, date_var, flow_vars, device_var)
  )

  metadata <- combined[
    ,
    lapply(.SD, first_non_missing),
    by = id_vars,
    .SDcols = metadata_vars
  ]

  monthly_flows <- combined[
    ,
    lapply(.SD, function(x) sum(x, na.rm = TRUE) / n_weeks),
    by = id_vars,
    .SDcols = flow_vars
  ]

  monthly <- merge(
    metadata,
    monthly_flows,
    by = id_vars,
    all = TRUE,
    sort = FALSE
  )

  if (device_var %in% month_columns) {
    device_by_week <- combined[
      !is.na(number_devices_primary_daytime),
      .(
        number_devices_primary_daytime =
          first_non_missing(number_devices_primary_daytime),
        .device_value_count = uniqueN(number_devices_primary_daytime)
      ),
      by = c(".week_number", "geoid_o")
    ]

    conflicting_devices <- device_by_week[.device_value_count > 1L, .N]
    if (conflicting_devices > 0L) {
      warning(
        sprintf(
          "[%s] %d origin-week groups had multiple device counts; ",
          month_id,
          conflicting_devices
        ),
        "the first non-missing value was used for each affected origin-week."
      )
    }

    monthly_devices <- device_by_week[
      ,
      .(
        number_devices_primary_daytime =
          mean(number_devices_primary_daytime, na.rm = TRUE)
      ),
      by = "geoid_o"
    ]

    monthly <- merge(
      monthly,
      monthly_devices,
      by = "geoid_o",
      all.x = TRUE,
      sort = FALSE
    )
  }

  coverage_start <- min(month_files$week_start)
  coverage_end <- max(month_files$week_start) + 6
  monthly_date_range <- paste(
    format(coverage_start, "%m/%d/%y"),
    format(coverage_end, "%m/%d/%y"),
    sep = " - "
  )
  monthly[, (date_var) := monthly_date_range]

  output_columns <- c(
    id_vars,
    setdiff(month_columns, c(id_vars, date_var, flow_vars)),
    date_var,
    flow_vars
  )
  setcolorder(monthly, output_columns)
  setorder(monthly, geoid_o, geoid_d)

  if (anyDuplicated(monthly, by = id_vars) > 0L) {
    stop("Duplicate monthly OD keys produced for ", month_id, call. = FALSE)
  }

  output_path <- file.path(
    output_dir,
    sprintf("monthly_county2county_%s.csv", gsub("-", "_", month_id))
  )
  fwrite(monthly, output_path, na = "")

  cat(
    sprintf(
      "[%s] Wrote %s rows to %s\n",
      month_id,
      format(nrow(monthly), big.mark = ",", scientific = FALSE),
      basename(output_path)
    )
  )

  rm(combined, metadata, monthly_flows, monthly)
  if (exists("device_by_week", inherits = FALSE)) {
    rm(device_by_week, monthly_devices)
  }
  invisible(gc())
}

cat("\nMonthly conversion complete. Output directory:\n", output_dir, "\n")
