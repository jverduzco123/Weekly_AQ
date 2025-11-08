
# Packages ----------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(showtext)
library(sysfonts)
library(tibble)
library(scales)

# Data --------------------------------------------------------------------
oakland_mon <- read.csv("~/Weekly_AQ/AQ_Weekly_Data/EastOaklandRoseOct19to26.csv")
#pittsburg <- read.csv("~/Weekly_AQ/AQ_Weekly_Data/PurpleAir_Week_Oct19_Oct26_2025/PittsburgRoseOct19to26.csv")
#pittsburg_high <- read.csv("~/Weekly_AQ/AQ_Weekly_Data/PurpleAir_Week_Oct19_Oct26_2025/PittsburgHighRoseOct19to26.csv")
#richmond <- read.csv("~/Weekly_AQ/AQ_Weekly_Data/PurpleAir_Week_Oct19_Oct26_2025/PointRichmondRoseOct19to26.csv")

# Finding best times + exposure windows
find_best_worst_windows <- function(hour_df,
                                    window_size = 3,
                                    allowed_start = 6,
                                    allowed_end = 22,
                                    require_full_window = TRUE) {
  
  # ensure one row per hour 0:23 (fill missing hours with NA)
  full_hours <- tibble(hour_of_day = 0:23) |> 
    left_join(hour_df |>  select(hour_of_day, hour_of_day_avg), by = "hour_of_day") |> 
    arrange(hour_of_day)

  
  n <- nrow(full_hours)
  full_hours <- full_hours |> 
    mutate(
      rolling_avg = sapply(1:n, function(i) {
        idx_end <- i + window_size - 1
        # if end goes past 24 wrap to beyond; here we will treat wrap as invalid window
        if (idx_end > n) return(NA_real_)
        mean(full_hours$hour_of_day_avg[i:idx_end], na.rm = TRUE)
      })
    )
  
  allowed_starts <- (allowed_start):(allowed_end)
  # e.g., 6:22
  # filter to starts where (start + window_size - 1) <= allowed_end
  allowed_starts <- allowed_starts[ (allowed_starts + window_size - 1) <= allowed_end ]

  if (length(allowed_starts) == 0) {
    return(list(
      error = TRUE,
      message = paste0("No valid ", window_size, "-hour windows fully inside ", allowed_start, "–", allowed_end, ". Try reducing window_size or expanding allowed range.")
    ))
  }
  
  candidates <- full_hours |>  filter(hour_of_day %in% allowed_starts)
  
  # if all candidate rolling_avg are NA, return message
  if (all(is.na(candidates$rolling_avg))) {
    return(list(
      error = TRUE,
      message = "No valid averages (maybe too many missing hourly values)."
    ))
  }
  
  best <- candidates |>  slice_min(rolling_avg, n = 1, with_ties = FALSE)
  worst <- candidates |>  slice_max(rolling_avg, n = 1, with_ties = FALSE)
  
  make_label <- function(start_hr) {
    end_hr <- (start_hr + window_size - 1) %% 24
    paste0(labels_12h[start_hr + 1], " – ", labels_12h[end_hr + 1])
  }
  
  list(
    error = FALSE,
    window_size = window_size,
    allowed_range = paste0(allowed_start, "–", allowed_end),
    best = list(
      start_hour = best$hour_of_day,
      label = make_label(best$hour_of_day),
      avg = round(best$rolling_avg, 1)
    ),
    worst = list(
      start_hour = worst$hour_of_day,
      label = make_label(worst$hour_of_day),
      avg = round(worst$rolling_avg, 1)
    )
  )
  
}

res <- find_best_worst_windows(
  hour_df = test_hour_of_day,
  window_size = 3,
  allowed_start = 6,
  allowed_end = 22
)

if (res$error) {
  message(res$message)
} else {
  cat("✅ Best time to go outside (lowest avg): ", res$best$label,
      " (avg ", res$best$avg, " µg/m³ )\n")
  cat("⚠️ Time to avoid (highest avg): ", res$worst$label,
      " (avg ", res$worst$avg, " µg/m³ )\n")
}

