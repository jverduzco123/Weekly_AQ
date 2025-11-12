# Multi-Week Implementation Guide

## Step-by-Step Code Changes

### Step 1: Update Data Loading Function

Modify `clean_monitor_data` to accept week information:

```r
# Modified data parsing function
clean_monitor_data <- function(path, location_name, week_id, week_start, week_end) {
  df <- read.csv(path)
  df <- df |>
    mutate(
      parsed_pt_date = with_tz(ymd_hms(time_stamp), "America/Los_Angeles"),
      hour_of_day = hour(parsed_pt_date),
      day = day(parsed_pt_date),
      dow = wday(parsed_pt_date),
      location = location_name,
      week_id = week_id,
      week_start = week_start,
      week_end = week_end,
      week_label = paste0("Week of ", format(week_start, "%B %d, %Y"))
    )
  df
}
```

### Step 2: Load Data for All Weeks

Replace the single week loading with a function that loads all weeks:

```r
# Load data for a single week
load_week_data <- function(week_folder, week_id, week_start, week_end) {
  base_path <- file.path("weekly_data", week_folder)
  
  oakland_mon <- clean_monitor_data(
    file.path(base_path, "EastOaklandRoseOct19to26.csv"),
    "East Oakland Sensor", week_id, week_start, week_end
  )
  pittsburg <- clean_monitor_data(
    file.path(base_path, "PittsburgRoseOct19to26.csv"),
    "Pittsburg Sensor", week_id, week_start, week_end
  )
  pittsburg_high <- clean_monitor_data(
    file.path(base_path, "PittsburgHighRoseOct19to26.csv"),
    "Pittsburg High Sensor", week_id, week_start, week_end
  )
  richmond <- clean_monitor_data(
    file.path(base_path, "PointRichmondRoseOct19to26.csv"),
    "Point Richmond Sensor", week_id, week_start, week_end
  )
  
  bind_rows(oakland_mon, pittsburg, pittsburg_high, richmond)
}

# Load all weeks
week_oct19 <- load_week_data(
  "PurpleAir_Week_Oct19_Oct26_2025",
  "oct19_oct26_2025",
  as.Date("2025-10-19"),
  as.Date("2025-10-26")
)

# When you add a new week, add it here:
# week_oct26 <- load_week_data(
#   "PurpleAir_Week_Oct26_Nov02_2025",
#   "oct26_nov02_2025",
#   as.Date("2025-10-26"),
#   as.Date("2025-11-02")
# )

# Combine all weeks
df_all <- bind_rows(week_oct19)  # Add more weeks: bind_rows(week_oct19, week_oct26, ...)

# Create week choices for dropdown (sorted by date, most recent first)
week_choices <- df_all |>
  distinct(week_id, week_label, week_start) |>
  arrange(desc(week_start)) |>
  deframe()  # Creates named vector: c("Week of Oct 19, 2025" = "oct19_oct26_2025", ...)
```

### Step 3: Add Week Selector to UI

Add the week selector right after the location selector:

```r
h4("Filters"),
selectInput("area", "Choose location",
  choices = c(unique(df_all$location)),
  selected = "East Oakland Sensor"
),
selectInput("week", "Choose week",
  choices = week_choices,
  selected = names(week_choices)[1]  # Default to most recent week
),
```

### Step 4: Update Filtering Logic

Modify `df_filtered` reactive to filter by both location and week:

```r
# Location and week filtered data
df_filtered <- reactive({
  req(nrow(df_all) > 0, input$area, input$week)
  df_all |> 
    filter(location == input$area, week_id == input$week)
})
```

## Alternative: Dynamic Week Detection

If you want to automatically detect weeks from the folder structure:

```r
# Auto-detect weeks from folder structure
detect_weeks <- function(base_path = "weekly_data") {
  week_folders <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
  week_folders <- week_folders[grepl("PurpleAir_Week_", week_folders)]
  
  # Extract dates from folder names
  week_info <- lapply(week_folders, function(folder) {
    # Parse "PurpleAir_Week_Oct19_Oct26_2025" to extract dates
    parts <- strsplit(folder, "_")[[1]]
    # Extract and parse dates
    # Return: list(week_id, week_start, week_end, folder_name)
  })
  
  week_info
}

# Use detected weeks to load data
```

## Complete Example Integration

Here's how the modified sections would look:

**Data Loading Section:**
```r
# Week 1: October 19-26, 2025
week_oct19 <- load_week_data(
  "PurpleAir_Week_Oct19_Oct26_2025",
  "oct19_oct26_2025",
  as.Date("2025-10-19"),
  as.Date("2025-10-26")
)

# Week 2: October 26-November 2, 2025 (add when available)
# week_oct26 <- load_week_data(
#   "PurpleAir_Week_Oct26_Nov02_2025",
#   "oct26_nov02_2025",
#   as.Date("2025-10-26"),
#   as.Date("2025-11-02")
# )

# Combine all weeks
df_all <- bind_rows(week_oct19)  # Add: week_oct26, etc.

# Create week choices for UI
week_choices <- df_all |>
  distinct(week_id, week_label, week_start) |>
  arrange(desc(week_start)) |>
  { function(x) setNames(x$week_id, x$week_label) }()
```

**UI Section:**
```r
h4("Filters"),
selectInput("area", "Choose location",
  choices = c(unique(df_all$location)),
  selected = "East Oakland Sensor"
),
selectInput("week", "Choose week",
  choices = week_choices,
  selected = names(week_choices)[1]
),
```

**Server Section:**
```r
df_filtered <- reactive({
  req(nrow(df_all) > 0, input$area, input$week)
  df_all |> 
    filter(location == input$area, week_id == input$week)
})
```

## Adding New Weeks (Weekly Workflow)

1. **Create new folder**: `weekly_data/PurpleAir_Week_Oct26_Nov02_2025/`
2. **Add CSV files** to the folder
3. **Update app.R**:
   - Add a new `load_week_data()` call
   - Add the new week to `bind_rows()`
4. **Deploy** the updated app

The week will automatically appear in the dropdown, sorted by date.

## Notes

- Week labels are user-friendly (e.g., "Week of October 19, 2025")
- Weeks are sorted with most recent first
- Default selection is the most recent week
- Easy to add new weeks: just add one `load_week_data()` call and update `bind_rows()`
