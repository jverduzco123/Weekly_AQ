# Recommendations for Multi-Week Data Support

## Overview
This document outlines the recommended approach for adding multi-week data support to your Shiny app.

## 1. Data Organization Strategy

### Option A: Load All Weeks at Startup (Recommended for small datasets)
- **Pros**: Fast filtering, works offline, simpler code
- **Cons**: Slower initial load if you have many weeks
- **Best for**: < 20 weeks of data

### Option B: Load Weeks Dynamically (Recommended for large datasets)
- **Pros**: Faster initial load, scalable
- **Cons**: More complex code, requires file system access
- **Best for**: > 20 weeks of data

**Recommendation**: Start with Option A (load all weeks). You can migrate to Option B later if needed.

## 2. Folder Structure

Keep your current structure but ensure consistent naming:
```
weekly_data/
  PurpleAir_Week_Oct19_Oct26_2025/
    EastOaklandRoseOct19to26.csv
    PittsburgRoseOct19to26.csv
    ...
  PurpleAir_Week_Oct26_Nov02_2025/
    EastOaklandRoseOct26toNov02.csv
    PittsburgRoseOct26toNov02.csv
    ...
  ...
```

## 3. Recommended Implementation Steps

### Step 1: Modify Data Loading to Include Week Information

Add a week identifier and date range to each data file when loading:

```r
# Function to extract week info from folder name
extract_week_info <- function(folder_path) {
  # Extract date range from folder name like "PurpleAir_Week_Oct19_Oct26_2025"
  folder_name <- basename(folder_path)
  # Parse dates and create a week identifier
  # Return: list(week_id, start_date, end_date, display_label)
}

# Modified data loading
load_weekly_data <- function(week_folder, week_info) {
  # Load all CSV files in the folder
  # Add week_id, start_date, end_date columns to each dataset
  # Return combined data frame
}
```

### Step 2: Add Week Selector to UI

Add a week selector dropdown under the location selector:

```r
selectInput("week", "Choose week",
  choices = week_choices,  # List of available weeks
  selected = latest_week   # Default to most recent week
)
```

### Step 3: Update Filtering Logic

Modify `df_filtered` reactive to filter by both location AND week:

```r
df_filtered <- reactive({
  req(nrow(df_all) > 0, input$area, input$week)
  df_all |> 
    filter(location == input$area, week_id == input$week)
})
```

## 4. Code Implementation

See the attached `app_multiweek.R` file for a complete implementation example.

## 5. Adding New Weeks (Weekly Workflow)

When adding a new week:

1. Create a new folder: `PurpleAir_Week_Nov02_Nov09_2025/`
2. Add CSV files to the folder
3. Update the data loading section to include the new week
4. Deploy the updated app

**Future enhancement**: Automate week detection by scanning the `weekly_data/` folder.

## 6. Week Display Labels

Use user-friendly labels like:
- "Week of October 19, 2025" instead of "Oct19_Oct26_2025"
- Sort chronologically with most recent first

## 7. Performance Considerations

- If loading all weeks, consider lazy loading (load on demand)
- Cache week summaries if calculations are expensive
- Consider data compression if files are large

## 8. Future Enhancements

- **Week comparison view**: Show multiple weeks side-by-side
- **Trend analysis**: Show how values change across weeks
- **Automatic week detection**: Scan folder and auto-discover weeks
- **Date range selector**: Allow custom date ranges instead of fixed weeks
