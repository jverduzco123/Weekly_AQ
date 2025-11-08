
# Packages ----------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


# Read in Data and Handle Timezones ---------------------------------------

clean_monitor_data <- function(path, location_name) {
  df <- read.csv(path)
  
  df <- df |> 
    mutate(
      parsed_pt_date = with_tz(ymd_hms(time_stamp),"America/Los_Angeles"),
      hour_of_day = hour(parsed_pt_date),
      day  = day(parsed_pt_date),
      dow  = wday(parsed_pt_date),
      location = location_name
    )
  
  return(df)
}


# Function to plot by hour of day -----------------------------------------

plot_hour_of_day <- function(df) {
  
  #labels for hours
  labels_12h <- c("12 AM","1 AM","2 AM","3 AM","4 AM","5 AM",
                  "6 AM","7 AM","8 AM","9 AM","10 AM","11 AM",
                  "12 PM","1 PM","2 PM","3 PM","4 PM","5 PM",
                  "6 PM","7 PM","8 PM","9 PM","10 PM","11 PM")
  
  df_hour <- df |> 
    group_by(hour_of_day) |> 
    summarize(hour_of_day_avg = mean(pm2.5_atm, na.rm = TRUE), .groups = "drop")
  
  #plot by hour of day
  ggplot(df_hour, aes(x = hour_of_day, y = hour_of_day_avg)) +
    geom_area(fill = "#8CC0DE", alpha = 0.3) +       # soft shaded background
    geom_line(color = "#0077B6", size = 1) +         # main line
    geom_point(color = "#023E8A", size = 2) +        # points for clarity
    scale_x_continuous(breaks = 0:23,labels = labels_12h) +              # show all hours
    labs(
      title = "Average PM2.5 Levels by Hour of Day",
      subtitle = paste("Weekly summary for", unique(df$location)),
      x = "Hour of Day (PT)",
      y = "Average PM2.5 (µg/m³)",
      caption = "Data from community air monitor • Source: PurpleAir"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray30"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(angle = 45, hjust = 1, vjust =1, color = "gray20",size = 10),
      axis.title = element_text(face = "bold"),
      plot.caption = element_text(size = 9, color = "gray40", hjust = 0)
    )
}


# Function to plot by day of week -----------------------------------------

plot_day_of_week <- function(df) {
   
  #labels for days
  labels_dow <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  
  #average by day of week
  df_dow <- df |>
    group_by(dow) |>
    summarize(avg_pm25 = mean(pm2.5_atm, na.rm = TRUE), .groups = "drop")
  
  ggplot(df_dow, aes(x = factor(dow, levels = 1:7, labels = labels_dow), y = avg_pm25)) +
    geom_col(fill = "#0077B6", alpha = 0.8) +
    geom_text(aes(label = round(avg_pm25, 1)), vjust = -0.4, size = 3.5) +
    labs(
      title = paste("Average PM2.5 by Day of Week -", unique(df$location)),
      x = "",
      y = "Average PM2.5 (µg/m³)",
      caption = "Data: PurpleAir community monitor"
    ) +
    theme_minimal()
}

oakland <- clean_monitor_data("~/Weekly_AQ/AQ_Weekly_Data/PurpleAir_Week_Oct19_Oct26_2025/EastOaklandRoseOct19to26.csv","East Oakland Rose Monitor")

