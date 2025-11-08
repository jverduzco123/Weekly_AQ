

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

# Timestamps -----------------------------------------------------------------

# parse time stamp and turn from UTC into PT
oakland_working <-
  oakland_mon |> 
  mutate(
    parsed_pt_date = with_tz(ymd_hms(time_stamp),"America/Los_Angeles")
  )

#add in hour of day, day number, day of week
oakland_working <- oakland_working |>
  mutate(
    hour_of_day = hour(parsed_pt_date),
    day  = day(parsed_pt_date),
    dow  = wday(parsed_pt_date)
  )

#labels for hours
labels_12h <- c("12 AM","1 AM","2 AM","3 AM","4 AM","5 AM",
                "6 AM","7 AM","8 AM","9 AM","10 AM","11 AM",
                "12 PM","1 PM","2 PM","3 PM","4 PM","5 PM",
                "6 PM","7 PM","8 PM","9 PM","10 PM","11 PM")

#average by hour of day
test_hour_of_day <- oakland_working |>
  group_by(hour_of_day) |>
  summarize(hour_of_day_avg = mean(pm2.5_atm, na.rm = TRUE), .groups = "drop")

#plot by hour of day
ggplot(test_hour_of_day, aes(x = hour_of_day, y = hour_of_day_avg)) +
  geom_area(fill = "#8CC0DE", alpha = 0.3) +       # soft shaded background
  geom_line(color = "#0077B6", size = 1) +         # main line
  geom_point(color = "#023E8A", size = 2) +        # points for clarity
  scale_x_continuous(breaks = 0:23,labels = labels_12h) +              # show all hours
  labs(
    title = "Average PM2.5 Levels by Hour of Day",
    subtitle = "Weekly summary for [Location Name]",
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

#labels for days
labels_dow <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")

#average by day of week
test_day_of_week <- oakland_working |>
  group_by(dow) |>
  summarize(dow_avg = mean(pm2.5_atm, na.rm = TRUE), .groups = "drop")

#plot
ggplot(test_day_of_week, aes(x = factor(dow, levels = 1:7, labels = labels_dow),
                             y = dow_avg)) +
  geom_col(fill = "#0077B6", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = round(dow_avg, 1)), vjust = -0.4, size = 3.5, color = "gray20") +
  labs(
    title = "Average PM2.5 Levels by Day of Week",
    subtitle = "Weekly summary for [Location Name]",
    x = "",
    y = "Average PM2.5 (µg/m³)",
    caption = "Data from community air monitor • Source: PurpleAir"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "gray20", size = 10),
    axis.text.y = element_text(color = "gray20", size = 10),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0)
  )


# Other summarizing options
line_plot <- ggplot(data = test_hour_of_day, aes(x=hour_of_day, y = hour_of_day_avg))+
  geom_line()+
  geom_point()

bar_chart <- ggplot(data = test_hour_of_day, aes(x=hour_of_day, y = hour_of_day_avg))+
  geom_col()

smooth_curve <- ggplot(data = test_hour_of_day, aes(x=hour_of_day, y = hour_of_day_avg))+
  geom_point()+
  geom_smooth(se = FALSE)
