#### Set up ####
{
  require(dplyr)
  library(lubridate)
  require(ggplot2)
  library(viridis)
  library(here)
  library(sf)
  library(scales)
}

# ggplot theme
# set gridline and background color
mytheme <- theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 20),
        panel.border = element_blank())


#### YOY ####
#### Data ####

# Harbor physical data last accessed 4/28/25
# http://ftp.mwra.com/harbor/html/wq_data.htm

# clipped sites in QGIS to outer BH
sites <- read.csv(here("data", "sampling_data_locations_outer_harbor.csv"))
head(sites)

wq <- read.csv(here("data", "bh_physical.csv"))
head(wq)

# remove duplicates caused by different projects
sites <- sites %>% distinct(STAT_ID, .keep_all = TRUE)

# clean
wq <- wq %>%
  rename(STAT_ID = Station.ID,
         Date = Date.time..EASTERN.STANDARD.TIME.,
         Depth = Depth.of.measurement..m.,
         Temp = Temperature..C., 
         Salinity = Salinity..PSU.,
         DO = Dissolved.Oxygen..mg.L.,
         Turbidity = Turbidity..NTU.)

wq$Date<- as.POSIXct(wq$Date, format = "%m/%d/%y", tz = "UTC")
wq$Date <- as.Date(wq$Date, format = "%m/%d/%y")
wq$Month <-  as.numeric(format(wq$Date, "%m"))
wq$Year <- as.numeric(format(wq$Date, "%Y"))
wq$STAT_ID <- as.character(wq$STAT_ID)

sites$STAT_ID <- as.character(sites$STAT_ID)

# clip wq to bottom temps & select years
wq_select <- wq[which(wq$Surface.or.Bottom == "B" &
                 wq$Year < 2024 &
                 wq$Year > 2020),]

# link to locations to clip to outer harbor sites only
wq_select <- wq_select %>%
  inner_join(sites, by = "STAT_ID")

# save for QGIS mapping of sites by depth
write.csv(wq_select, "wq.csv", row.names = F)

# clip wq to bottom temps & select years
wq_decade <- wq[which(wq$Surface.or.Bottom == "B" &
                        wq$Year < 2024 &
                        wq$Year > 2012),]

# link to locations to clip to outer harbor sites only
wq_decade <- wq_decade %>%
  inner_join(sites, by = "STAT_ID")


#### Sum stats ####

# range in temp Jan-Jun
range(wq_select$Temp[wq_select$Month %in% 1:6], na.rm = TRUE)

se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}

# Calculate monthly average
monthly_avg <- wq_select %>%
  group_by(Year, Month) %>%                           # Group by the month
  summarise(AvgTemp = mean(Temp, na.rm = TRUE),
            AvgSalinity = mean(Salinity, na.rm = TRUE),
            AvgDO = mean(DO, na.rm = TRUE),
            AvgpH = mean(pH, na.rm = TRUE),
            AvgTurbidity = mean(Turbidity, na.rm = TRUE))  # Calculate monthly average

# Calculate daily average
daily_avg <- wq_select %>%
  group_by(Date) %>%  # Group by Date
  summarise(AvgTemp = mean(Temp, na.rm = TRUE),
            AvgSalinity = mean(Salinity, na.rm = TRUE),
            AvgDO = mean(DO, na.rm = TRUE),
            AvgpH = mean(pH, na.rm = TRUE),
            AvgTurbidity = mean(Turbidity, na.rm = TRUE))  %>%  # Calculate daily averages
  mutate(Year = year(Date),
         Month_Day = format(Date, "%m/%d"))  # Add Year column

write.csv(daily_avg, "daily_avg.csv", row.names = F)

# Calculate daily average for the decade
daily_avg_decade <- wq_decade %>%
  group_by(Date) %>%  # Group by Date
  summarise(
    AvgTemp = mean(Temp, na.rm = TRUE),
    AvgTempSE = se(Temp),
    AvgTemp_Lower = AvgTemp - 1.96 * AvgTempSE,
    AvgTemp_Upper = AvgTemp + 1.96 * AvgTempSE,
    AvgSalinity = mean(Salinity, na.rm = TRUE),
    AvgDO = mean(DO, na.rm = TRUE),
    AvgpH = mean(pH, na.rm = TRUE),
    AvgTurbidity = mean(Turbidity, na.rm = TRUE)
  ) %>%
  mutate(
    Year = year(Date),
    Month_Day = format(Date, "%m/%d")
  )


#### Visualize ####

# Plot average monthly temperature for each year
ggplot(monthly_avg, aes(x = Month, y = AvgTemp, color = as.factor(Year), group = Year)) +
  geom_line(lwd = 1) +  # Plot lines
  labs(title = "MWRA monthly average temperature",
       x = "Month",
       y = "Temperature (°C)",
       color = "Year") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +  # Label months
  scale_color_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity

# Plot monthly temperature for each year in last decade
ggplot(monthly_avg_dec, aes(x = Month, y = AvgTemp, color = as.factor(Year), group = Year)) +
  geom_line(lwd = 1) +  # Plot lines
  labs(title = "MWRA monthly average temperature",
       x = "Month",
       y = "Temperature (°C)",
       color = "Year") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +  # Label months
  scale_color_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity


# Plot average daily temperature for each year
ggplot(daily_avg, aes(x = Month_Day, y = AvgTemp, color = as.factor(Year), group = Year)) +
  geom_line(lwd = 1) +  # To plot lines
  labs(title = "MWRA daily average temperature",
       x = "Month/Day",
       y = "Average Temperature (°C)",
       color = "Year") +
  scale_x_discrete(labels = function(x) substr(x, 1, 5)) +  # Trim the labels to "MM/DD" format
  scale_color_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity

# Plot daily temperature for each year in last decade
ggplot(daily_avg_decade, aes(x = Month_Day, y = AvgTemp, color = as.factor(Year), group = Year)) +
  geom_line(lwd = 1) +  # To plot lines
  labs(x = "Month/Day",
       y = "Average Temperature (°C)",
       color = "Year") +
  scale_x_discrete(labels = function(x) substr(x, 1, 5)) +  # Trim the labels to "MM/DD" format
  scale_color_viridis_d() +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity




# decadal temps + median date for peak spawning and settlement
daily_avg_decade %>%
  mutate(
    temp_week = cut(Date, breaks = "week"),
    temp_week_display = as.Date(format(as.Date(temp_week), "2025-%m-%d")),
    Year_Group = case_when(
      Year %in% 2013:2020 ~ "2013–2020",
      Year == 2021 ~ "2021",
      Year == 2022 ~ "2022",
      Year == 2023 ~ "2023"
    )
  ) %>%
  ggplot(aes(x = temp_week_display, y = AvgTemp, color = Year_Group, group = Year)) +
  # Ribbon now uses the same color as the line
  geom_ribbon(aes(ymin = AvgTemp_Lower, ymax = AvgTemp_Upper, fill = Year_Group),
              alpha = 0.5, color = NA) +
  geom_line(linewidth = 2) +
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 month"
  ) +
  scale_color_manual(
    values = c(
      "2013–2020" = "grey",
      "2021" = "#33638DFF",
      "2022" = "#29AF7FFF",
      "2023" = "#DCE319FF"
    ),
    name = "Year"
  ) +
  scale_fill_manual(
    values = c(
      "2013–2020" = "grey",
      "2021" = "#33638DFF",
      "2022" = "#29AF7FFF",
      "2023" = "#DCE319FF"
    ),
    guide = "none"  # remove extra fill legend
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  # Vertical reference lines
  geom_vline(xintercept = as.Date(c("2025-05-03","2025-06-10")),
             color = "#33638DFF", linetype = c("dotted","dotdash"), linewidth = 2) +
  geom_vline(xintercept = as.Date(c("2025-04-04","2025-05-13")),
             color = "#29AF7FFF", linetype = c("dotted","dotdash"), linewidth = 2) +
  geom_vline(xintercept = as.Date(c("2025-03-12","2025-04-21")),
             color = "#DCE319FF", linetype = c("dotted","dotdash"), linewidth = 2) +
  labs(x = "Month", y = "Mean daily temperature (°C)") +
  mytheme +
  theme(
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("temp.png", width = 12, height = 8, dpi = 500, bg = "white") 

# peaks
# 2021 hatch May 3─May 24; spawn Apr 22 - May 13 (May 3); settle May 30  - June 20  (June 10)
# 2022 hatch Apr 3─Apr 27; spawn Mar 23 - Apr 16 (Apr 4); settle Apr 20 - Jun 4  (May 13 )
# 2023 hatch Mar 13─Apr 3; spawn Mar 2 - Mar 23 (Mar 12); settle Apr 4 - May 7  (April 21 )


# alternate
# helper dataframe for median date for peak spawning and settlement
ref_points <- data.frame(
  Date = as.Date(c(
    "2025-04-27","2025-07-11",   # 2021
    "2025-03-30","2025-06-14",   # 2022
    "2025-03-06","2025-05-21"    # 2023
  )),
  Year_Group = c(
    "2021","2021",
    "2022","2022",
    "2023","2023"
  ),
  Shape = c(16, 17, 16, 17, 16, 17)  # 16 = circle, 17 = triangle
)


# decadal temps + median date for peak spawning and settlement
daily_avg_decade %>%
  mutate(
    temp_week = cut(Date, breaks = "week"),
    temp_week_display = as.Date(format(as.Date(temp_week), "2025-%m-%d")),
    Year_Group = case_when(
      Year %in% 2013:2020 ~ "2013–2020",
      Year == 2021 ~ "2021",
      Year == 2022 ~ "2022",
      Year == 2023 ~ "2023"
    )
  ) %>%
  ggplot(aes(x = temp_week_display, y = AvgTemp, color = Year_Group, group = Year)) +
  # Ribbon now uses the same color as the line
  geom_ribbon(aes(ymin = AvgTemp_Lower, ymax = AvgTemp_Upper, fill = Year_Group),
              alpha = 0.5, color = NA) +
  geom_line(linewidth = 1) +
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 month"
  ) +
  scale_color_manual(
    values = c(
      "2013–2020" = "grey",
      "2021" = "#33638DFF",
      "2022" = "#29AF7FFF",
      "2023" = "#DCE319FF"
    ),
    name = "Year"
  ) +
  scale_fill_manual(
    values = c(
      "2013–2020" = "grey",
      "2021" = "#33638DFF",
      "2022" = "#29AF7FFF",
      "2023" = "#DCE319FF"
    ),
    guide = "none"  # remove extra fill legend
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  # reference dates
  geom_point(
    data = ref_points,
    aes(x = Date, y = 0, color = Year_Group, shape = factor(Shape)),
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_shape_manual(values = c("16" = 16, "17" = 17), guide = "none") +
  labs(x = "Month", y = "Average daily temperature (°C)") +
  mytheme +
  theme(
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#### Map ####

# Load shapefile
coastline <- st_read("C:/Users/kzarrellasmi/OneDrive - University of Massachusetts/Documents/GitHub/Mapping/MACZM_COAST25K/COAST25K_ARC.shp")

# make sure data in right projection  
outer_harbor_fix_sf <- wq %>%
  st_as_sf(coords = c("TARGET_LON", "TARGET_LAT"), crs = 4326)

# Ensure the data is in WGS84 (EPSG:4326)
outer_harbor_fix_sf <- st_transform(outer_harbor_fix_sf, crs = 4326)
coastline <- st_transform(coastline, crs = 4326)


#### Depth x Year ####

# Plot the data with the corrected CRS, zoomed in range, and reversed viridis scale
ggplot() +
  # Plot the coastline with land in grey
  geom_sf(data = coastline, fill = "darkgrey", color = "black") +
  
  # Plot the centroids colored by avg_depth, using reversed viridis scale
  geom_sf(data = outer_harbor_fix_sf, aes(color = Depth), size = 3) +
  
  # Zoom to the specified coordinates
  coord_sf(xlim = c(-71.04557, -70.88105), ylim = c(42.25307, 42.36859)) + 
  scale_x_continuous(breaks = seq(min(round(sites2$TARGET_LON, 2)), 
                                  max(round(sites2$TARGET_LON, 2)), 
                                  length.out = 2)) +
  scale_y_continuous(breaks = seq(min(round(sites2$TARGET_LAT, 2)), 
                                  max(round(sites2$TARGET_LAT, 2)), 
                                  length.out = 2)) +
  # Labels and theme
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Depth") +
  theme_bw()

ggsave("map.png", width = 8, height = 4, units = "in", dpi = 300, bg = "white") 

