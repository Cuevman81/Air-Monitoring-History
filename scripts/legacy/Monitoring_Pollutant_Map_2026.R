# --- 1. Load Libraries ---
list_of_packages <- c("ggplot2", "ggspatial", "maps", "ggrepel", "dplyr", "stringr", "sf")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(ggplot2)
library(ggspatial) 
library(maps)
library(ggrepel)   
library(dplyr)     
library(stringr)   
library(sf)        

# --- 2. Prepare Data ---

# Mississippi state and county boundaries
mississippi_map <- map_data("state", region = "mississippi")
county_map <- map_data("county", region = "mississippi")

# Monitoring site data (INCLUDES SHUT DOWN MERIDIAN SITE)
sites <- data.frame(
  County = c("Bolivar", "DeSoto", "Forrest", "Hancock", "Harrison", "Hinds", "Hinds", "Jackson", "Lee", "Jackson", "Lauderdale"),
  City = c("CLEVELAND", "HERNANDO", "HATTIESBURG", "WAVELAND", "GULFPORT", "JACKSON (HINDS CC)", "JACKSON (N-CORE)", "PASCAGOULA", "TUPELO", "CHEROKEE", "MERIDIAN"),
  Monitoring_Site_ID = c("28-011-0002", "28-033-0002", "28-035-0004", "28-045-0003", "28-047-0008", "28-049-0021", "28-049-0020", "28-059-0006", "28-081-0005", "28-059-0007", "28-075-0003"),
  Status = c(rep("Active", 10), "Shut Down"),
  Pollutants_Monitored = c("Ozone, PM2.5 Continuous", 
                           "Ozone, PM2.5 Continuous", 
                           "PM2.5 6-Day, PM2.5 Continuous", 
                           "Ozone, PM2.5 Continuous", 
                           "Ozone, PM2.5 Continuous", 
                           "Ozone, PM2.5 Continuous", 
                           "Ozone, PM2.5 3-Day, PM2.5 Continuous, Speciated PM2.5, PM10-2.5, CO, NOy, SO2", 
                           "Ozone, PM2.5 Continuous, NO, NO2, NOx, SO2", 
                           "Ozone", 
                           "PM10, PM10 Metals, VOC, TRS, PM2.5",
                           "Ozone (Shut Down 2026)"),
  Latitude = c(33.75083, 34.82056, 31.32306, 30.30083, 30.39000, 32.34667, 32.32917, 30.37833, 34.26500, 30.37833, 32.36444), 
  Longitude = c(-90.73417, -89.98778, -89.28750, -89.39583, -89.04972, -90.22556, -90.18278, -88.53417, -88.76611, -88.53417, -88.73139) 
)

# Prepare labels
sites <- sites %>%
  mutate(
    Pollutants_Formatted = str_replace_all(Pollutants_Monitored, ", ", "\n"),
    LabelText = ifelse(Status == "Shut Down",
                       paste0(City, "\n(SHUT DOWN)\n", Pollutants_Formatted),
                       paste(City, Pollutants_Formatted, sep = "\n"))
  )

# --- 3. Create the Map ---

lon_limits <- c(-91.8, -87.8)
lat_limits <- c(30.0, 35.3)

map_plot <- ggplot() +
  # Add State boundary
  geom_polygon(data = mississippi_map, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "grey40") +
  
  # Add County boundaries
  geom_polygon(data = county_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "white", linetype = "dashed",
               linewidth = 0.3) +
  
  # Add Monitoring Sites (Points)
  geom_point(data = sites, aes(x = Longitude, y = Latitude, color = Status, shape = Status, size = Status)) +
  scale_color_manual(values = c("Active" = "red", "Shut Down" = "grey30")) +
  scale_shape_manual(values = c("Active" = 19, "Shut Down" = 4)) + # 19=circle, 4=X
  scale_size_manual(values = c("Active" = 3.5, "Shut Down" = 4.5)) +
  
  # Add Monitoring Site Labels
  geom_label_repel(
    data = sites,
    aes(x = Longitude, y = Latitude, label = LabelText),
    size = 2.0,
    box.padding = 0.4,
    point.padding = 0.5,
    segment.color = 'grey50',
    segment.size = 0.3,
    max.overlaps = Inf,
    min.segment.length = 0,
    force = 40,
    max.iter = 4000,
    seed = 42
  ) +
  
  # Set Coordinate System
  coord_sf(xlim = lon_limits, ylim = lat_limits,
           crs = 4326, default_crs = 4326,
           expand = FALSE) +
  
  # Add Scale Bar and North Arrow
  annotation_scale(location = "bl",
                   width_hint = 0.3,
                   style = "ticks",
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tr",
                         which_north = "true",
                         height = unit(1.2, "cm"), width = unit(1.0, "cm"),
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering) +
  
  # Updated Title/Subtitle for 2026 Plan
  labs(
    title = "Mississippi Air Monitoring Network (2026)",
    subtitle = "Monitoring sites and primary pollutants measured",
    caption = "Source: MDEQ Data"
  ) +
  
  # Apply Theme
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10)),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )

# --- 4. Display and Save ---

print(map_plot)

# Updated filename for 2026
ggsave("MS_Air_Monitoring_Network_Map_2026.jpg",
       plot = map_plot,
       width = 8.5, height = 11, dpi = 300)