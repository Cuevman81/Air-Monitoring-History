# --- 1. Load Required Libraries ---
library(sf)
library(ggplot2)
library(dplyr)
library(tigris)
library(ggrepel)
library(ggspatial)
library(stringr)
library(parzer)

# --- 2. Define Monitoring Site Data ---
monitoring_sites_df <- data.frame(
  County = c("Bolivar", "DeSoto", "Forrest", "Hancock", "Harrison",
             "Hinds", "Hinds", "Jackson", "Lauderdale", "Lee", "Jackson"),
  City = c("Cleveland", "Hernando", "Hattiesburg", "Waveland", "Gulfport",
           "Jackson (Hinds CC)", "Jackson (N-CORE)", "Pascagoula", "Meridian", "Tupelo", "Cherokee"),
  Site_ID = c("28-011-0002", "28-033-0002", "28-035-0004", "28-045-0003", "28-047-0008",
              "28-049-0021", "28-049-0020", "28-059-0006", "28-075-0003", "28-081-0005", "28-059-0007"),
  Pollutants = c("Ozone, PM2.5 Continuous",
                 "Ozone, PM2.5 Continuous",
                 "PM2.5 6-Day, PM2.5 Continuous",
                 "Ozone, PM2.5 Continuous",
                 "Ozone, PM2.5 Continuous",
                 "Ozone, PM2.5 Continuous",
                 "Ozone, PM2.5 3-Day, PM2.5 Continuous, Speciated PM2.5, PM10-2.5, CO, NOy, SO2",
                 "Ozone, PM2.5 Continuous, NO, NO2, NOx, SO2",
                 "Ozone",
                 "Ozone",
                 "PM10, PM10 Metals, VOC, TRS"),
  Latitude = c("33° 45' 03\"", "34° 49' 14\"", "31° 19' 23\"", "30° 18' 3\"", "30° 23' 24\"",
               "32° 20' 48\"", "32° 19' 45\"", "30° 22' 42\"", "32° 21' 52\"", "34° 15' 54\"",
               "30° 22' 42\""),
  Longitude = c("-90° 44' 03\"", "-89° 59' 16\"", "-89° 17' 15\"", "-89° 23' 45\"", "-89° 02' 59\"",
                "-90° 13' 32\"", "-90° 10' 58\"", "-88° 32' 03\"", "-88° 43' 53\"", "-88° 45' 58\"",
                "-88° 32' 03\"")
)

# --- 3. Process and Clean the Data ---
sites_processed <- monitoring_sites_df %>%
  mutate(
    lat_dd = parzer::parse_lat(Latitude),
    lon_dd = parzer::parse_lon(Longitude)
  ) %>%
  # Create a 'Site_Type' category for clearer symbology on the map.
  # The specific line for "N-CORE" has been removed to group it with other multi-pollutant sites.
  mutate(
    Site_Type = case_when(
      grepl("SO2|NOy|NO2", Pollutants) ~ "Multi-Pollutant Site",
      grepl("Ozone", Pollutants) & grepl("PM2.5", Pollutants) ~ "Ozone & PM2.5",
      grepl("Ozone", Pollutants) ~ "Ozone Only",
      grepl("PM2.5", Pollutants) ~ "PM2.5 Only",
      TRUE ~ "Special Purpose"
    ),
    # Create a cleaner label for the map
    Map_Label = str_replace(City, " \\(Hinds CC\\)", "") # Clean up Hinds CC label
  )

# Convert the processed data frame into a spatial object (sf)
sites_sf <- st_as_sf(sites_processed, coords = c("lon_dd", "lat_dd"), crs = 4326)

# --- 4. Get Mississippi County Map Data ---
ms_counties <- counties(state = "MS", cb = TRUE)

# --- 5. Build the Improved Map ---
improved_map <- ggplot() +
  # Draw the county map as the base layer
  geom_sf(data = ms_counties, fill = "gray92", color = "white", linewidth = 0.5) +
  
  # Draw the monitoring site points
  geom_sf(data = sites_sf, aes(shape = Site_Type, color = Site_Type), size = 4, stroke = 1.5) +
  
  # Add labels directly to the map
  geom_label_repel(
    data = sites_sf,
    aes(label = Map_Label, geometry = geometry),
    stat = "sf_coordinates",
    box.padding = 0.75,
    point.padding = 0.5,
    segment.color = "grey50",
    size = 3.5,
    fontface = "bold"
  ) +
  
  # --- 6. Add Professional Touches and Theming ---
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  scale_color_brewer(palette = "Dark2", name = "Site Type") +
  # Note: The number of values in scale_shape_manual might need to be adjusted
  # if the number of categories changes. We now have 5 categories.
  scale_shape_manual(values = c(17, 15, 16, 7, 8), name = "Site Type") +
  
  labs(
    title = "Mississippi Air Quality Monitoring Network",
    subtitle = "Locations and primary designation of monitoring sites",
    caption = "Source: Mississippi Department of Environmental Quality, Air Division."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold")
  )

# --- 7. Display and Save the Map ---
print(improved_map)

ggsave("Mississippi_Air_Monitoring_Network.png", plot = improved_map, width = 8.5, height = 11, dpi = 300)