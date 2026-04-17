library(ggplot2)
library(ggspatial)
library(maps)
library(ggrepel)

# Generate the map of Mississippi
mississippi <- map_data("state", region = "mississippi")

# Create a data frame with site information
sites <- data.frame(
  County = c("Bolivar", "DeSoto", "Forrest", "Hancock", "Harrison", "Hinds", "Hinds", "Jackson", "Lauderdale", "Lee"),
  City = c("CLEVELAND", "HERNANDO", "HATTIESBURG", "WAVELAND", "GULFPORT", "JACKSON (HINDS CC)", "JACKSON (N-CORE)", "PASCAGOULA", "MERIDIAN", "TUPELO"),
  Monitoring_Site_ID = c("28-011-0002", "28-033-0002", "28-035-0004", "28-045-0003", "28-047-0008", "28-049-0021", "28-049-0020", "28-059-0006", "28-075-0003", "28-081-0005"),
  Pollutants_Monitored = c("Ozone, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "PM2.5 6-Day, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "Ozone, PM2.5 3-Day, PM2.5 Continuous, Speciated PM2.5, PM10-2.5, CO, NOy, SO2", "Ozone, PM2.5 Continuous, NO, NO2, NOx, SO2", "Ozone", "Ozone"),
  Latitude = c(33 + 45/60 + 3/3600, 34 + 49/60 + 14/3600, 31 + 19/60 + 23/3600, 30 + 18/60 + 3/3600, 30 + 23/60 + 24/3600, 32 + 20/60 + 48/3600, 32 + 19/60 + 45/3600, 30 + 22/60 + 42/3600, 32 + 21/60 + 52/3600, 34 + 15/60 + 54/3600),
  Longitude = c(-90 - 44/60 - 3/3600, -89 - 59/60 - 16/3600, -89 - 17/60 - 15/3600, -89 - 23/60 - 45/3600, -89 - 2/60 - 59/3600, -90 - 13/60 - 32/3600, -90 - 10/60 - 58/3600, -88 - 32/60 - 3/3600, -88 - 43/60 - 53/3600, -88 - 45/60 - 58/3600)
)

# Create a blank map of Mississippi
map <- ggplot() +
  theme_void() +
  coord_map(xlim = c(-91.5, -88), ylim = c(30.2, 35.2))

# Add Mississippi map outline
map <- map + geom_polygon(data = mississippi, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black")

# Add county boundaries
county_boundaries <- map_data("county", region = "mississippi")
map <- map + geom_polygon(data = county_boundaries, aes(x = long, y = lat, group = group), fill = NA, color = "black", linetype = "dashed")

# Split the pollutants into separate lines
sites$Pollutants_Monitored <- strsplit(sites$Pollutants_Monitored, ", ")

# Add monitoring sites and labels
map <- map + 
  geom_point(data = sites, aes(x = Longitude, y = Latitude), color = "red", size = 3) +
  geom_label_repel(
    data = sites,
    aes(x = Longitude, y = Latitude, label = paste(City, "\n", sapply(Pollutants_Monitored, paste, collapse = "\n"))),
    size = 3,
    nudge_x = 0.1,
    segment.color = "black",
    segment.size = 0.2,
    label.padding = 0.3,
    force = 2
  ) +
  theme(text = element_text(angle = 90, vjust = 0.5))

map <- map + labs(title = "2024 MDEQ Air Monitoring Network") +
  theme(plot.title = element_text(size = 10))  # Adjust the size value as needed

# Rotate the title horizontally and customize font size and alignment
map <- map + theme(plot.title = element_text(angle = 0, size = 24, hjust = 0.5))

# Save the map as a JPEG file
ggsave("2023_Air_Monitoring_Network_map.jpg", map, width = 10, height = 12, dpi = 300)





#Air Monitoring Network Map with Cherokee Included
# Generate the map of Mississippi
mississippi <- map_data("state", region = "mississippi")

# Create a data frame with site information
sites <- data.frame(
  County = c("Bolivar", "DeSoto", "Forrest", "Hancock", "Harrison", "Hinds", "Hinds", "Jackson", "Lauderdale", "Lee", "Jackson"),
  City = c("CLEVELAND", "HERNANDO", "HATTIESBURG", "WAVELAND", "GULFPORT", "JACKSON (HINDS CC)", "JACKSON (N-CORE)", "PASCAGOULA", "MERIDIAN", "TUPELO", "CHEROKEE"),
  Monitoring_Site_ID = c("28-011-0002", "28-033-0002", "28-035-0004", "28-045-0003", "28-047-0008", "28-049-0021", "28-049-0020", "28-059-0006", "28-075-0003", "28-081-0005", "28-059-0007"),
  Pollutants_Monitored = c("Ozone, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "PM2.5 6-Day, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "Ozone, PM2.5 3-Day, PM2.5 Continuous, Speciated PM2.5, PM10-2.5, CO, NOy, SO2", "Ozone, PM2.5 Continuous, NO, NO2, NOx, SO2", "Ozone", "Ozone", "PM10, PM10 Metals, VOC, TRS"),
  Latitude = c(33 + 45/60 + 3/3600, 34 + 49/60 + 14/3600, 31 + 19/60 + 23/3600, 30 + 18/60 + 3/3600, 30 + 23/60 + 24/3600, 32 + 20/60 + 48/3600, 32 + 19/60 + 45/3600, 30 + 22/60 + 42/3600, 32 + 21/60 + 52/3600, 34 + 15/60 + 54/3600, 30 + 22/60 + 42/3600),
  Longitude = c(-90 - 44/60 - 3/3600, -89 - 59/60 - 16/3600, -89 - 17/60 - 15/3600, -89 - 23/60 - 45/3600, -89 - 2/60 - 59/3600, -90 - 13/60 - 32/3600, -90 - 10/60 - 58/3600, -88 - 32/60 - 3/3600, -88 - 43/60 - 53/3600, -88 - 45/60 - 58/3600, -88 - 32/60 - 3/3600)
)

# Create a blank map of Mississippi
map <- ggplot() +
  theme_void() +
  coord_map(xlim = c(-91.5, -88), ylim = c(30.2, 35.2))

# Add Mississippi map outline
map <- map + geom_polygon(data = mississippi, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black")

# Add county boundaries
county_boundaries <- map_data("county", region = "mississippi")
map <- map + geom_polygon(data = county_boundaries, aes(x = long, y = lat, group = group), fill = NA, color = "black", linetype = "dashed")

# Split the pollutants into separate lines
sites$Pollutants_Monitored <- strsplit(sites$Pollutants_Monitored, ", ")

# Add monitoring sites and labels
map <- map + 
  geom_point(data = sites, aes(x = Longitude, y = Latitude), color = "red", size = 3) +
  geom_label_repel(
    data = sites,
    aes(x = Longitude, y = Latitude, label = paste(City, "\n", sapply(Pollutants_Monitored, paste, collapse = "\n"))),
    size = 3,
    nudge_x = 0.1,
    segment.color = "black",
    segment.size = 0.2,
    label.padding = 0.3,
    force = 2
  ) +
  theme(text = element_text(angle = 90, vjust = 0.5))

map <- map + labs(title = "2024 MDEQ Air Monitoring Network") +
  theme(plot.title = element_text(size = 10))  # Adjust the size value as needed

# Rotate the title horizontally and customize font size and alignment
map <- map + theme(plot.title = element_text(angle = 0, size = 24, hjust = 0.5))

# Save the map as a JPEG file
ggsave("2024_Air_Monitoring_Network_map.jpg", map, width = 10, height = 12, dpi = 300)

#Map 2nd Option

# Check package availability and load packages
list_of_packages <- c("ggplot2", "ggspatial", "maps", "ggrepel")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(list_of_packages, require, character.only = TRUE)

# Set default theme
theme_set(theme_minimal())

# Generate the map of Mississippi
mississippi <- map_data("state", region = "mississippi")

# Create a data frame with site information
sites <- data.frame(
  County = c("Bolivar", "DeSoto", "Forrest", "Hancock", "Harrison", "Hinds", "Hinds", "Jackson", "Lauderdale", "Lee"),
  City = c("CLEVELAND", "HERNANDO", "HATTIESBURG", "WAVELAND", "GULFPORT", "JACKSON (HINDS CC)", "JACKSON (N-CORE)", "PASCAGOULA", "MERIDIAN", "TUPELO"),
  Monitoring_Site_ID = c("28-011-0002", "28-033-0002", "28-035-0004", "28-045-0003", "28-047-0008", "28-049-0021", "28-049-0020", "28-059-0006", "28-075-0003", "28-081-0005"),
  Pollutants_Monitored = c("Ozone, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "PM2.5 6-Day, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "Ozone, PM2.5 Continuous", "Ozone, PM2.5 3-Day, PM2.5 Continuous, Speciated PM2.5, PM10-2.5, CO, NOy, SO2", "Ozone, PM2.5 Continuous, NO, NO2, NOx, SO2", "Ozone", "Ozone"),
  Latitude = c(33 + 45/60 + 3/3600, 34 + 49/60 + 14/3600, 31 + 19/60 + 23/3600, 30 + 18/60 + 3/3600, 30 + 23/60 + 24/3600, 32 + 20/60 + 48/3600, 32 + 19/60 + 45/3600, 30 + 22/60 + 42/3600, 32 + 21/60 + 52/3600, 34 + 15/60 + 54/3600),
  Longitude = c(-90 - 44/60 - 3/3600, -89 - 59/60 - 16/3600, -89 - 17/60 - 15/3600, -89 - 23/60 - 45/3600, -89 - 2/60 - 59/3600, -90 - 13/60 - 32/3600, -90 - 10/60 - 58/3600, -88 - 32/60 - 3/3600, -88 - 43/60 - 53/3600, -88 - 45/60 - 58/3600)
)

# Function to add monitoring sites and labels to the map
add_sites_to_map <- function(map, sites) {
  map <- map + 
    geom_point(data = sites, aes(x = Longitude, y = Latitude), color = "red", size = 3) +
    geom_label_repel(
      data = sites,
      aes(x = Longitude, y = Latitude, label = paste(City, "\n", sapply(Pollutants_Monitored, paste, collapse = "\n"))),
      size = 3,
      nudge_x = 0.1,
      segment.color = "black",
      segment.size = 0.2,
      label.padding = 0.3,
      force = 2
    )
  return(map)
}

# Create a blank map of Mississippi
map <- ggplot() +
  theme_void() +
  coord_map(xlim = c(-91.5, -88), ylim = c(30.2, 35.2))

# Add Mississippi map outline
map <- map + geom_polygon(data = mississippi, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black")

# Add county boundaries
county_boundaries <- map_data("county", region = "mississippi")
map <- map + geom_polygon(data = county_boundaries, aes(x = long, y = lat, group = group), fill = NA, color = "black", linetype = "dashed")

# Add monitoring sites and labels
map <- add_sites_to_map(map, sites)

# Customize and save the map
map <- map + labs(title = "2023 MDEQ Air Monitoring Network") +
  theme(plot.title = element_text(size = 10))  # Adjust the size value as needed
map <- map + theme(plot.title = element_text(angle = 0, size = 24, hjust = 0.5))
ggsave("2023_Air_Monitoring_Network_map.jpg", map, width = 10, height = 12, dpi = 300)

