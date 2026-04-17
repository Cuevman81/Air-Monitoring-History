# ==============================================================================
# USEPA/RAQSAPI: MS Monitoring History + Mapping (Cached)
# ==============================================================================

library(RAQSAPI)
library(dplyr)
library(purrr)     # For cleaner looping
library(lubridate)
library(leaflet)
library(leaflet.extras) # For extra map features like search
library(htmltools)
library(htmlwidgets)

# 1. SETUP & CONFIGURATION
# ------------------------------------------------------------------------------
# EPA AQS Credentials from .Renviron
my_email <- Sys.getenv("AQS_EMAIL")
my_key   <- Sys.getenv("AQS_KEY")

if (my_email == "" || my_key == "") {
  message("Hint: Set AQS_EMAIL and AQS_KEY in .Renviron for security.")
}
aqs_credentials(username = my_email, key = my_key)

# File paths (Relative to scripts/ folder)
cache_file <- "../cache/MS_Raw_Monitor_Cache.rds"
csv_file   <- "../output/MS_Site_History_Clean.csv"
map_file   <- "../output/MS_Site_Map.html"

# Pollutant Definitions (Easy to expand)
pollutants <- list(
  "Ozone" = "44201", "PM2.5" = "88101", "PM10"  = "81102",
  "SO2"   = "42401", "NO2"   = "42602", "CO"    = "42101", "Lead"  = "14129"
)

# 2. DATA LOADING (CACHE vs DOWNLOAD)
# ------------------------------------------------------------------------------

# Function to download data if cache is missing
download_data <- function() {
  message("--- NO CACHE FOUND: Downloading from EPA (This takes ~30 seconds) ---")
  
  start_date <- as.Date("1950-01-01")
  end_date   <- Sys.Date()
  
  # Fetch all pollutants using purrr
  combined_data <- imap_dfr(pollutants, function(p_code, p_name) {
    message(paste("Fetching:", p_name, "..."))
    
    tryCatch({
      data <- aqs_monitors_by_state(stateFIPS = "28", parameter = p_code, 
                                    bdate = start_date, edate = end_date)
      if (!is.null(data) && nrow(data) > 0) {
        data$pollutant_type <- p_name
        return(data %>% mutate(across(everything(), as.character)))
      }
    }, error = function(e) {
      message(paste("   Error fetching", p_name, ":", e$message))
    })
    
    Sys.sleep(1) # Be nice to the API
    return(NULL)
  })
  
  if (nrow(combined_data) > 0) {
    message("--- SAVING CACHE ---")
    saveRDS(combined_data, cache_file)
    return(combined_data)
  } else {
    stop("Download failed: No data returned.")
  }
}

# Load Cache OR Download
if (file.exists(cache_file)) {
  message("--- CACHE FOUND: Loading data from disk... ---")
  raw_data <- readRDS(cache_file)
} else {
  raw_data <- download_data()
}

# 3. DATA PROCESSING
# ------------------------------------------------------------------------------
message("--- Processing Data ---")

site_history <- raw_data %>%
  as_tibble() %>% # Ensure standard tibble behavior for dplyr
  # Ensure dates are proper objects
  mutate(
    open_date = as.Date(open_date),
    close_date = as.Date(close_date),
    # Use today's date if close_date is NA for calculation purposes
    calc_close = if_else(is.na(close_date), Sys.Date(), close_date)
  ) %>%
  group_by(state_code, county_code, site_number) %>%
  summarize(
    local_site_name  = first(na.omit(local_site_name)),
    address          = first(na.omit(address)),
    city_name        = first(na.omit(city_name)),
    county_name      = first(na.omit(county_name)),
    cbsa_name        = first(na.omit(cbsa_name)),
    monitoring_agency = first(na.omit(monitoring_agency)),
    elevation        = as.numeric(first(na.omit(elevation))),
    latitude         = as.numeric(first(na.omit(latitude))),
    longitude        = as.numeric(first(na.omit(longitude))),
    
    # Timeline
    Site_Established = min(open_date, na.rm = TRUE),
    Site_Closed_Raw  = if(any(is.na(close_date))) NA else max(close_date, na.rm = TRUE),
    Years_Active     = round(as.numeric(difftime(max(calc_close), min(open_date), units = "days")) / 365.25, 1),
    
    # Pollutants
    Pollutants       = paste(sort(unique(pollutant_type)), collapse = ", "),
    .groups = "drop"
  ) %>%
  # Explicitly ungroup to avoid scope issues in mutate
  ungroup() %>% 
  mutate(
    Status = ifelse(is.na(Site_Closed_Raw), "Active", "Closed"),
    Site_Closed = Site_Closed_Raw,
    # Clean up site name if missing
    local_site_name = if_else(is.na(local_site_name), paste("Site", site_number), local_site_name)
  ) %>%
  select(-Site_Closed_Raw)

# Save CSV
write.csv(site_history, csv_file, row.names = FALSE)
message(paste("CSV Saved:", csv_file))

# 4. MAPPING (Leaflet)
# ------------------------------------------------------------------------------
message("--- Generating Premium Map ---")

# Define high-contrast colors
status_pal <- colorFactor(c("#27ae60", "#c0392b"), domain = c("Active", "Closed"))

# Create Map
map <- leaflet(site_history) %>%
  addProviderTiles(providers$CartoDB.Voyager, group = "Modern Light") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Mode") %>%
  
  # Active Sites Layer
  addCircleMarkers(
    data = filter(site_history, Status == "Active"),
    lng = ~longitude, lat = ~latitude,
    group = "Active Sites",
    color = "white", weight = 1,
    fillColor = ~status_pal(Status), fillOpacity = 0.8,
    radius = 6,
    clusterOptions = markerClusterOptions(),
    popup = ~paste0(
      "<div style='font-family: sans-serif; min-width: 200px;'>",
      "<h3 style='margin:0; color:#27ae60;'>", local_site_name, "</h3>",
      "<p style='margin:5px 0;'><b>ID:</b> ", state_code, "-", county_code, "-", site_number, "</p>",
      "<table style='width:100%; border-collapse: collapse; font-size: 12px;'>",
      "<tr style='background:#f9f9f9;'><td><b>Status:</b></td><td><span style='color:green;'>Active</span></td></tr>",
      "<tr><td><b>Established:</b></td><td>", Site_Established, "</td></tr>",
      "<tr style='background:#f9f9f9;'><td><b>Years active:</b></td><td>", Years_Active, "</td></tr>",
      "<tr><td><b>City/County:</b></td><td>", city_name, " / ", county_name, "</td></tr>",
      "<tr style='background:#f9f9f9;'><td><b>Elevation:</b></td><td>", elevation, " m</td></tr>",
      "</table>",
      "<hr style='margin:10px 0; border:0; border-top:1px solid #eee;'>",
      "<b>Pollutants:</b><br><small>", Pollutants, "</small>",
      "</div>"
    )
  ) %>%
  
  # Closed Sites Layer
  addCircleMarkers(
    data = filter(site_history, Status == "Closed"),
    lng = ~longitude, lat = ~latitude,
    group = "Closed Sites",
    color = "white", weight = 1,
    fillColor = ~status_pal(Status), fillOpacity = 0.6,
    radius = 5,
    clusterOptions = markerClusterOptions(),
    popup = ~paste0(
      "<div style='font-family: sans-serif; min-width: 200px;'>",
      "<h3 style='margin:0; color:#c0392b;'>", local_site_name, "</h3>",
      "<p style='margin:5px 0;'><b>ID:</b> ", state_code, "-", county_code, "-", site_number, "</p>",
      "<table style='width:100%; border-collapse: collapse; font-size: 12px;'>",
      "<tr style='background:#f9f9f9;'><td><b>Status:</b></td><td><span style='color:red;'>Closed</span></td></tr>",
      "<tr><td><b>Operated:</b></td><td>", Site_Established, " to ", Site_Closed, "</td></tr>",
      "<tr style='background:#f9f9f9;'><td><b>Years active:</b></td><td>", Years_Active, "</td></tr>",
      "<tr><td><b>City/County:</b></td><td>", city_name, " / ", county_name, "</td></tr>",
      "</table>",
      "<hr style='margin:10px 0; border:0; border-top:1px solid #eee;'>",
      "<b>Pollutants:</b><br><small>", Pollutants, "</small>",
      "</div>"
    )
  ) %>%
  
  # Controls
  addLayersControl(
    baseGroups = c("Modern Light", "Dark Mode"),
    overlayGroups = c("Active Sites", "Closed Sites"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend("bottomright", pal = status_pal, values = ~Status,
            title = "Monitor Status",
            opacity = 1) %>%
  addSearchOSM() %>%
  addControl(
    html = paste0("<div style='background: white; padding: 5px; border-radius: 5px; box-shadow: 0 0 5px rgba(0,0,0,0.2); font-family: sans-serif;'>",
                  "<b>Mississippi Air Monitoring Network</b><br>",
                  "<small>Historical View - Generated: ", Sys.Date(), "</small></div>"),
    position = "topright"
  )

# Save Map
# Note: Self-contained requires pandoc (installed with RStudio)
tryCatch({
  saveWidget(map, map_file, selfcontained = TRUE)
  message(paste("--- Premium self-contained map saved to:", map_file, "---"))
}, error = function(e) {
  message("Pandoc not found. Saving map as standard HTML (with dependencies folder)...")
  saveWidget(map, map_file, selfcontained = FALSE)
  message(paste("--- Map saved (non-self-contained) to:", map_file, "---"))
})