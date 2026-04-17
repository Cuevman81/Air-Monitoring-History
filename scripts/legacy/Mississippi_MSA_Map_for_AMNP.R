# --------------------------------------------------------------------------
# Script to Fetch Latest Census Data for Specific MSAs in Mississippi,
# Create a Population Table, Generate a Map Visualization,
# and Combine Map & Table Side-by-Side, Shifted Left on Page
# --------------------------------------------------------------------------

# --- 1. Load Required Libraries ---
# Data acquisition & spatial data
library(tidycensus)
library(sf)
library(purrr)     # Needed for map_dfr to fetch multiple years

# Data manipulation
library(dplyr)
library(tidyr)     # Needed for pivot_wider
library(stringr)

# Plotting & Arrangement
library(ggplot2)
library(scales)    # For formatting population numbers
library(gridExtra) # For arranging plots and tables
library(grid)      # Needed for table grob themes/units and viewports

# --- 2. Set Up Credentials and Parameters ---

# a) Census API Key (Ensure it's set in your environment)
# census_api_key("YOUR_API_KEY_HERE", install = TRUE, overwrite = TRUE)
# readRenviron("~/.Renviron")

# b) Define Parameters
map_target_year <- 2023 # Most recent year for map display and latest column in table
years_for_table <- c(2021, 2022, map_target_year) # Years needed for the table
target_state <- "MS"
target_msa_names <- c(
  "Memphis, TN-MS-AR Metro Area",
  "Jackson, MS Metro Area",
  "Hattiesburg, MS Metro Area",
  "Gulfport-Biloxi, MS Metro Area"
)
# Optional: Define colors for MSAs
msa_colors <- setNames(
  c("pink", "orange", "steelblue", "brown"), # Approximate colors from your map
  target_msa_names # Uses the updated names
)

# --- 3. Fetch Mississippi County Geographic Data (for Map) ---

print(paste("Fetching county boundaries for", target_state, "..."))
ms_counties_sf <- get_acs(
  geography = "county",
  state = target_state,
  variables = "B01003_001",
  year = map_target_year, # Use latest year for county geometry consistency
  survey = "acs5",
  geometry = TRUE,
  cache_table = TRUE
)

if (is.null(ms_counties_sf) || nrow(ms_counties_sf) == 0) {
  stop("Failed to fetch Mississippi county boundaries.")
}

# --- 4. Fetch Target MSA Geographic Data (for Map Labels/Joining) ---

print(paste("Fetching MSA boundaries and population for map year", map_target_year, "..."))
msa_geo_sf <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = c(total_pop = "B01003_001"),
  year = map_target_year,
  survey = "acs5",
  geometry = TRUE, # Need geometry for map labels
  cache_table = TRUE
)

if (is.null(msa_geo_sf) || nrow(msa_geo_sf) == 0) {
  stop("Failed to fetch MSA geometry data for the map year.")
}

# Filter for the target MSAs for the map
target_msa_map_sf <- msa_geo_sf %>%
  filter(NAME %in% target_msa_names) %>%
  select(GEOID, NAME, population = estimate, geometry)

if (nrow(target_msa_map_sf) != length(target_msa_names)) {
  warning("Could not find all target MSAs for the map geometry. Check names.")
}
if (nrow(target_msa_map_sf) == 0) {
  stop("None of the target MSAs were found for map geometry. Stopping.")
}


# --- 5. Fetch Multi-Year MSA Population Data (for Table) ---

print(paste("Fetching MSA population data for table years:", paste(years_for_table, collapse=", "), "..."))

msa_pop_table_raw <- map_dfr(years_for_table, ~{
  year_to_fetch <- .x
  print(paste("... fetching ACS 5-Year data ending in", year_to_fetch))
  data_for_year <- tryCatch({
    get_acs(
      geography = "metropolitan statistical area/micropolitan statistical area",
      variables = c(total_pop = "B01003_001"), # Total Population
      survey = "acs5",
      year = year_to_fetch,
      geometry = FALSE, # Set to FALSE for table data - much faster
      cache_table = TRUE
    )
  }, error = function(e) {
    warning("Failed to get ACS data for year ", year_to_fetch, ": ", e$message)
    return(NULL)
  })
  # Add year column if data was fetched successfully
  if (!is.null(data_for_year)) {
    data_for_year$year <- year_to_fetch
    return(data_for_year)
  } else {
    return(NULL)
  }
})

# Check if any data was fetched
if (is.null(msa_pop_table_raw) || nrow(msa_pop_table_raw) == 0) {
  stop("Failed to fetch any multi-year population data for the table.")
}

# Filter for target MSAs and select columns
msa_pop_table_filtered <- msa_pop_table_raw %>%
  filter(NAME %in% target_msa_names) %>%
  select(NAME, year, population = estimate)

# Check if all MSAs and years were found
if (length(unique(msa_pop_table_filtered$NAME)) != length(target_msa_names) ||
    length(unique(msa_pop_table_filtered$year)) != length(years_for_table)) {
  warning("Could not find population data for all target MSAs for all requested years.")
  print("Summary of data found for table:")
  print(table(msa_pop_table_filtered$NAME, msa_pop_table_filtered$year))
}

# --- 6. Create the Population Table Data Frame ---

print("Formatting population data into table data frame...")

# Pivot data to wide format
population_table_wide <- msa_pop_table_filtered %>%
  pivot_wider(names_from = year, values_from = population, names_prefix = "Pop_")

# Rename columns for clarity and sort
population_table_final <- population_table_wide %>%
  # Ensure correct column order based on years_for_table
  select(NAME, paste0("Pop_", years_for_table)) %>%
  rename(
    `Geographic Area` = NAME,
    # Dynamically create names like "Population Estimate 2021"
    !!!setNames(paste0("Pop_", years_for_table), paste("Population Estimate", years_for_table))
  ) %>%
  arrange(`Geographic Area`) # Optional: sort alphabetically

# Format numbers with commas
population_table_formatted <- population_table_final %>%
  mutate(across(starts_with("Population Estimate"), ~scales::comma(.x, accuracy = 1)))

# Optional: Print table to console during script run
# print("---------------------------------------------------------------")
# print("Population Estimates for Target MSAs (ACS 5-Year)")
# print("---------------------------------------------------------------")
# options(width = 150); print.data.frame(population_table_formatted, row.names = FALSE, right=TRUE); options(width = 80)
# print("---------------------------------------------------------------")
# cat("\n\n")

# --- 7. Identify Counties within Target MSAs (Spatial Join for Map) ---

print("Identifying counties within target MSAs for map...")

# Ensure CRS match before spatial operations
if (st_crs(ms_counties_sf) != st_crs(target_msa_map_sf)) {
  print("Aligning Coordinate Reference Systems...")
  ms_counties_sf <- st_transform(ms_counties_sf, st_crs(target_msa_map_sf))
}

# Spatial join using map data
counties_in_msa <- st_join(ms_counties_sf, target_msa_map_sf, join = st_intersects, left = FALSE) %>%
  select(county_GEOID = GEOID.x, county_NAME = NAME.x, MSA_NAME = NAME.y) %>%
  st_drop_geometry()

# Add MSA info back to MS county data
ms_counties_sf <- ms_counties_sf %>%
  left_join(counties_in_msa, by = c("GEOID" = "county_GEOID")) %>%
  mutate(MSA_NAME = if_else(is.na(MSA_NAME), NA_character_, MSA_NAME))

print("County identification complete.")
print("Counties identified within target MSAs (for map):")
print(table(ms_counties_sf$MSA_NAME, useNA = "ifany"))


# --- 8. Prepare Data for Map Labels ---

print("Preparing map labels...")
label_points <- target_msa_map_sf %>%
  mutate(
    label_text = paste0(gsub(",.*", "", NAME), " MSA\nEstimated ", map_target_year, " Population:\n", scales::comma(population)),
    label_geom = sf::st_point_on_surface(geometry)
  ) %>%
  select(MSA_NAME = NAME, label_text, label_geom) %>%
  st_as_sf()

# --- 9. Create the Map Plot using ggplot2 ---

print("Generating map plot...")

# Ensure MSA_NAME is a factor for consistent coloring
ms_counties_sf$MSA_NAME <- factor(ms_counties_sf$MSA_NAME, levels = target_msa_names)

map_plot <- ggplot() +
  # Layer 1: All Mississippi counties
  geom_sf(data = ms_counties_sf, aes(fill = MSA_NAME), color = "black", linewidth = 0.3, show.legend = FALSE) +
  # Color the counties
  scale_fill_manual(
    values = msa_colors,
    na.value = "white",
    name = "Metropolitan Statistical Area"
  ) +
  # Layer 2: Labels
  geom_sf_text(
    data = label_points,
    aes(geometry = label_geom, label = label_text),
    size = 2.8,
    fontface = "bold",
    lineheight = 0.9,
    check_overlap = FALSE # Set to TRUE if labels severely overlap despite arrangement
  ) +
  # Map Theme and Titles (These are part of the map plot itself)
  labs(
    # title = "Selected Mississippi Metropolitan Statistical Areas", # Title is now added globally
    subtitle = paste("Counties within MSA colored. Population is ACS 5-Year Estimate for", map_target_year, "(entire MSA)"),
    caption = "Data Source: US Census Bureau ACS 5-Year Estimates via tidycensus"
  ) +
  theme_void() + # Minimal theme
  theme(
    # plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Title removed from individual plot
    plot.subtitle = element_text(hjust = 0.5, size = 10), # Subtitle centered under map
    plot.caption = element_text(hjust = 0.5, size = 8)  # Caption centered under map
    # plot.margin = unit(c(0,0,0,0), "cm") # Reduce plot margins if needed
  )


# --- 10. Create Table Grob and Arrange Map and Table Side-by-Side ---

print("Creating table graphical object...")

# Customize the theme for the table grob
table_theme <- ttheme_minimal(
  base_size = 8, # Adjust font size as needed
  padding = unit(c(3, 3), "mm"),
  core = list(fg_params = list(hjust = 1, x = 0.95)), # Right-align core cells
  colhead = list(fg_params = list(hjust = 1, x = 0.95)) # Right-align header cells
)

# Create the table grob
table_grob <- tableGrob(population_table_formatted, rows = NULL, theme = table_theme)

print("Arranging map and table side-by-side...")

# Arrange the map plot and the table grob side-by-side
combined_plot_grob <- grid.arrange(
  map_plot,
  table_grob,
  ncol = 2, # Arrange in 2 COLUMNS
  widths = c(2, 2.6), # Relative widths (Map gets 2.5 parts, Table gets 1.2 parts) - Adjust as needed
  top = textGrob("Mississippi MSA Population Estimates & Map", gp = gpar(fontsize = 16, fontface = "bold"))
)

# --- 11. Display and Save Combined Plot within a Viewport ---

# Save the combined plot using standard graphics devices (landscape orientation)
print("Saving combined map and table plot (landscape)...")
png("MS_MSA_Map_with_Table_Landscape_2023.png", width = 11, height = 8.5, units = "in", res = 300) # Landscape

# --- Create a viewport to contain the plot, shifted left ---
# vp_width: How much of the page width the plot area should take (e.g., 0.95 = 95%)
# vp_x_pos: The horizontal position for the LEFT edge of the viewport (e.g., 0.02 = 2% from left)
vp_width <- 0.98 # Use slightly more width
vp_x_pos <- 0.01 # Start closer to left edge

# Define the viewport: starts vp_x_pos from left, takes vp_width of page, centered vertically
plot_vp <- viewport(
  x = unit(vp_x_pos, "npc"),      # Start slightly from left edge
  y = unit(0.5, "npc"),           # Center vertically
  width = unit(vp_width, "npc"),  # Use less than full width
  height = unit(0.95, "npc"),     # Use slightly less height for top/bottom margin
  just = c("left", "center")      # Justify from the left-center edge of the viewport
)

# --- Draw the plot inside the viewport ---
# 1. Push the viewport to make it the active drawing area
pushViewport(plot_vp)

# 2. Draw the arranged plot object INSIDE the current viewport
#    Need to use the object returned by grid.arrange directly
grid.draw(combined_plot_grob)

# 3. Pop the viewport (good practice, returns to parent context)
popViewport()

# --- Close the device ---
dev.off()

# Optional: Save as PDF (landscape)
# pdf("MS_MSA_Map_with_Table_Landscape_2023.pdf", width = 11, height = 8.5)
# pushViewport(plot_vp)
# grid.draw(combined_plot_grob)
# popViewport()
# dev.off()


print("Script finished.")
# --- End of Script ---