# ==============================================================================
# Air Monitoring History Explorer (Multi-State Shiny Dashboard)
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(DT)
library(RAQSAPI)
library(bsicons) # For modern icons in value boxes
library(shinycssloaders) # For non-blocking loading spinners
library(tidycensus) # For population-based metrics
library(htmltools)
library(htmlwidgets)

# 1. GLOBAL CONFIGURATION
# ------------------------------------------------------------------------------
# Load local environment variables (.Renviron)
if (file.exists(".Renviron")) readRenviron(".Renviron")

# EPA AQS Credentials from .Renviron (Recommended)
my_email <- Sys.getenv("AQS_EMAIL")
my_key   <- Sys.getenv("AQS_KEY")

# Rotate or set these in .Renviron for security
if (my_email == "" || my_key == "") {
  # Fallback for manual run if .Renviron isn't loaded
  message("Hint: Set AQS_EMAIL and AQS_KEY in .Renviron for security.")
}
aqs_credentials(username = my_email, key = my_key)

# Census Configuration
if (Sys.getenv("CENSUS_API_KEY") != "") {
  census_api_key(Sys.getenv("CENSUS_API_KEY"))
} else {
  message("Warning: CENSUS_API_KEY not found in environment.")
}

# Ensure cache directory exists
if (!dir.exists("cache")) dir.create("cache")

# Configuration: Criteria Pollutants with multi-code recovery
pollutants_default <- list(
  "Ozone" = "44201", 
  "PM2.5" = c("88101", "88502"), 
  "PM10"  = "81102",
  "SO2"   = "42401", 
  "NO2"   = "42602", 
  "CO"    = "42101", 
  "Lead"  = c("12128", "85129"),
  "PAMS VOCs" = "45201",    # Using Benzene as PAMS indicator
  "Air Toxics" = "43502",   # Using Formaldehyde as Toxics indicator
  "PM Speciation" = "88403" # PM2.5 Speciation (Sulfate/etc)
)

# Fetch States list (Static Reference for high-reliability startup)
state_df <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
            "Delaware", "District Of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
            "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
            "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", 
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virgin Islands", 
            "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  stateFIPS = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", 
                "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", 
                "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "72", "44", "45", 
                "46", "47", "48", "49", "50", "78", "51", "53", "54", "55", "56"),
  stringsAsFactors = FALSE
)

state_choices <- setNames(state_df$stateFIPS, state_df$state)

# 2. UI DEFINITION
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Air Monitoring Network History",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  
  sidebar = sidebar(
    title = "Controls",
    width = 300,
    selectInput("state", "Select US State:", choices = state_choices, selected = "28"), # Default to MS
    selectizeInput("counties", "Filter by County:", choices = NULL, multiple = TRUE, options = list(placeholder = 'All Counties')),
    selectizeInput("cbsas", "Filter by CBSA:", choices = NULL, multiple = TRUE, options = list(placeholder = 'All CBSAs')),
    selectizeInput("agencies", "Filter by Agency:", choices = NULL, multiple = TRUE, options = list(placeholder = 'All Agencies')),
    selectizeInput("programs", "Network Program Level:", 
                   choices = c("All", "SLAMS", "PAMS", "NCORE", "NATTS", "SPM (Special)", "Tribal", "Industrial"), 
                   selected = "All", multiple = TRUE, options = list(placeholder = 'All Programs')),
    checkboxGroupInput("pollutants", "Pollutants to Include:", 
                       choices = names(pollutants_default), 
                       selected = c("Ozone", "PM2.5", "PM10", "SO2", "NO2", "CO")),
    checkboxInput("only_active", "Only Show Active Sites", value = TRUE),
    checkboxInput("reg_only", "Regulatory Monitors Only", value = FALSE),
    hr(),
    sliderInput("year_range", "Historical View (Year):", 
                min = 1950, max = year(Sys.Date()), 
                value = c(1950, year(Sys.Date())), 
                sep = ""),
    hr(),
    actionButton("refresh", "Sync Latest Data", class = "btn-secondary btn-sm"),
    helpText("Switching states may trigger a download if no local cache is found."),
    hr(),
    # Attribution Section
    div(style = "font-size: 0.8em; color: #7f8c8d; line-height: 1.4;",
        tags$p(HTML("<b>Developed by:</b><br>Rodney Cuevas, Meteorologist<br>Mississippi Department of Environmental Quality")),
        tags$p(HTML("For bugs, comments or questions, contact:<br>"),
               tags$a(href = "mailto:RCuevas@mdeq.ms.gov", "RCuevas@mdeq.ms.gov"))
    )
  ),
  
  # Dashboard Layout
  layout_column_wrap(
    width = 1/3, height = "120px",
    value_box(
      title = "Active Coverage",
      value = uiOutput("active_coverage"),
      showcase = bsicons::bs_icon("people-fill"),
      theme = "primary"
    ),
    value_box(
      title = "Network Record Holders",
      value = htmlOutput("site_records"),
      showcase = bsicons::bs_icon("trophy-fill"),
      theme = "success"
    ),
    value_box(
      title = "County Coverage",
      value = textOutput("county_coverage"),
      showcase = bsicons::bs_icon("map-fill"),
      theme = "info"
    )
  ),
  
  navset_card_pill(
    title = "Network Explorer",
    nav_panel("Interactive Map", 
              shinycssloaders::withSpinner(leafletOutput("map", height = "600px"))
    ),
    nav_panel("Network History", 
              shinycssloaders::withSpinner(plotlyOutput("trend_plot", height = "400px")),
              hr(),
              shinycssloaders::withSpinner(plotlyOutput("pollutant_plot", height = "400px"))
    ),
    nav_panel("Detailed Data", 
              p(tags$small(class = "text-muted", "Note: CSV/Excel exports reflect your currently selected sidebar filters.")),
              shinycssloaders::withSpinner(DTOutput("table"))
    ),
    nav_panel("About & Methodology", 
              card(
                card_header("Technical Methodology & Data Integrity"),
                p("This dashboard provides a high-resolution technical audit of the US Air Monitoring Network, combining historical EPA metadata with real-time operational metrics for professional air quality analysis."),
                
                h5("1. Universal State Architecture"),
                p("The dashboard is fully state-agnostic and scales to any US region. Upon selecting a state, the system dynamically fetches official county counts via the EPA AQS API (e.g., 82 for Mississippi) to provide accurate network coverage metrics. All technical hardware and regulatory audits are applied universally across the selected region."),

                h5("2. Regulatory Hierarchy & Compliance"),
                p("To ensure cross-state accuracy, monitors are identified using national EPA-standard metadata. A monitor is classified as 'Regulatory' if it is officially designated as SLAMS, NCORE, TRIBAL, or PAMS. The system also distinguishes between standard regulatory parameters (e.g., PM2.5 Code 88101) and AQI-only parameters (e.g., PM2.5 Code 88502/TEOMs) used primarily for public health reporting."),
                
                h5("3. Hardware-First Instrumentation Parsing"),
                p("Utilizing a smart-parsing engine, the dashboard identifies specific instrument models. For particulate matter, the system strips redundant EPA metadata to reveal core manufacturing hardware (e.g., Teledyne T640, Met One BAM, Thermo TEOM). For gaseous pollutants (Ozone, SO2, NO2), the system includes a technical decoder that intelligently maps legacy 'Instrumental' labels to specific instrumentation categories like UV Photometric and Chemiluminescence Analyzers."),
                
                h5("4. Multi-Parameter History Recovery"),
                p("The database uses an expanded search algorithm to recover 100% of a network's history, handling 'Parameter Splits' such as Lead (matching both TSP STP Code 12128 and PM10 LC Code 85129) and PM2.5 (matching both Standard LC and Acceptable codes)."),
                
                h5("5. Network Program Intelligence"),
                p("The dashboard intelligently classifies sites into their primary regulatory programs. By parsing the 'networks' and 'monitor_type' metadata, it distinguishes between SLAMS (State/Local), NCore (National Core), PAMS (Photochemical), NATTS (Air Toxics), and Tribal stations. This allows for professional auditing of the specific mission and funding stream of any monitor in the US."),

                h5("6. Specialized National Audits (VOCs & Toxics)"),
                p("To support specialized air quality missions, the system incorporates 'Indicator Parameters' for high-level auditing. The 'PAMS VOC' suite leverages Benzene (45201) to identify active photochemical networks, while 'Air Toxics' uses Formaldehyde (43502) as the primary indicator for NATTS and Toxics trends stations across all 50 states."),

                h5("Data Sources"),
                tags$ul(
                  tags$li("EPA Air Quality System (AQS) API"),
                  tags$li("US Census Bureau (Population & Geographic Reference)"),
                  tags$li("Historical AQS Metadata Archive (1950 - Present)")
                ),
                footer = list(
                  p("Developed & Maintained by: Rodney Cuevas, Meteorologist (MDEQ)"),
                  p(tags$small("Project Version: 2.1 (Elite Program Edition) - April 2026"))
                )
              )
    )
  )
)

server <- function(input, output, session) {
  
  # Session-scoped cache for Census data
  pop_cache <- list()
  
  # Shared Constants
  status_pal <- colorFactor(c("#27ae60", "#c0392b"), domain = c("Active", "Closed"))
  
  # Helper: Vectorized Site Popup HTML (High-Performance)
  render_site_popup <- function(sites) {
    status_colors <- if_else(sites$Status == "Active", "#27ae60", "#c0392b")
    op_text <- if_else(
      sites$Status == "Active", 
      paste(sites$Site_Established, "to Present"), 
      paste(sites$Site_Established, "to", sites$Site_Closed)
    )
    
    # Clean up address display (Handle NA/Empty)
    clean_addr <- if_else(is.na(sites$address) | sites$address == "", "", paste0(sites$address, ", "))
    
    # Construct Clean EPA Search Link (bare landing page)
    epa_url <- "https://www.epa.gov/outdoor-air-quality-data/interactive-map-air-quality-monitors"
    
    # Construct Force-Satellite Google Maps Link
    # Uses the @lat,lng,zoomz/data=!3m1!1e3 structure for instant satellite layer
    gmaps_url <- paste0("https://www.google.com/maps/@", sites$latitude, ",", sites$longitude, ",18z/data=!3m1!1e3")
    # Create popup content
    paste0(
      "<div style='font-family: sans-serif; min-width: 250px;'>",
      "<h4 style='margin:0; color:", status_colors, ";'>", sites$local_site_name, "</h4>",
      "<small style='color: #666;'>", clean_addr, sites$city_name, "</small><hr style='margin: 10px 0;'>",
      "<table style='width: 100%; font-size: 12px; border-collapse: collapse;'>",
      "<tr><td><b>AQS ID:</b></td><td style='text-align: right;'>", sites$state_code, "-", sites$county_code, "-", sites$site_number, "</td></tr>",
      "<tr><td><b>Status:</b></td><td style='text-align: right;'>", sites$Status, "</td></tr>",
      "<tr><td><b>Type:</b></td><td style='text-align: right;'>", sites$monitor_type, "</td></tr>",
      "<tr><td><b>Scale:</b></td><td style='text-align: right;'>", sites$measurement_scale, "</td></tr>",
      "<tr><td><b>Operated:</b></td><td style='text-align: right;'>", op_text, "</td></tr>",
      "<tr><td><b>Agency:</b></td><td style='text-align: right;'>", sites$monitoring_agency, "</td></tr>",
      "<tr><td><b>Tribal:</b></td><td style='text-align: right;'>", sites$Tribal, "</td></tr>",
      "</table><hr style='margin: 10px 0;'>",
      if_else(!is.na(sites$Active_Pollutants) & sites$Active_Pollutants != "", 
             paste0("<b style='color:#27ae60;'>Current Monitors:</b><br><small style='color: #444;'>", sites$Active_Pollutants, "</small><br>"), ""),
      if_else(!is.na(sites$Past_Pollutants) & sites$Past_Pollutants != "", 
             paste0("<b style='color:#666;'>Historical Monitors:</b><br><small style='color: #666;'>", sites$Past_Pollutants, "</small><br>"), ""),
      "<div style='margin-top:10px; text-align:center;'>",
      "<a href='", epa_url, "' target='_blank' style='font-size: 10px; color: #3498db;'>Search EPA AirData</a>",
      "<br><a href='", gmaps_url, "' target='_blank' style='font-size: 10px; color: #e67e22;'>View in Google Maps Satellite</a>",
      "</div>",
      "</div>"
    )
  }
  
  # Reactive values to hold the current raw data and population
  data_store <- reactiveValues(raw = NULL, history = NULL, pop = NA, trigger = 0)
  
  # Action: Refresh Cache
  observeEvent(input$refresh, {
    state_code <- input$state
    
    # 1. Force Reset Memory immediately (Triggers Spinners)
    data_store$raw <- NULL
    data_store$pop <- NA
    pop_cache[[state_code]] <<- NULL
    
    # 2. Wipe physical cache file
    cache_path <- paste0("cache/state_", state_code, ".rds")
    if (file.exists(cache_path)) {
      message(paste("Clearing cache for state-code:", state_code))
      file.remove(cache_path)
    }
    
    # 3. Trigger Invalidation
    data_store$trigger <- data_store$trigger + 1
    showNotification(paste("Refreshing data for State", state_code, "..."), type = "message")
  })
  
  # Synchronized Data Fetching
  observeEvent(list(input$state, data_store$trigger), {
    req(input$state)
    
    state_code <- input$state
    state_name <- state_df$state[state_df$stateFIPS == state_code]
    cache_path <- paste0("cache/state_", state_code, ".rds")
    
    # 1. Fetch Population (from Census API with session caching)
    if (is.null(pop_cache[[state_code]])) {
      tryCatch({
        pop_data <- get_estimates(geography = "state", product = "population", state = state_name, vintage = 2023)
        if ("variable" %in% colnames(pop_data)) {
           val <- pop_data$value[toupper(pop_data$variable) == "POPESTIMATE"]
           pop_cache[[state_code]] <<- if(length(val) > 0 && !is.na(val[1])) val[1] else 0
        } else {
           pop_cache[[state_code]] <<- if(nrow(pop_data) > 0 && !is.na(pop_data$value[1])) pop_data$value[1] else 0
        }
      }, error = function(e) {
        showNotification(paste("Census API Error:", e$message), type = "warning")
        pop_cache[[state_code]] <<- 0
      })
    }
    data_store$pop <- pop_cache[[state_code]]
    
    # 2. Load Monitoring Data
    if (file.exists(cache_path)) {
      raw_data <- readRDS(cache_path)
    } else {
      message(paste("Downloading data for state:", state_code))
      start_date <- as.Date("1950-01-01")
      end_date   <- Sys.Date()
      
      # Fetch monitors for each pollutant name, potentially trying multiple codes
      raw_data <- map_dfr(names(pollutants_default), function(p_name) {
        codes <- pollutants_default[[p_name]]
        
        # Iterate through codes (e.g., TSP Lead and PM10 Lead)
        map_df(codes, function(p_code) {
          tryCatch({
            data <- aqs_monitors_by_state(stateFIPS = state_code, parameter = p_code, bdate = start_date, edate = end_date)
            if (!is.null(data) && nrow(data) > 0) {
              data$pollutant_type <- p_name # Group under display name
              Sys.sleep(0.1) # Respect API throttling
              return(data %>% mutate(across(everything(), as.character)))
            }
            return(NULL)
          }, error = function(e) { return(NULL) })
        })
      })
      
      if (!is.null(raw_data) && nrow(raw_data) > 0) {
        saveRDS(raw_data, cache_path)
      } else {
        showNotification(paste("No monitoring data found for State", state_code), type = "warning")
        raw_data <- NULL
      }
    }
    
    # 3. Fetch official county list for this state (True Denominator)
    try({
      all_counties <- aqs_counties_by_state(stateFIPS = state_code)
      if (!is.null(all_counties)) {
        data_store$total_counties_in_state <- nrow(all_counties)
      }
    }, silent = TRUE)
    
    # Update Sidebar Filters & Slider
    if (!is.null(raw_data) && nrow(raw_data) > 0) {
      updateSelectizeInput(session, "counties", choices = sort(unique(raw_data$county_name)), server = TRUE)
      updateSelectizeInput(session, "cbsas", choices = sort(unique(na.omit(raw_data$cbsa_name))), server = TRUE)
      updateSelectizeInput(session, "agencies", choices = sort(unique(na.omit(raw_data$monitoring_agency))), server = TRUE)
      
      # Calculate Bounds
      years <- year(as.Date(raw_data$open_date))
      min_yr <- max(1950, min(years, na.rm=TRUE))
      if (is.infinite(min_yr) || is.na(min_yr)) min_yr <- 1950
      max_yr <- year(Sys.Date())
      
      # Preserve existing slider value if it's still valid
      current_val <- input$year_range
      new_val <- c(max(min_yr, current_val[1]), min(max_yr, current_val[2]))
      
      updateSliderInput(session, "year_range", min = min_yr, max = max_yr, value = new_val)
    }
    data_store$raw <- raw_data
  })
  
  # Refined Data Filtering (Shared across map and charts)
  filtered_raw <- reactive({
    req(data_store$raw)
    raw <- data_store$raw
    
    # 1. Primary Filters
    if (length(input$counties) > 0) raw <- raw %>% filter(county_name %in% input$counties)
    if (length(input$cbsas) > 0)    raw <- raw %>% filter(cbsa_name %in% input$cbsas)
    if (length(input$agencies) > 0) raw <- raw %>% filter(monitoring_agency %in% input$agencies)
    raw <- raw %>% filter(pollutant_type %in% input$pollutants)
    
    # 2. Universal Regulatory Filter (State-Agnostic)
    if (input$reg_only) {
      raw <- raw %>%
        filter(parameter_code != "88502") %>% # Non-reg PM2.5 standard
        filter(!grepl("NON-REGULATORY|INDUSTRIAL|SPECIAL PURPOSE", monitor_type, ignore.case = TRUE))
    }
    
    # 3. Network Program Classification & Filter
    raw <- raw %>%
      mutate(
        # Identify Primary Network Program
        network_program = case_when(
          grepl("NCORE", networks, ignore.case = TRUE) ~ "NCORE",
          grepl("NATTS", networks, ignore.case = TRUE) ~ "NATTS",
          grepl("PAM", networks, ignore.case = TRUE) ~ "PAMS",
          grepl("CSN|STN", networks, ignore.case = TRUE) ~ "CSN",
          grepl("SLAMS", monitor_type, ignore.case = TRUE) ~ "SLAMS",
          grepl("SPM|SPECIAL PURPOSE", monitor_type, ignore.case = TRUE) ~ "SPM (Special)",
          grepl("TRIBAL", monitor_type, ignore.case = TRUE) ~ "Tribal",
          grepl("INDUSTRIAL", monitor_type, ignore.case = TRUE) ~ "Industrial",
          TRUE ~ "Other"
        )
      )
    
    if (length(input$programs) > 0 && !"All" %in% input$programs) {
      raw <- raw %>% filter(network_program %in% input$programs)
    }
    
    return(raw)
  })
  
  # Processed Data (Refined History Logic)
  processed_history <- reactive({
    sites <- filtered_raw()
    
    sites <- sites %>%
      as_tibble() %>%
      mutate(
        open_date = as_date(open_date),
        close_date = as_date(close_date),
        calc_close = if_else(is.na(close_date), Sys.Date(), close_date),
        
        # Detect Method (FRM/FEM)
        method_type = case_when(
          grepl("FRM", last_method_description, ignore.case = TRUE) ~ "FRM",
          grepl("FEM", last_method_description, ignore.case = TRUE) ~ "FEM",
          grepl("Continuous", last_method_description, ignore.case = TRUE) ~ "Continuous",
          TRUE ~ ""
        ),
        
        # Extract Clean Instrument Name (Hardware First + Gaseous Decoder)
        instrument_name = case_when(
          # 1. Gaseous Hardware Decoding (Enriching 'Instrumental')
          parameter_code == "44201" & grepl("ULTRA VIOLET|UV ABSORPTION", last_method_description, ignore.case = TRUE) ~ 
            "UV Photometric (Thermo 49/API 400)",
          parameter_code == "42401" & grepl("FLUORESCENCE", last_method_description, ignore.case = TRUE) ~ 
            "Fluorescence Analyzer (Thermo 43/API 100)",
          parameter_code == "42602" & grepl("CHEMILUMINESCENCE", last_method_description, ignore.case = TRUE) ~ 
            "Chemiluminescence (Thermo 42/API 200)",
          parameter_code == "42101" & grepl("INFRARED|IR", last_method_description, ignore.case = TRUE) ~ 
            "NDIR Gas Analyzer (Thermo 48/API 300)",
            
          # 2. Particulate Hardware Extraction
          grepl("Correction Factor - ", last_method_description) ~ sub(".*Correction Factor - ", "", last_method_description),
          TRUE ~ sub(" [;\\-].*", "", last_method_description)
        ),
        instrument_name = sub(" - .*", "", instrument_name), # Cleanup secondary hyphens
        # Final Generic Cleanup
        instrument_name = if_else(instrument_name == "INSTRUMENTAL", "Automated Gas Analyzer", instrument_name),
        
        # Universal Regulatory Check
        is_reg = case_when(
          parameter_code == "88502" ~ FALSE,
          grepl("NON-REGULATORY|INDUSTRIAL|SPECIAL PURPOSE", monitor_type, ignore.case = TRUE) ~ FALSE,
          TRUE ~ TRUE
        ),
        
        # Operational Timeline Text
        date_text = if_else(is.na(close_date), 
                            paste0(open_date, " to Present"), 
                            paste0(open_date, " to ", close_date)),
        
        # Build specific pollutant tag
        poll_tag = paste0(
          pollutant_type, 
          " (POC ", poc, 
          if_else(instrument_name != "", paste0(" - ", instrument_name), ""),
          if_else(method_type != "", paste0(" - ", method_type), ""), 
          if_else(is_reg, "", " - Non-Reg"),
          " - [", date_text, "])"
        )
      ) %>%
      # Filter by Year Range (Site must have been open at some point in the range)
      filter(year(open_date) <= input$year_range[2]) %>%
      filter(is.na(close_date) | year(close_date) >= input$year_range[1]) %>%
      
      group_by(state_code, county_code, site_number) %>%
      summarize(
        local_site_name        = first(na.omit(local_site_name)),
        city_name              = first(na.omit(city_name)),
        county_name            = first(na.omit(county_name)),
        latitude               = as.numeric(first(na.omit(latitude))),
        longitude              = as.numeric(first(na.omit(longitude))),
        address                = first(na.omit(address)),
        elevation              = first(na.omit(elevation)),
        cbsa_name              = first(na.omit(cbsa_name)),
        monitoring_agency      = first(na.omit(monitoring_agency)),
        monitor_type           = paste(sort(unique(na.omit(monitor_type))), collapse = ", "),
        measurement_scale      = first(na.omit(measurement_scale)),
        tribe_name             = first(na.omit(tribe_name)),
        network_program        = first(network_program),
        networks               = first(na.omit(networks)),
        poc                    = paste(sort(unique(na.omit(poc))), collapse = ", "),
        
        # Timeline
        Site_Established       = min(open_date, na.rm = TRUE),
        Site_Closed            = if_else(any(is.na(close_date)), as.Date(NA), max(close_date, na.rm = TRUE)),
        Years_Active           = round(as.numeric(difftime(max(calc_close), min(open_date), units = "days")) / 365.25, 1),
        
        # Pollutant Split Logic with POC/Method tags
        Active_Pollutants      = paste(sort(unique(poll_tag[is.na(close_date)])), collapse = "<br>"),
        Past_Pollutants        = paste(sort(unique(poll_tag[!is.na(close_date)])), collapse = "<br>"),
        
        .groups = "drop"
      ) %>%
      mutate(
        Status = if_else(is.na(Site_Closed), "Active", "Closed"),
        local_site_name = if_else(is.na(local_site_name), paste("Site", site_number), local_site_name),
        Tribal = if_else(!is.na(tribe_name) & tribe_name != "", "Yes", "No")
      ) %>%
      ungroup()
    
    # Apply 'Only Active' filter if requested
    if (input$only_active) {
      sites <- sites %>% filter(Status == "Active")
    }
    
    return(sites)
  })
  
  # Metrics
  output$active_coverage <- renderUI({ 
    hist <- processed_history()
    validate(need(nrow(hist) > 0, "No data..."))
    active <- sum(hist$Status == "Active")
    
    if (is.na(data_store$pop)) {
      # Tri-state Loading View
      HTML(paste0(
        "<div>", active, " Active Sites</div>",
        "<div style='font-size: 0.6em; opacity: 0.8;'>Loading Population...</div>"
      ))
    } else if (data_store$pop > 0) {
      density <- round(active / (data_store$pop / 100000), 2)
      pop_fmt <- format(data_store$pop, big.mark = ",")
      HTML(paste0(
        "<div>", density, " Monitors / 100k</div>",
        "<div style='font-size: 0.6em; opacity: 0.8;'>Pop: ", pop_fmt, "</div>"
      ))
    } else {
      HTML(paste0(
        "<div>", active, " Active Sites</div>",
        "<div style='font-size: 0.6em; opacity: 0.8;'>Pop: Unknown (Check API Key)</div>"
      ))
    }
  })
  
  output$site_records <- renderUI({ 
    hist <- processed_history()
    validate(need(nrow(hist) > 0, "No records found"))
    active_sites <- hist %>% filter(Status == "Active" & !is.na(Site_Established))
    
    if (nrow(active_sites) > 0) {
      oldest <- active_sites %>% arrange(Site_Established) %>% slice(1)
      newest <- hist %>% arrange(desc(Site_Established)) %>% slice(1)
      
      HTML(paste0(
        "<div style='font-size: 0.8em;'>",
        "<b>Oldest Active:</b> ", oldest$local_site_name, " (", year(oldest$Site_Established), ")<br>",
        "<b>Newest Overall:</b> ", newest$local_site_name, " (", year(newest$Site_Established), ")",
        "</div>"
      ))
    } else {
      HTML("<div style='font-size: 0.8em;'>No Active Sites Found</div>")
    }
  })
  
  output$county_coverage <- renderText({ 
    req(data_store$raw)
    hist <- processed_history()
    validate(need(nrow(hist) > 0, "0/0"))
    
    # Use the official denominator if available, else fallback to data-driven
    total_counties <- data_store$total_counties_in_state %||% length(unique(data_store$raw$county_name))
    
    monitored_counties <- length(unique(hist$county_name))
    paste0(monitored_counties, "/", total_counties, " Counties (", round(monitored_counties/total_counties*100), "%)")
  })
  
  # Leaflet Map (Base)
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager, group = "Modern Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Mode") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Modern Light", "Dark Mode", "Satellite"),
        overlayGroups = c("Active Sites", "Closed Sites"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addMiniMap(tiles = providers$CartoDB.Voyager, toggleDisplay = TRUE) %>%
      addSearchOSM()
  })
  
  # Reactive Legend Update
  observeEvent(list(input$only_active, input$state), {
    req(input$state)
    legend_values <- if(input$only_active) "Active" else c("Active", "Closed")
    
    leafletProxy("map") %>%
      removeControl("status_legend") %>%
      addLegend("bottomright", pal = status_pal, values = legend_values, 
                title = "Status", layerId = "status_legend")
  })
  
  # Leaflet Map (Reactive Markers)
  observe({
    req(input$state, data_store$raw)
    history <- processed_history()
    
    proxy <- leafletProxy("map", data = history) %>%
      clearMarkers() %>%
      clearMarkerClusters()
    
    # Active Sites
    active_data <- filter(history, Status == "Active")
    if (nrow(active_data) > 0) {
      proxy %>%
        addCircleMarkers(
          data = active_data,
          lng = ~longitude, lat = ~latitude,
          group = "Active Sites",
          color = "white", weight = 1,
          fillColor = ~status_pal(Status), fillOpacity = 0.8,
          radius = 6,
          clusterOptions = markerClusterOptions(),
          popup = render_site_popup(active_data)
        )
    }
    
    # Closed Sites
    closed_data <- filter(history, Status == "Closed")
    if (nrow(closed_data) > 0) {
      proxy %>%
        addCircleMarkers(
          data = closed_data,
          lng = ~longitude, lat = ~latitude,
          group = "Closed Sites",
          color = "white", weight = 1,
          fillColor = ~status_pal(Status), fillOpacity = 0.6,
          radius = 5,
          clusterOptions = markerClusterOptions(),
          popup = render_site_popup(closed_data)
        )
    }
  })
  
  # Logic to auto-center map on new data load
  observeEvent(data_store$raw, {
    req(data_store$raw)
    raw <- data_store$raw
    if (nrow(raw) > 0) {
      leafletProxy("map") %>%
        fitBounds(
          lng1 = min(raw$longitude, na.rm = TRUE),
          lat1 = min(raw$latitude, na.rm = TRUE),
          lng2 = max(raw$longitude, na.rm = TRUE),
          lat2 = max(raw$latitude, na.rm = TRUE)
        )
    }
  })
  
  # Trends Plot (Network Growth)
  output$trend_plot <- renderPlotly({
    req(input$year_range[1], input$year_range[2]) # Prevent transition errors
    hist <- processed_history()
    validate(need(nrow(hist) > 0, "No data for selected range"))
    
    raw <- filtered_raw() %>%
      mutate(open = as_date(open_date), close = as_date(close_date))
    
    # Sync with Slider Range
    years_seq <- seq(input$year_range[1], input$year_range[2])
    
    trend_data <- map_df(years_seq, function(y) {
      y_date_start <- as_date(paste0(y, "-01-01"))
      y_date_end   <- as_date(paste0(y, "-12-31"))
      
      active_count <- raw %>%
        filter(open <= y_date_end & (is.na(close) | close >= y_date_start)) %>%
        summarize(sites = n_distinct(paste(state_code, county_code, site_number))) %>%
        pull(sites)
      
      data.frame(Year = y, Count = active_count)
    })
    
    p <- ggplot(trend_data, aes(x = Year, y = Count)) +
      geom_line(color = "#2c3e50", linewidth = 1) +
      geom_area(fill = "#3498db", alpha = 0.3) +
      theme_minimal() +
      labs(title = "Network Growth: Active Monitors per Year", y = "Count", x = "")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Pollutant Mix over Time (Stacked Area Chart)
  output$pollutant_plot <- renderPlotly({
    req(input$year_range[1], input$year_range[2]) # Prevent transition errors
    req(data_store$raw)
    validate(need(nrow(processed_history()) > 0, "No data..."))
    
    raw <- filtered_raw() %>%
      mutate(
        open = as_date(open_date), 
        close = as_date(close_date),
        # 1. Robust Method Detection (Hardware First)
        is_frm = grepl("FRM|Manual|Gravimetric|Sequential|Sampl|2025", last_method_description, ignore.case = TRUE) & 
                 !grepl("FEM|Continuous|Broadband|T640|BAM|TEOM", last_method_description, ignore.case = TRUE),
        
        plot_group = case_when(
          pollutant_type != "PM2.5" ~ pollutant_type,
          is_frm ~ "PM2.5 (FRM)",
          TRUE ~ "PM2.5 (Continuous)"
        ),
        
        # 2. Determine Fill Type for the Stacked Area
        is_reg = case_when(
          parameter_code == "88502" ~ FALSE,
          grepl("NON-REGULATORY|INDUSTRIAL|SPECIAL PURPOSE", monitor_type, ignore.case = TRUE) ~ FALSE,
          TRUE ~ TRUE
        ),
        
        fill_type = case_when(
          pollutant_type != "PM2.5" ~ pollutant_type,
          plot_group == "PM2.5 (FRM)" ~ "PM2.5 (FRM)",
          is_reg ~ "Regulatory (88101)",
          TRUE ~ "Non-Regulatory (88502/TEOM)"
        )
      )
    
    years_seq <- seq(input$year_range[1], input$year_range[2])
    
    # Calculate active monitors per year AND fill category
    mix_data <- map_df(years_seq, function(y) {
      y_date_start <- as_date(paste0(y, "-01-01"))
      y_date_end   <- as_date(paste0(y, "-12-31"))
      
      raw %>%
        filter(open <= y_date_end & (is.na(close) | close >= y_date_start)) %>%
        group_by(plot_group, fill_type) %>%
        summarize(
          Count = n_distinct(paste(state_code, county_code, site_number)),
          # Pick a representative method/hardware string for the year
          raw_method = first(last_method_description),
          .groups = "drop"
        ) %>%
        mutate(Year = y)
    }) %>%
      # Data Densification: Ensure every year has a record for every category to fix Plotly sync
      tidyr::complete(Year = years_seq, 
                      tidyr::nesting(plot_group, fill_type), 
                      fill = list(Count = 0)) %>%
      # Clean the hardware names (Professional Terminology Upgrade with Gaseous Decoder)
      mutate(
        Method = case_when(
          is.na(raw_method) ~ "Method Not Specified",
          
          # 1. Professional Mapping for Gaseous Methods
          grepl("ULTRA VIOLET|UV ABSORPTION", raw_method, ignore.case = TRUE) ~ "UV Photometric (Thermo 49/API 400)",
          grepl("FLUORESCENCE", raw_method, ignore.case = TRUE) ~ "Fluorescence Analyzer (Thermo 43/API 100)",
          grepl("CHEMILUMINESCENCE", raw_method, ignore.case = TRUE) ~ "Chemiluminescence (Thermo 42/API 200)",
          grepl("INFRARED|IR", raw_method, ignore.case = TRUE) ~ "NDIR Gas Analyzer (Thermo 48/API 300)",
          
          # 2. Standard Hardware Extraction
          grepl(" - ", raw_method) ~ {
            name <- sub(" - .*", "", raw_method)
            # Strip EPA prefixes
            name <- sub(".*: |.* - |PM2.5 SCC w/Correction Factor - |Correction Factor - ", "", name)
            # Refine Generic Terminology
            case_when(
              grepl("Instrumental", name, ignore.case = TRUE) ~ "Automated Gas Analyzer",
              name == "Manual" ~ "Filter-Based Sampling",
              name == "Gravimetric" ~ "Laboratory Analysis",
              TRUE ~ name
            )
          },
          raw_method == "Instrumental" ~ "Continuous Analyzer",
          raw_method == "Manual" ~ "Filter-Based Sampling",
          raw_method == "Gravimetric" ~ "Laboratory Analysis",
          TRUE ~ raw_method
        )
      ) %>%
      # Ensure any remaining 'Instrumental' prefixes are cleaned up
      mutate(Method = sub("^Instrumental - ", "", Method)) %>%
      # Strict ordering to prevent trace ghosting
      arrange(plot_group, Year, fill_type) %>%
      mutate(tooltip_text = paste0("<b>Year: ", Year, "</b><br>",
                                   "Type: ", fill_type, "<br>",
                                   "Method: ", Method, "<br>",
                                   "Active Total: ", Count))
    
    # Use geom_col (width=1) instead of geom_area for bulletproof tooltips
    p <- ggplot(mix_data, aes(x = Year, y = Count, fill = fill_type, group = fill_type, text = tooltip_text)) +
      geom_col(position = "stack", width = 1, color = "white", linewidth = 0.05) +
      scale_fill_brewer(palette = "Set3") +
      facet_wrap(~plot_group, scales = "free_y") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "Network Trends: Continuous vs. Manual Sampling & Regulatory Mix", 
           x = "", y = "Active Monitors", fill = "Monitor Type")
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
      )
  })
  
  # Data Table
  output$table <- renderDT({
    hist <- processed_history()
    validate(need(nrow(hist) > 0, "No data available"))
    
    # Wrap in datatable() to use formatStyle extension correctly
    datatable(
      hist %>%
        mutate(AQS_ID = paste0(state_code, "-", county_code, "-", site_number)) %>%
        select(AQS_ID, local_site_name, county_name, Status, monitor_type, 
               measurement_scale, Years_Active, Active_Pollutants, Past_Pollutants, elevation, address, poc) %>%
        rename(Elevation = elevation, Address = address, POC = poc, `Current Pollutants` = Active_Pollutants, `Past Pollutants` = Past_Pollutants),
      extensions = 'Buttons',
      escape = FALSE,
      options = list(
        pageLength = 15, 
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      )
    ) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(c("Active", "Closed"), c('#d4efdf', '#f9ebea')),
        color = styleEqual(c("Active", "Closed"), c('#145a32', '#78281f')),
        fontWeight = 'bold'
      )
  })
  
}

# 4. RUN APP
# ------------------------------------------------------------------------------
shinyApp(ui, server)
