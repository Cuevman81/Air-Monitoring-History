# Air Monitoring Network History Explorer 🌍🔬

A professional, state-agnostic R-Shiny dashboard designed for technical auditing and visualization of the US Air Monitoring Network. Developed to provide air quality agencies with a high-resolution view of historical network growth, instrumentation evolution, and regulatory compliance.

### 🚀 [Live Demo (shinyapps.io)](https://rcuevas.shinyapps.io/Air_Monitoring_History/)

---

## 🔬 Core Features

- **Universal State Architecture**: Dynamically scales to any US state or territory, fetching official county denominators and monitor metadata via the EPA AQS API.
- **National Program Intelligence**: Automatically classifies and audits monitors by regulatory program, including **NCore** (Flagship Multipollutant), **PAMS** (Photochemical), **NATTS** (Air Toxics), **CSN/Speciation**, and **CASTNET** networks.
- **NAAQS Primary Designation**: Surfaces the official AQS `naaqs_primary_monitor` flag (the design-value monitor) and 40 CFR Part 58 Appendix D monitoring objectives in popups and the data table. Special Purpose Monitors carrying the NAAQS-primary flag are treated as regulatory per 40 CFR 58.20(e).
- **Full Historical Recovery**: Includes Pb-TSP LC (14129, the 2008 Pb NAAQS design-value parameter), historical **TSP (11101)** for pre-1987 particulate site histories, NCore trace parameters (NOy, PM10-2.5), and core meteorology (wind, temperature).
- **Reporting Audit**: On-demand cross-check of AQS monitor metadata against submitted annual summary data — flags open monitor records that reported zero observations in the most recent complete year (phantom records), supporting AQS monitor-maintenance housekeeping ahead of Annual Network Plan submissions.
- **Hardware-First Technical Audit**: Utilizes a sophisticated parsing engine to identify specific manufacturer hardware (e.g., **Teledyne T640**, **Met One BAM**, **Thermo TEOM**).
- **Specialized Network Auditing**: Integrated indicators for **VOC chemistry** (Benzene) and **Air Toxics** (Formaldehyde) trends.
- **Gaseous Hardware Decoder**: Intelligently maps legacy EPA "Instrumental" labels to specific scientific categories such as **UV Photometric** and **Chemiluminescence** Analyzers.
- **High-Resolution Interactive Charts**: Features "X-Unified" hover tracking for synchronized temporal analysis of network growth and technical methods.

## 🛠️ Installation (Local Run)

To run this dashboard on your local machine, follow these steps:

### 1. Clone the Repository
```bash
git clone https://github.com/[your-username]/Air-Monitoring-History.git
cd Air-Monitoring-History
```

### 2. Configure API Credentials
The dashboard requires access to the **EPA AQS API** and the **US Census Bureau API**. Create a file named `.Renviron` in the root directory and add your keys:

```text
AQS_EMAIL="your_email@example.com"
AQS_KEY="your_epa_aqs_key"
CENSUS_API_KEY="your_census_bureau_key"
```

### 3. Install Dependencies
Open R and run:
```r
install.packages(c("shiny", "bslib", "dplyr", "purrr", "lubridate", "ggplot2", 
                   "plotly", "leaflet", "leaflet.extras", "DT", "RAQSAPI", 
                   "bsicons", "shinycssloaders", "tidycensus", "tidyr",
                   "htmltools", "htmlwidgets"))
```

### 4. Run the App
```r
shiny::runApp()
```

### 5. Deploying (shinyapps.io)
shinyapps.io does not support server-side environment variables, so the `.Renviron`
file must be included in the deployment bundle (it is visible only to your
shinyapps.io account — never commit it to git). A `.rscignore` file keeps local
data, docs, and scripts out of the bundle. Deploy from the R console:

```r
source("deploy.R")
```

`deploy.R` handles a known issue: terra 1.9-34 (a transitive dependency via
leaflet → raster) fails to compile against the GDAL 3.4.1 on shinyapps.io's
build image, so the script pins terra to 1.8-86 in the deployment manifest.
Once a fixed terra release ships, plain `rsconnect::deployApp()` will work again.

If you migrate to Posit Connect, exclude `.Renviron` from the bundle and pass
credentials with `deployApp(envVars = c("AQS_EMAIL", "AQS_KEY", "CENSUS_API_KEY"))` instead.

---

## 📊 Methodology & Data Sources

This project leverages the following technical resources:
- **EPA AQS API**: Source for all historical monitor metadata and measurement methods.
- **US Census Bureau**: Population estimates and geographic reference data.
- **Hardware Parsing Engine**: Custom regex-based logic to extract technical equipment models from scientific method descriptions.

**Maintained by**: Rodney Cuevas, Meteorologist (RCuevas@mdeq.ms.gov)
**Last Updated**: July 2026 (v2.3 Regulatory Audit Edition)
