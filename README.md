# Air Monitoring Network History Explorer 🌍🔬

A professional, state-agnostic R-Shiny dashboard designed for technical auditing and visualization of the US Air Monitoring Network. Developed to provide air quality agencies with a high-resolution view of historical network growth, instrumentation evolution, and regulatory compliance.

### 🚀 [Live Demo (shinyapps.io)](https://rcuevas.shinyapps.io/Air_Monitoring_History/)

---

## 🔬 Core Features

- **Universal State Architecture**: Dynamically scales to any US state or territory, fetching official county denominators and monitor metadata via the EPA AQS API.
- **Hardware-First Technical Audit**: Utilizes a sophisticated parsing engine to identify specific manufacturer hardware (e.g., **Teledyne T640**, **Met One BAM**, **Thermo TEOM**).
- **Gaseous Hardware Decoder**: Intelligently maps legacy EPA "Instrumental" labels to specific scientific categories such as **UV Photometric** and **Chemiluminescence** Analyzers.
- **Regulatory Hierarchy Visualization**: Distinguishes between Regulatory (SLAMS/NCORE) and AQI-only (88502/TEOM) monitors across historical timelines.
- **High-Resolution Interactive Charts**: Features "X-Unified" hover tracking for synchronized temporal analysis of data counts and technical methods.

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
                   "bsicons", "shinycssloaders", "tidycensus", "tidyr"))
```

### 4. Run the App
```r
shiny::runApp()
```

---

## 📊 Methodology & Data Sources

This project leverages the following technical resources:
- **EPA AQS API**: Source for all historical monitor metadata and measurement methods.
- **US Census Bureau**: Population estimates and geographic reference data.
- **Hardware Parsing Engine**: Custom regex-based logic to extract technical equipment models from scientific method descriptions.

**Maintained by**: Rodney Cuevas, Meteorologist ([Mississippi Department of Environmental Quality](https://www.mdeq.ms.gov/))
**Last Updated**: April 2026
