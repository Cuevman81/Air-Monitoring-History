# Load required libraries
library(knitr)
library(kableExtra)

# Create a data frame with the monitoring site information
monitoring_sites <- data.frame(
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

# Create a nicely formatted table
table_output <- kable(monitoring_sites, format = "html", 
                      col.names = c("County", "City", "Monitoring Site ID", 
                                    "Pollutants Monitored", "Latitude", "Longitude"),
                      align = c("l", "l", "c", "l", "c", "c"),
                      caption = "Air Quality Monitoring Sites") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = TRUE) %>%
  column_spec(4, width = "25em") # Make pollutants column wider

# Print the table
table_output