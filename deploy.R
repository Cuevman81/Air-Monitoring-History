# ==============================================================================
# One-command deployment to shinyapps.io
#
# Usage:  source("deploy.R")
#
# Why this exists: terra 1.9-34 (pulled in via leaflet -> raster -> terra)
# fails to compile against the GDAL 3.4.1 on shinyapps.io's Ubuntu jammy
# build image. Until a fixed terra release ships, we pin terra to 1.8-86
# (the last 1.8-series release, which builds cleanly there) by editing the
# deployment manifest and deploying from it. All other packages deploy at
# the versions installed locally.
#
# When terra > 1.9-34 fixes the old-GDAL build, TERRA_PIN can be removed and
# this reduces to a plain rsconnect::deployApp().
# ==============================================================================

TERRA_PIN <- "1.8-86"

message("1/3 Capturing dependencies (writeManifest)...")
rsconnect::writeManifest()

message("2/3 Pinning terra to ", TERRA_PIN, " in manifest.json...")
txt <- readLines("manifest.json")
tline <- grep('"Package": "terra"', txt, fixed = TRUE)
if (length(tline) == 1) {
  vline <- grep('"Version":', txt)
  vline <- vline[vline > tline][1] # first Version field after the Package line
  txt[vline] <- sub('"Version": *"[^"]*"', sprintf('"Version": "%s"', TERRA_PIN), txt[vline])
  writeLines(txt, "manifest.json")
  m <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
  stopifnot(m$packages[["terra"]]$description$Version == TERRA_PIN)
  message("    terra pinned: ", m$packages[["terra"]]$description$Version)
} else {
  message("    terra not found in manifest - deploying as-is")
}

message("3/3 Deploying to shinyapps.io...")
rsconnect::deployApp(
  manifestPath = "manifest.json",
  appName = "Air_Monitoring_History",
  forceUpdate = TRUE
)
