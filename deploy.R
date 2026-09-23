# ==============================================================================
# One-command deployment to shinyapps.io
#
# Usage:  source("deploy.R")
#
# Credentials: shinyapps.io has no server-side environment variables, so the
# AQS credentials must ship inside the bundle (visible only to this
# shinyapps.io account, never served to visitors). This script copies ONLY
# AQS_EMAIL and AQS_KEY from ~/.Renviron into a temporary 'aqs.env'
# (git-ignored), bundles it, and deletes it when the deploy finishes or fails.
# The values are never printed. app.R loads aqs.env with readRenviron() and
# logs "[startup] AQS credentials present: TRUE/FALSE"; check that line with
#   rsconnect::showLogs(appName = "Air_Monitoring_History", account = "rcuevas")
#
# Why the manifest step exists: terra 1.9-34 (pulled in via leaflet -> raster
# -> terra) fails to compile against the GDAL 3.4.1 on shinyapps.io's Ubuntu
# jammy build image. Until a fixed terra release ships, we pin terra to 1.8-86
# (the last 1.8-series release, which builds cleanly there) by editing the
# deployment manifest and deploying from it. All other packages deploy at
# the versions installed locally. When terra > 1.9-34 fixes the old-GDAL
# build, TERRA_PIN can be removed.
# ==============================================================================

TERRA_PIN <- "1.8-86"

# Read only the AQS credentials from an .Renviron-format file, leaving this
# session's environment exactly as it was
read_aqs_credentials <- function(path = "~/.Renviron") {
  vars <- c("AQS_EMAIL", "AQS_KEY")
  before <- Sys.getenv()
  on.exit({
    after <- Sys.getenv()
    added <- setdiff(names(after), names(before))
    if (length(added) > 0) Sys.unsetenv(added)
    now <- after[names(before)]
    reset <- names(before)[is.na(now) | now != before]
    if (length(reset) > 0) do.call(Sys.setenv, as.list(before[reset]))
  }, add = TRUE)
  if (!file.exists(path.expand(path))) stop(path, " not found")
  Sys.unsetenv(vars) # so only this file can supply them
  readRenviron(path.expand(path))
  Sys.getenv(vars, names = TRUE)
}

deploy_app <- function() {
  # --- 1. Temporary credentials file (deleted on exit, even on error) ---------
  if (dir.exists(".git") && nzchar(Sys.which("git")) &&
      system2("git", c("check-ignore", "-q", "aqs.env")) != 0) {
    stop("aqs.env is not git-ignored. Add 'aqs.env' to .gitignore before deploying.")
  }
  on.exit(unlink("aqs.env"), add = TRUE)
  creds <- read_aqs_credentials()
  if (!all(nzchar(creds))) stop("AQS_EMAIL and AQS_KEY must both be set in ~/.Renviron")
  writeLines(sprintf('%s="%s"', names(creds), creds), "aqs.env")
  Sys.chmod("aqs.env", "0600")
  if (!identical(read_aqs_credentials("aqs.env"), creds)) stop("aqs.env did not read back correctly")
  message("1/4 Wrote temporary aqs.env (AQS_EMAIL and AQS_KEY only)")

  # --- 2. Manifest: the bundle is exactly these files -------------------------
  app_files <- c("app.R", "aqs.env", list.files("cache", pattern = "\\.rds$", full.names = TRUE))
  message("2/4 Capturing dependencies (writeManifest) for: ", paste(app_files, collapse = ", "))
  rsconnect::writeManifest(appFiles = app_files)

  # --- 3. Pin terra -----------------------------------------------------------
  message("3/4 Pinning terra to ", TERRA_PIN, " in manifest.json...")
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

  # --- 4. Deploy (updates the existing app; never creates a new one) ----------
  message("4/4 Deploying to shinyapps.io...")
  rsconnect::deployApp(
    manifestPath = "manifest.json",
    appName = "Air_Monitoring_History",
    account = "rcuevas",
    server = "shinyapps.io",
    forceUpdate = TRUE
  )
}

deploy_app()
