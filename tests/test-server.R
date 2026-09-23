# ==============================================================================
# Server tests for app.R: the real server() under shiny::testServer, with EPA
# AQS and the Census mocked (no network) and FAKE credentials only.
#
# Run from the repo root (keeps your real ~/.Renviron out of the process):
#   R_ENVIRON_USER=/dev/null Rscript --vanilla tests/test-server.R [app_dir]
#
# Each check prints PASS or FAIL; the exit status is the number of failures.
# Mocked AQS errors carry the fake email and key in their text, like
# RAQSAPI's real errors (which embed the request URL), so the leak checks at
# the end prove no credential reaches an output, notification, log or cache.
# ==============================================================================

suppressPackageStartupMessages({ library(shiny); library(jsonlite) })

args    <- commandArgs(trailingOnly = TRUE)
app_src <- normalizePath(if (length(args) > 0) args[1] else ".")
stopifnot(file.exists(file.path(app_src, "app.R")))
home_wd <- getwd()

FAKE_EMAIL <- "fake.tester@example.com"
FAKE_KEY   <- "FAKEkey9x2"
THIS_YEAR  <- as.integer(format(Sys.Date(), "%Y"))

# --- Results -----------------------------------------------------------------
results <- data.frame(id = character(0), ok = logical(0), note = character(0))
check <- function(id, ok, note = "") {
  ok <- isTRUE(ok)
  results[nrow(results) + 1, ] <<- list(id, ok, note)
  cat(sprintf("%s  %-58s %s\n", if (ok) "PASS" else "FAIL", id, note))
}
# Run one scenario; an error inside it fails that scenario, not the script
scenario <- function(id, code) {
  tryCatch(code, error = function(e) check(id, FALSE, paste("error:", conditionMessage(e))))
}

# --- Mock AQS data -------------------------------------------------------------
mon <- function(county, site, name, param, type = "SLAMS", primary = NA,
                open = "2000-01-01", close = NA, poc = "1",
                method = "INSTRUMENTAL - ULTRA VIOLET ABSORPTION", state = "28",
                county_name = "Hinds", lat = "32.3", lon = "-90.2") {
  data.frame(
    state_code = state, county_code = county, site_number = site, parameter_code = param,
    poc = poc, open_date = open, close_date = close, measurement_scale = "NEIGHBORHOOD",
    monitoring_objective = "POPULATION EXPOSURE", last_method_description = method,
    naaqs_primary_monitor = primary, monitor_type = type, networks = NA,
    monitoring_agency = "Test Agency", latitude = lat, longitude = lon, elevation = "90",
    local_site_name = name, address = "1 Test St", county_name = county_name,
    city_name = "Testville", cbsa_name = "Test CBSA", tribe_name = NA,
    stringsAsFactors = FALSE
  )
}
BAD_NAME   <- "<img src=x onerror=alert(document.domain)>Refinery"
BAD_METHOD <- "<b>UV</b> Photometric Analyzer"
MOCK_MONITORS <- rbind(
  mon("049", "0020", "Jackson NCore", "44201", primary = "Y", open = "2005-01-01"),
  mon("049", "0020", "Jackson NCore", "88101", primary = "Y", open = "1999-01-01",
      method = "R & P Model 2025 PM-2.5 Sequential Air Sampler w/VSCC - Gravimetric"),
  mon("049", "0020", "Jackson NCore", "86101", open = "2020-01-01",   # the one true phantom
      method = "Teledyne T640X at 16.67 LPM - Broadband spectroscopy"),
  mon("033", "0002", "Hernando", "44201", open = "1998-01-01", method = BAD_METHOD),
  mon("081", "0005", "Tupelo SPM ozone", "44201", type = "SPM", open = "2015-01-01"),
  mon("059", "0006", BAD_NAME, "44201", type = "INDUSTRIAL", open = "1960-01-01"),
  mon("047", "0008", "Gulfport SPM", "88101", type = "SPM", open = "2019-01-01",
      method = "Met One BAM-1022 Mass Monitor w/ VSCC - Beta Attenuation"),
  mon("059", "0007", "Pascagoula primary SPM", "88101", type = "SPM", primary = "Y",
      open = "2016-01-01", method = "Met One BAM-1022 Mass Monitor w/ VSCC - Beta Attenuation"),
  mon("075", "0003", "Meridian (closed)", "42401", open = "1980-01-01", close = "1995-12-31",
      method = "INSTRUMENTAL - PULSED FLUORESCENT")
)
MOCK_MONITORS_AL <- mon("073", "0023", "Birmingham", "44201", state = "01", county_name = "Jefferson")
N_COUNTIES <- c("28" = 82, "01" = 67)

MOCK <- new.env()
reset_mock <- function() {
  MOCK$fail_monitors <- character(0)   # parameter codes whose monitors call errors
  MOCK$fail_annual   <- character(0)   # parameter codes whose annualData call errors
  MOCK$calls <- list(monitors = 0, annual = 0, counties = 0)
  MOCK$notes <- character(0)           # every notification shown to the visitor
}
reset_mock()
LOG <- character(0)                    # every message() written to the log
ALL_NOTES <- character(0)              # notifications across all scenarios

aqs_error <- function(service) {
  # Same content as RAQSAPI_error_msg(): the request URL, with email and key
  stop(errorCondition(paste0(
    "RAQSAPI experienced an error while processing the following url: https://aqs.epa.gov/data/api/",
    service, "/byState?email=", FAKE_EMAIL, "&key=", FAKE_KEY, " with status_code: 429"),
    class = c("httr2_http_429", "httr2_http", "httr2_error")))
}

# Mocks live in the global environment, which app.R's server closures search
# before the attached packages
aqs_monitors_by_state <- function(stateFIPS, parameter, bdate, edate, ...) {
  MOCK$calls$monitors <- MOCK$calls$monitors + 1
  if (parameter %in% MOCK$fail_monitors) aqs_error("monitors")
  src <- if (stateFIPS == "28") MOCK_MONITORS else MOCK_MONITORS_AL
  src[src$parameter_code == parameter, , drop = FALSE]
}
aqs_annualsummary_by_state <- function(parameter, bdate, edate, stateFIPS, ...) {
  MOCK$calls$annual <- MOCK$calls$annual + 1
  if (parameter %in% MOCK$fail_annual) aqs_error("annualData")
  src <- if (stateFIPS == "28") MOCK_MONITORS else MOCK_MONITORS_AL
  rep <- src[src$parameter_code == parameter & is.na(src$close_date) &
               !(src$parameter_code == "86101"), , drop = FALSE]
  data.frame(county_code = rep$county_code, site_number = rep$site_number,
             parameter_code = rep$parameter_code, poc = rep$poc,
             observation_count = rep(300, nrow(rep)))
}
aqs_counties_by_state <- function(stateFIPS, ...) {
  MOCK$calls$counties <- MOCK$calls$counties + 1
  data.frame(county_code = sprintf("%03d", seq_len(N_COUNTIES[[stateFIPS]])))
}
get_estimates <- function(...) data.frame(variable = "POPESTIMATE", value = 2939690)
showNotification <- function(ui, action = NULL, duration = 5, closeButton = TRUE,
                             id = NULL, type = "default", session = NULL) {
  note <- paste0("[", type, "] ", paste(as.character(ui), collapse = " "))
  MOCK$notes <- c(MOCK$notes, note); ALL_NOTES <<- c(ALL_NOTES, note)
  invisible(id %||% "note")
}
# MockShinySession has no progress, custom-message or data-object channel, so
# stub the UI-only calls that need them
withProgress <- function(expr, ...) expr
incProgress  <- function(...) invisible()
updateSelectizeInput <- function(...) invisible()
leafletProxy <- function(...) structure(list(), class = "mock_proxy")
for (.f in c("clearMarkers", "clearMarkerClusters", "removeControl", "addLegend", "fitBounds"))
  assign(.f, function(map, ...) map)
POPUPS <- character(0)                 # every map popup the server built
addCircleMarkers <- function(map, ..., popup = NULL) { POPUPS <<- c(POPUPS, seen(popup)); map }

# --- Harness helpers -----------------------------------------------------------
# Copy app.R to a fresh temp dir (optionally with cache files and aqs.env),
# source it, and return the app object. Fresh globals each time.
load_app <- function(cache = list(), env_file = TRUE) {
  dir <- tempfile("amh-"); dir.create(file.path(dir, "cache"), recursive = TRUE)
  file.copy(file.path(app_src, "app.R"), dir)
  if (env_file) writeLines(c(sprintf('AQS_EMAIL="%s"', FAKE_EMAIL), sprintf('AQS_KEY="%s"', FAKE_KEY)),
                           file.path(dir, "aqs.env"))
  for (f in names(cache)) saveRDS(cache[[f]], file.path(dir, "cache", f))
  setwd(dir)
  Sys.unsetenv(c("AQS_EMAIL", "AQS_KEY"))
  app <- withCallingHandlers(source("app.R", local = FALSE)$value,
                             message = function(m) { LOG <<- c(LOG, conditionMessage(m)); invokeRestart("muffleMessage") })
  list(dir = dir, app = app)
}
# A pre-seeded cache in the app's format (all character, grouped by pollutant)
as_cache <- function(df) {
  lut <- c("44201" = "Ozone", "88101" = "PM2.5", "86101" = "PM10-2.5 (NCore)", "42401" = "SO2")
  df$pollutant_type <- unname(lut[df$parameter_code])
  df[] <- lapply(df, as.character)
  df
}
SEEDED <- list("state_28_v3.rds" = as_cache(MOCK_MONITORS))

POLLS <- c("Ozone", "PM2.5", "PM10", "SO2", "NO2", "CO", "PM10-2.5 (NCore)")
start_inputs <- function(session, state = "28")
  session$setInputs(state = state, pollutants = POLLS, only_active = TRUE, reg_only = FALSE,
                    programs = "All", year_range = c(1950, THIS_YEAR))

# Everything a visitor could see, for the leak checks
SEEN <- character(0)
seen <- function(x) { SEEN <<- c(SEEN, paste(as.character(x), collapse = " ")); x }
# An output as the text the browser receives (renderUI outputs are lists)
out <- function(output, name) {
  v <- get("session", parent.frame())$getOutput(name)
  seen(if (is.list(v) && !is.null(v$html)) as.character(v$html) else paste(as.character(unlist(v)), collapse = " "))
}
quietly <- function(expr) withCallingHandlers(expr, message = function(m) {
  LOG <<- c(LOG, conditionMessage(m)); invokeRestart("muffleMessage") })
cache_rows <- function(dir, code = NULL) {
  p <- file.path(dir, "cache", "state_28_v3.rds")
  if (!file.exists(p)) return(NA)
  d <- readRDS(p); if (is.null(code)) nrow(d) else sum(d$parameter_code == code)
}

cat("Testing app.R in", app_src, "\n\n")

# --- 1. Credentials load from aqs.env and only their presence is logged ---------
scenario("creds: aqs.env loaded, startup line TRUE", {
  LOG <- character(0); a <- load_app(SEEDED, env_file = TRUE)
  check("creds: aqs.env loaded, startup line TRUE",
        any(trimws(LOG) == "[startup] AQS credentials present: TRUE") && identical(getOption("aqs_key"), FAKE_KEY))
})
scenario("creds: no aqs.env, startup line FALSE", {
  LOG <- character(0); a <- load_app(SEEDED, env_file = FALSE)
  check("creds: no aqs.env, startup line FALSE", any(trimws(LOG) == "[startup] AQS credentials present: FALSE"))
})

# --- 2. #1 Audit: a failed AQS call must not flag or save anything --------------
scenario("#1 audit, all answered: 1 true phantom saved", {
  reset_mock(); a <- load_app(SEEDED)
  testServer(a$app, quietly({
    start_inputs(session); session$setInputs(audit = 1)
    f <- data_store$audit_flags
    saved <- Sys.glob(file.path(a$dir, "cache", "audit_28_*.rds"))
    check("#1 audit, all answered: 1 true phantom saved",
          !is.null(f) && nrow(f) == 1 && f$parameter_code == "86101" && length(saved) == 1,
          paste("flags:", if (is.null(f)) "NULL" else nrow(f)))
    out(output, "audit_summary"); out(output, "audit_table")
    check("#7 audit summary shows its run time", grepl("Audit run 20", out(output, "audit_summary")))
  }))
})
scenario("#1 audit, ozone call fails: nothing flagged/saved", {
  reset_mock(); MOCK$fail_annual <- "44201"; a <- load_app(SEEDED)
  testServer(a$app, quietly({
    start_inputs(session); session$setInputs(audit = 1)
    f <- data_store$audit_flags
    saved <- Sys.glob(file.path(a$dir, "cache", "audit_28_*.rds"))
    check("#1 audit, ozone call fails: nothing flagged/saved",
          is.null(f) && length(saved) == 0 && any(grepl("Audit incomplete.*44201", MOCK$notes)),
          paste("flags:", if (is.null(f)) "NULL" else nrow(f), "| saved:", length(saved)))
  }))
})
scenario("#7 legacy audit file: run date not recorded", {
  reset_mock()
  legacy <- as_cache(MOCK_MONITORS[3, ]); legacy$audit_year <- THIS_YEAR - 1
  a <- load_app(c(SEEDED, setNames(list(legacy), paste0("audit_28_", THIS_YEAR - 1, ".rds"))))
  testServer(a$app, quietly({
    start_inputs(session)
    check("#7 legacy audit file: run date not recorded", grepl("run date not recorded", out(output, "audit_summary")))
  }))
})

# --- 3. #3 Download: a partial answer is never cached ---------------------------
scenario("#3 download, PM2.5 fails: not cached, visitor warned", {
  reset_mock(); MOCK$fail_monitors <- "88101"; a <- load_app()
  testServer(a$app, quietly({
    start_inputs(session)
    check("#3 download, PM2.5 fails: not cached, visitor warned",
          is.na(cache_rows(a$dir)) && any(grepl("88101", MOCK$notes)) && nrow(data_store$raw) > 0,
          paste("cache rows:", cache_rows(a$dir), "| notes:", length(MOCK$notes)))
  }))
})
scenario("#3 download, all answered: full cache written", {
  reset_mock(); a <- load_app()
  testServer(a$app, quietly({
    start_inputs(session)
    check("#3 download, all answered: full cache written",
          identical(cache_rows(a$dir), nrow(MOCK_MONITORS)), paste("cache rows:", cache_rows(a$dir)))
    check("#7 fresh download shows its time", grepl("AQS metadata downloaded 20", out(output, "data_age")))
  }))
})
scenario("#3 Sync with PM2.5 failing keeps the saved cache", {
  reset_mock(); a <- load_app(SEEDED)
  testServer(a$app, quietly({
    start_inputs(session)
    MOCK$fail_monitors <- "88101"; session$setInputs(refresh = 1)
    check("#3 Sync with PM2.5 failing keeps the saved cache",
          identical(cache_rows(a$dir, "88101"), 3L) && sum(data_store$raw$parameter_code == "88101") == 3 &&
            any(grepl("Sync incomplete.*88101", MOCK$notes)),
          paste("88101 rows on disk:", cache_rows(a$dir, "88101")))
  }))
})
scenario("#3 Sync, all answered: cache replaced", {
  reset_mock(); a <- load_app(SEEDED)
  testServer(a$app, quietly({
    start_inputs(session); calls0 <- MOCK$calls$monitors
    session$setInputs(refresh = 1)
    d <- readRDS(file.path(a$dir, "cache", "state_28_v3.rds"))
    check("#3 Sync, all answered: cache replaced",
          MOCK$calls$monitors > calls0 && nrow(d) == nrow(MOCK_MONITORS) && !is.null(attr(d, "fetched")))
  }))
})
scenario("#7 legacy cache: date not recorded", {
  reset_mock(); a <- load_app(SEEDED)
  testServer(a$app, quietly({
    start_inputs(session)
    check("#7 legacy cache: date not recorded", grepl("date not recorded", out(output, "data_age")))
  }))
})

# --- 4. #2 Regulatory filter, #4 escaping, #9 labels, all outputs render --------
scenario("#2/#4/#9 main view", {
  reset_mock(); a <- load_app(SEEDED)
  testServer(a$app, quietly({
    start_inputs(session)
    for (o in c("active_coverage", "site_records", "county_coverage", "map", "trend_plot",
                "pollutant_plot", "table", "audit_summary")) out(output, o)
    check("all outputs render (8)", TRUE)

    # CARTO raster basemaps now need an API key (tiles carry an "API KEY
    # REQUIRED" watermark), so every basemap must be a keyless Esri service
    mp <- out(output, "map")
    canvas <- c("World_Light_Gray_Base", "World_Light_Gray_Reference",
                "World_Dark_Gray_Base", "World_Dark_Gray_Reference")
    check("map basemaps are keyless Esri tiles, no CARTO",
          all(sapply(canvas, grepl, x = mp, fixed = TRUE)) &&
            grepl("Esri.WorldImagery", mp, fixed = TRUE) && grepl("Esri.WorldGrayCanvas", mp, fixed = TRUE) &&
            !grepl("carto", mp, ignore.case = TRUE) && grepl('"maxNativeZoom":16', mp, fixed = TRUE))

    h <- processed_history()
    gp <- h$Active_Pollutants[h$local_site_name == "Gulfport SPM"]
    pp <- h$Active_Pollutants[h$local_site_name == "Pascagoula primary SPM"]
    check("#2 non-primary SPM tagged Non-Reg", grepl("Non-Reg", gp), gp)
    check("#2 NAAQS-primary SPM not tagged Non-Reg", !grepl("Non-Reg", pp))
    check("#2 chart counts non-primary SPM as non-regulatory",
          grepl("Non-Regulatory (88502/TEOM)", out(output, "pollutant_plot"), fixed = TRUE))

    session$setInputs(reg_only = TRUE)
    kept <- unique(filtered_raw()$local_site_name)
    check("#2 'Regulatory Only' drops non-primary SPMs and INDUSTRIAL",
          !any(c("Gulfport SPM", "Tupelo SPM ozone", BAD_NAME) %in% kept) &&
            all(c("Pascagoula primary SPM", "Jackson NCore", "Hernando") %in% kept),
          paste("kept:", paste(sort(kept), collapse = "; ")))
    session$setInputs(reg_only = FALSE)

    sr <- out(output, "site_records")
    check("#4 record-holder box escapes site name", !grepl("<img", sr) && grepl("&lt;img", sr))
    hp <- h$Active_Pollutants[h$local_site_name == "Hernando"]
    tb <- out(output, "table")
    check("#4 pollutant tag escapes AQS method (popup + table)",
          !grepl("<b>UV", hp, fixed = TRUE) && grepl("&lt;b&gt;UV", hp, fixed = TRUE) &&
            !grepl("<b>UV", tb, fixed = TRUE) && !grepl("\\u003cb>UV", tb, fixed = TRUE))
    pop <- POPUPS[grepl("Hernando|Refinery", POPUPS)]
    check("#4 map popups escape site name and method",
          length(pop) >= 2 && !any(grepl("<img|<b>UV", pop)) && any(grepl("&lt;b&gt;UV", pop, fixed = TRUE)),
          paste(length(pop), "popups"))
    check("#4 R & P method still reads correctly", any(grepl("R &amp; P Model 2025", h$Active_Pollutants, fixed = TRUE)))

    ac <- out(output, "active_coverage"); tp <- out(output, "trend_plot"); pp <- out(output, "pollutant_plot")
    check("#9 coverage tile says sites, not monitors",
          grepl("Active Sites / 100k", ac) && !grepl("Monitors / 100k", ac))
    check("#9 chart labels say sites",
          grepl("Active Sites per Year", tp) && grepl("Active Sites", pp) && !grepl("Active Monitors", paste(tp, pp)))
    cc <- out(output, "county_coverage")
    check("county coverage uses the AQS count (82)", grepl("/82 Counties", cc), cc)
  }))
})

# --- 5. #5 Year range sent by the client is clamped ------------------------------
scenario("#5 year clamp", {
  reset_mock(); a <- load_app(SEEDED)
  testServer(a$app, quietly({
    start_inputs(session)
    # outputs re-render during the flush that setInputs() triggers, so time that
    t <- system.time({ session$setInputs(year_range = c(-3000, THIS_YEAR))
                       tp <- out(output, "trend_plot"); pp <- out(output, "pollutant_plot") })[["elapsed"]]
    n <- length(fromJSON(tp, simplifyVector = FALSE)$x$data[[1]]$x)
    check("#5 [-3000, now]: renders fast, only 1950-now",
          t < 10 && n == THIS_YEAR - 1950 + 1, sprintf("%.1f s, %d years", t, n))
    session$setInputs(year_range = c(1990, 2010))
    n2 <- length(fromJSON(out(output, "trend_plot"), simplifyVector = FALSE)$x$data[[1]]$x)
    check("#5 normal range 1990-2010 still renders 21 years", n2 == 21, paste(n2, "years"))
  }))
})

# --- 6. #6 County count is fetched once per state per process --------------------
scenario("#6 county cache", {
  reset_mock(); a <- load_app(SEEDED)
  for (i in 1:5) testServer(a$app, quietly(start_inputs(session)))
  check("#6 five cached visits: 1 county call", MOCK$calls$counties == 1, paste(MOCK$calls$counties, "calls"))
  reset_mock(); a <- load_app(SEEDED)
  testServer(a$app, quietly({
    start_inputs(session)
    for (s in c("01", "28", "01", "28")) session$setInputs(state = s)
  }))
  check("#6 MS/AL toggled 5 times: 2 county calls", MOCK$calls$counties == 2, paste(MOCK$calls$counties, "calls"))
})

# --- 7. #8 About footer renders as a real card footer ---------------------------
scenario("#8 footer", {
  a <- load_app(SEEDED)
  html <- seen(htmltools::renderTags(ui)$html)
  check("#8 About footer is a card-footer, not an attribute",
        grepl("card-footer", html) && !grepl('footer="list(', html, fixed = TRUE) &&
          grepl("Developed &amp; Maintained by", html))
  check("#2 About page cites 40 CFR 58.20(c)", grepl("58.20(c)", html, fixed = TRUE) && !grepl("58.20(e)", html, fixed = TRUE))
})

# --- 8. No credential reaches a visitor, the log or a cache file -----------------
setwd(home_wd)
leaks <- function(x) sum(grepl(FAKE_KEY, x, fixed = TRUE) | grepl(FAKE_EMAIL, x, fixed = TRUE))
cache_text <- unlist(lapply(Sys.glob(file.path(tempdir(), "amh-*", "cache", "*.rds")),
                            function(f) paste(capture.output(str(readRDS(f), list.len = 999, vec.len = 999)), collapse = " ")))
check("leak control: collector finds a planted key", leaks(c("x", paste("planted", FAKE_KEY))) == 1)
check("no key/email in any output or page", leaks(SEEN) == 0, paste(length(SEEN), "items"))
check("no key/email in any notification", leaks(ALL_NOTES) == 0, paste(length(ALL_NOTES), "shown"))
check("no key/email in the log", leaks(LOG) == 0, paste(length(LOG), "lines"))
check("no key/email in any cache file", leaks(cache_text) == 0, paste(length(cache_text), "files"))

cat(sprintf("\n%d passed, %d failed\n", sum(results$ok), sum(!results$ok)))
quit(status = min(sum(!results$ok), 100), save = "no")
