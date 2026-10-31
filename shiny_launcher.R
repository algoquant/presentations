#!/usr/bin/env Rscript

# Start before opening the presentation:
# Rscript /Users/jerzy/Develop/Presentations/shiny_launcher.R
# Double-click a linked slide image to start its app. Ctrl-C stops the launcher
# and the app processes it started. Existing servers are never terminated.
# Dependencies: install.packages(c("httpuv", "callr", "httr2", "shiny"))

create_launcher <- function(directory, launcher_port = 3870L) {
  apps <- list(
    ema = list(file = "app_ema_plot2.R", port = 3838L),
    crossover = list(file = "app_ema_crossover.R", port = 3839L),
    performance = list(file = "strat_ema_crossover.R", port = 3840L),
    bollinger = list(file = "app_bollinger.R", port = 3841L),
    voltiming = list(file = "strat_voltiming.R", port = 3842L),
    reverting = list(file = "strat_bollinger_price.R", port = 3843L),
    momentum = list(file = "strat_moment_etf.R", port = 3844L),
    regime = list(file = "strat_regime_vol.R", port = 3845L),
    ladder = list(file = "app_prices_annotated.R", port = 3846L)
  )
  processes <- new.env(parent = emptyenv())
  logs <- new.env(parent = emptyenv())
  started_at <- new.env(parent = emptyenv())
  ready <- function(url) {
    tryCatch({
      request <- httr2::req_timeout(httr2::request(url), 0.5)
      response <- httr2::req_perform(request)
      httr2::resp_status(response) == 200L
    }, error = function(error) FALSE)
  }
  respond <- function(status, body, refresh = FALSE) {
    headers <- list("Content-Type" = "text/html; charset=utf-8",
      "Cache-Control" = "no-store", "X-Content-Type-Options" = "nosniff")
    if (refresh) headers$Refresh <- "1"
    list(status = as.integer(status), headers = headers,
      body = paste0("<!doctype html><html><head><title>Shiny Launcher</title></head>",
        "<body><p>", body, "</p></body></html>"))
  }
  handler <- function(request) {
    if (!identical(request$HTTP_HOST, paste0("127.0.0.1:", launcher_port))) {
      return(respond(403, "Invalid host."))
    }
    if (!identical(request$REQUEST_METHOD, "GET")) {
      return(respond(405, "Only GET requests are supported."))
    }
    if (identical(request$PATH_INFO, "/")) {
      return(respond(200, "Launcher running. Double-click a linked presentation image."))
    }
    app_name <- sub("^/launch/", "", request$PATH_INFO)
    if (!startsWith(request$PATH_INFO, "/launch/") || !app_name %in% names(apps)) {
      return(respond(404, "Unknown app."))
    }
    app <- apps[[app_name]]
    url <- paste0("http://127.0.0.1:", app$port, "/")
    if (exists(app_name, envir = processes, inherits = FALSE)) {
      process <- processes[[app_name]]
      if (!process$is_alive()) {
        message("App failed or stopped: ", app_name, "; log: ", logs[[app_name]])
        return(respond(500, "The app stopped. Check the launcher terminal for its log path. Restart the launcher to retry."))
      }
    }
    if (ready(url)) {
      return(list(status = 302L, headers = list(Location = url,
        "Cache-Control" = "no-store"), body = ""))
    }
    if (!exists(app_name, envir = processes, inherits = FALSE)) {
      app_file <- file.path(directory, app$file)
      if (!file.exists(app_file)) return(respond(404, "The app file does not exist."))
      logs[[app_name]] <- tempfile(paste0("shiny_", app_name, "_"), fileext = ".log")
      processes[[app_name]] <- callr::r_bg(function(app_file, port) {
        shiny::runApp(app_file, host = "127.0.0.1", port = port, launch.browser = FALSE)
      }, args = list(app_file = app_file, port = app$port), wd = directory,
        stdout = logs[[app_name]], stderr = logs[[app_name]], supervise = TRUE)
      message("Starting ", app_name, " at ", url, "; log: ", logs[[app_name]])
      started_at[[app_name]] <- Sys.time()
    }
    if (as.numeric(difftime(Sys.time(), started_at[[app_name]],
        units = "secs")) > 60) {
      return(respond(504, "App startup exceeded 60 seconds. Check the launcher terminal for its log path."))
    }
    respond(200, "Starting Shiny app...", refresh = TRUE)
  }
  cleanup <- function() {
    for (app_name in ls(processes)) {
      process <- processes[[app_name]]
      if (process$is_alive()) process$kill()
    }
  }
  list(call = handler, cleanup = cleanup)
}

main <- function() {
  for (package in c("httpuv", "callr", "httr2", "shiny")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Install R package: ", package)
  }
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  directory <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
  launcher <- create_launcher(directory)
  on.exit(launcher$cleanup(), add = TRUE)
  server <- httpuv::startServer("127.0.0.1", 3870L, list(call = launcher$call))
  on.exit(httpuv::stopServer(server), add = TRUE)
  message("Shiny launcher: http://127.0.0.1:3870/ (Ctrl-C to stop)")
  tryCatch(while (TRUE) httpuv::service(100), interrupt = function(error) {
    message("Stopping launcher and its Shiny apps.")
  })
}

if (sys.nframe() == 0L) main()