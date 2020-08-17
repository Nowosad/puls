#' @export
run_watchdog = function() {
  appDir = system.file("shiny", "augeas", package = "puls")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `puls`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
