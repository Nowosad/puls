#' @export
run_watchdog = function(data_folder, result_folder, ...) {
  appDir = system.file("shiny", "augeas", package = "puls")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `puls`.", call. = FALSE)
  }
  source(paste0(appDir, "/header.R"), local = TRUE)
  # https://stackoverflow.com/questions/44999615/passing-parameters-into-shiny-server/48831832#48831832
  ui = NULL # avoid NOTE about undefined globals
  server = NULL # avoid NOTE about undefined globals
  source(paste0(appDir, "/server.R"), local = TRUE)
  server_env = environment(server)

  brody_files = dir(paste0(data_folder, "/Brody"), pattern = ".dat", full.names = TRUE)
  kusowo_files = dir(paste0(data_folder, "/Kusowo"), pattern = ".dat", full.names = TRUE)
  rzecin_files = dir(paste0(data_folder, "/Rzecin"), pattern = ".dat", full.names = TRUE)

  server_env$stacje = list(Brody = brody_files,
                Kusowo = kusowo_files,
                Rzecin = rzecin_files)

  server_env$brody_error = fst::read_fst(paste0(result_folder, "/brody_error.fst"))
  server_env$kusowo_error = fst::read_fst(paste0(result_folder, "/kusowo_error.fst"))
  server_env$rzecin_error = fst::read_fst(paste0(result_folder, "/rzecin_error.fst"))

  source(paste0(appDir, "/ui.R"), local = TRUE)

  app = shiny::shinyApp(ui, server)
  shiny::runApp(app, display.mode = "normal", ...)
}
