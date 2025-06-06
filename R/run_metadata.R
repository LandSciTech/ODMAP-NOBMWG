run_metadata <- function() {
  appDir <- system.file("app", package = "ODMAPNOBMWG")
  if (appDir == "") {
    stop("Could not find app directory. Try reinstalling the package.")
  }
  shiny::runApp(appDir)
}
