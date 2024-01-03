#' run_shiny
#' @param ... other arguments passed to `[shiny::runApp]`
#' @export
run_shiny <- function(...) {
    appDir <- system.file("shiny", package = "clustersimulation")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `clustersimulation`.", call. = FALSE)
    }
    shiny::runApp(appDir, ...)
}