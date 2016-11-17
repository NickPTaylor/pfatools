#' Launch \code{\link{at_intervals}} shiny application.
#'
#' This is a wrapper function for launching a shiny application example of the
#' \code{\link{at_intervals}} function.
#'
#' @param ... Arguments to \code{\link[shiny]{runApp}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' launch_application()
#' }
#'
app_at_intervals <- function(...) {
    # nocov start
    rmarkdown::run(file = system.file("app.Rmd", "at-intervals-demo",
                                      package = "pfatools"),
                   dir = system.file("at-intervals-demo", package = "pfatools"),
                   ...)
}   # nocov end
