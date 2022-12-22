#' @title Run LESLIE Shiny App
#'
#' @description Runs LESLIE Shiny App
#'
#' @param ... arguments passed to [shiny::runApp]
#'
#' @author Sebastian Kreutzer, Ruprecht-Karl-University of Heidelberg (Germany)
#'
#' @examples
#' \dontrun{
#' app_LESLIE()
#' }
#'
#' @md
#' @export
app_LESLIE <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("[app_LESLIE()] To use the shiny app, you have to install the pacakge 'shiny' first.",
         call. = FALSE)
  }

  shiny::runApp(system.file("shiny/leslie", package = "LESLIE"),  ...)

}
