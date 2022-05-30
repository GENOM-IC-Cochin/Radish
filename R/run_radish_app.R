#' Runs the Radish application
#' Interactively or not, dependending on the context.
#' @param ... parameters passed to runApp and shinyAppDir
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @import ggplot2
#' @import dplyr
#' @import bs4Dash
#' @importFrom BiocGenerics rowSums
#' @importFrom SummarizedExperiment assay
#' @importFrom stats na.omit prcomp relevel setNames
#' @importFrom utils head write.csv
#' @importFrom grDevices colorRampPalette dev.off pdf
#' @export
run_radish_app <- function(...) {
  if(interactive()) {
    runApp(appDir = system.file("app", package = "Radish"))
  } else {
    shinyAppDir(appDir = system.file("app", package = "Radish"), ...)
  }
}
