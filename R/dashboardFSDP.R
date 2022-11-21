#' @title dashboardFSDP
#' @description FDSP dashboard
#'
#' @export
#'
#' @param repReg rds file or data.frame with regional data from all MAgPIE runs,
#' produced with FSDP_collect.R output script.
#' @param repIso rds file or data.frame with country-level data from all MAgPIE runs,
#' produced with FSDP_collect.R output script.
#' @param repGrid rds file or data.frame with gridded data from all MAgPIE runs,
#' produced with FSDP_collect.R output script.
#' @param regionSel Region that should be plotted
#' @param outputDir output directory
#' @param file file name
#' @details FDSP dashboard
#' @return html file
#' @author Florian Humpenoeder
#' @import DT data.table knitr htmlwidgets rmarkdown

dashboardFSDP <- function(repReg, repIso, repGrid, regionSel = "GLO", outputDir = "", file = "dashboardFSDP.html") {

  if (!is.data.frame(repReg)) repReg <- readRDS(repReg)
  if (!is.data.frame(repIso)) repIso <- readRDS(repIso)
  if (!is.data.frame(repGrid)) repGrid <- readRDS(repGrid)

  rmarkdown::render(
    input = system.file(package = "m4fsdp", "rmd", "FSDP.Rmd"),
    knit_root_dir = getwd(),
    output_dir = file.path(getwd(), outputDir),
    output_file = file,
    encoding     = "UTF-8"
  )
}
