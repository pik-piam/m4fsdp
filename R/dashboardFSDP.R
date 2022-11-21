#' @title dashboardFSDP
#' @description FDSP dashboard
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param regionSel Region that should be plotted
#' @param outputDir output directory
#' @details FDSP dashboard
#' @return html file
#' @author Florian Humpenoeder
#' @import DT data.table knitr htmlwidgets rmarkdown

dashboardFSDP <- function(repReg, regionSel = "GLO", outputDir = "") {

  if(!is.data.frame(repReg)) repReg <- readRDS(repReg)

  rmarkdown::render(
    input = paste0(system.file(package = "m4fsdp"), "/rmd/FSDP.Rmd"),
    knit_root_dir = getwd(),
    output_dir = file.path(getwd(),outputDir),
    output_file = "dashboardFSDP.html",
    encoding     = 'UTF-8'
  )
}
