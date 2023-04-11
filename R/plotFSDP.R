#' @title plotFSDP
#' @description Creates all FSDP plots
#'
#' @export
#'
#' @param outputfolder output folder
#' @param reg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script. NULL will automatically detect the most recent version.
#' @param iso rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script. NULL will automatically detect the most recent version.
#' @param grid rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script. NULL will automatically detect the most recent version.#' @details uses the most recent vXX_reg/grid/iso.rds files in the "output" folder by default
#' @param val rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script. NULL will automatically detect the most recent version.#' @return plots in "output" folder
#' @param reg2iso rds file or data.frame with Mapping, produced with FSDP_collect.R output script. NULL will automatically detect the most recent version.#' @author Florian Humpenoeder
#' @param rev revision (e.g. "v28"). NULL will automatically detect the most recent version.
#' @importFrom utils glob2rx

plotFSDP <- function(outputfolder = "output", reg = NULL, iso = NULL, grid = NULL, val = NULL, reg2iso = NULL, rev = NULL) {

  if(is.null(reg)) {
    a <- list.files(outputfolder,pattern = glob2rx("v*FSDP_reg.rds"))
    a <- a[order(a,decreasing = TRUE)][1]
    reg <- file.path(outputfolder,a)
  }

  if(is.null(iso)) {
    a <- list.files(outputfolder,pattern = glob2rx("v*FSDP_iso.rds"))
    a <- a[order(a,decreasing = TRUE)][1]
    iso <- file.path(outputfolder,a)
  }

  if(is.null(grid)) {
    a <- list.files(outputfolder,pattern = glob2rx("v*FSDP_grid.rds"))
    a <- a[order(a,decreasing = TRUE)][1]
    grid <- file.path(outputfolder,a)
  }

  if(is.null(val)) {
    a <- list.files(outputfolder,pattern = glob2rx("v*FSDP_validation.rds"))
    a <- a[order(a,decreasing = TRUE)][1]
    val <- file.path(outputfolder,a)
  }

  if(is.null(reg2iso)) {
    a <- list.files(outputfolder,pattern = glob2rx("reg2iso.rds"))
    reg2iso <- file.path(outputfolder,a[order(a,decreasing = TRUE)][1])
  }

  #get revision
  if(is.null(rev)) {
    rev <- c(basename(reg),basename(iso),basename(grid),basename(val))
    rev <- unlist(lapply(strsplit(rev,"_"),function(x) x[[1]]))
    if (all(rev[1] == rev)) rev <- rev[1] else stop("No complete version found")
  }

  message("Reading RDS files ...")
  if (!is.data.frame(reg)) reg <- readRDS(reg)
  if (!is.data.frame(iso)) iso <- readRDS(iso)
  if (!is.data.frame(grid)) grid <- readRDS(grid)
  if (!is.data.frame(val)) val <- readRDS(val)
  if (!is.data.frame(reg2iso)) reg2iso <- readRDS(reg2iso)

  message("Plotting figures ...")
  try(heatmapFSDP(reg, tableType = 1,    file = file.path(outputfolder, paste0(rev, "_FSDP_heatmap1.png"))))
  #try(heatmapFSDP(reg, tableType = 2, file = file.path(outputfolder, paste0(rev, "_FSDP_heatmap2.png"))))
  try(heatmapFSDP(reg, tableType = 3,    file = file.path(outputfolder, paste0(rev, "_FSDP_heatmap3.png")), height = 4.5))
  try(bundlesFSDP(reg, file = file.path(outputfolder, paste0(rev, "_FSDP_bundle.png"))))
  try(spatialMapsFSDP(reg, iso, grid, reg2iso, file = file.path(outputfolder, paste0(rev, "_FSDP_spatialMaps.png"))))
  try(spatialMapsAllFSMDiffmap(reg, iso, grid, reg2iso, file = file.path(outputfolder, paste0(rev, "_FSDP_spatialMaps_diffmap.png"))))
  try(SupplPlotsFSDP(repReg = reg, scenarioType = "manuscript", outFolder = file.path(outputfolder)))
  #SupplPlotsCropShr(gdx = gdx, file = file.path(outputfolder, paste0(rev, "_FSDP_supplPlotCropShr.png")))
  try(validationFSDP(repReg = reg, val = val, regionSel = "aggregate", folder = file.path(outputfolder), scens = "BAU_FSEC"))
  try(lineplotFSDP(reg,val,file = file.path(outputfolder, paste0(rev, "_FSDP_lineplots.png"))))
  try(dashboardFSDP(repReg = reg, repIso = iso, repGrid = grid, outputDir = file.path(outputfolder), file = paste0(rev, "_FSDP_dashboard.html")))
  message("Finished")

  }
