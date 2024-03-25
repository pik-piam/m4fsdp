globalVariables(c("gdx", "suppFolder", "Data1", "Value", "Year", "Region", "Crop", "TotalArea", "CropShare", "BarPos"))

#' @title SupplPlotsLandConsv
#' @description Creates land conservation map
#'
#' @export
#'
#' @param outFolder output folder
#' @param file Name of file output
#' @param gdxFolder Run folder
#' @details Produces map of land targeted in the food system measure LandConservation
#' @return Map with cell share included in land conservation measure
#' @author Patrick v. Jeetze
#' @import magclass luplot ggplot2
#' @importFrom scales squish


SupplPlotsLandConsv <- function(outFolder, file = "suppPlotsLandConsv.png", gdxFolder = NULL) {
  # Chose run folder to grab file
  if (is.null(gdxFolder)) {
    runFolder <- file.path(list.dirs(outFolder, full.names = FALSE, recursive = FALSE))
    x <- unlist(lapply(strsplit(basename(runFolder), "_"), function(x) x[3]))
    ## any output folder could be chosen, fall back to BAU for simplicity
    gdxFolder <- runFolder[which(x %in% "BAU")]
  }

  # Get revision number
  x <- unlist(lapply(strsplit(basename(gdxFolder), "_"), function(x) x[1]))
  if (length(unique(x)) == 1) rev <- unique(x) else stop("version prefix is not identical. Check run outputs")

  suppFolder <- file.path(outFolder, "supplPlots/")
  if (!dir.exists(suppFolder)) {
    dir.create(suppFolder)
  }

  consvPrio <- c(
    file.path(outFolder, gdxFolder, "consv_prio_areas_0.5.mz"),
    "input/consv_prio_areas_0.5.mz",
    "modules/22_land_conservation/input/consv_prio_areas_0.5.mz",
    "../input/consv_prio_areas_0.5.mz"
  )
  consvPrio <- suppressWarnings(consvPrio[min(which(file.exists(consvPrio)))])

  consvVar <- "BH_IFL"
  consvNames <- "Biodiversity Hotspots & Intact Forest Landscapes"
  # consvVar <- "30by30"
  # consvNames <- "30x30 land conservation"
  names(consvNames) <- consvVar

  consvPrio <- read.magpie(consvPrio)[, , consvVar]

  land <- read.magpie(file.path(outFolder, gdxFolder, "cell.land_0.5.mz"))
  consvPrio_shr <- consvPrio / setYears(dimSums(land[, "y1995", ], dim = 3), NULL)
  consvPrio_shr <- madrat::toolConditionalReplace(consvPrio_shr, conditions = ">1", replaceby = 1)

  data <- dimSums(consvPrio_shr, dim = 3.2)
  data <- madrat::toolConditionalReplace(data, conditions = "is.na()", replaceby = 0)

  p1 <- luplot::plotmap2(data,
                         sea = FALSE, legend_height = 1.8, title = "",
                         land_colour = "white", facet_grid = ".~Data1", facet_style = "paper"
  ) +
    theme(plot.margin = margin(c(0, 0, 5, 0))) +
    scale_fill_gradient2(
      low = "grey95", mid = "#6cc08b",
      high = "#074050", midpoint = 0.5,
      name = "Cell land share",
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1),
      labels = seq(0, 1, 0.2),
      oob = squish
    ) +
    facet_wrap(. ~ Data1, ncol = 1, labeller = labeller(Data1 = consvNames)) +
    theme(plot.title = element_text(size = 14))

  if (!is.null(file)) {
    ggsave(filename = paste0(suppFolder, rev, "_", file), p1, width = 24, height = 14, units = "cm", scale = 1)
    ggsave(filename = paste0(suppFolder, rev, "_", sub("png", "pdf", file)), p1, width = 24, height = 14, units = "cm", scale = 1)
  }
}
