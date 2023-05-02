globalVariables(c("gdx", "suppFolder", "Data1", "Value", "Year", "Region", "Crop", "TotalArea", "CropShare", "BarPos"))

#' @title SupplPlotsCropShr
#' @description Creates crop share plot
#'
#' @export
#'
#' @param outFolder output folder
#' @param file Name of file output
#' @param panel plot regions as "row" or as "matrix"
#' @param scenarios scenarios used for plotting
#' @param plotyears years for the plot
#' @param combined whether regional and global plots should be combined
#' @details blub
#' @return Crop share on the y-axis and cropland area in each cluster on the x-axis.
#' @author Patrick v. Jeetze, Benjamin Bodirsky
#' @import ggplot2 data.table scales magpiesets magpie4
#' @importFrom stats weighted.mean
#' @importFrom dplyr case_when filter group_by right_join mutate arrange desc select %>%

SupplPlotsCropShr <- function(outFolder, file = NULL, scenarios = c("BAU", "FSDP"),
                              plotyears = c("2020", "2050"), panel = "row", combined = TRUE) {
  runFolder <- file.path(list.dirs(outFolder, full.names = FALSE, recursive = FALSE))

  # Select runs to be displayed
  x <- unlist(lapply(strsplit(basename(runFolder), "_"), function(x) x[3]))
  ## only for BAU and SDP in 2020 and 2050 to save time and storage
  gdxFolder <- runFolder[which(x %in% scenarios)]

  # Get revision number
  x <- unlist(lapply(strsplit(basename(gdxFolder), "_"), function(x) x[1]))
  if (length(unique(x)) == 1) rev <- unique(x) else stop("version prefix is not identical. Check run outputs")

  suppFolder <- file.path(outFolder, "supplPlots/")
  if (!dir.exists(suppFolder)) {
    dir.create(suppFolder)
  }

  .plotCropShr <- function(gdx, file, scenario, year, combined) {
    #-----------------------------
    # Data processing
    # ----------------------------

    # Get total cropland area in each cluster
    cropland <- magpie4::land(gdx, types = "crop", level = "cell")
    cropData <- as.data.frame(cropland)
    cropData <- dplyr::select(cropData, -Data1)
    cropData <- dplyr::rename(cropData, TotalArea = Value)

    # Get area of different crop types in each cluster
    cropArea <- magpie4::croparea(gdx, level = "cell", product_aggr = FALSE)
    getNames(cropArea) <- magpiesets::reportingnames(getNames(cropArea))
    cropArea <- mbind(cropArea, setNames(cropland - dimSums(cropArea, dim = 3), "Fallow"))
    cropArea[cropArea < 0] <- 0

    # Define different crop groups
    cereal <- c("Temperate cereals", "Maize", "Tropical cereals", "Rice")
    legumes <- c("Soybean", "Groundnuts", "Pulses")
    plant <- c("Short rotation trees", "Short rotation grasses", "Sugar cane", "Oilpalms")
    other <- c(
      "Other oil crops incl rapeseed", "Sunflower", "Potatoes", "Tropical roots",
      "Sugar beet", "Cotton seed", "Forage"
    )
    fruit <- c("Fruits Vegetables Nuts")

    # Sort object according to crop group
    cropArea <- mbind(
      cropArea[, , fruit],
      cropArea[, , "Fallow"],
      cropArea[, , other],
      cropArea[, , plant],
      cropArea[, , legumes],
      cropArea[, , cereal]
    )

    # Calculate crop shares
    CropShr <- cropArea / dimSums(cropArea, dim = 3)
    CropShr[is.na(CropShr)] <- 0
    CropShrData <- as.data.frame(CropShr)

    # Assign crops to crop groups
    CropShrData <- dplyr::mutate(CropShrData, CropGroup = dplyr::case_when(
      Data1 %in% cereal ~ "Cereals",
      Data1 %in% legumes ~ "Legumes",
      Data1 %in% plant ~ "Plantations",
      Data1 %in% other ~ "Other",
      Data1 == "Fallow" ~ "Fallow",
      Data1 %in% fruit ~ "Fruits & Vegetables"
    ), CropGroup = factor(CropGroup))
    CropShrData <- dplyr::rename(CropShrData, Crop = Data1, CropShare = Value)

    # Calculate rank (for plotting) depending on total cereal area
    CerealRank <- setNames(dimSums(CropShr[, , cereal], dim = 3), "Cereals")
    CerealRank <- as.data.frame(CerealRank)
    CerealRank <- CerealRank %>%
      arrange(desc(Value)) %>%
      group_by(Year) %>%
      mutate(CerealRank = 201 - rank(Value, ties.method = "first"))
    CerealRank <- dplyr::select(CerealRank, -Data1, -Value)

    # Combine data sets for plotting
    plotData <- CropShrData %>%
      right_join(cropData, by = c("Cell", "Year", "Region")) %>%
      right_join(CerealRank, by = c("Cell", "Year", "Region"))

    # Subset years
    plotData <- plotData[plotData[, "Year"] %in% year, ]
    # Set Names
    names(plotData) <- c("Cell", "Region", "Year", "Crop", "CropShare", "CropGroup", "TotalArea", "CerealRank")

    # Sort data and calculate bar positions
    plotData <- plotData %>%
      arrange(Year, Region, CerealRank) %>%
      group_by(Year, Region, Crop, CropGroup) %>%
      mutate(BarPos = 0.5 * (cumsum(TotalArea) + cumsum(c(0, TotalArea[-length(TotalArea)]))))

    # Create a global version of the data set
    plotDataGlo <- plotData %>%
      arrange(Year, CerealRank) %>%
      group_by(Year, Crop, CropGroup) %>%
      mutate(
        BarPos = 0.5 * (cumsum(TotalArea) + cumsum(c(0, TotalArea[-length(TotalArea)]))),
        Region = "GLO"
      )

    # -------------------------------
    # Plotting
    # -------------------------------

    # Colors
    # Based on palette 'Bold' from: https://carto.com/carto-colors/

    colors <- c(
      "#F2B701",
      "#E73F74",
      "#7F3C8D",
      "#3969AC",
      "#11A579",
      "#A5AA99"
    )

    ### Regional plot

    CropShrReg <-
      ggplot(
        aes(
          y = CropShare, x = BarPos,
          fill = factor(CropGroup, levels = rev(c("Cereals", "Legumes", "Plantations", "Other", "Fruits & Vegetables", "Fallow")))
        ),
        data = filter(plotData, Year == year)
      ) +
      geom_col(position = "fill", width = filter(plotData, Year == year)$TotalArea) +
      scale_fill_manual(
        values = colors,
        limits = c("Cereals", "Legumes", "Plantations", "Other", "Fruits & Vegetables", "Fallow")
      ) +
      labs(fill = "Crop group") +
      xlab("Cropland area (Mha)") +
      ylab("Crop share")

    if (panel == "row") {
      CropShrReg <- CropShrReg +
        facet_grid(. ~ Region, scales = "free_x", space = "free_x") +
        theme(legend.position = "bottom", legend.box = "horizontal") +
        scale_x_continuous(breaks = c(0, 50, 100, 200, 300)) +
        guides(fill = guide_legend(nrow = 1))
    } else {
      CropShrReg <- CropShrReg +
        facet_wrap(~Region, ncol = 3) +
        theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
        guides(fill = guide_legend(ncol = 2))
    }

    CropShrReg <- CropShrReg +
      theme(
        strip.background = element_blank(),
        strip.clip = "off"
      )

    if (!combined) {
      if (!is.null(file)) {
        if (panel == "row") {
          ggsave(filename = paste0(suppFolder, rev, "_", scenario, "_", year, "_REG_", file), CropShrReg, width = 9, height = 3, scale = 1)
        } else {
          ggsave(filename = paste0(suppFolder, rev, "_", scenario, "_", year, "_REG_", file), CropShrReg, width = 9, height = 9, scale = 1)
        }
      }
    }

    CropShrGlo <-
      ggplot(
        aes(
          y = CropShare, x = BarPos,
          fill = factor(CropGroup, levels = rev(c("Cereals", "Legumes", "Plantations", "Other", "Fallow", "Fruits & Vegetables")))
        ),
        data = filter(plotDataGlo, Year == year)
      ) +
      geom_col(position = "fill", width = filter(plotDataGlo, Year == year)$TotalArea) +
      scale_fill_manual(
        values = colors,
        limits = c("Cereals", "Legumes", "Plantations", "Other", "Fallow", "Fruits & Vegetables")
      ) +
      labs(fill = "Crop group") +
      xlab("Cropland area (Mha)") +
      ylab("Crop share") +
      facet_wrap(~Region, ncol = 3) +
      theme(
        strip.background = element_blank(),
        strip.clip = "off"
      )

    CropShrAll <- CropShrGlo +
      theme(legend.position = "none") +
      ggtitle(paste(scenario, year)) +
      theme(plot.title = element_text(size = 14)) +
      CropShrReg +
      plot_layout(guides = "keep", ncol = 1, byrow = FALSE)

    if (!is.null(file)) {
      if (!combined) {
        ggsave(filename = paste0(suppFolder, rev, "_", scenario, "_", year, "_GLO_", file), CropShrGlo, width = 12, height = 6, scale = 1)
      } else {
        ggsave(filename = paste0(suppFolder, rev, "_", scenario, "_", year, "_", file), CropShrAll, width = 6, height = 7, scale = 1.4)
      }
    } else {
      return(CropShrAll)
    }
  }
  ### end of plotting function

  # ==============================
  # Execute .plotCropShr()
  # ==============================

  for (scen in scenarios) {
    for (yr in plotyears) {
      if (!(yr == "2020" && scen != "BAU")) {
        gdx <- paste(outFolder, gdxFolder[grep(scen, gdxFolder)], "fulldata.gdx", sep = "/")
        .plotCropShr(gdx = gdx, file = file, scenario = scen, year = yr, combined = TRUE)
      }
    }
  }
}
