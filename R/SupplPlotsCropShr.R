globalVariables(c("Data1", "Value", "Year", "Region", "Crop", "TotalArea", "CropShare", "BarPos"))

#' @title SupplPlotsCropShr
#' @description Creates crop share plot
#'
#' @export
#'
#' @param gdx gdx File
#' @param file Name of file output
#' @details blub
#' @return Crop share on the y-axis and cropland area in each cluster on the x-axis.
#' @author Patrick v. Jeetze
#' @import ggplot2 data.table scales magpiesets magpie4
#' @importFrom stats weighted.mean
#' @importFrom dplyr case_when filter group_by right_join mutate arrange desc select %>%

SupplPlotsCropShr <- function(gdx, file = NULL) {


  # Years of interest
  yrs <- c(2015, 2050)

  #-----------------------------
  # Data processing
  # ----------------------------

  # Get total cropland area in each cluster
  cropland <- land(gdx, types = "crop", level = "cell")
  cropData <- as.data.frame(cropland)
  cropData <- select(cropData, -Data1)
  cropData <- rename(cropData, TotalArea = Value)

  # Get area of different crop types in each cluster
  cropArea <- croparea(gdx, level = "cell", product_aggr = FALSE)
  getNames(cropArea) <- reportingnames(getNames(cropArea))
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
  CropShrData <- mutate(CropShrData, CropGroup = case_when(
    Data1 %in% cereal ~ "Cereals",
    Data1 %in% legumes ~ "Legumes",
    Data1 %in% plant ~ "Plantations",
    Data1 %in% other ~ "Other",
    Data1 == "Fallow" ~ "Fallow",
    Data1 %in% fruit ~ "Fruits & Vegetables"
  ), CropGroup = factor(CropGroup))
  CropShrData <- rename(CropShrData, Crop = Data1, CropShare = Value)

  # Calculate rank (for plotting) depending on total cereal area
  CerealRank <- setNames(dimSums(CropShr[, , cereal], dim = 3), "Cereals")
  CerealRank <- as.data.frame(CerealRank)
  CerealRank <- CerealRank %>%
    arrange(desc(Value)) %>%
    group_by(Year) %>%
    mutate(CerealRank = 201 - rank(Value, ties.method = "first"))
  CerealRank <- select(CerealRank, -Data1, -Value)

  # Combine data sets for plotting
  plotData <- CropShrData %>%
    right_join(cropData, by = c("Cell", "Year", "Region")) %>%
    right_join(CerealRank, by = c("Cell", "Year", "Region"))

  # Subset years
  plotData <- plotData[plotData[, "Year"] %in% yrs, ]
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
    mutate(BarPos = 0.5 * (cumsum(TotalArea) + cumsum(c(0, TotalArea[-length(TotalArea)]))),
           Region = "GLO")

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
    "#A5AA99",
    "#11A579"
  )

  # set plot year (preliminary)
  plotyear <- "2050"

  ### Regional plot

  CropShrReg <-
    ggplot(aes(
      y = CropShare, x = BarPos,
      fill = factor(CropGroup, levels = rev(c("Cereals", "Legumes", "Plantations", "Other", "Fallow", "Fruits & Vegetables")))
    ),
    data = filter(plotData, Year == plotyear)
    ) +
    geom_col(position = "fill", width = filter(plotData, Year == plotyear)$TotalArea) +
    scale_fill_manual(
      values = colors,
      limits = c("Cereals", "Legumes", "Plantations", "Other", "Fallow", "Fruits & Vegetables")
    ) +
    labs(fill = "Crop group") +
    xlab("Cropland area (Mha)") +
    ylab("Crop share") +
    facet_wrap(~Region, ncol = 3) +
    theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
    guides(fill = guide_legend(ncol = 2))

  ### Global plot

  CropShrGlo <-
    ggplot(aes(
      y = CropShare, x = BarPos,
      fill = factor(CropGroup, levels = rev(c("Cereals", "Legumes", "Plantations", "Other", "Fallow", "Fruits & Vegetables")))
    ),
    data = filter(plotDataGlo, Year == plotyear)
    ) +
    geom_col(position = "fill", width = filter(plotDataGlo, Year == plotyear)$TotalArea) +
    scale_fill_manual(
      values = colors,
      limits = c("Cereals", "Legumes", "Plantations", "Other", "Fallow", "Fruits & Vegetables")
    ) +
    labs(fill = "Crop group") +
    xlab("Cropland area (Mha)") +
    ylab("Crop share") +
    facet_wrap(~Region, ncol = 3)

  combined <- CropShrReg +
              CropShrGlo +
              plot_layout(guides = "keep", ncol = 1, byrow = FALSE)

  if (!is.null(file)){
    ggsave(filename = file, combined, width = 12, height = 6, scale = 1, bg = "white")
  } else {
    return(combined)
  }

}
