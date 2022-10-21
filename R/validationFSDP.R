#' @title validationFSDP
#' @description Validation for FSDP MAgPIE runs
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param valfile validation file
#' @param folder output folder
#' @details creates validation for FSDP MAgPIE runs
#' @return NULL
#' @author Florian Humpenoeder
#' @import ggplot2 data.table patchwork
#' @importFrom utils write.csv
#' @importFrom stats reorder

validationFSDP <- function(repReg, valfile, folder = "output") {

  #### get version
  rev <- unlist(strsplit(repReg, "_"))[1]

  #### read in data files
  rep <- convertReportFSDP(repReg, scengroup = c("FSECc", "FSECe"), subset = FALSE)
  rep$scenset <- NULL
  val <- readRDS(valfile)
  val[region == "World", region := "GLO"]

  themeMy <- function(baseSize = 11, baseFamily = "", rotateX = FALSE, panelSpacing = 3) {
    txt <- element_text(size = baseSize, colour = "black", face = "plain")
    boldTxt <- element_text(size = baseSize, colour = "black", face = "bold")

    theme_bw(base_size = baseSize, base_family = baseFamily) +
      theme(
        legend.key = element_blank(),
        strip.background = element_rect(color = "black", fill = "grey95"),
        axis.text.x = if (rotateX) element_text(angle = 90, hjust = 1, vjust = 0.5) else element_text(angle = 0),
        axis.title.x = element_text(vjust = 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 5)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),

        panel.spacing.x = unit(panelSpacing, "mm"),
        panel.spacing.y = unit(panelSpacing, "mm"),

        text = txt,
        plot.title = boldTxt,

        axis.title = boldTxt,
        axis.text = txt,

        legend.text = element_text(margin = margin(r = 10), hjust = 0, size = baseSize,
                                   colour = "black", face = "plain"),
        legend.title = element_text(margin = margin(r = 10), hjust = 0, size = baseSize,
                                    colour = "black", face = "bold")
      ) + theme(legend.position = "bottom", legend.box = "horizontal", legend.title.align = 0)
  }

  #### mapping for regional aggregation
  map <- data.frame(matrix(nrow = 15, ncol = 2))
  names(map) <- c("region", "region_class")
  map[1, ] <- c("ANZ", "HIR")
  map[2, ] <- c("BRA", "MIR")
  map[3, ] <- c("CAN", "HIR")
  map[4, ] <- c("CHA", "MIR")
  map[5, ] <- c("EUR", "HIR")
  map[6, ] <- c("IND", "LIR")
  map[7, ] <- c("JKO", "HIR")
  map[8, ] <- c("LAM", "MIR")
  map[9, ] <- c("MEA", "MIR")
  map[10, ] <- c("NEA", "MIR")
  map[11, ] <- c("NEU", "MIR")
  map[12, ] <- c("OAS", "MIR")
  map[13, ] <- c("SSA", "LIR")
  map[14, ] <- c("USA", "HIR")
  map[15, ] <- c("GLO", "GLO")
  rep <- merge(rep, map)
  val <- merge(val, map)

  regSubOrder <- c("LIR", "MIR", "HIR", "GLO")
  rep$region_class <- factor(rep$region_class, levels = regSubOrder)
  val$region_class <- factor(val$region_class, levels = regSubOrder)

  # plot function
  plotVal <- function(var, units = NULL, varName = NULL, unitName = NULL, weight = NULL, hist = NULL, histName = NULL) {
    if (is.null(units)) {
      units <- levels(rep$unit)
    }
    b <- rep[rep$variable == var & rep$unit %in% units & rep$region_class != "GLO", ]
    b <- droplevels(b)
    units <- levels(b$unit)
    unitHist <- levels(val$unit)[grep(units, levels(val$unit), fixed = TRUE)][1]
    if (is.null(hist)) {
      h <- val[val$variable == var & val$unit == unitHist & val$region_class != "GLO" & val$scenario == "historical" &
                 val$period >= 1980 & val$period <= 2020, ]
    } else {
      h <- val[val$variable == var & val$unit == unitHist & val$region_class != "GLO" & val$scenario == "historical" &
                 val$period >= 1980 & val$period <= 2020 & val$model %in% hist, ]
      h <- droplevels(h)
      if (!is.null(histName)) levels(h$model) <- histName
    }

    if (!is.null(weight)) {
      w1 <- rep[rep$variable == weight & rep$region_class != "GLO", ]
      w2 <- val[val$variable == weight & val$region_class != "GLO" & val$scenario == "historical" &
                  val$period >= 1980 & val$period <= 2020, ]
      b <- cbind(b, w1$value)
      h <- cbind(h, w2$value)
      b <- b[, list(value = weighted.mean(get("value"), get("V2"))),
             by = c("region_class", "model", "scenario", "variable", "unit", "period")]
      h <- h[, list(value = weighted.mean(get("value"), get("V2"))),
             by = c("region_class", "model", "scenario", "variable", "unit", "period")]
    } else {
      b <- b[, list(value = sum(get("value"))),
             by = c("region_class", "model", "scenario", "variable", "unit", "period")]
      h <- h[, list(value = sum(get("value"))),
             by = c("region_class", "model", "scenario", "variable", "unit", "period")]
    }

    if (is.null(varName)) varName <- var
    if (is.null(unitName)) unitName <- units

    p <- ggplot(b, aes_string(x = "period", y = "value"))
    p <- p + labs(title = varName) + ylab(unitName) + xlab(NULL) + themeMy(rotateX = 90)
    if (nrow(h) > 0) p <- p + geom_point(data = h, aes_string(shape = "model"))
    p <- p + geom_line(aes_string(color = "scenario")) + facet_wrap("region_class")
    p <- p + scale_shape_discrete("Historical data", solid = 0)
    p <- p + scale_color_brewer("MAgPIE scenario", palette = "Set2")
    p <- p + guides(color = guide_legend(order = 1, title.position = "top"),
                    shape = guide_legend(order = 2, title.position = "top"))
  }

  # Validation assumptions
  p1 <- plotVal(var = "Population")
  p2 <- plotVal(var = "Income", units = "US$05 PPP/cap/yr", varName = "Per-capita income", weight = "Population")
  p3 <- plotVal(var = "Nutrition|Calorie Supply|+|Crops", weight = "Population",
                varName = "Per-capita calorie supply from crops", hist = "FAOmassbalance")
  p4 <- plotVal(var = "Nutrition|Calorie Supply|+|Livestock products",
                varName = "Per-capita calorie supply from livestock products",
                weight = "Population", hist = "FAOmassbalance")
  p5 <- plotVal(var = "Demand|++|Crops", varName = "Total crop demand including feed and feed")
  p6 <- plotVal(var = "Demand|++|Livestock products", varName = "Total livestock product demand")

  combined <- p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
  combined <- combined + plot_layout(guides = "keep", ncol = 2) & theme(legend.position = "bottom")
  ggsave(filename = file.path(folder, paste(rev, "valAssumptions.png", sep = "_")), combined,
         width = 10, height = 10, scale = 1.3)

  # Validation land
  p1 <- plotVal(var = "Resources|Land Cover|+|Cropland", varName = "Land Cover|Cropland", hist = "FAO_crop_past")
  p2 <- plotVal(var = "Resources|Land Cover|+|Pastures and Rangelands",
                varName = "Land Cover|Pastures and Rangelands", hist = "FAO_crop_past")
  p3 <- plotVal(var = "Resources|Land Cover|Forest|+|Managed Forest",
                varName = "Managed forest incl. afforestation", hist = "MAgPIEown", histName = "Based on LUH2")
  p4 <- plotVal(var = "Resources|Land Cover|Forest|+|Natural Forest",
                varName = "Primary and secondary forest", hist = "MAgPIEown", histName = "Based on LUH2")
  p5 <- plotVal(var = "Resources|Land Cover|+|Other Land",
                varName = "Other natural land", hist = "MAgPIEown", histName = "Based on LUH2")
  p6 <- plotVal(var = "Resources|Land Cover|+|Urban Area",
                varName = "Urban land", hist = "MAgPIEown", histName = "Based on LUH2")

  combined <- p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
  combined <- combined + plot_layout(guides = "keep", ncol = 2) & theme(legend.position = "bottom")
  ggsave(filename = file.path(folder, paste(rev, "valLand.png", sep = "_")), combined,
         width = 10, height = 10, scale = 1.3)

  # Validation Yields and TC
  p1 <- plotVal(var = "Productivity|Landuse Intensity Indicator Tau", varName = "Landuse Intensity Indicator Tau")
  p2 <- plotVal(var = "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer",
                varName = "Sythetic nitrogen fertilizer", hist = "Bodirsky")
  p3 <- plotVal(var = "Productivity|Yield|Crops|+|Cereals",
                varName = "Cereal crop yields")
  p4 <- plotVal(var = "Productivity|Yield|Crops|+|Sugar crops",
                varName = "Sugar crop yields")
  p5 <- plotVal(var = "Productivity|Yield|Crops|+|Oil crops",
                varName = "Oil crop yields")
  p6 <- plotVal(var = "Productivity|Yield|+|Pasture",
                varName = "Pasture yields")

  combined <- p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
  combined <- combined + plot_layout(guides = "keep", ncol = 2) & theme(legend.position = "bottom")
  ggsave(filename = file.path(folder, paste(rev, "valYield.png", sep = "_")), combined,
         width = 10, height = 10, scale = 1.3)


  # BII
  p1 <- plotVal(var = "Biodiversity|BII", varName = "Biodiversity Intactness Index", hist = "Phillips et al")

  ggsave(filename = file.path(folder, paste(rev, "valBII.png", sep = "_")), p1,
         width = 10, height = 10, scale = 1.3)


  # Global Surface Temeprature
  p1 <- plotVal(var = "Global Surface Temperature", varName = "Global Surface Temperature", hist = "GISTEMP")

  ggsave(filename = file.path(folder, paste(rev, "valGlobalSurfaceTemperature.png", sep = "_")), p1,
         width = 10, height = 10, scale = 1.3)
}
