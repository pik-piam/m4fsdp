globalVariables(c("value", "V2"))
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
  plotVal <- function(var, units = NULL, weight = NULL, hist = NULL) {
    if (is.null(units)) {
      units <- levels(rep$unit)
    }
    b <- rep[rep$variable == var & rep$unit %in% units & rep$region_class != "GLO", ]
    b <- droplevels(b)
    units <- levels(b$unit)
    unitHist <- levels(val$unit)[grep(units, levels(val$unit), fixed = TRUE)]
    if (is.null(hist)) {
      h <- val[val$variable == var & val$unit == unitHist & val$region_class != "GLO" & val$scenario == "historical" &
                 val$period >= 1980 & val$period <= 2020, ]
    } else {
      h <- val[val$variable == var & val$unit == unitHist & val$region_class != "GLO" & val$scenario == "historical" &
                 val$period >= 1980 & val$period <= 2020 & val$model %in% hist, ]
    }

    if (!is.null(weight)) {
      w1 <- rep[rep$variable == weight & rep$region_class != "GLO", ]
      if (is.null(hist)) {
        w2 <- val[val$variable == weight & val$region_class != "GLO" & val$scenario == "historical" &
                    val$period >= 1980 & val$period <= 2020, ]
      } else {
        w2 <- val[val$variable == weight & val$region_class != "GLO" & val$scenario == "historical" &
                    val$period >= 1980 & val$period <= 2020 & val$model %in% hist, ]
      }
      b <- cbind(b, w1$value)
      h <- cbind(h, w2$value)
      b <- b[, .(value = weighted.mean(value, V2)),
             by = c("region_class", "model", "scenario", "variable", "unit", "period")]
      h <- h[, .(value = weighted.mean(value, V2)),
             by = c("region_class", "model", "scenario", "variable", "unit", "period")]
    } else {
      b <- b[, list(value = sum(value)), by = c("region_class", "model", "scenario", "variable", "unit", "period")]
      h <- h[, list(value = sum(value)), by = c("region_class", "model", "scenario", "variable", "unit", "period")]
    }

    p <- ggplot(b, aes_string(x = "period", y = "value"))
    p <- p + labs(title = var) + ylab(units) + xlab(NULL) + themeMy(rotateX = 90)
    if (nrow(h) > 0) p <- p + geom_point(data = h, aes_string(shape = "model"))
    p <- p + geom_line(aes_string(color = "scenario")) + facet_wrap("region_class")
    p <- p + scale_shape_discrete("Historical data", solid = 0)
    p <- p + scale_color_brewer("MAgPIE scenario", palette = "Set2")
    p <- p + guides(color = guide_legend(order = 1, title.position = "top"),
                    shape = guide_legend(order = 2, title.position = "top"))
  }

  p1 <- plotVal(var = "Population")
  p2 <- plotVal(var = "Income", units = "US$05 PPP/cap/yr", weight = "Population")
  p3 <- plotVal(var = "Nutrition|Calorie Supply|+|Crops", weight = "Population")
  p4 <- plotVal(var = "Nutrition|Calorie Supply|+|Livestock products", weight = "Population")
  p5 <- plotVal(var = "Demand|++|Crops")
  p6 <- plotVal(var = "Demand|++|Livestock products")

  combined <- p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
  combined <- combined + plot_layout(guides = "keep", ncol = 2) & theme(legend.position = "bottom")
  ggsave(filename = file.path(folder, paste(rev, "valAssumptions.png", sep = "_")), combined,
         width = 10, height = 10, scale = 1.3)
}
