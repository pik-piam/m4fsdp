#' @title validationFSDP
#' @description Validation for FSDP MAgPIE runs
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param val rds file or data.frame with validation data
#' @param regionSel Region that should be plotted (e.g. c("IND","EUR","GLO")). Aggregate will return LIR, MIR and HIR.
#' @param folder output folder
#' @param scens if "BAU_FSEC", BAU and FSEC scenarios are plotted, with "central" plots the core scenarios, "extended" plot the core scenarios and all ssps.
#' @details creates validation for FSDP MAgPIE runs
#' @return NULL
#' @author Florian Humpenoeder
#' @import ggplot2 data.table patchwork
#' @importFrom utils write.csv
#' @importFrom stats reorder

validationFSDP <- function(repReg, val, regionSel = "aggregate", folder = "output", scens="BAU_FSEC") {

  #### read in data files
  if(scens=="central"){
    rep <- convertReportFSDP(repReg, scengroup = c("FSECc", "FSECd","FSECe"), subset = FALSE)
    rep <- rep[rep$scenario %in%c("SSP1bau","SSP1PLUSbau", "SSP2bau", "SSP5bau", "FSDP"), ]
  } else if (scens=="BAU_FSEC") {
    rep <- convertReportFSDP(repReg, scengroup = c("FSECc","FSECe"), subset = FALSE)
  } else if (scens=="extended") {
    rep <- convertReportFSDP(repReg, scengroup = c("FSECc", "FSECd","FSECe"), subset = FALSE)
    rep <- rep[rep$scenario %in%c("SSP1bau","SSP1PLUSbau", "SSP2bau","SSP2fsdp","SSP3bau","SSP4bau", "SSP5bau", "FSDP"), ]
  } else {stop("unknown scens")}

  rev <- levels(rep$version)
  rep$scenset <- NULL
  if (!is.data.frame(val)) valdata <- readRDS(val)
  valdata[region == "World", region := "GLO"]

  safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#332288", "#AA4499",
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
  fsec <- c("#88CCEE", "#CC6677", "#DDCC77", "#332288", "#AA4499",
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

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

  if (all(length(regionSel) == 1 & regionSel == "aggregate")) {
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
    valdata <- merge(valdata, map)

    regSubOrder <- c("LIR", "MIR", "HIR", "GLO")
    rep$region_class <- factor(rep$region_class, levels = regSubOrder)
    valdata$region_class <- factor(valdata$region_class, levels = regSubOrder)

    rep <- rep[rep$region_class != "GLO", ]
    valdata <- valdata[valdata$region_class != "GLO", ]

  } else {

    rep$region_class <- rep$region
    valdata$region_class <- valdata$region
    rep <- rep[rep$region_class %in% regionSel, ]
    valdata <- valdata[valdata$region_class %in% regionSel, ]

  }

  # plot function
  plotVal <- function(var, units = NULL, varName = NULL, unitName = NULL, weight = NULL, hist = NULL, histName = NULL) {
    empty2null<-function(x){out<-x; if(!is.null(x)){if(x=="empty"){out<-NULL}}; return(out)}
    varName=empty2null(varName)
    weight=empty2null(weight)
    hist=empty2null(hist)
    units=empty2null(units)
    unitName=empty2null(unitName)
    histName=empty2null(histName)

    if (var %in% rep$variable){
      if (is.null(units)) {
        units <- levels(rep$unit)
      }
      b <- rep[rep$variable == var & rep$unit %in% units & rep$period >= 1995 & rep$period <= 2050, ]
      b <- droplevels(b)
      units <- levels(b$unit)
      unitHist <- levels(valdata$unit)[grep(units, levels(valdata$unit), fixed = TRUE)][1]
      if (is.null(hist)) {
        h <- valdata[valdata$variable == var & valdata$unit == unitHist & valdata$scenario == "historical" &
                   valdata$period >= 1980 & valdata$period <= 2020, ]
      } else {
        h <- valdata[valdata$variable == var & valdata$unit == unitHist & valdata$scenario == "historical" &
                   valdata$period >= 1980 & valdata$period <= 2020 & valdata$model %in% hist, ]
        h <- droplevels(h)
        if (!is.null(histName)) levels(h$model) <- histName
      }

      if (!is.null(weight)) {
        w1 <- rep[rep$variable == weight, ]
        w2 <- valdata[valdata$variable == weight & valdata$scenario == "historical" &
                    valdata$period >= 1980 & valdata$period <= 2020, ]
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
      #p <- p + scale_color_brewer("MAgPIE scenario", palette = "Set2")
      p <- p + scale_color_manual("MAgPIE scenario", values = safe_colorblind_palette)
      p <- p + guides(color = guide_legend(order = 1, title.position = "top"),
                      shape = guide_legend(order = 2, title.position = "top"))
      return(p)
    } else {
      warning(paste0("Missing Variable: ",var))
      return(NULL)
    }

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
  ggsave(filename = file.path(folder, paste(rev, "valAssumptions.pdf", sep = "_")), combined,
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
  ggsave(filename = file.path(folder, paste(rev, "valLand.pdf", sep = "_")), combined,
         width = 10, height = 10, scale = 1.3)

  # Validation Yields and TC
  p1 <- plotVal(var = "Productivity|Landuse Intensity Indicator Tau",
                varName = "Landuse Intensity Indicator Tau",
                weight = "Resources|Land Cover|+|Cropland")
  p2 <- plotVal(var = "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer",
                varName = "Sythetic nitrogen fertilizer", hist = "Bodirsky")
  p3 <- plotVal(var = "Productivity|Yield|Crops|+|Cereals",
                varName = "Cereal crop yields",
                weight = "Resources|Land Cover|+|Cropland")
  p4 <- plotVal(var = "Productivity|Yield|Crops|+|Sugar crops",
                varName = "Sugar crop yields",
                weight = "Resources|Land Cover|+|Cropland")
  p5 <- plotVal(var = "Productivity|Yield|Crops|+|Oil crops",
                varName = "Oil crop yields",
                weight = "Resources|Land Cover|+|Cropland")
  p6 <- plotVal(var = "Productivity|Yield|+|Pasture",
                varName = "Pasture yields",
                weight = "Resources|Land Cover|+|Pastures and Rangelands")

  combined <- p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
  combined <- combined + plot_layout(guides = "keep", ncol = 2) & theme(legend.position = "bottom")
  ggsave(filename = file.path(folder, paste(rev, "valYield.png", sep = "_")), combined,
         width = 10, height = 10, scale = 1.3)
  ggsave(filename = file.path(folder, paste(rev, "valYield.pdf", sep = "_")), combined,
         width = 10, height = 10, scale = 1.3)

  # Validation outcome inidicators:


  varname_ind <- list("Health|Prevalence of underweight"=c( #varname
                          "SDG|SDG02|Prevalence of underweight",#var
                          "empty", #valdataname
                          "empty"), #weight
                   "Health|Prevalence of obesity"=c(
                     "SDG|SDG03|Prevalence of obesity", #var
                     "empty", #valdataname
                     "empty"), #weight
                   "Health|Years of life lost"=c(
                     "Health|Years of life lost|Risk|Diet and anthropometrics", #var
                     "empty", #valdataname
                     "empty"), #weight
                   "Environment|Biodiversity Intactness"=c(
                     "Biodiversity|BII", #var
                     "Phillips et al", #valdataname
                     "Resources|Land Cover"), #weight
                   "Environment|Shannon crop area diversity index"=c(
                     "Biodiversity|Shannon crop area diversity index", #var
                     "empty", #valdataname
                     "empty"), #weight
                   "Environment|Nitrogen surplus"=c(
                     "Resources|Nitrogen|Nutrient surplus incl natural vegetation", #var
                     "MADRaT", #valdataname
                     "empty"), #weight
                   "Environment|Water environmental flow violations"=c(
                     "Water|Environmental flow violation volume", #var
                     "empty", #valdataname
                     "empty"), #weight
                   "Environment|Cumulative GHG emissions"=c(
                     "Emissions|GWP100AR6|Land|Cumulative", #var
                     "empty", #valdataname
                     "empty"), #weight
                   "Environment|Global Surface Temperature"=c(
                     "Global Surface Temperature", #var
                     "GISTEMP", #valdataname
                     "empty"), #weight
                   "Inclusion|Expenditure for agric. products"=c(
                     "Household Expenditure|Food|Expenditure", #var
                     "empty", #valdataname
                     "empty"), #weight
                   "Inclusion|Number of People Below 3.20$/Day"=c(
                     "Number of People Below 3.20$/Day", #var
                     "empty", #valdataname
                     "empty"), #weight
                   "Inclusion|Agricultural employment"=c(
                     "Agricultural employment|Crop and livestock products", #var
                     "empty", #valdataname
                     "empty"), #weight,
                   "Inclusion|Agricultural wages"=c(
                     "Hourly labor costs relative to 2000", #var
                     "empty", #valdataname
                     "empty"), #weight,
                   "Economy|Bioeconomy Supply"=c(
                     "Value|Bioeconomy Demand",#var
                     "empty", #valdataname
                     "empty"), #weight
                   "Economy|Costs"=c(
                     "Costs Without Incentives", #var
                     "empty", #valdataname
                     "empty") #weight
  )

  names(varname_ind)<-gsub(pattern = "\\|",replacement = "\n",x = names(varname_ind))

  # BII
  combined=NULL
  for (count in 1:length(varname_ind)){
    tmp <- plotVal(var = varname_ind[[count]][[1]],
                varName = names(varname_ind[count]),
                hist = varname_ind[[count]][[2]],
                weight = varname_ind[[count]][[3]])
    if(!is.null(tmp)){
      if(!is.null(combined)){
        combined <- combined+tmp
      } else {combined <- tmp}
    }
  }

  combined <- combined + plot_annotation(tag_levels = "a")
  combined <- combined + plot_layout(guides = "collect", ncol = 5) & theme(legend.position = "bottom")
  ggsave(filename = file.path(folder, paste(rev, "valOutcome.png", sep = "_")), combined,
         width = 10, height = 8, scale = 1.3)
  ggsave(filename = file.path(folder, paste(rev, "valOutcome.pdf", sep = "_")), combined,
         width = 10, height = 10, scale = 1.3)

}
