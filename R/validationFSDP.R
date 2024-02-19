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
#' @param showHistLegend show legend for historic data sets. TRUE / FALSE
#' @details creates validation for FSDP MAgPIE runs
#' @return NULL
#' @author Florian Humpenoeder
#' @import ggplot2 data.table patchwork
#' @importFrom utils write.csv
#' @importFrom stats reorder

validationFSDP <-  function(repReg,
                            val,
                            regionSel = "aggregate",
                            folder = "output",
                            scens = "BAU_FSEC",
                            showHistLegend = TRUE) {
    #### read in data files
    if (scens == "central") {
      rep <-
        convertReportFSDP(repReg,
                          scengroup = c("FSECc", "FSECd", "FSECe"),
                          subset = FALSE)
      rep <-
        rep[rep$scenario %in% c("SSP1bau", "SSP1PLUSbau", "SSP2bau", "SSP5bau", "FSDP"),]
    } else if (scens == "BAU_FSEC") {
      rep <-
        convertReportFSDP(repReg,
                          scengroup = c("FSECc", "FSECe"),
                          subset = FALSE)
      scenOrder <- c("FSDP", "BAU")
      scenOrder <- intersect(scenOrder, levels(factor(rep$scenario)))
      scenNames <- as.data.table(m4fsdp::getScenarios())
      scenNames <- scenNames[get("modelrun") %in% scenOrder, ]
      scenNames <- scenNames[match(scenOrder, get("modelrun")), ]
      names(scenOrder) <- unlist(scenNames[, "scenarioname"])
      rep <- rep[get("scenario") %in% scenOrder,]
      rep$scenario <-
        factor(rep$scenario,
               levels = scenOrder,
               labels = names(scenOrder))
      override.linetype <- rev(c("solid", "solid"))
      names(override.linetype) <- names(scenOrder)
      rep[get("scenset") %in% c("FSECc", "FSECe"), "scenset" := "SSP2 BAU / FSDP"]
      rep$scenset <- factor(rep$scenset, c("SSP2 BAU / FSDP"))
      rep <- droplevels(rep)
    } else if (scens == "extended") {
      rep <-
        convertReportFSDP(repReg,
                          scengroup = c("FSECc", "FSECd", "FSECe"),
                          subset = FALSE)
      rep <-
        rep[rep$scenario %in% c(
          "SSP1bau",
          "SSP1PLUSbau",
          "SSP2bau",
          "SSP2fsdp",
          "SSP3bau",
          "SSP4bau",
          "SSP5bau",
          "FSDP"
        ),]
    } else if (scens == "bundles") {
      rep <-
        convertReportFSDP(
          repReg,
          scengroup = c("FSECa", "FSECb", "FSECc", "FSECd", "FSECe"),
          subset = FALSE
        )
      scenOrder <-
        c(
          "AgroMngmt",
          "NatureSparing",
          "Livelihoods",
          "Diet",
          "ExternalPressures",
          "FSDP",
          "BAU"
        )
      scenOrder <- intersect(scenOrder, levels(factor(rep$scenario)))
      scenNames <- as.data.table(m4fsdp::getScenarios())
      scenNames <- scenNames[get("modelrun") %in% scenOrder, ]
      scenNames <- scenNames[match(scenOrder, get("modelrun")), ]
      names(scenOrder) <- unlist(scenNames[, "scenarioname"])
      rep <- rep[get("scenario") %in% scenOrder,]
      rep$scenario <-
        factor(rep$scenario,
               levels = scenOrder,
               labels = names(scenOrder))
      override.linetype <-
        rev(c(
          "dashed",
          "dashed",
          "dashed",
          "dashed",
          "dashed",
          "solid",
          "solid"
        ))
      names(override.linetype) <- names(scenOrder)
      rep[get("scenset") %in% c("FSECc", "FSECe"), "scenset" := "SSP2 BAU / FSDP"]
      rep[get("scenset") %in% c("FSECb"), "scenset" := "Bundles"]
      rep$scenset <-
        factor(rep$scenset, c("SSP2 BAU / FSDP", "Bundles"))
    } else {
      stop("unknown scens")
    }

    rev <- tail(levels(rep$version), n = 1)
    if (!is.data.frame(val)) val <- readRDS(val)
    val[region == "World", region := "GLO"]


    #safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#332288", "#AA4499",
    #                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

    # safe_colorblind_palette <- c("#68023F","#008169","#EF0096","#00DCB5","#FFCFE2",
    #   "#003C86","#9400E6","#009FFA","#FF71FD","#7CFFFA","#6A0213","#008607","#F60239","#00E307","#FFDC3D")
    # order=safe_colorblind_palette[c(15,9,8,12,13,7,c(1:15)[!(1:15%in%c(15,9,8,12,13,7))])]
    # # alternative colors in this order"#00463C","#C00B6F","#00A090","#FF95BA","#5FFFDE","#590A87","#0063E5","#ED0DFD","#00C7F9","#FFD5FD","#3D3C04","#C80B2A","#00A51C","#FFA035","#9BFF2D"

    safe_colorblind_palette <- assignScenarioColors(scenOrder)
    names(safe_colorblind_palette) <- names(scenOrder)


    themeMy <-
      function(baseSize = 11,
               baseFamily = "",
               rotateX = FALSE,
               panelSpacing = 3) {
        txt <-
          element_text(size = baseSize,
                       colour = "black",
                       face = "plain")
        boldTxt <-
          element_text(size = baseSize,
                       colour = "black",
                       face = "bold")

        theme_bw(base_size = baseSize, base_family = baseFamily) +
          theme(
            legend.key = element_blank(),
            strip.background = element_rect(color = "black", fill = "grey95"),
            axis.text.x = if (rotateX)
              element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              )
            else
              element_text(angle = 0),
            axis.title.x = element_text(vjust = 0),
            axis.title.y = element_text(margin = margin(
              t = 0,
              r = 5,
              b = 0,
              l = 0
            )),
            axis.title.y.right = element_text(margin = margin(
              t = 0,
              r = 0,
              b = 0,
              l = 5
            )),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),

            panel.spacing.x = unit(panelSpacing, "mm"),
            panel.spacing.y = unit(panelSpacing, "mm"),

            text = txt,
            plot.title = boldTxt,

            axis.title = boldTxt,
            axis.text = txt,

            legend.text = element_text(
              margin = margin(r = 10),
              hjust = 0,
              size = baseSize,
              colour = "black",
              face = "plain"
            ),
            legend.title = element_text(
              margin = margin(r = 10),
              hjust = 0,
              size = baseSize,
              colour = "black",
              face = "bold"
            )
          ) + theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title.align = 0
          )
      }

    if (all(length(regionSel) == 1 & regionSel == "aggregate")) {
      #### mapping for regional aggregation
      map <- data.frame(matrix(nrow = 15, ncol = 2))
      names(map) <- c("region", "region_class")
      map[1,] <- c("ANZ", "HIR")
      map[2,] <- c("BRA", "MIR")
      map[3,] <- c("CAN", "HIR")
      map[4,] <- c("CHA", "MIR")
      map[5,] <- c("EUR", "HIR")
      map[6,] <- c("IND", "LIR")
      map[7,] <- c("JKO", "HIR")
      map[8,] <- c("LAM", "MIR")
      map[9,] <- c("MEA", "MIR")
      map[10,] <- c("NEA", "MIR")
      map[11,] <- c("NEU", "HIR")
      map[12,] <- c("OAS", "MIR")
      map[13,] <- c("SSA", "LIR")
      map[14,] <- c("USA", "HIR")
      map[15,] <- c("GLO", "GLO")
      rep <- merge(rep, map)
      val <- merge(val, map)

      regSubOrder <- c("LIR", "MIR", "HIR", "GLO")
      rep$region_class <-
        factor(rep$region_class, levels = regSubOrder)
      val$region_class <-
        factor(val$region_class, levels = regSubOrder)

      rep <- rep[rep$region_class != "GLO",]
      val <- val[val$region_class != "GLO",]

    } else {
      rep$region_class <- rep$region
      val$region_class <- val$region
      rep <- rep[rep$region_class %in% regionSel,]
      val <- val[val$region_class %in% regionSel,]

    }

    # plot function
    plotVal <-
      function(var,
               units = NULL,
               varName = NULL,
               unitName = NULL,
               weight = NULL,
               hist = NULL,
               histName = NULL,
               histweight = NULL) {
        empty2null <-
          function(x) {
            out <-
              x
            if (!is.null(x)) {
              if (any(x == "empty")) {
                out <- NULL
              }
            }
            return(out)
          }
        varName = empty2null(varName)
        weight = empty2null(weight)
        hist = empty2null(hist)
        units = empty2null(units)
        unitName = empty2null(unitName)
        histName = empty2null(histName)
        histweight = empty2null(histweight)

        if (var %in% rep$variable) {
          if (is.null(units)) {
            units <- levels(rep$unit)
          }
          b <-
            rep[rep$variable == var &
                  rep$unit %in% units & rep$period >= 2000 & rep$period <= 2050,]
          b <- droplevels(b)
          units <- levels(b$unit)
          unitHist <-
            levels(val$unit)[grep(units, levels(val$unit), fixed = TRUE)][1]
          if (is.null(hist)) {
            h <-
              val[val$variable == var &
                    val$unit == unitHist & val$scenario == "historical" &
                    val$period >= 1980 & val$period <= 2020,]
          } else {
            h <-
              val[val$variable == var &
                    val$unit == unitHist & val$scenario == "historical" &
                    val$period >= 1980 &
                    val$period <= 2020 & val$model %in% hist,]
            h <- droplevels(h)
            if (!is.null(histName)) {
              h$model <- factor(h$model, hist, histName)
            }
          }

          if (!is.null(weight)) {
            w1 <-
              rep[rep$variable == weight &
                    rep$period >= 2000 & rep$period <= 2050,]
            w1 <- droplevels(w1)
            w1 <-
              w1[, c("region",
                     "scenario",
                     "scenset",
                     "period",
                     "value",
                     "region_class")]
            names(w1)[names(w1) == "value"] <- "weight"
            b <-
              merge(b,
                    w1,
                    by = c(
                      "region",
                      "scenario",
                      "scenset",
                      "period",
                      "region_class"
                    ))

            w2 <-
              val[val$variable == weight &
                    val$scenario == "historical" & val$model == histweight &
                    val$period >= 1980 & val$period <= 2020,]
            w2 <- droplevels(w2)
            w2 <-
              w2[, c("region",
                     "scenario",
                     "period",
                     "value",
                     "region_class")]
            names(w2)[names(w2) == "value"] <- "weight"
            h <-
              merge(h,
                    w2,
                    by = c("region", "scenario", "period", "region_class"))

            b <-
              b[, list(value = weighted.mean(get("value"), get("weight"))),
                by = c("region_class",
                       "model",
                       "scenario",
                       "scenset",
                       "variable",
                       "unit",
                       "period")]
            h <-
              h[, list(value = weighted.mean(get("value"), get("weight"))),
                by = c("region_class",
                       "model",
                       "scenario",
                       "variable",
                       "unit",
                       "period")]
          } else {
            b <- b[, list(value = sum(get("value"))),
                   by = c("region_class",
                          "model",
                          "scenario",
                          "scenset",
                          "variable",
                          "unit",
                          "period")]
            h <- h[, list(value = sum(get("value"))),
                   by = c("region_class",
                          "model",
                          "scenario",
                          "variable",
                          "unit",
                          "period")]
          }

          if (is.null(varName))
            varName <- var
          if (is.null(unitName))
            unitName <- units

          p <- ggplot(b, aes(x = get("period"), y = get("value")))
          p <-
            p + labs(title = varName) + ylab(unitName) + xlab(NULL) + themeMy(rotateX = 90)
          p <- p + scale_x_continuous(NULL, breaks = c(1980, 2000, 2025, 2050)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
          if (nrow(h) > 0)
            p <-
            p + geom_point(data = h,
                           aes(shape = get("model")),
                           show.legend = showHistLegend)
          p <-
            p + geom_line(aes(
              color = get("scenario"),
              linetype = get("scenset")
            )) + facet_wrap("region_class")
          p <- p + scale_shape_discrete("Historical data", solid = 0)
          #p <- p + scale_color_brewer("MAgPIE scenario", palette = "Set2")
          p <-
            p + scale_color_manual("MAgPIE scenario", values = safe_colorblind_palette)
          p <-
            p + guides(
              color = guide_legend(
                order = 1,
                ncol = 1,
                title.position = "top",
                override.aes = list(
                  linetype = override.linetype,
                  size = 1,
                  shape = NA
                ),
                reverse = TRUE
              ),
              shape = guide_legend(order = 2, nrow = 2, title.position = "top"),
              linetype = "none"
            )
          return(p)
        } else {
          warning(paste0("Missing Variable: ", var))
          return(ggplot(get("mtcars"), aes(
            x = get("wt"), y = get("mpg")
          )) + geom_blank() + ggtitle("DUMMY / Placeholder"))
        }

      }

    # Validation assumptions
    p1 <- plotVal(var = "Population")
    p2 <-
      plotVal(
        var = "Income",
        units = "US$05 PPP/cap/yr",
        varName = "Per-capita income",
        weight = "Population",
        histweight = "WDI-UN_PopDiv-MI"
      )
    p3 <-
      plotVal(
        var = "Nutrition|Calorie Supply|+|Crops",
        weight = "Population",
        histweight = "WDI-UN_PopDiv-MI",
        varName = "Per-capita calorie supply from crops",
        hist = "FAOmassbalance"
      )
    p4 <-
      plotVal(
        var = "Nutrition|Calorie Supply|+|Livestock products",
        varName = "Per-capita calorie supply from livestock products",
        weight = "Population",
        histweight = "WDI-UN_PopDiv-MI",
        hist = "FAOmassbalance"
      )
    p5 <-
      plotVal(var = "Demand|++|Livestock products", varName = "Total livestock product demand")
    #p7 <- plotVal(var = "Income|Number of People Below 3p20 USDppp11/day", varName = "Number of people under 3.20 USDppp11/day poverty line")
    #p8 <- plotVal(var = "Income|Gini Coefficient", varName = "Gini Coefficient", weight = "Population")
    #poverty validation requires new input data validation

    combined <-
      p1 + p2 + p3 + p4 + p5 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valAssumptions.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valAssumptions.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )


    # Crop demand
    p1 <-
      plotVal(var = "Demand|++|Crops", varName = "Total demand for crops")
    p2 <-
      plotVal(var = "Demand|Food|+|Crops", varName = "Food demand for crops")
    p3 <-
      plotVal(var = "Demand|Feed|+|Crops", varName = "Feed demand for crops")
    p4 <-
      plotVal(var = "Demand|Seed|+|Crops", varName = "Seed demand for crops")
    p5 <-
      plotVal(var = "Demand|Material|+|Crops", varName = "Material demand for crops")
    p6 <-
      plotVal(var = "Demand|Processing|+|Crops", varName = "Processing demand for crops")

    combined <-
      p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valCropDemand.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valCropDemand.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )

    # Feed demand
    p1 <-
      plotVal(var = "Demand|Feed|+|Crops", varName = "Feed demand for crops")
    p2 <-
      plotVal(var = "Demand|Feed|+|Pasture", varName = "Feed demand for pasture")
    p3 <-
      plotVal(var = "Demand|Feed|+|Secondary products", varName = "Feed demand for primary-processed products")
    p4 <-
      plotVal(var = "Demand|Feed|+|Crop residues", varName = "Feed demand for crop residues")

    combined <- p1 + p2 + p3 + p4 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valFeedDemand.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valFeedDemand.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )


    # Feed efficiency
    p1 <-
      plotVal(var = "Productivity|Feed conversion efficiency|Ruminant meat and dairy",
              varName = "Ruminant feed conversion efficiency",
              weight = "Production|+|Livestock products",
              histweight = "FAO")
    p2 <-
      plotVal(var = "Productivity|Feed conversion efficiency|Monogastric meat",
              varName = "Monogastric feed conversion efficiency",
              weight = "Production|+|Livestock products",
              histweight = "FAO")
    p3 <-
      plotVal(var = "Productivity|Feed conversion efficiency|Poultry meat and eggs",
              varName = "Poultry feed conversion efficiency",
              weight = "Production|+|Livestock products",
              histweight = "FAO")
    p4 <-
      plotVal(var = "Productivity|Feed conversion efficiency|+|Cereal Intensity",
              varName = "Cereal feed intensity per animal product",
              weight = "Production|+|Livestock products",
              histweight = "FAO")
    p5 <-
      plotVal(var = "Productivity|Feed conversion efficiency|+|Oilcrop intensity",
              varName = "Oilcrop feed intensity per animal product",
              weight = "Production|+|Livestock products",
              histweight = "FAO")
    p6 <-
      plotVal(var = "Productivity|Feed conversion efficiency|+|Pasture intensity",
              varName = "Pasture feed intensity per animal product",
              weight = "Production|+|Livestock products",
              histweight = "FAO")

    combined <-
      p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valFeedEfficiency.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valFeedEfficiency.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )

    # Crop production
    p1 <-
      plotVal(var = "Production|+|Crops", varName = "Total crop production")
    p2 <-
      plotVal(var = "Production|Crops|+|Cereals", varName = "Cereal production")
    p3 <-
      plotVal(var = "Production|Crops|+|Oil crops", varName = "Oil crop production")
    p4 <-
      plotVal(var = "Production|Crops|+|Sugar crops", varName = "Sugar crop production")
    p5 <-
      plotVal(var = "Production|Crops|Other crops|+|Fruits Vegetables Nuts", varName = "Fruits Vegetables Nuts production")

    combined <-
      p1 + p2 + p3 + p4 + p5 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valCropProduction.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valCropProduction.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )

    # Other production
    p1 <-
      plotVal(var = "Production|+|Livestock products", varName = "Livestock production")
    p2 <-
      plotVal(var = "Production|+|Secondary products", varName = "Primary-processed production")
    p3 <-
      plotVal(var = "Production|+|Pasture", varName = "Grazed and mowed grass")
    p4 <-
      plotVal(var = "Timber|Volumetric|Production|Roundwood", varName = "Roundwood production")

    combined <- p1 + p2 + p3 + p4 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valOtherProduction.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valOtherProduction.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )

    # Validation land
    p1 <-
      plotVal(
        var = "Resources|Land Cover|+|Cropland",
        varName = "Land Cover|Cropland",
        hist = c("FAO_crop_past", "MAgPIEown", "LUH2v2"),
        histName = c("FAO", "mod. LUH2v2", "orig. LUH2v2")
      )
    p2 <-
      plotVal(
        var = "Resources|Land Cover|+|Pastures and Rangelands",
        varName = "Land Cover|Pastures and Rangelands",
        hist = c("FAO_crop_past", "MAgPIEown", "LUH2v2"),
        histName = c("FAO", "mod. LUH2v2", "orig. LUH2v2")
      )
    p3 <-
      plotVal(
        var = "Resources|Land Cover|Forest|+|Managed Forest",
        varName = "Managed forest incl. afforestation",
        hist = "MAgPIEown",
        histName = "FRA"
      )
    p4 <- plotVal(
      var = "Resources|Land Cover|+|Forest",
      varName = "Total forest area",
      hist = c("FAO_forest", "MAgPIEown", "LUH2v2"),
      histName = c("FAO", "mod. LUH2v2", "orig. LUH2v2")
    )
    p5 <- plotVal(
      var = "Resources|Land Cover|+|Other Land",
      varName = "Other natural land",
      hist = c("MAgPIEown", "LUH2v2"),
      histName = c("mod. LUH2v2", "orig. LUH2v2")
    )
    p6 <- plotVal(
      var = "Resources|Land Cover|+|Urban Area",
      varName = "Urban land",
      hist = c("MAgPIEown", "LUH2v2"),
      histName = c("mod. LUH2v2", "orig. LUH2v2")
    )

    combined <-
      p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valLand.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valLand.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )

    # Validation croparea

    p1 <-
      plotVal(var = "Resources|Land Cover|Cropland|+|Croparea",
              varName = "Croparea cropped",
              unitName = "Mha physical area")
    p2 <-
      plotVal(var = "Resources|Land Cover|Cropland|+|Fallow Cropland",
              varName = "Croparea fallow",
              unitName = "Mha physical area")
    p3 <-
      plotVal(var = "Resources|Land Cover|Cropland|Croparea|Crops|+|Cereals",
              varName = "Croparea cereals",
              unitName = "Mha physical area")
    p4 <-
      plotVal(var = "Resources|Land Cover|Cropland|Croparea|Crops|+|Oil crops",
              varName = "Croparea oil crops",
              unitName = "Mha physical area")
    p5 <-
      plotVal(var = "Resources|Land Cover|Cropland|Croparea|Crops|+|Sugar crops",
              varName = "Croparea sugar crops",
              unitName = "Mha physical area")
    p6 <-
      plotVal(var = "Resources|Land Cover|Cropland|Croparea|Crops|Other crops|+|Fruits Vegetables Nuts",
              varName = "Croparea fruits vegetables nuts",
              unitName = "Mha physical area")

    combined <-
      p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valCroparea.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valCroparea.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )

    # Nitrogen inputs
    p1 <-
      plotVal(var = "Resources|Nitrogen|Cropland Budget|Inputs", varName = "Total nitrogen inputs")
    p2 <-
      plotVal(var = "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer", varName = "Inorganic fertilizer")
    p3 <-
      plotVal(var = "Resources|Nitrogen|Cropland Budget|Inputs|+|Biological Fixation Symbiotic Crops", varName = "Symbiotic fixation")
    p4 <-
      plotVal(var = "Resources|Nitrogen|Cropland Budget|Inputs|+|Manure Recycled from Confinements", varName = "Manure from confinement")
    p5 <-
      plotVal(var = "Resources|Nitrogen|Cropland Budget|Inputs|+|Recycled Aboveground Crop Residues", varName = "Crop residues left on field")

    combined <-
      p1 + p2 + p3 + p4 + p5 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valNitrogenInputs.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valNitrogenInputs.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )


    #Nitrogen outputs
    p1 <-
      plotVal(var = "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Harvested Crops", varName = "N in harvested crops")
    p2 <-
      plotVal(var = "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Aboveground Crop Residues", varName = "N in aboveground residues")
    p3 <-
      plotVal(var = "Resources|Nitrogen|Cropland Budget|Balance|+|Nutrient Surplus", varName = "N in nutrient surplus")
    #p4 <-
    #  plotVal(var = "Resources|Nitrogen|Cropland Budget|Balance|+|Soil Organic Matter", varName = "N change in soil organic matter")

    combined <- p1 + p2 + p3 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valNitrogenOutputs.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valNitrogenOutputs.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )


    # Yields

    p1 <-
      plotVal(var = "Productivity|Yield by physical area|+|Crops",
              varName = "Average yields",
              weight = "Resources|Land Cover|Cropland|+|Croparea",
              histweight = "Ostberg2023_FAO_LUH2v2")
    p2 <-
      plotVal(var = "Productivity|Yield by physical area|Crops|+|Cereals",
              varName = "Average cereal yields",
              weight = "Resources|Land Cover|Cropland|Croparea|Crops|+|Cereals",
              histweight = "Ostberg2023_FAO_LUH2v2")
    p3 <-
      plotVal(var = "Productivity|Yield by physical area|Crops|+|Oil crops",
              varName = "Average oil crop yields",
              weight = "Resources|Land Cover|Cropland|Croparea|Crops|+|Oil crops",
              histweight = "Ostberg2023_FAO_LUH2v2")
    p4 <-
      plotVal(var = "Productivity|Yield by physical area|Crops|+|Sugar crops",
              varName = "Average sugar crop yields",
              weight = "Resources|Land Cover|Cropland|Croparea|Crops|+|Sugar crops",
              histweight = "Ostberg2023_FAO_LUH2v2")
    p5 <-
      plotVal(var = "Productivity|Yield by physical area|Crops|Other crops|+|Fruits Vegetables Nuts",
              varName = "Average yield of fruits, vegetables, nuts",
              weight = "Resources|Land Cover|Cropland|Croparea|Crops|Other crops|+|Fruits Vegetables Nuts",
              histweight = "Ostberg2023_FAO_LUH2v2")
    p6 <-
      plotVal(
        var = "Productivity|Landuse Intensity Indicator Tau",
        varName = "Landuse Intensity Indicator Tau",
        weight = "Resources|Land Cover|+|Cropland",
        histweight = "FAO_crop_past",
        hist = "dietrich_et_al_2012_updated",
        histName = "Dietrich 2012"
      )

    combined <-
      p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valYields.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valYields.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )


    # Validation emissions
    p1 <-
      plotVal(
        var = "Emissions|CO2|Land|+|Land-use Change",
        varName = "CO2 emissions from land-use change",
        hist = c("FAO_EmisLUC", "EDGAR_LU", "Gasser et al 2020"),
        histName = c("FAO", "EDGAR", "Gasser et al. 2020")
      ) +
      scale_y_continuous(expand = c(0, 0), limits = c(NA, NA))
    p2 <-
      plotVal(
        var = "Emissions|CH4|Land|+|Agriculture",
        varName = "CH4 emissions from agriculture",
        hist = c("FAO_EmisAg", "EDGAR_LU"),
        histName = c("FAO", "EDGAR")
      ) +
      scale_y_continuous(expand = c(0, 0), limits = c(NA, NA))
    p3 <-
      plotVal(
        var = "Emissions|N2O|Land|+|Agriculture",
        varName = "N2O emissions from agriculture",
        hist = c("FAO_EmisAg", "EDGAR_LU"),
        histName = c("FAO", "EDGAR")
      ) +
      scale_y_continuous(expand = c(0, 0), limits = c(NA, NA))

    combined <- p1 + p2 + p3 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 1) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valEmissions.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valEmissions.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )

    # Water

    p1 <-
      plotVal(var = "Resources|Land Cover|Cropland|Area equipped for irrigation", varName = "Area equipped for irrigation")
    p2 <- plotVal(
      var = "Resources|Water|Withdrawal|Agriculture",
      varName = "Water Withdrawal Agriculture",
      hist = c(
        "LPJmL:ipsl-cm5a-lr",
        "MATSIRO:ipsl-cm5a-lr",
        "MPI-HM:ipsl-cm5a-lr"
      ),
      histName = c("LPJmL:IPSL", "MATSIRO:IPSL", "MPI-HM:IPSL")
    )

    combined <- p1 + p2 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valWater.png", sep = "_")),
      combined,
      width = 10,
      height = 6,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valWater.pdf", sep = "_")),
      combined,
      width = 10,
      height = 6,
      scale = 1.3
    )

    # Validation ag com. price index
    p1 <-
      plotVal(
        var = "Productivity|Landuse Intensity Indicator Tau",
        varName = "Landuse Intensity Indicator Tau",
        weight = "Resources|Land Cover|+|Cropland",
        histweight = "FAO_crop_past",
        hist = "dietrich_et_al_2012_updated",
        histName = "Dietrich 2012"
      )
    p2 <- plotVal(var = "Costs|TC",
                  weight = "Population",
                  histweight = "WDI-UN_PopDiv-MI")
    p3 <- plotVal(var = "Agricultural Research Intensity",
                  weight = "Population",
                  histweight = "WDI-UN_PopDiv-MI")
    p4 <- plotVal(var = "SDG|SDG02|Investment in AgR&D",
                  weight = "Population",
                  histweight = "WDI-UN_PopDiv-MI")
    p5 <- plotVal(var = "Prices|Index2020|Agriculture|Food products",
                  weight = "Population",
                  histweight = "WDI-UN_PopDiv-MI")
    p6 <-
      plotVal(var = "Prices|Index2020|Agriculture|Food products|Plant-based",
              weight = "Population",
              histweight = "WDI-UN_PopDiv-MI")

    combined <-
      p1 + p2 + p3 + p4 + p5 + p6 + plot_annotation(tag_levels = "a")
    combined <-
      combined + plot_layout(guides = ifelse(showHistLegend, "keep", "collect"),
                             ncol = 2) &
      theme(
        legend.position = "bottom",
        legend.box = ifelse(scens == "BAU_FSEC", "horizontal", "vertical")
      )
    ggsave(
      filename = file.path(folder, paste(rev, "valPricesCosts.png", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )
    ggsave(
      filename = file.path(folder, paste(rev, "valPricesCosts.pdf", sep = "_")),
      combined,
      width = 10,
      height = 10,
      scale = 1.3
    )

  }
