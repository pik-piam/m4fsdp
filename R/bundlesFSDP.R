#' @title bundlesFSDP
#' @description FDSP plot with single measure and bundles
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param regionSel Region that should be plotted
#' @param file file name (e.g. BundleFSDP.pdf or BundleFSDP.jpg) or NULL
#' @details plot with single measure and bundles
#' @return depends on file argument. If file==NULL, the default, the plot is returned.
#' If a filename is provided the plot is saved as file
#' @author Florian Humpenoeder
#' @import ggplot2 ggiraph patchwork ggh4x forcats data.table scales htmlwidgets
#' @importFrom rlang .data
#' @importFrom stats reorder
#' @importFrom ggtext element_markdown

bundlesFSDP <- function(repReg, regionSel = "GLO", file = NULL) {
  #### read in data files
  rep <- convertReportFSDP(repReg, scengroup = c("FSECa", "FSECb", "FSECc", "FSECe"), subset = FALSE)

  #needed for some nitrogen variables
  levels(rep$region)[levels(rep$region) == "World"] <- "GLO"

  #get variable list
  var <- getVariables(levels(rep$variable))

  b <- rep[get("variable") %in% var & get("region") == regionSel & get("period") == 2050, ]
  b <- droplevels(b)

  b$variable <- factor(b$variable, levels = var, labels = names(var))
  b[, c("vargroup", "order", "variableName", "unit", "improvment", "rounding","factor") := tstrsplit(get("variable"), "|", fixed = TRUE)]
  b$order <- as.numeric(b$order)
  b$rounding <- as.numeric(b$rounding)
  b$factor <- as.numeric(b$factor)

  vargroupOrder <- c("Health", "Environment", "Inclusion", "Economy")
  b$vargroup <- factor(b$vargroup, levels = vargroupOrder)

  #b$variable <- reorder(b$variable, b$order)
  b[,"variable" := paste(get("variableName"),get("unit"),sep="\n")]
  b$variable <- reorder(b$variable, b$order)


  b$unit <- reorder(b$unit, b$order)

  b[,"value" := get("value") * get("factor")]

  b[, "value" := get("value") - get("value")[get("scenario") == "BAU" & get("period") == "2050"], by = "variable"]

  safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#332288", "#AA4499",
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
  bundlecolor <- "#117733"
  # colors <- setNames(c(safe_colorblind_palette, bundlecolor),
  #                   c(paste0("single", c(1:length(safe_colorblind_palette))), "bundle"))
  colors <- setNames(c(safe_colorblind_palette),
                     c(paste0("single", c(1:length(safe_colorblind_palette)))))
  # colors <- setNames(c("#C29511", "#4499FF", "#BD36FF", "#FC8014", "#26AD4C"),
  #                   c("single1", "single2", "single3", "single4", "bundle"))

  selBundle <- function(b, bundleScen, singles, bundleOrder = "A", colors) {
    if(!(all(c(bundleScen, singles)%in%unique(b$scenario)))){
      warning(
        paste0("bundle ", bundleScen, " is not including all scenarios necessary. The following are missing: ", paste(setdiff(c(bundleScen, singles), unique(b$scenario)),collapse = ", "))
        )}
    x <- b[get("scenario") %in% c(bundleScen, singles), ]
    single_label <- NULL
    if(length(singles) >= 6) {
      sep <- ", "
    } else {
      sep <- "<br>"
    }
    for (count in 1:length(singles)) {
      if (count < length(singles)) {
        single_label <- paste0(single_label, paste0("<span style='color: ", colors[paste0("single", count)], "'>", singles[count], "</span>", sep))
      } else {
        single_label <- paste0(single_label, paste0("<span style='color: ", colors[paste0("single", count)], "'>", singles[count], "</span>"))
      }
      x[, "bundle" := single_label]
      x[get("scenario") %in% singles[count], "scenCol" := paste0("single", count)]
    }

    x[, "bundleOrder" := bundleOrder]
    x[, "bundleName" := bundleScen]
    x[get("scenario") %in% bundleScen, "scenCol" := "bundle"]
    x[get("scenario") %in% singles, "scenset" := "FSECa"]
    x[get("scenario") %in% bundleScen, "scenset" := "FSECb"]

    return(x)
  }

  x <- selBundle(b, "Diet",
             singles = c("DietEmptyCals", "DietLegumes", "DietMonogastrics",
                       "DietRuminants", "DietVegFruitsNutsSeeds", "NoOverweight","HalfOverweight", "NoUnderweight","LessFoodWaste"),
             bundleOrder = 1, colors = colors)

  x <- rbind(x, selBundle(b, "Livelihoods",
                          singles = c("LiberalizedTrade","MinWage","CapitalSubst"),
                          bundleOrder = 2, colors = colors))

  x <- rbind(x, selBundle(b, "NatureSparing",
                          singles = c("REDDaff","LandSparing","PeatlandSparing","WaterSparing","BiodivSparing"),
                          bundleOrder = 3, colors = colors))

  x <- rbind(x, selBundle(b, "AgroMngmt",
                          singles = c("NitrogenEff", "CropRotations", "LandscapeElements", "RiceMit", "LivestockMngmt", "ManureMngmt", "SoilCarbon"),
                          bundleOrder = 4, colors = colors))

  x <- rbind(x, selBundle(b, "ExternalPressures", singles = c("Population", "EconDevelop", "EnergyTrans", "Bioplastics", "TimberCities"),
                 bundleOrder = 5, colors = colors))

  x <- rbind(x, selBundle(b, "FSDP",
                          singles = c("ExternalPressures", "Diet", "Livelihoods", "NatureSparing", "AgroMngmt"),
                          bundleOrder = 6, colors = colors))

  #x <- rbind(x, selBundle(b, "AllNitrogen",
  #                        singles = c("LivestockManureMngmt", "DietMonogastrics",
  #                                  "DietRuminants", "LessFoodWaste", "NitrogenEff"),
  #                        bundleOrder = 1, colors = colors))

'
  x <- rbind(x, selBundle(b, "WaterSoil", singles=c("WaterSparing", "SoilCarbon"), bundleOrder = 2, colors = colors))
  x <- rbind(x, selBundle(b, "REDDaffRuminants", singles=c("DietRuminants", "REDDaff"), bundleOrder = 3, colors = colors))
  x <- rbind(x, selBundle(b, "DietRotations", singles=c("AllHealth", "CropRotations"), bundleOrder = 4, colors = colors))
  x <- rbind(x, selBundle(b, "MonogastricsRotations", singles=c("DietMonogastrics", "CropRotations"),
                         bundleOrder = 5, colors = colors))
  x <- rbind(x, selBundle(b, "TradeRotations", singles=c("LiberalizedTrade", "CropRotations"), bundleOrder = 6, colors = colors))
  x <- rbind(x, selBundle(b, "TradeREDDaff", singles=c("LiberalizedTrade", "REDDaff"), bundleOrder = 7, colors = colors))
  x <- rbind(x, selBundle(b, "TradeSoil", singles=c("LiberalizedTrade", "SoilCarbon"), bundleOrder = 8, colors = colors))
  x <- rbind(x, selBundle(b, "TradeMonogastrics", singles=c("LiberalizedTrade", "DietMonogastrics"), bundleOrder = 9, colors = colors))
  x <- rbind(x, selBundle(b, "TradeRuminants", singles=c("LiberalizedTrade", "DietRuminants"), bundleOrder = 10, colors = colors))
  x <- rbind(x, selBundle(b, "TradeVeggies", singles=c("LiberalizedTrade", "DietVegFruitsNutsSeeds"),
                          bundleOrder = 11, colors = colors))
  x <- rbind(x, selBundle(b, "MonogastricsVeggies", singles=c("DietMonogastrics", "DietVegFruitsNutsSeeds"),
                         bundleOrder = 12, colors = colors))
  x <- rbind(x, selBundle(b, "SoilMonogastric", singles=c("SoilCarbon", "DietMonogastrics"), bundleOrder = 13, colors = colors))
  x <- rbind(x, selBundle(b, "SoilMonogastricRuminants", singles=c("SoilCarbon", "DietMonogastrics", "DietRuminants"),
                         bundleOrder = 14, colors = colors))
  x <- rbind(x, selBundle(b, "SoilRotations", singles=c("SoilCarbon", "CropRotations"), bundleOrder = 15, colors = colors))
  x <- rbind(x, selBundle(b, "LivestockManureMngmt", singles=c("LivestockMngmt", "ManureMngmt"),
                          bundleOrder = 16, colors = colors))
  x <- rbind(x, selBundle(b, "LivestockNUEMngmt", singles=c("LivestockMngmt", "NitrogenEff"),
                          bundleOrder = 17, colors = colors))
'
  b <- x
  b$bundle <- factor(b$bundle)
  b$bundleOrder <- factor(b$bundleOrder)
  b$bundleName <- factor(b$bundleName)
  b$bundle <- fct_reorder(b$bundle, as.integer(b$bundleOrder))
  b$bundleName <- fct_reorder(b$bundleName, as.integer(b$bundleOrder))


  b[, "bundlesum" := sum(get("value"), na.rm = TRUE), by = list(get("variable"), get("bundle"), get("scenset"))]
  b[, "valuefill" := list(get("value") / max(max(abs(get("value")), na.rm = TRUE),
                                             max(abs(get("bundlesum")), na.rm = TRUE),
                                      na.rm = TRUE)), by = list(get("variable"))]

  b <-  b[,"label" := round(sum(get("value")), get("rounding")), by = c("region", "model", "variable","improvment", "unit", "vargroup", "period",
                                                                                 "scenset", "bundle", "bundleOrder", "rounding")]

  bSum <- b[, list(
    "label" = if (max(abs(get("value")) > 0, na.rm = TRUE))
      round(sum(get("value")), get("rounding")) else NULL,
    "valuefill" = if (max(abs(get("valuefill")) > 0, na.rm = TRUE))
      sum(get("valuefill")) else NULL),by = c("region", "model", "variable", "variableName","improvment", "unit", "vargroup", "period",
                                              "scenset", "bundle", "bundleOrder", "bundleName", "rounding")]
  bSum[,"color" := ifelse((get("improvment") == "increase" & get("valuefill") > 0) | (get("improvment") == "decrease" & get("valuefill") < 0),"green","red")]

  bSum <- droplevels(bSum)
  b <- droplevels(b)

  # bSum[bundleOrder==6 & variableName == "Croparea diversity",c("scenset","label","valuefill")]
  # b[bundleOrder==6 & variableName == "Croparea diversity",c("scenset","scenario","label","value","valuefill")]
  # sum(b[bundleOrder==6 & variableName == "Croparea diversity" & scenset=="FSECa",c("valuefill")])

  plotBundle3 <- function(plotData) {
    set.seed(42)
    p <- ggplot(plotData, aes(x = get("valuefill"), y = get("bundleName"))) +
      theme_minimal() + theme(panel.border = element_rect(colour = NA, fill = NA)) +
      facet_nested(get("bundle") ~ get("vargroup") + get("variable"), scales = "free_y", space = "free_y", switch = "y",
                   strip = strip_nested(size = "variable", text_x = elem_list_text(angle = c(0, 90), face=c("bold","plain")), #text_y = elem_list_text(angle = c(90, 0)),
                                        by_layer_x = TRUE)) +
      geom_bar_interactive(data = plotData[get("scenset") == "FSECa", ],
                           mapping = aes(fill = get("scenCol"),
                               tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))),
                               data_id = get("bundleOrder")), position = "stack", stat = "identity", width = 0.45, show.legend = FALSE) +
      geom_errorbar(data = plotData[get("valuefill") != 0, ], mapping = aes(x = 0, xmax = 0, xmin = 0), width = 0.6, color = "grey", show.legend = FALSE) +
      geom_point_interactive(data = bSum[get("scenset") %in% c("FSECa"), ], mapping = aes(shape = get("scenset"), color = get("color"),
        tooltip = paste0("Sum of \nindividual \neffects","\nValue: ", get("label")),
        data_id = get("bundleOrder")), position = position_nudge(y = 0.3), show.legend = TRUE) +
      geom_point_interactive(data = bSum[get("scenset") %in% c("FSECb"), ], mapping = aes(shape = get("scenset"), color = get("color"),
        tooltip = paste0("Bundle","\nValue: ", get("label")),
        data_id = get("bundleOrder")), position = position_nudge(y = -0.3), show.legend = TRUE) +
      geom_text(data = bSum[get("scenset") %in% c("FSECa"), ], aes(color = get("color"), label = get("label"), hjust = ifelse(abs(get("valuefill")) > 0.9, "inward",0.5)),
                size = 3, angle = 0, nudge_y = 0.4, show.legend = FALSE) +
      geom_text(data = bSum[get("scenset") %in% c("FSECb"), ], aes(color = get("color"), label = get("label"), hjust = ifelse(abs(get("valuefill")) > 0.9, "inward",0.5)),
                size = 3, angle = 0, nudge_y = -0.4, show.legend = FALSE, fontface="bold") + #, hjust = "inward") +#if ("valuefill" < 0) 0 else
      scale_fill_manual_interactive("Scenario", values = colors) +
      # scale_color_manual_interactive("Net Effect", values = c("#26AD4C","#AD1515"),labels=c("Single Measures","Bundle")) +

      scale_colour_manual(name = "Direction",
                          labels = c("Positive", "Negative"),
                          values = c("#26AD4C","#AD1515")) +
      scale_shape_manual(name = "Net Effect",
                         labels = c("Sum of \nindividual \neffects\n", "Bundle or\nFSDP (last row)"),
                         values = c(6, 17)) +
      guides(colour = guide_legend(override.aes = list(shape = c(6, 2, 6, 2))), fill = "none", shape = "none") +
      guides(fill = "none", color = "none", shape = guide_legend(order = 1)) +
      labs(y = NULL, x = NULL) + #scale_x_continuous(limits = c(-1.25, 1.25)) +
      theme(legend.position = c(-0.15, 1.13), legend.direction = "vertical",legend.background = element_rect(colour = "black", linewidth = 0.5)) + # scale_fill_manual(values=rev(c("grey80","grey20","black","white"))) +
      theme(plot.background = element_rect(fill = "white"), strip.background = element_rect(color = "grey50"),
            axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x.bottom = element_blank()) + #axis.text.y = element_blank()#theme(plot.margin = margin(1, 35, 1, 1, "pt")) +
      #scale_y_discrete(labels = wrap_format(10)) +
      theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
            strip.text.y.left = ggtext::element_textbox_simple(size = 10, width = unit(200, "pt"), padding = margin(4, 4, 2, 4), margin = margin(0, 0, 2, 0),))
            #strip.text.y.left = ggtext::element_markdown(size = 10, angle = 0))

    return(p)
  }

  m <- plotBundle3(b)
  #ggsave("SingleBundle7.png", m, width = 12, height = 8.5, scale = 1)

  p <- girafe(
    ggobj = m,
    options = list(
      opts_sizing(rescale = TRUE, width = .95),
      opts_hover(css = "fill:NULL;cursor:pointer;"),
      opts_hover_inv(css = "opacity:0.1;"),
      opts_selection(girafe_css(css = "fill:NULL;stroke:grey"),
                     only_shiny = FALSE, type = "multiple", selected = NULL),
      opts_tooltip(css = "background-color:white;padding:5px;
                     border-radius:2px;border: black 1px solid;color:black;")
    ),
    width_svg = 12,
    height_svg = 8.5)

  if (is.null(file)) {
    x <- NULL
    x[["plot"]] <- p
    b[, "value" := round(get("value"), get("rounding"))]
    b$unit <- NULL
    b$bundle <- NULL
    b[, c("variable", "unit") := tstrsplit(get("variable"), "\n", fixed = TRUE)]
    setnames(b, "bundleOrder", "bundle")
    b <- b[, c("bundle", "scenset", "scenario", "region", "period", "vargroup", "variable", "unit", "value")]
    b$vargroup <- factor(b$vargroup, levels = vargroupOrder)
    x[["data"]] <- b
    return(x)
  } else {
    ggsave(file, m, scale = 1, width = 12, height = 8.5, bg = "white")
    ggsave(paste0(substring(file, 1, nchar(file) - 3), "pdf"), m, scale = 1, width = 12, height = 8.5, bg = "white")
    saveWidget(p, paste0(substring(file, 1, nchar(file) - 3), "html"))
  }
}
