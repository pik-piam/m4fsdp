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
  rep <- convertReportFSDP(repReg, scengroup = c("FSECa", "FSECb", "FSECc"), subset = FALSE)

  var <- c("SDG|SDG02|Prevalence of underweight",
           "SDG|SDG03|Prevalence of obesity",
           "Health|Attributable deaths|Risk|Diet and anthropometrics",
           "Health|Years of life lost|Risk|Diet and anthropometrics",
           "Biodiversity|BII",
           "Biodiversity|Shannon crop area diversity index",
           "Resources|Nitrogen|Nutrient surplus incl natural vegetation",
           "Water|Environmental flow violation volume",
           "Emissions|GWP100AR6|Land|Cumulative",
           "Global Surface Temperature",
           "Household Expenditure|Food|Expenditure",
           "Number of People Below 3.20$/Day",
           "Agricultural employment|Crop and livestock products",
           "Hourly labor costs relative to 2000",
           "Value|Bioeconomy Demand",
           "Costs Without Incentives")

  names(var) <- c("Health|Underweight\nmio people|1|decrease|0",
                  "Health|Obesity\nmio people|2|decrease|0",
                  "Health|Attributable deaths\nmillion people|3|decrease|0",
                  "Health|Years of life lost\nmillion years|4|decrease|0",
                  "Environment|Biodiversity\nBII|1|increase|2",
                  "Environment|Croparea diversity\nShannon Index (1)|2|increase|2",
                  "Environment|Nitrogen surplus\nMt N/yr|3|decrease|0",
                  "Environment|Water flow violations\nkm3/yr|4|decrease|0",
                  "Environment|Cum CO2 emissions\nGtCO2eq since 1995|5|decrease|0",
                  "Environment|Global Surface Temp\ndeg C|6|decrease|2",
                  "Inclusion|Cost agric. products\nUSD/person|1|decrease|0",
                  "Inclusion|People Below 3.20$/Day\nmio people|2|decrease|0",
                  "Inclusion|Agri. employment\nmio people|3|increase|0",
                  "Inclusion|Agri. wages\nIndex|4|increase|2",
                  "Economy|Bioeconomy Supply\nbillion US$05/yr|1|increase|0",
                  "Economy|Costs\nbillion US$05/yr|1|decrease|0")

  if(any(!var%in%rep$variable)){
    warning(paste(c("The following indicators are missing: ",var=var[!var%in%rep$variable]),collapse = " "))
  }
  var=var[var%in%rep$variable]

  levels(rep$region)[levels(rep$region) == "World"] <- "GLO"
  b <- rep[get("variable") %in% var & get("region") == regionSel & get("period") == 2050, ]
  b <- droplevels(b)

  b$variable <- factor(b$variable, levels = var, labels = names(var))
  b[, c("vargroup", "variable", "order", "improvment", "rounding") := tstrsplit(get("variable"), "|", fixed = TRUE)]
  b$order <- as.numeric(b$order)
  b$rounding <- as.numeric(b$rounding)

  vargroupOrder <- c("Health", "Environment", "Inclusion", "Economy")
  b$vargroup <- factor(b$vargroup, levels = vargroupOrder)

  b$variable <- reorder(b$variable, b$order)

  b[variable %in% c("Costs\nbillion US$05/yr"), "value" := get("value") / 1000]
  b[variable %in% c("Bioeconomy Supply\nbillion US$05/yr"), "value" := get("value") / 1000]
  b[variable %in% c("Biodiversity\nBII"), "value" := get("value") * 100]

  b[, "value" := get("value") - get("value")[get("scenario") == "BAU" & get("period") == "2050"], by = "variable"]

  safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#332288", "#AA4499",
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
  bundlecolor <- "#117733"
  colors <- setNames(c(safe_colorblind_palette, bundlecolor),
                    c(paste0("single", c(1:length(safe_colorblind_palette))), "bundle"))
  # colors <- setNames(c("#C29511", "#4499FF", "#BD36FF", "#FC8014", "#26AD4C"),
  #                   c("single1", "single2", "single3", "single4", "bundle"))

  selBundle <- function(b, bundle, singles, bundleOrder = "A", colors) {
    x <- b[get("scenario") %in% c(bundle, singles), ]
    single_label <- NULL
    for (count in 1:length(singles)) {
      single_label <- c(single_label, paste0("<span style='color: ", colors[paste0("single", count)], "'>", singles[count], "</span>", "<br>"))
    }

    x[, "bundle" := paste0(c(single_label,
                           "<span style='color: ", colors["bundle"], "'>", "bundle", "</span>"), collapse = "")]
    x[, "bundleOrder" := bundleOrder]
    for (count in 1:length(singles)) {
      x[get("scenario") %in% singles[count], "scenCol" := paste0("single", count)]
    }
    x[get("scenario") %in% bundle, "scenCol" := "bundle"]
    x[get("scenario") %in% singles, "scenset" := "FSECa"]
    x[get("scenario") %in% bundle, "scenset" := "FSECb"]

    return(x)
  }

  x <- selBundle(b, "ExternalPressures", singles = c("Population", "EconDevelop", "EnergyTrans", "Bioplastics"),
                 bundleOrder = 1, colors = colors)

  x <- rbind(x, selBundle(b, "Sufficiency",
             singles = c("DietEmptyCals", "DietFish", "DietLegumes", "DietMonogastrics",
                       "DietRuminants", "DietVegFruitsNutsSeeds", "NoOverweight", "NoUnderweight","LessFoodWaste"),
             bundleOrder = 2, colors = colors))
  x <- rbind(x, selBundle(b, "Livelihoods",
                          singles = c("LiberalizedTrade","MinWage"),
                          bundleOrder = 3, colors = colors))
  x <- rbind(x, selBundle(b, "NatureSparing",
                          singles = c("REDDaff","LandSparing","PeatlandSparing","WaterSparing","BiodivSparing"),
                          bundleOrder = 4, colors = colors))
  x <- rbind(x, selBundle(b, "AgroMngmt",
                          singles = c("CropRotatons", "NitrogenEff", "RiceMit", "LivestockMngmt", "ManureMngmt", "SoilCarbon"),
                          bundleOrder = 5, colors = colors))
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
  b$bundle <- fct_reorder(b$bundle, as.integer(b$bundleOrder))


  b[, "bundlesum" := sum(get("value"), na.rm = TRUE), by = list(get("variable"), get("bundle"), get("scenset"))]
  b[, "valuefill" := list(get("value") / max(max(abs(get("value")), na.rm = TRUE),
                                             max(abs(get("bundlesum")), na.rm = TRUE),
                                      na.rm = TRUE)), by = list(get("variable"))]

  bSum <- b[, list("label" = if (max(abs(get("valuefill")) > 0, na.rm = TRUE))
    round(sum(get("value")), get("rounding"))
    else NULL, valuefill = sum(get("valuefill"), na.rm = TRUE) / 2),
             by = c("region", "model", "variable", "unit", "vargroup", "period",
                    "scenset", "bundle", "bundleOrder", "rounding")]
  # bSum <- b[, list("label" = round(sum(get("value")), get("rounding")),
  #  valuefill = sum(get("valuefill"), na.rm = TRUE) / 2),
  #  by = c("region", "model", "variable", "unit", "vargroup", "period",
  #         "scenset", "bundle", "bundleOrder", "rounding")]

  bSum <- droplevels(bSum)
  b <- droplevels(b)

  plotBundle2 <- function(plotData) {
    set.seed(42)
    plotData <- droplevels(plotData[get("scenset") %in% c("FSECa", "FSECb"), ])
    p <- ggplot(plotData, aes(x = get("valuefill"), y = reorder(get("scenset"), dplyr::desc(get("scenset"))))) +
      theme_minimal() + theme(panel.border = element_rect(colour = NA, fill = NA)) +
      facet_nested(get("bundle") ~ get("vargroup") + get("variable"), scales = "free_y", space = "free_y", switch = "y",
                   strip = strip_nested(size = "variable", text_x = elem_list_text(angle = c(0, 90)),
                                        by_layer_x = TRUE)) +
      geom_bar_interactive(aes(fill = get("scenCol"),
                               tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))),
                               data_id = get("bundleOrder")), position = "stack", stat = "identity", width = 0.5) +
      geom_bar_interactive(data = plotData[get("scenset") == "FSECb" & get("improvment") == "increase" & value > 0, ],
                           mapping = aes(tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))), data_id = get("bundleOrder")),
                           position = "stack", stat = "identity", width = 0.5, fill = "#26AD4C", size = 0) +
      geom_bar_interactive(data = plotData[get("scenset") == "FSECb" & get("improvment") == "increase" & value < 0, ],
                           mapping = aes(tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))), data_id = get("bundleOrder")),
                           position = "stack", stat = "identity", width = 0.5, fill = "#AD1515", size = 0) +
      geom_bar_interactive(data = plotData[get("scenset") == "FSECb" & get("improvment") == "decrease" & value > 0, ],
                           mapping = aes(tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))), data_id = get("bundleOrder")),
                           position = "stack", stat = "identity", width = 0.5, fill = "#AD1515", size = 0) +
      geom_bar_interactive(data = plotData[get("scenset") == "FSECb" & get("improvment") == "decrease" & value < 0, ],
                           mapping = aes(tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))), data_id = get("bundleOrder")),
                           position = "stack", stat = "identity", width = 0.5, fill = "#26AD4C", size = 0) +
      geom_errorbar(data = plotData[get("valuefill") != 0, ], mapping = aes(x = 0, xmax = 0, xmin = 0), width = 0.6, color = "grey") +
      geom_text(data = bSum[get("scenset") %in% c("FSECa"), ], aes(x = 0, label = get("label")),
                size = 3, colour = "black", angle = 0, nudge_y = 0.4) +
      geom_text(data = bSum[get("scenset") %in% c("FSECb"), ], aes(x = 0, label = get("label")),
                size = 3, colour = "black", angle = 0, nudge_y = -0.4) +
      scale_fill_manual_interactive("Scenario", values = colors) +
      guides(fill = guide_legend(order = 1)) +
      labs(y = NULL, x = NULL) + scale_x_continuous(limits = c(-1.25, 1.25)) +
      theme(legend.position = "none") + # scale_fill_manual(values=rev(c("grey80","grey20","black","white"))) +
      theme(plot.background = element_rect(fill = "white"), strip.background = element_rect(color = "grey50"),
            axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x.bottom = element_blank(),
            axis.text.y = element_blank()) + theme(plot.margin = margin(1, 35, 1, 1, "pt")) +
      theme(axis.text.x = element_text(angle = 30, hjust = 0),
            strip.text.y.left = ggtext::element_markdown(size = 10, angle = 0))

    return(p)
  }
  m <- plotBundle2(b)
  # ggsave("SingleBundle2.png", m, width = 11, height = 7, scale = 1)

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
    width_svg = 11,
    height_svg = 7)

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
    ggsave(file, m, scale = 1, width = 11, height = 7, bg = "white")
    ggsave(paste0(substring(file, 1, nchar(file) - 3), "pdf"), m, scale = 1, width = 11, height = 13, bg = "white")
    saveWidget(p, paste0(substring(file, 1, nchar(file) - 3), "html"))
  }
}
