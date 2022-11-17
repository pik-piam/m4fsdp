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
           # "Health|Attributable deaths|Risk|Diet and anthropometrics",
           # "Health|Years of life lost|Risk|Diet and anthropometrics",
           "Biodiversity|BII",
           #           "Biodiversity|Shannon croparea diversity index",
           #           "Resources|Nitrogen|Nutrient surplus incl natural vegetation",
           "Water|Environmental flow violation volume",
           "Emissions|GWP100AR6|Land|Cumulative",
           "Global Surface Temperature",
           "Household Expenditure|Food|Expenditure",
           #           "Number of People Below 3.20$/Day",
           "Agricultural employment|Crop and livestock products",
           "Hourly labor costs relative to 2020",
           "Value|Bioeconomy Demand",
           "Costs Without Incentives")

  names(var) <- c("Health|Underweight\nmio people|1|decrease|0",
                  "Health|Obesity\nmio people|2|decrease|0",
                  # "Health|Attributable deaths (million people)|3",
                  # "Health|Years of life lost (million years)|4",
                  "Environment|Biodiversity\nBII|1|increase|2",
                  #                  "Environment|Croparea diversity|Shannon|2|increase|2",
                  #                  "Environment|Nitrogen surplus (Mt N/yr)|3",
                  "Environment|Water flow violations\nkm3/yr|4|decrease|0",
                  "Environment|Cum CO2 emissions\nGtCO2eq since 1995|5|decrease|0",
                  "Environment|Global Surface Temp\ndeg C|6|decrease|2",
                  "Inclusion|Cost agric. products\nUSD/person|1|decrease|0",
                  #                  "Inclusion|People Below 3.20$/Day\nmio people|2|decrease|0",
                  "Inclusion|Agri. employment\nmio people|3|increase|0",
                  "Inclusion|Agri. wages\nIndex|4|increase|0",
                  "Economy|Bioeconomy Supply\nbillion US$05/yr|1|increase|0",
                  "Economy|Costs\nbillion US$05/yr|1|decrease|0")

  levels(rep$region)[levels(rep$region)=="World"] <- "GLO"
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

  colors <- setNames(c("#C29511", "#4499FF", "#BD36FF", "#FC8014", "#26AD4C"),
                     c("single1", "single2", "single3", "single4", "bundle"))

  selBundle <- function(b, bundle, single1, single2, single3 = NULL, single4 = NULL, bundleOrder = "A", colors) {
    x <- b[get("scenario") %in% c(bundle, single1, single2, single3, single4), ]
    if (!is.null(single3)) {
      single3Label <- paste0("<span style='color: ", colors["single3"], "'>", single3, "</span>", "<br>")
    } else {
      single3Label <- NULL
    }
    if (!is.null(single4)) {
      single4Label <- paste0("<span style='color: ", colors["single4"], "'>", single4, "</span>", "<br>")
    } else {
      single4Label <- NULL
    }
    x[, "bundle" := paste0("<span style='color: ", colors["single1"], "'>", single1, "</span>", "<br>",
                      "<span style='color: ", colors["single2"], "'>", single2, "</span>", "<br>",
                      single3Label,
                      single4Label,
                      "<span style='color: ", colors["bundle"], "'>", "bundle", "</span>")]
    x[, "bundleOrder" := bundleOrder]
    x[get("scenario") %in% single1, "scenCol" := "single1"]
    x[get("scenario") %in% single2, "scenCol" := "single2"]
    x[get("scenario") %in% single3, "scenCol" := "single3"]
    x[get("scenario") %in% single4, "scenCol" := "single4"]
    x[get("scenario") %in% bundle, "scenCol" := "bundle"]
    x[get("scenario") %in% c(single1, single2, single3, single4), "scenset" := "FSECa"]
    x[get("scenario") %in% bundle, "scenset" := "FSECb"]

    return(x)
  }

  x <- selBundle(b, "ExternalPressures", "Population", "EconDevelop", "EnergyTrans", "Bioplastics",
                 bundleOrder = 1, colors = colors)
  x <- rbind(x, selBundle(b, "WaterSoil", "WaterSparing", "SoilCarbon", bundleOrder = 2, colors = colors))
  x <- rbind(x, selBundle(b, "REDDaffRuminants", "DietRuminants", "REDDaff", bundleOrder = 3, colors = colors))
  x <- rbind(x, selBundle(b, "DietRotations", "AllHealth", "CropRotations", bundleOrder = 4, colors = colors))
  x <- rbind(x, selBundle(b, "MonogastricsRotations", "DietMonogastrics", "CropRotations",
                         bundleOrder = 5, colors = colors))
  x <- rbind(x, selBundle(b, "TradeRotations", "FairTrade", "CropRotations", bundleOrder = 6, colors = colors))
  x <- rbind(x, selBundle(b, "TradeREDDaff", "FairTrade", "REDDaff", bundleOrder = 7, colors = colors))
  x <- rbind(x, selBundle(b, "TradeSoil", "FairTrade", "SoilCarbon", bundleOrder = 8, colors = colors))
  x <- rbind(x, selBundle(b, "TradeMonogastrics", "FairTrade", "DietMonogastrics", bundleOrder = 9, colors = colors))
  x <- rbind(x, selBundle(b, "TradeRuminants", "FairTrade", "DietRuminants", bundleOrder = 10, colors = colors))
  x <- rbind(x, selBundle(b, "TradeVeggies", "FairTrade", "DietVegFruitsNutsSeeds",
                          bundleOrder = 11, colors = colors))
  x <- rbind(x, selBundle(b, "MonogastricsVeggies", "DietMonogastrics", "DietVegFruitsNutsSeeds",
                         bundleOrder = 12, colors = colors))
  x <- rbind(x, selBundle(b, "SoilMonogastric", "SoilCarbon", "DietMonogastrics", bundleOrder = 13, colors = colors))
  x <- rbind(x, selBundle(b, "SoilMonogastricRuminants", "SoilCarbon", "DietMonogastrics", "DietRuminants",
                         bundleOrder = 14, colors = colors))
  x <- rbind(x, selBundle(b, "SoilRotations", "SoilCarbon", "CropRotations", bundleOrder = 15, colors = colors))
  x <- rbind(x, selBundle(b, "LivestockManureMngmt", "LivestockMngmt", "ManureMngmt",
                          bundleOrder = 16, colors = colors))
  x <- rbind(x, selBundle(b, "LivestockNUEMngmt", "LivestockMngmt", "NitrogenEff",
                          bundleOrder = 17, colors = colors))

  b <- x
  b$bundle <- factor(b$bundle)
  b$bundleOrder <- factor(b$bundleOrder)
  b$bundle <- fct_reorder(b$bundle, as.integer(b$bundleOrder))


  b[, "bundlesum" := sum(get("value"), na.rm = TRUE), by = list(get("variable"), get("bundle"), get("scenset"))]
  b[, "valuefill" := list(get("value") / max(max(abs(get("value")), na.rm = TRUE),
                                             max(abs(get("bundlesum")), na.rm = TRUE),
                                      na.rm = TRUE)), by = list(get("variable"))]

  bSum <- b[, list("label" = if (max(abs(get("valuefill")) > 0.5, na.rm = TRUE))
    round(sum(get("value")), get("rounding"))
    else NULL, valuefill = sum(get("valuefill"), na.rm = TRUE) / 2),
             by = c("region", "model", "variable", "unit", "vargroup", "period",
                    "scenset", "bundle", "bundleOrder", "rounding")]
  bSum <- droplevels(bSum)
  b <- droplevels(b)

  plotBundle2 <- function(plotData) {
    set.seed(42)
    plotData <- droplevels(plotData[scenset %in% c("FSECa", "FSECb"), ])
    p <- ggplot(plotData, aes(x = get("valuefill"), y = reorder(get("scenset"), dplyr::desc(get("scenset"))))) +
      theme_minimal() + theme(panel.border = element_rect(colour = NA, fill = NA)) +
      facet_nested(get("bundle") ~ get("vargroup") + get("variable"), scales = "free", space = "free", switch = "y",
                   strip = strip_nested(size = "variable", text_x = elem_list_text(angle = c(0, 90)),
                                        by_layer_x = TRUE)) +
      geom_vline(xintercept = 0) +
      geom_bar_interactive(aes(fill = get("scenCol"),
                               tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ", round(get("value"), get("rounding"))),
                               data_id = get("variable")), position = "stack", stat = "identity") +
      geom_bar_interactive(data = plotData[get("scenset") == "FSECb" & get("improvment") == "increase" & value > 0, ],
                           mapping = aes(tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))), data_id = get("variable")),
                           position = "stack", stat = "identity", fill = "#26AD4C", size = 0) +
      geom_bar_interactive(data = plotData[get("scenset") == "FSECb" & get("improvment") == "increase" & value < 0, ],
                           mapping = aes(tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))), data_id = get("variable")),
                           position = "stack", stat = "identity", fill = "#AD1515", size = 0) +
      geom_bar_interactive(data = plotData[get("scenset") == "FSECb" & get("improvment") == "decrease" & value > 0, ],
                           mapping = aes(tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))), data_id = get("variable")),
                           position = "stack", stat = "identity", fill = "#AD1515", size = 0) +
      geom_bar_interactive(data = plotData[get("scenset") == "FSECb" & get("improvment") == "decrease" & value < 0, ],
                           mapping = aes(tooltip = paste0("Scenario: ", get("scenario"), "\nValue: ",
                                                round(get("value"), get("rounding"))), data_id = get("variable")),
                           position = "stack", stat = "identity", fill = "#26AD4C", size = 0) +
      geom_text(data = bSum[get("scenset") %in% c("FSECa", "FSECb"), ], aes(label = get("label")),
                size = 2.5, colour = "white", angle = 0) +
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
  #ggsave("SingleBundle2.png", m, width = 11, height = 13, scale = 1)

  if (is.null(file)) {
    return(m)
  } else {
    ggsave(file, m, scale = 1, width = 11, height = 13, bg = "white")
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
      height_svg = 13
    )
    saveWidget(p, paste0(substring(file, 1, nchar(file) - 3), "html"))
  }
}
