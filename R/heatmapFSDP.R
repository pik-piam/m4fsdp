globalVariables(c("model", "scenario", "region", "period", "unit", "variable",
                  "varunit", "valuefill", "value", "label", "vargroup", ".", ".label"))
#' @title heatmapFSDP
#' @description creates a heatmap for FSDP MAgPIE runs
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param regionSel Region that should be plotted
#' @param tableType options: 1 (FSECa,FSECc), 2 (FSECb,FSECc,FSECd)
#' @param file file name (e.g. FSDP_heatmap.pdf or FSDP_heatmap.jpg) or NULL
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author Florian Humpenoeder
#' @import ggplot2 ggiraph forcats data.table scales htmlwidgets tidyr
#' @importFrom utils write.csv
#' @importFrom stats reorder

heatmapFSDP <- function(repReg, regionSel = "GLO", tableType = 1, file = NULL) {

  #### read in data files
  if (tableType == 1) {
    rep <- convertReportFSDP(repReg, scengroup = c("FSECa", "FSECc"), subset = FALSE, varlist = "magpie_vars.csv")
  } else if (tableType == 2) {
    rep <- convertReportFSDP(repReg, scengroup = c("FSECb", "FSECc", "FSECd", "FSECe"), subset = FALSE,
                             varlist = "magpie_vars.csv")
  } else {
    stop("Table type does not exist")
  }

  var <- c("SDG|SDG02|Prevalence of underweight",
           "SDG|SDG03|Prevalence of obesity",
           "Health|Attributable deaths|Risk|Diet and anthropometrics",
           "Health|Years of life lost|Risk|Diet and anthropometrics",
           "Biodiversity|BII",
           "Biodiversity|Shannon croparea diversity index",
           "Resources|Nitrogen|Nutrient surplus incl natural vegetation",
           "Water|Environmental flow violation volume",
           "Emissions|GWP100AR6|Land|Cumulative",
           "Global Surface Temperature",
           "Household Expenditure|Food|Expenditure",
           "Number of People Below 3.20$/Day",
           "Agricultural employment|Crop and livestock products",
           "Hourly labor costs relative to 2020",
           "Value|Bioeconomy Demand",
           "Costs Without Incentives")

  names(var) <- c("Health|Prevalence of underweight (million people)|1",
                  "Health|Prevalence of obesity (million people)|2",
                  "Health|Attributable deaths (million people)|3",
                  "Health|Years of life lost (million years)|4",
                  "Environment|Biodiversity Intactness (Index)|1",
                  "Environment|Shannon croparea diversity index (Index)|2",
                  "Environment|Nitrogen surplus (Mt N/yr)|3",
                  "Environment|Water environmental flow violations (km3/yr)|4",
                  "Environment|Cumulative CO2 emissions (GtCO2eq since 1995)|5",
                  "Environment|Global Surface Temperature (deg C)|6",
                  "Inclusion|Expenditure for agric. products (USD/person)|1",
                  "Inclusion|Number of People Below 3.20$/Day (million people)|2",
                  "Inclusion|Agricultural employment (million people)|3",
                  "Inclusion|Agricultural wages (Index)|4",
                  "Value|Bioeconomy Demand (billion US$05/yr)|1",
                  "Costs|Agriculture (billion US$05/yr)|1")

  rep[region == "World", region := "GLO"]
  b <- rep[variable %in% var & region == regionSel & period == 2050, ]
  b <- droplevels(b)

  bb <- rep[variable %in% var & region == regionSel & period == 2020 & scenario == "BAU", ]
  bb <- droplevels(bb)
  b <- rbind(bb, b)

  b$variable <- factor(b$variable, levels = var, labels = names(var))
  b[, c("vargroup", "variable", "order") := tstrsplit(variable, "|", fixed = TRUE)]
  b$order <- as.numeric(b$order)

  vargroupOrder <- c("Health", "Environment", "Inclusion", "Costs", "Income")
  b$vargroup <- factor(b$vargroup, levels = vargroupOrder)

  b$variable <- reorder(b$variable, b$order)

  b[, valuefill := value - value[scenario == "BAU" & period == "2050"], by = .(variable)]

  b[variable %in% c("Biodiversity Intactness (Index)",
                    "Shannon croparea diversity index (Index)",
                    "Agricultural wages (Index)",
                    "Agricultural employment (million people)"
                    ), valuefill := -valuefill]

  # greying out non-nutrition scenarios
  b[!scenario %in% c("BAU", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5", "FSDP",
                     "NoOverweight", "NoUnderweight", "AllHealth", "DietRotations",
                     "Population", "ExternalPressures", "AllInclusion",
                     "SocioEconDevelop", "DietHealth") &
      variable %in% c("Prevalence of underweight (million people)",
                      "Prevalence of obesity (million people)"),
    valuefill := NA]

  # greying out non-inclusion scenarios
  b[!scenario %in% c("BAU", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5", "FSDP",
                     "ExternalPressures", "AllInclusion", "SocioEconDevelop") &
      variable %in% c("Agricultural wages (Index)"),
    valuefill := NA]

  b[, valuefill := valuefill / max(abs(valuefill), na.rm = TRUE), by = .(variable)]

  b[variable %in% c("Agriculture (billion US$05/yr)"), value := value / 1000]
  b[variable %in% c("Bioeconomy Demand (billion US$05/yr)"), value := value / 1000]
  b[variable %in% c("Biodiversity Intactness (Index)"), value := value * 100]

  b[, label := fifelse(max(value) > 100, formatC(value, 0, format = "f"),
                       formatC(value, 2, format = "f")), by = .(region, model, scenario, variable, unit, period)]

  b[scenario == "BAU", scenario := paste("SSP2", period)]
  b$period <- factor(b$period)
  b[scenario %in% c("SSP2 2020", "SSP2 2050"), period := "Ref"]
  b[!scenario %in% c("SSP2 2020", "SSP2 2050"), period := "Scenario outcomes for 2050"]
  b$period <- factor(b$period)
  b <- droplevels(b)

  scenFirst <- c("SSP2 2020", "SSP2 2050", "Population", "SocioEconDevelop",
                 "EnergyTrans", "TimberCities", "Bioeconomy")
  scenLast <- c("FSDP")
  scenSSPs <- c("SSP1", "SSP3", "SSP4", "SSP5", "ExternalPressures")
  scenDiet <- c("NoUnderweight", "NoOverweight", "LessFoodWaste")
  scenDiet2 <- c("DietVegFruitsNutsSeeds", "DietRuminants", "DietMonogastrics",
                 "DietLegumes", "DietFish", "DietEmptyCals")
  scenProtect <- c("WaterSparing", "LandSparing", "LandUseDiversity", "PeatlandSparing")
  scenClimate <- c("REDD", "REDDaff", "SoilCarbon")
  scenMngmt <- c("CropRotations", "NitrogenUptakeEff", "LivestockMngmt", "AnimalWasteMngmt",
                 "AirPollution")
  scenInclusion <- c("FairTrade")
  scenCombinations <- c("WaterSoil", "DietRotations", "SoilRotations", "SoilMonogastric",
                        "REDDaffDietRuminants", "FullBiodiv")
  scenArchetypes <- c("Sufficiency", "Efficiency", "Protection", "AllHealth", "AllEnvironment",
                      "AllClimate", "AllInclusion")


  scenOrder <- levels(fct_reorder(b$scenario, b$valuefill, sum, .desc = FALSE))
  scenMiddle <- c(scenSSPs, scenDiet, scenDiet2, scenProtect, scenMngmt,
  scenClimate, scenInclusion, scenCombinations,
  scenArchetypes)
  scenOrder <- c(rev(scenLast), rev(scenMiddle), scenOrder[!scenOrder %in% c(
    scenFirst, scenMiddle, scenLast)], rev(scenFirst))
  b$scenario <- factor(b$scenario, levels = scenOrder)
  b <- droplevels(b)

  makeExp <- function(x, y) {
    exp <- vector(length = 0, mode = "expression")
    for (i in seq_along(x)) {
      if (x[i] %in% y) exp[[i]] <- bquote(bold(.(x[i])))
      else exp[[i]] <- x[i]
    }
    return(exp)
  }

  m <- ggplot(b, aes(y = scenario, x = variable)) + theme_minimal() +
    theme(panel.border = element_rect(colour = NA, fill = NA)) +
    geom_tile_interactive(aes(fill = valuefill,
                              tooltip = paste0("Scenario: ", scenario, "\nIndicator: ", variable),
                              data_id = interaction(variable)), colour = "white") +
    scale_fill_gradient2_interactive(midpoint = 0, low = "#91cf60", mid = "white",
                                     na.value = "grey95", high = "#fc8d59", breaks = c(-1, 0, 1),
                                     labels = c("positive", "zero", "negative")) +
    geom_text_interactive(aes(label = label, tooltip = paste0("Sceanrio: ", scenario, "\nIndicator: ", variable),
                              data_id = interaction(variable)), size = 3, color = "grey50") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(y = NULL, x = "Indicator",
         fill = bquote(atop(atop(textstyle("Impact"), textstyle("relative to")), textstyle(bold("SSP2 2050"))))) +
    theme(legend.position = "right") +
    guides(fill = guide_colorbar_interactive(mapping = aes(data_id = interaction(variable)),
                                             reverse = FALSE, title.hjust = 0, title.vjust = 2,
                                             title.position = "top", barwidth = 1, barheight = 20,
                                             legend.direction = "vertical")) +
    theme(plot.background = element_rect(fill = "white"), strip.background = element_rect(color = "grey50"),
          axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_discrete(labels = function(x) makeExp(x, "SSP2 2050"))

  m <- m + facet_grid(vars(period), vars(vargroup), scales = "free", space = "free") +
    scale_x_discrete(position = "top") + theme(axis.text.x = element_text(angle = 30, hjust = 0))

  if (is.null(file)) {
    return(m)
  } else {
    ggsave(file, m, scale = 1.2, height = 8, width = 7, bg = "white")
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
      width_svg = 10,
      height_svg = 10
    )
    saveWidget(p, paste0(substring(file, 1, nchar(file) - 3), "html"))
  }
}
