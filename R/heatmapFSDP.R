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
#' @import ggplot2 ggiraph forcats data.table scales htmlwidgets
#' @importFrom utils write.csv

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
           "Biodiversity|BII",
           "Biodiversity|Shannon croparea diversity index",
           "SDG|SDG06|Nitrogen surplus on cropland",
           "Water|Environmental flow violation volume",
           "Emissions|CO2|Land|Cumulative|+|Land-use Change",
           "GlobalSurfaceTemperature",
           "Household Expenditure|Food|Expenditure",
           "Agricultural employment",
           "Hourly labor costs relative to 2020",
           "Costs Without Incentives")

  names(var) <- c("Health|Prevalence of underweight (million people)",
                  "Health|Prevalence of obesity (million people)",
                  "Environment|Biodiversity Intactness (Index)",
                  "Environment|Shannon croparea diversity index (Index)",
                  "Environment|Nitrogen surplus on cropland (Mt N/yr)",
                  "Environment|Water environmental flow violations (km3/yr)",
                  "Environment|Cumulative CO2 emissions (GtCO2 since 1995)",
                  "Environment|Global Surface Temperature (deg C)",
                  "Inclusion|Expenditure for agric. products (USD/person)",
                  "Inclusion|Agricultural employment (million people)",
                  "Inclusion|Agricultural wages (index)",
                  "Costs|Agriculture (billion US$05/yr)")

  rep[region == "World", region := "GLO"]
  b <- rep[variable %in% var & region == regionSel & period == 2050, ]
  b <- droplevels(b)

  bb <- rep[variable %in% var & region == regionSel & period == 2020 & scenario == "BAU", ]
  bb <- droplevels(bb)
  b <- rbind(bb, b)

  b$variable <- factor(b$variable, levels = var, labels = names(var))
  b[, c("vargroup", "variable") := tstrsplit(variable, "|", fixed = TRUE)]

  vargroupOrder <- c("Health", "Environment", "Inclusion", "Costs")
  b$vargroup <- factor(b$vargroup, levels = vargroupOrder)

  b[, valuefill := value - value[scenario == "BAU" & period == "2050"], by = .(variable)]
  b[variable %in% c("Biodiversity Intactness (Index)",
                    "Shannon croparea diversity index (Index)",
                    "Agricultural wages (index)",
                    "Agricultural employment (million people)"
                    ), valuefill := -valuefill]
  b[valuefill > 0, valuefill := rescale(valuefill, to = c(0, 1)), by = .(variable)]
  b[valuefill < 0, valuefill := rescale(valuefill, to = c(-1, 0)), by = .(variable)]

  # greying out nutrition scenarios
  b[!scenario %in% c("BAU", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5", "FSDP",
                     "NoOverweight", "NoUnderweight", "AllHealth", "DietRotations",
                     "Population", "ExternalPressures", "AllInclusion",
                     "SocioEconDevelop", "DietHealth") &
      variable %in% c("Prevalence of underweight (million people)",
                      "Prevalence of obesity (million people)"),
    valuefill := NA]

  # greying out inclusion scenarios
  b[!scenario %in% c("BAU", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5", "FSDP",
                     "ExternalPressures", "AllInclusion", "SocioEconDevelop") &
      variable %in% c("Agricultural wages (index)"),
    valuefill := NA]

  b[variable %in% c("Agriculture (billion US$05/yr)"), value := value / 1000]
  b[variable %in% c("Biodiversity Intactness (Index)"), value := value * 100]

  b[, label := fifelse(max(value) > 100, formatC(value, 0, format = "f"),
                       formatC(value, 2, format = "f")), by = .(region, model, scenario, variable, unit, period)]

  b[scenario == "BAU", scenario := paste("SSP2", period)]
  b <- droplevels(b)
  scenFirst <- c("SSP2 2020", "SSP2 2050", "Population", "SocioEconDevelop", "EnergyTrans", "Bioeconomy")
  scenLast <- c("SDP")
  scenSSPs <- c("SSP1", "SSP3", "SSP4", "SSP5", "ExternalPressures")
  scenDiet <- c("NoUnderweight", "NoOverweight", "LessFoodWaste")
  scenDiet2 <- c("DietVegFruitsNutsSeeds", "DietRuminants", "DietMonogastrics", "DietLegumes","DietFish","DietEmptyCals")
  scenProtect <- c("WaterSparing", "LandSparing", "PeatlandSparing", "LandUseDiversity")
  scenClimate <- c("SoilCarbon","REDD","REDDaff")
  scenEff <- c("NitrogenUptakeEfficiency","LivestockMngmt","AnimalWasteMngmt","AirPollution","CropRotations")
  scenInclusion <- c("TimberCities","FairTrade")
  scenCombinations <- c("WaterSoil","DietRotations","SoilRotations","SoilMonogastric","REDDaffDietRuminants","FullBiodiv")
  scenArchetypes <- c("Sufficiency","Efficiency","Protection","AllHealth","AllEnvironment","AllClimate","AllInclusion")


  scenOrder <- levels(fct_reorder(b$scenario, b$valuefill, sum, .desc = FALSE))
  scenMiddle <- c(scenSSPs,scenDiet,scenDiet2,scenProtect,
  scenClimate,scenEff,scenInclusion,scenCombinations,
  scenArchetypes)
  scenOrder <- c(rev(scenLast), rev(scenMiddle), scenOrder[!scenOrder %in% c(
    scenFirst,scenMiddle,scenLast)], rev(scenFirst))
  b$scenario <- factor(b$scenario, levels = scenOrder)
  b <- droplevels(b)

  m <- ggplot(b, aes(y = scenario, x = variable)) + theme_minimal(base_family = "Arial") +
    theme(panel.border = element_rect(colour = NA, fill = NA)) +
    geom_tile_interactive(aes(fill = valuefill,
                              tooltip = paste0("Scenario: ", scenario, "\nIndicator: ", variable),
                              data_id = interaction(variable)), colour = "white") +
    scale_fill_gradient2_interactive(midpoint = 0, low = "#91cf60", mid = "#ffffbf",
                                     na.value = "grey80", high = "#fc8d59", breaks = c(-1, 0, 1),
                                     labels = c("positive", "zero", "negative")) +
    geom_text_interactive(aes(label = label, tooltip = paste0("Sceanrio: ", scenario, "\nIndicator: ", variable),
                              data_id = interaction(variable)), size = 3, color = "grey50") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(y = "Scenario", x = "Indicator", fill = "Impact\nrelative to\nSSP2 2050") +
    theme(legend.position = "right") +
    guides(fill = guide_colorbar_interactive(mapping = aes(data_id = interaction(variable)),
                                             reverse = FALSE, title.hjust = 0, title.vjust = 2,
                                             title.position = "top", barwidth = 1, barheight = 20,
                                             legend.direction = "vertical")) +
    theme(plot.background = element_rect(fill = "white"), strip.background = element_rect(color = "grey50"),
          axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

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
