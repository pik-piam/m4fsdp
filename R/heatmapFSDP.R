globalVariables(c("model", "scenario", "region", "period", "unit", "variable",
                  "varunit", "valuefill", "value", "label", "vargroup", ".", ".label"))
#' @title heatmapFSDP
#' @description creates a heatmap for FSDP MAgPIE runs
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param regionSel Region that should be plotted. select "IND" for India plots
#' @param tableType options: 1 (FSECa,FSECc), 2 (FSECb,FSECc,FSECd)
#' @param file file name (e.g. FSDP_heatmap.pdf or FSDP_heatmap.jpg) or NULL
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author Florian Humpenoeder, Vartika Singh
#' @import ggplot2 ggiraph forcats data.table scales htmlwidgets tidyr
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter pull select mutate
#' @importFrom utils write.csv
#' @importFrom stats reorder

heatmapFSDP <- function(repReg, regionSel = "GLO", tableType = 1, file = NULL) {

  #### read in data files
  if (tableType == 1) {
    rep <- convertReportFSDP(repReg, scengroup = c("FSECa", "FSECc"), subset = FALSE, varlist = "magpie_vars.csv")
  } else if (tableType == 2) {
    rep <- convertReportFSDP(repReg, scengroup = c("FSECb", "FSECc", "FSECd", "FSECe"), subset = FALSE,
                             varlist = "magpie_vars.csv")
  } else if (tableType == "2a") {
    rep <- convertReportFSDP(repReg, scengroup = c("FSECb", "FSECc"), subset = FALSE,
                             varlist = "magpie_vars.csv")
  } else if (tableType == 3) {
    rep <- convertReportFSDP(repReg, scengroup = c("FSECc", "FSECd", "FSECe"), subset = FALSE,
                             varlist = "magpie_vars.csv")
  } else {
    stop("Table type does not exist")
  }

  # var <- c("SDG|SDG02|Prevalence of underweight",
  #          "SDG|SDG03|Prevalence of obesity",
  #          "Health|Years of life lost|Risk|Diet and anthropometrics",
  #          "Biodiversity|BII",
  #          "Biodiversity|Shannon crop area diversity index",
  #          "Resources|Nitrogen|Nutrient surplus incl natural vegetation",
  #          "Water|Environmental flow violation volume",
  #          "Emissions|GWP100AR6|Land|Cumulative",
  #          "Global Surface Temperature",
  #          "Household Expenditure|Food|Expenditure",
  #          "Income|Number of People Below 3.20$/Day",
  #          "Agricultural employment|Crop and livestock products",
  #          "Hourly labor costs relative to 2000",
  #          "Hourly labor costs relative to 2020",
  #          "Value|Bioeconomy Demand",
  #          "Costs Without Incentives")
  #
  # names(var) <- c("Health|Prevalence of underweight (million people)|1",
  #                 "Health|Prevalence of obesity (million people)|2",
  #                 "Health|Years of life lost (million years)|3",
  #                 "Environment|Biodiversity Intactness (Index)|1",
  #                 "Environment|Shannon crop area diversity index (Index)|2",
  #                 "Environment|Nitrogen surplus (Mt N/yr)|3",
  #                 "Environment|Water environmental flow violations (km3/yr)|4",
  #                 "Environment|Cumulative GHG emissions (Gt CO2eq since 2000)|5",
  #                 "Environment|Global Surface Temperature (deg C)|6",
  #                 "Inclusion|Expenditure for agric. products (USD/person)|1",
  #                 "Inclusion|Number of People Below 3.20$/Day (million people)|2",
  #                 "Inclusion|Agricultural employment (million people)|3",
  #                 "Inclusion|Agricultural wages (Index)|4",
  #                 "Inclusion|Agricultural wages (Index)|4",
  #                 "Economy|Bioeconomy Supply (billion US$05/yr)|1",
  #                 "Economy|Costs (billion US$05/yr)|1")

  var <- getVariables()

  if (regionSel == "GLO") {
  rep[region == "World", region := "GLO"]
  }
  b <- rep[variable %in% var & region == regionSel & period == 2050, ]
  b <- droplevels(b)

  bb <- rep[variable %in% var & region == regionSel & period == 2020 & scenario == "BAU", ]
  bb <- droplevels(bb)
  b <- rbind(bb, b)

  b$variable <- factor(b$variable, levels = var, labels = names(var))
  b[, c("vargroup", "order", "variableName", "unit", "improvment", "rounding","factor") := tstrsplit(get("variable"), "|", fixed = TRUE)]
  #b[, c("vargroup", "variable", "order") := tstrsplit(variable, "|", fixed = TRUE)]
  b$order <- as.numeric(b$order)
  b$rounding <- as.numeric(b$rounding)
  b$factor <- as.numeric(b$factor)

  vargroupOrder <- c("Health", "Environment", "Inclusion", "Economy")
  b$vargroup <- factor(b$vargroup, levels = vargroupOrder)

  b[,"variable" := paste(get("variableName"),get("unit"),sep="\n")]
  b$variable <- reorder(b$variable, b$order)

  b[,"value" := get("value") * get("factor")]

  b[get("improvment") == "increase", "value" := -get("value")]
  b[, "valuefill" := get("value") - get("value")[get("scenario") == "BAU" & get("period") == "2050"], by = "variable"]


  # greying out non-nutrition scenarios
  b[!scenario %in% c("BAU", "SSP1bau", "SSP2bau", "SSP3bau", "SSP4bau", "SSP5bau",
                     "SSP1fsdp", "SSP2fsdp", "SSP3fsdp", "SSP4fsdp", "SSP5fsdp", "FSDP",
                     "NoOverweight", "HalfOverweight", "NoUnderweight", "AllHealth", "DietRotations",
                     "Population", "ExternalPressures", "AllInclusion", "Sufficiency",
                     "EconDevelop", "DietHealth") &
      get("variableName") %in% c("Underweight",
                      "Obesity"),
    c("valuefill","value") := NA]

  # greying out non-inclusion scenarios
  b[!scenario %in% c("BAU", "SSP1bau", "SSP2bau", "SSP3bau", "SSP4bau", "SSP5bau",
                     "SSP1fsdp", "SSP2fsdp", "SSP3fsdp", "SSP4fsdp", "SSP5fsdp", "FSDP",
                     "ExternalPressures", "AllInclusion", "EconDevelop", "MinWage") &
      get("variableName") %in% c("Agri. wages"),
    c("valuefill","value") := NA]

  # Adding and greying-out years of life lost for non-dietary scenarios
  tb <- as.data.frame(b)

  allScenarios <- tb %>%
    pull(.data$scenario) %>%
    unique() %>%
    as.character()

  haveYLLScenarios <- tb %>%
    filter(.data$variableName == "Years of life lost") %>%
    pull(.data$scenario) %>%
    unique() %>%
    as.character()

  nonDietaryScenarios <- setdiff(allScenarios, haveYLLScenarios)

  tb <- tb %>%
    filter(.data$scenario == "BAU",
           .data$period == "2050",
           .data$variableName %in% "Years of life lost") %>%
    select(-.data$scenario)

  tb <- tidyr::expand_grid(tb, scenario = nonDietaryScenarios) %>%
    mutate(valuefill = NA, value = NA)

  b <- rbind(b, as.data.table(tb))
  # end greying-out attributable deaths and YLL for non-dietary scenarios

  # greying out scenarios without remind
  b[scenario %in% c("SSP3bau", "SSP4bau", "SSP5bau",
                     "SSP3fsdp", "SSP4fsdp", "SSP5fsdp",
                     "Population", "EconDevelop", "TimberCities", "Bioplastics",
                     "ExternalPressures") &
      get("variableName") %in% c("Global Surface Temp"),
    c("value", "valuefill") := list(NA, NA)]

  b[, valuefill := valuefill / max(abs(valuefill), na.rm = TRUE), by = .(variable)]

  # b[variable %in% c("Costs (billion US$05/yr)"), value := value / 1000]
  # b[variable %in% c("Bioeconomy Supply (billion US$05/yr)"), value := value / 1000]
  # b[variable %in% c("Biodiversity Intactness (Index)"), value := value * 100]

  #b[, label := fifelse(max(value) > 100, formatC(value, 0, format = "f"),
  #                     formatC(value, 2, format = "f")), by = .(region, model, scenario, variable, unit, period)]
  #  b[, label := formatC(value, if (max(value) > 100) 0 else 2, format = "f"),
  #    by = .(region, model, scenario, variable, unit, period)]
  # b[, label := if (max(value,na.rm=T) > 100)
  #   formatC(value, 0, format = "f")
  #   else
  #     formatC(value, 2, format = "f"),
  #   by = .(region, model, scenario, variable, unit, period)]
  #b <-  b[,"label" := round(sum(get("value")), get("rounding"))]
  b <-  b[,"label" := round(sum(get("value")), get("rounding")), by = c("region", "scenario", "model", "variable","unit", "period")]


  #b[, label := formatC(value, 1, format = "f"), by = .(region, model, scenario, variable, unit, period)]


if (regionSel == "IND") {

  ##Dropping scenarios not relevant for India
  b <- b[scenario != "SSP1",]
  b <- b[scenario != "SSP3",]
  b <- b[scenario != "SSP4",]
  b <- b[scenario != "SSP5",]
  b <- b[scenario != "ExternalPressures",]
  b <- b[scenario != "NoUnderweight",]
  b <- b[scenario != "NoOverweight",]
  b <- b[scenario != "HalfOverweight",]
  b <- b[scenario != "LessFoodWaste",]
  b <- b[scenario != "DietVegFruitsNutsSeeds",]
  b <- b[scenario != "DietRuminants",]
  b <- b[scenario != "DietMonogastrics",]
  b <- b[scenario != "DietLegumes",]
  b <- b[scenario != "DietFish",]
  b <- b[scenario != "DietEmptyCals",]
  b <- b[scenario != "WaterSparing",]
  b <- b[scenario != "LandSparing",]
  b <- b[scenario != "BiodivSparing",]
  b <- b[scenario != "PeatlandSparing",]
  b <- b[scenario != "REDD",]
  b <- b[scenario != "REDDaff",]
  b <- b[scenario != "SoilCarbon",]
  b <- b[scenario != "CropRotations",]
  b <- b[scenario != "NitrogenUptakeEff",]
  b <- b[scenario != "LivestockMngmt",]
  b <- b[scenario != "AnimalWasteMngmt",]
  b <- b[scenario != "AirPollution",]
  b <- b[scenario != "LiberalizedTrade",]
  b <- b[scenario != "DietRotations",]
  b <- b[scenario != "FullBiodiv",]
  b <- b[scenario != "Protection",]
  b <- b[scenario != "REDDaffDietRuminants",]
  b <- b[scenario != "SoilMonogastric",]
  b <- b[scenario != "SoilRotations",]
  b <- b[scenario != "Sufficiency",]


  scenFirst <- c("SSP2 2020", "SSP2 2050")
  scenLast <- c("FSDP")

  scenCombinations <- c("WaterSoil", "Efficiency")
  scenArchetypes <- c("AllHealth", "AllEnvironment",
                      "AllClimate", "AllInclusion")
  scenOrder <- levels(fct_reorder(b$scenario, b$valuefill, sum, .desc = FALSE))
  scenMiddle <- c(scenCombinations,scenArchetypes)
  scenOrder <- c(rev(scenLast), rev(scenMiddle), scenOrder[!scenOrder %in% c(scenFirst, scenMiddle, scenLast)], rev(scenFirst))
  b$scenario <- factor(b$scenario, levels = scenOrder)
  b <- droplevels(b)

} else {

  scenFirst <- c("SSP2 2020", "SSP2 2050")
  scenExt <- c("Population", "EconDevelop", "EnergyTrans", "TimberCities", "Bioplastics")
  scenLast <- c("FSDP")
  scenSSPs <- c("SSP1bau", "SSP2bau", "SSP3bau", "SSP4bau", "SSP5bau","SSP1PLUSbau")
  scenFSTs <- c("SSP1fsdp", "SSP2fsdp", "SSP3fsdp", "SSP4fsdp", "SSP5fsdp","SSP1PLUSfsdp")
  scenDiet <- c("NoUnderweight", "HalfOverweight", "NoOverweight", "LessFoodWaste")
  scenDiet2 <- c("DietVegFruitsNutsSeeds", "DietRuminants", "DietMonogastrics",
                 "DietLegumes", "DietFish", "DietEmptyCals")
  scenProtect <- c("WaterSparing", "LandSparing", "BiodivSparing", "PeatlandSparing","REDD", "REDDaff")
  scenMngmt <- c("SoilCarbon","CropRotations", "NitrogenEff", "CropeffTax", "RiceMit", "LivestockMngmt", "ManureMngmt",
                 "AirPollution")
  scenInclusion <- c("LiberalizedTrade","MinWage")
  scenCombinations <- c("WaterSoil", "DietRotations", "SoilRotations", "SoilMonogastric",
                        "REDDaffDietRuminants", "FullBiodiv")
  scenArchetypes <- c("Sufficiency", "Efficiency", "Protection", "AllHealth", "AllEnvironment",
                      "AllClimate", "AllInclusion")

  b[scenario == "BAU", scenario := paste("SSP2", period)]
  b$period <- factor(b$period)
  b[scenario %in% scenFirst, period := "Ref"]
  b[scenario %in% c(scenDiet,scenDiet2), period := "Diet"]
  b[scenario %in% c(scenInclusion), period := "Incl."]
  b[scenario %in% c(scenProtect), period := "Protect"]
  b[scenario %in% c(scenMngmt), period := "Management"]
  b[scenario %in% scenExt, period := "Ext. Transf."]
  b[scenario %in% scenSSPs, period := "SSPs"]
  b[scenario %in% scenFSTs, period := "FSTs"]
  b[scenario %in% c(scenCombinations, scenArchetypes), period := "Food System Measure Bundles"]

  b[!scenario %in% c(scenFirst,scenExt,scenSSPs,scenFSTs,scenCombinations,scenArchetypes), period := "Food System Measures"]
  b$period <- factor(b$period)
  b <- droplevels(b)

  scenOrder <- levels(fct_reorder(b$scenario, b$valuefill, sum, .desc = FALSE))
  scenMiddle <- c(scenSSPs, scenFSTs, scenDiet2, scenDiet, scenInclusion, scenProtect, scenMngmt,
  scenCombinations, scenArchetypes)
  scenOrder <- c(rev(scenLast), rev(scenExt), rev(scenMiddle), scenOrder[!scenOrder %in% c(
    scenFirst, scenMiddle, scenExt, scenLast)], rev(scenFirst))
  b$scenario <- factor(b$scenario, levels = scenOrder)
  b <- droplevels(b)

}


  makeExp <- function(x, y) {
    exp <- vector(length = 0, mode = "expression")
    for (i in seq_along(x)) {
      if (x[i] %in% y) exp[[i]] <- bquote(bold(.(x[i])))
      else exp[[i]] <- x[i]
    }
    return(exp)
  }

  m <- ggplot(b, aes(y = scenario, x = 1)) + theme_minimal() +
    theme(panel.border = element_rect(colour = NA, fill = NA)) +
    geom_tile_interactive(aes(fill = valuefill,
                              tooltip = paste0("Scenario: ", scenario, "\nIndicator: ", variable),
                              data_id = interaction(variable)), colour = "white") +
    scale_fill_gradient2_interactive(midpoint = 0, low = "#91cf60", mid = "white",
                                     na.value = "grey95", high = "#fc8d59", breaks = c(-1, 0, 1),
                                     labels = c("positive", "zero", "negative")) +
    geom_text_interactive(aes(label = label, tooltip = paste0("Sceanrio: ", scenario, "\nIndicator: ", variable),
                              data_id = interaction(variable)), size = 2.5, color = "grey50") +
    #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(axis.text.x = element_blank()) +
    labs(y = NULL, x = NULL,
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

  m <- m + facet_nested(get("period") ~ get("vargroup") + get("variable"), scales = "free_y", space = "free_y", switch = "y",
                        strip = strip_nested(size = "variable", text_x = elem_list_text(angle = c(0, 90), face=c("bold","plain")), #text_y = elem_list_text(angle = c(90, 0)),
                                                   by_layer_x = TRUE))
    #facet_grid(vars(period), vars(vargroup), scales = "free", space = "free")# +
    #scale_x_discrete(position = "top") + theme(axis.text.x = element_text(angle = 30, hjust = 0))

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

  if (is.null(file)) {
    x <- NULL
    x[["plot"]] <- p
    b[, "value" := get("label")]
    b$unit <- NULL
    b[, c("variable", "unit") := tstrsplit(get("variable"), " (", fixed = TRUE)]
    b$unit <- substring(b$unit,1,nchar(b$unit)-1)
    setnames(b, "period", "scentype")
    #b$period <- NULL
    b[, c("scenario", "period") := tstrsplit(get("scenario"), " ", fixed = TRUE)]
    b$period <- as.numeric(b$period)
    b[is.na(get("period")), "period" := 2050]
    b <- b[, c("scenset", "scentype", "scenario", "region", "period", "vargroup", "variable", "unit", "value")]
    b$vargroup <- factor(b$vargroup, levels = vargroupOrder)
    x[["data"]] <- b
    return(x)
  } else {
    ggsave(file, m, scale = 1.2, height = 6, width = 7, bg = "white")
    ggsave(paste0(substring(file, 1, nchar(file) - 3), "pdf"), m, scale = 1.2, height = 6, width = 7, bg = "white")
    saveWidget(p, paste0(substring(file, 1, nchar(file) - 3), "html"))
  }
}
