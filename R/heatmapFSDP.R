globalVariables(c("model", "scenario", "region", "period", "unit", "variable",
                  "varunit", "valuefill", "value", "label", "vargroup", ".", ".label"))
#' @title heatmapFSDP
#' @description creates a heatmap for FSDP MAgPIE runs
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param regionSel Region that should be plotted. select "IND" for India plots
#' @param tableType options: 1, 2, 3
#' @param file file name (e.g. FSDP_heatmap.pdf or FSDP_heatmap.jpg) or NULL
#' @param width width
#' @param height height
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author Florian Humpenoeder, Vartika Singh
#' @import ggplot2 ggiraph forcats data.table scales htmlwidgets tidyr
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter pull select mutate
#' @importFrom utils write.csv
#' @importFrom stats reorder
#' @importFrom ggtext element_markdown

heatmapFSDP <- function(repReg, regionSel = "GLO", tableType = 1, file = NULL, width = 10.5, height = 9) {

  #### read in data files
  rep <- convertReportFSDP(repReg, scengroup = c("FSECa", "FSECb", "FSECc","FSECd", "FSECe"), subset = FALSE, varlist = "magpie_vars.csv")

  #needed for some nitrogen variables
  levels(rep$region)[levels(rep$region) == "World"] <- "GLO"

  #get variable list
  var <- getVariables(levels(rep$variable))

  #sub-setting variables, regions and years
  b <- rep[variable %in% var & region == regionSel & period == 2050, ]
  b <- droplevels(b)

  if (tableType %in% c(2,3)) {
    bb <- rep[variable %in% var & region == regionSel & period == 2020 & scenario == "BAU", ]
    bb <- droplevels(bb)
    b <- rbind(bb, b)
  }

  #rename variables
  b$variable <- factor(b$variable, levels = var, labels = names(var))
  b[, c("vargroup", "order", "variableName", "unit", "improvment", "rounding","factor") := tstrsplit(get("variable"), "|", fixed = TRUE)]
  b$order <- as.numeric(b$order)
  b$rounding <- as.numeric(b$rounding)
  b$factor <- as.numeric(b$factor)

  vargroupOrder <- c("Health", "Environment", "Inclusion", "Economy")
  b$vargroup <- factor(b$vargroup, levels = vargroupOrder)

  #b[,"variable" := paste0("atop(textstyle('",get("variableName"),"'),textstyle('",get("unit"),"'))")]
  b[,"variable" := paste(get("variableName"),get("unit"),sep="\n")]
  b$variable <- reorder(b$variable, b$order)

  b[,"value" := get("value") * get("factor")]

  b[, "valuefill" := get("value") - get("value")[get("scenario") == "BAU" & get("period") == "2050"], by = "variable"]
  b[get("variableName") == "People Below 3.20$/Day", "valuefill" := ifelse(get("valuefill") < -100,-100,get("valuefill"))]
  b[get("variableName") == "People Below 3.20$/Day", "valuefill" := ifelse(get("valuefill") > 100,100,get("valuefill"))]

  b[get("improvment") == "increase", "valuefill" := -get("valuefill")]


  # greying out non-nutrition scenarios
  b[!scenario %in% c("BAU", "SSP1bau", "SSP2bau", "SSP3bau", "SSP4bau", "SSP5bau",
                     "SSP1fsdp", "SSP2fsdp", "SSP3fsdp", "SSP4fsdp", "SSP5fsdp", "FSDP",
                     "NoOverweight", "HalfOverweight", "NoUnderweight", "AllHealth", "DietRotations",
                     "Population", "ExternalPressures", "AllInclusion", "Diet",
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
    select(-"scenario")

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

  b <-  b[,"label" := round(sum(get("value")), get("rounding")), by = c("region", "scenario", "model", "variable","unit", "period")]

  b[scenario == "BAU", scenario := paste("SSP2", period)]

  #scneario selection, grouping and ordering
  if (tableType == 1) {

    #scennameInDataframe::scenGroup|scennameInPlot
    #order is maintained
    scenGrouping <-
      c("SSP2 2050::A|<b>Ref SSP2 2050</b>",
        "Diet::Diets|<b>Diets</b>",
        "DietEmptyCals::Diets|LowEmptyCals",
        "DietFish::Diets|BalancedFish",
        "DietLegumes::Diets|HighLegumes",
        "DietMonogastrics::Diets|LowMonogastrics",
        "DietRuminants::Diets|LowRuminants",
        "DietVegFruitsNutsSeeds::Diets|HighVegFruitsNuts",
        "NoOverweight::Diets|NoOverweight",
        "HalfOverweight::Diets|HalfOverweight",
        "NoUnderweight::Diets|NoUnderweight",
        "LessFoodWaste::Diets|LowFoodWaste",
        "Livelihoods::Livelihoods|<b>Livelihoods</b>",
        "LiberalizedTrade::Livelihoods|LibTrade",
        "MinWage::Livelihoods|MinWage",
        "CapitalSubst::Livelihoods|CapitalSubst",
        "NatureSparing::Biosphere|<b>Biosphere</b>",
        "REDDaff::Biosphere|REDD+",
#        "REDD::NatureSparing|REDD",
        "LandSparing::Biosphere|LandSparing",
        "PeatlandSparing::Biosphere|PeatlandSparing",
        "WaterSparing::Biosphere|WaterSparing",
        "BiodivSparing::Biosphere|BiodivSparing",
        "AgroMngmt::Agriculture|<b>Agriculture</b>",
        "NitrogenEff::Agriculture|NitrogenEff",
        "CropRotations::Agriculture|CropRotations",
        "LandscapeElements::Agriculture|LandscapeElements",
        "RiceMit::Agriculture|RiceMitigation",
        "LivestockMngmt::Agriculture|LivestockMngmt",
        "ManureMngmt::Agriculture|ManureMngmt",
        "SoilCarbon::Agriculture|SoilCarbon",
        "ExternalPressures::ExtTransformation|<b>ExtTransformation</b>",
        "Population::ExtTransformation|Population",
        "EconDevelop::ExtTransformation|EconDevelop",
        "EnergyTrans::ExtTransformation|EnergyTrans",
        "Bioplastics::ExtTransformation|Bioplastics",
        "TimberCities::ExtTransformation|TimberCities",
        "SSP2fsdp::Z|allFSMs",
        "FSDP::Z|<b>FSDP</b>")

    legendPosition <- theme(legend.position = c(-0.08, 1.12), plot.margin = margin(5, 5, 5, 5, "pt"))

  } else if (tableType == 2) {

    #scennameInDataframe::scenGroup|scennameInPlot
    #order is maintained
    scenGrouping <-
      c("SSP2 2020::Ref|SSP2 2020",
        "SSP2 2050::Ref|<b>SSP2 2050</b>",
        "AgroMngmt::FSMs|Agriculture",
        "AllNitrogen::FSMs|AllNitrogen",
        "Bioeconomy::FSMs|Bioeconomy",
        "ExternalPressures::FSMs|ExternalPressures",
        "Livelihoods::FSMs|Livelihoods",
        "LivelihoodsExt::FSMs|LivelihoodsExt",
        "LivestockManureMngmt::FSMs|LivestockManureMngmt",
        "LivestockNUEMngmt::FSMs|LivestockNUEMngmt",
        "MonogastricsRotations::FSMs|MonogastricsRotations",
        "MonogastricsVeggies::FSMs|MonogastricsVeggies",
        "NatureSparing::FSMs|NatureSparing",
        "REDDaffRuminants::FSMs|REDDaffRuminants",
        "SoilMonogastricRuminants::FSMs|SoilMonogastricRuminants",
        "TradeMonogastrics::FSMs|TradeMonogastrics",
        "TradeREDDaff::FSMs|TradeREDDaff",
        "TradeRotations::FSMs|TradeRotations",
        "TradeRuminants::FSMs|TradeRuminants",
        "TradeSoil::FSMs|TradeSoil",
        "TradeVeggies::FSMs|TradeVeggies",
        "AllInclusion::FSM Bundles|AllInclusion",
        "AllClimate::FSM Bundles|AllClimate",
        "AllEnvironment::FSM Bundles|AllEnvironment",
        "AllHealth::FSM Bundles|AllHealth",
        "Protection::FSM Bundles|Protection",
        "Efficiency::FSM Bundles|Efficiency",
        "Diet::FSM Bundles|Diets",
        "FullBiodiv::FSM Bundles|FullBiodiv",
        "SoilMonogastric::FSM Bundles|SoilMonogastric",
        "SoilRotations::FSM Bundles|SoilRotations",
        "DietRotations::FSM Bundles|DietRotations",
        "WaterSoil::FSM Bundles|WaterSoil",
        "FSDP::Z|<b>FSDP</b>")

    legendPosition <- theme(legend.position = c(-0.10, 1.12), plot.margin = margin(5, 5, 5, 5, "pt"))

  } else if (tableType == 3) {

    #scennameInDataframe::scenGroup|scennameInPlot
    #order is maintained
    scenGrouping <-
      c("SSP2 2020::Ref|SSP2 2020",
        "SSP2 2050::Ref|<b>SSP2 2050</b>",
        "SSP1bau::SSPs|SSP1bau",
        "SSP3bau::SSPs|SSP3bau",
        "SSP4bau::SSPs|SSP4bau",
        "SSP5bau::SSPs|SSP5bau",
        "FSDP::Z|<b>FSDP</b>")

    legendPosition <- theme(legend.position = c(-0.05, 1.35), plot.margin = margin(5, 5, 5, 5, "pt"))

  }

  a <- strsplit(scenGrouping,"\\::")
  scenGroup <- unlist(lapply(a,function(x) x[1]))
  names(scenGroup) <- unlist(lapply(a,function(x) x[2]))

  b <- b[get("scenario") %in% scenGroup,]
  b <- droplevels(b)
  scenGroup <- scenGroup[scenGroup %in% intersect(scenGroup,levels(b$scenario))]
  b$scenario <- factor(b$scenario,levels = scenGroup,labels = names(scenGroup))

  b[, c("scenGroup","scenario") := tstrsplit(get("scenario"), "\\|")]

  scenGroupOrder <- unique(tstrsplit(names(scenGroup),"\\|")[[1]])
  scenarioOrder <- tstrsplit(names(scenGroup),"\\|")[[2]]

  stripBackground <- vector(length = length(scenGroupOrder))
  textElement <- vector(length = length(scenGroupOrder))

  for (i in 1:length(scenGroupOrder)) {
    if (scenGroupOrder[i] %in% c("A","Z")) {
      stripBackground[i] <- list(element_blank())
      textElement[i] <- list(element_blank())
    } else {
      stripBackground[i] <- list(NULL)
      textElement[i] <- list(element_text(angle = 90, face = "bold"))
    }
  }

  b$scenGroup <- factor(b$scenGroup,levels = scenGroupOrder)
  b$scenario <- factor(b$scenario,levels = scenarioOrder,ordered = TRUE)

  b[, valuefill := valuefill / max(abs(valuefill), na.rm = TRUE), by = .(variable)]

### todo: DELETE once scenario selection is approved

# if (regionSel == "IND") {
#
#   ##Dropping scenarios not relevant for India
#   b <- b[scenario != "SSP1",]
#   b <- b[scenario != "SSP3",]
#   b <- b[scenario != "SSP4",]
#   b <- b[scenario != "SSP5",]
#   b <- b[scenario != "ExternalPressures",]
#   b <- b[scenario != "NoUnderweight",]
#   b <- b[scenario != "NoOverweight",]
#   b <- b[scenario != "HalfOverweight",]
#   b <- b[scenario != "LessFoodWaste",]
#   b <- b[scenario != "DietVegFruitsNutsSeeds",]
#   b <- b[scenario != "DietRuminants",]
#   b <- b[scenario != "DietMonogastrics",]
#   b <- b[scenario != "DietLegumes",]
#   b <- b[scenario != "DietFish",]
#   b <- b[scenario != "DietEmptyCals",]
#   b <- b[scenario != "WaterSparing",]
#   b <- b[scenario != "LandSparing",]
#   b <- b[scenario != "BiodivSparing",]
#   b <- b[scenario != "PeatlandSparing",]
#   b <- b[scenario != "REDD",]
#   b <- b[scenario != "REDDaff",]
#   b <- b[scenario != "SoilCarbon",]
#   b <- b[scenario != "CropRotations",]
#   b <- b[scenario != "NitrogenUptakeEff",]
#   b <- b[scenario != "LivestockMngmt",]
#   b <- b[scenario != "AnimalWasteMngmt",]
#   b <- b[scenario != "AirPollution",]
#   b <- b[scenario != "LiberalizedTrade",]
#   b <- b[scenario != "DietRotations",]
#   b <- b[scenario != "FullBiodiv",]
#   b <- b[scenario != "Protection",]
#   b <- b[scenario != "REDDaffDietRuminants",]
#   b <- b[scenario != "SoilMonogastric",]
#   b <- b[scenario != "SoilRotations",]
#   b <- b[scenario != "Diet",]
#
#
#   scenFirst <- c("SSP2 2020", "SSP2 2050")
#   scenLast <- c("FSDP")
#
#   scenCombinations <- c("WaterSoil", "Efficiency")
#   scenArchetypes <- c("AllHealth", "AllEnvironment",
#                       "AllClimate", "AllInclusion")
#   scenOrder <- levels(fct_reorder(b$scenario, b$valuefill, sum, .desc = FALSE))
#   scenMiddle <- c(scenCombinations,scenArchetypes)
#   scenOrder <- c(rev(scenLast), rev(scenMiddle), scenOrder[!scenOrder %in% c(scenFirst, scenMiddle, scenLast)], rev(scenFirst))
#   b$scenario <- factor(b$scenario, levels = scenOrder)
#   b <- droplevels(b)
#
# } else {
#
#   scenFirst <- c("SSP2 2020", "SSP2 2050")
#   scenExt <- c("Population", "EconDevelop", "EnergyTrans", "TimberCities", "Bioplastics")
#   scenLast <- c("FSDP")
#   scenSSPs <- c("SSP1bau", "SSP2bau", "SSP3bau", "SSP4bau", "SSP5bau","SSP1PLUSbau")
#   scenFSTs <- c("SSP1fsdp", "SSP2fsdp", "SSP3fsdp", "SSP4fsdp", "SSP5fsdp","SSP1PLUSfsdp")
#   scenDiet <- c("NoUnderweight", "HalfOverweight", "NoOverweight", "LessFoodWaste")
#   scenDiet2 <- c("DietVegFruitsNutsSeeds", "DietRuminants", "DietMonogastrics",
#                  "DietLegumes", "DietFish", "DietEmptyCals")
#   scenProtect <- c("WaterSparing", "LandSparing", "BiodivSparing", "PeatlandSparing","REDD", "REDDaff")
#   scenMngmt <- c("SoilCarbon","CropRotations", "NitrogenEff", "CropeffTax", "RiceMit", "LivestockMngmt", "ManureMngmt",
#                  "AirPollution")
#   scenInclusion <- c("LiberalizedTrade","MinWage")
#   scenCombinations <- c("WaterSoil", "DietRotations", "SoilRotations", "SoilMonogastric",
#                         "REDDaffDietRuminants", "FullBiodiv")
#   scenArchetypes <- c("Sufficiency", "Efficiency", "Protection", "AllHealth", "AllEnvironment",
#                       "AllClimate", "AllInclusion")
#
#   b[scenario == "BAU", scenario := paste("SSP2", period)]
#   b$period <- factor(b$period)
#   b[!scenario %in% c(scenFirst,scenExt,scenSSPs,scenFSTs,scenCombinations,scenArchetypes), period := "FSMs"]
#   b[scenario %in% scenFirst, period := "Ref"]
#   b[scenario %in% c(scenDiet,scenDiet2), period := "Diet"]
#   b[scenario %in% c(scenInclusion), period := "Incl."]
#   b[scenario %in% c(scenProtect), period := "NatureSparing"]
#   b[scenario %in% c(scenMngmt), period := "AgroMngmt"]
#   b[scenario %in% scenExt, period := "Ext. Transf."]
#   b[scenario %in% scenSSPs, period := "SSPs"]
#   b[scenario %in% scenFSTs, period := "FSTs"]
#   b[scenario %in% c(scenCombinations, scenArchetypes), period := "Food System Measure Bundles"]
#
#   b$period <- factor(b$period)
#   b <- droplevels(b)
#
#   scenOrder <- levels(fct_reorder(b$scenario, b$valuefill, sum, .desc = FALSE))
#   scenMiddle <- c(scenSSPs, scenFSTs, scenDiet2, scenDiet, scenInclusion, scenProtect, scenMngmt,
#   scenCombinations, scenArchetypes)
#   scenOrder <- c(rev(scenLast), rev(scenExt), rev(scenMiddle), scenOrder[!scenOrder %in% c(
#     scenFirst, scenMiddle, scenExt, scenLast)], rev(scenFirst))
#   b$scenario <- factor(b$scenario, levels = scenOrder)
#   b <- droplevels(b)
#
# }


  makeExp <- function(x, y) {
    exp <- vector(length = 0, mode = "expression")
    for (i in seq_along(x)) {
      if (x[i] %in% y) exp[[i]] <- bquote(bold(.(x[i])))
      else exp[[i]] <- x[i]
    }
    return(exp)
  }

  #https://rworkshop.uni.lu/lectures/plotting_part2.html#37
  #https://github.com/andrewheiss/ath-hugo/blob/main/content/blog/2022-05-09_hurdle-lognormal-gaussian-brms/index.Rmarkdown
  #https://github.com/wilkelab/ggtext/issues/82

  m <- ggplot(b, aes(y = get("scenario"), x = 1)) + theme_minimal() +
    theme(panel.border = element_rect(colour = NA, fill = NA)) +
    geom_tile_interactive(aes(fill = valuefill,
                              tooltip = paste0("Scenario: ", scenario, "\nIndicator: ", variable),
                              data_id = interaction(variable)), colour = "white") +
    scale_fill_gradient2_interactive(midpoint = 0, low = "#91cf60", mid = "white",
                                     na.value = "grey95", high = "#fc8d59", breaks = c(-1, -0.5, 0, 0.5, 1), limit = c(-1, 1),
                                     labels = c("best", "better", "none", "worse", "worst")) +
    geom_text_interactive(aes(label = label, tooltip = paste0("Sceanrio: ", scenario, "\nIndicator: ", variable),
                              data_id = interaction(variable)), size = 3, color = "grey50") +
    #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(axis.text.x = element_blank()) +
    labs(y = NULL, x = NULL, fill = "Relative change") +
#    labs(y = NULL, x = NULL, fill = "Effect<br>relative<br>to<br><b>Ref<br>SSP2<br>2050</b>") +
         #fill = bquote(atop("Effect\nrelative to\n",bold("SSP2\n2050")))) +
    theme(legend.position = "right") +
    guides(fill = guide_colorbar_interactive(mapping = aes(data_id = interaction(variable)),
                                             reverse = TRUE, title.hjust = 1, #title.vjust = 2,
                                             title.position = "left", barwidth = 1, barheight = 6,
                                             legend.direction = "horizontal")) +
    theme(plot.background = element_rect(fill = "white"), strip.background = element_rect(color = "grey50"),
          axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + scale_y_discrete(limits=rev)
          #scale_y_discrete(labels = function(x) makeExp(x, "SSP2 2050")))

  m <- m + facet_nested(get("scenGroup") ~ get("vargroup") + get("variable"), scales = "free_y", space = "free_y", switch = "y",
                        strip = strip_nested(size = "variable", text_x = elem_list_text(angle = c(0, 90), face=c("bold","plain")),
                                             #text_y = elem_list_text(angle = c(90), face=c("bold")),#text_y = elem_list_text(angle = c(90, 0)),
                                             text_y = textElement,
                                             background_y = stripBackground,
                                             by_layer_x = TRUE, by_layer_y = FALSE))
  m <- m + theme(axis.text.y.left = ggtext::element_markdown(),strip.placement = "outside")
  m <- m + theme(legend.title = element_markdown(hjust = 0, size=9, angle = 90, face = "bold"), legend.text.align = 0,legend.title.align = 1)#legend.background = element_rect(colour = "black", size = 0.5)
  m <- m + legendPosition
  #if (tableType == 3) m <- m + theme(plot.margin = margin(0, 0, 0, 25, "pt"))
  #ggsave(file, m, scale = 1.2, height = 6, width = 7, bg = "white")

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
    width_svg = width,
    height_svg = height
  )
  #ggsave(file, m, scale = 1, height = height, width = width, bg = "white")

  if (is.null(file)) {
    x <- NULL
    x[["plot"]] <- p
    b[, "value" := get("label")]
    b$unit <- NULL
    b[, c("variable", "unit") := tstrsplit(get("variable"), " (", fixed = TRUE)]
    b$unit <- substring(b$unit,1,nchar(b$unit)-1)
    b <- b[, c("scenGroup", "scenario", "region", "period", "vargroup", "variable", "unit", "value")]
    b$vargroup <- factor(b$vargroup, levels = vargroupOrder)
    x[["data"]] <- b
    return(x)
  } else {
    ggsave(file, m, scale = 1, height = height, width = width, bg = "white")
    ggsave(paste0(substring(file, 1, nchar(file) - 3), "pdf"), m, scale = 1, height = height, width = width, bg = "white")
    saveWidget(p, paste0(substring(file, 1, nchar(file) - 3), "html"))
  }
}
