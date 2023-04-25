globalVariables(c("CalorieSupply", "CropGroup", "FoodGroup", "RegionG", "negative", "model", "percentage", "positive",
                  "hours", "Products", "RegionG_f", "selRegion", "perCap", "scenarioname", "cum"))

#' @title SupplPlotsFSDP
#' @description creates supplementary plots for FSDP MAgPIE runs
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param scenarioType options: all, or one of the a-e groupings
#' @param outFolder output folder to save to
#' @param calorieSupply parameter to set if calorie figures needed for calories supply or calorie intake, set FALSE for Intake
#' @param caseRegion set to a particular region (i.e. "IND")
#'                    if plots are being used for country case studies
#' @return if file is NULL a ggplot2 object will be return
#' @author David M Chen, Vartika Singh
#' @import ggplot2 data.table scales magpiesets
#' @importFrom scales rescale
#' @importFrom stats weighted.mean
#' @importFrom dplyr case_when filter group_by inner_join mutate summarise rename select %>% distinct
#' @importFrom patchwork plot_layout
#' @importFrom RColorBrewer brewer.pal

SupplPlotsFSDP <- function(repReg, scenarioType = "manuscript", outFolder, calorieSupply = TRUE, caseRegion = NULL) {

if (scenarioType == "all") {
  rep <- convertReportFSDP(repReg, scengroup = c("FSECa", "FSECb", "FSECc", "FSECd", "FSECe"), subset = FALSE, varlist = NULL)
} else if (scenarioType == "a") {
  rep <- convertReportFSDP(repReg, scengroup = c("FSECa"), subset = FALSE, varlist = NULL)
} else if (scenarioType == "b") {
  rep <- convertReportFSDP(repReg, scengroup = c("FSECb"), subset = FALSE,
                           varlist = NULL)
} else if (scenarioType == "c") {
  rep <- convertReportFSDP(repReg, scengroup = c("FSECc"), subset = FALSE, varlist = NULL)
} else if (scenarioType == "d") {
  rep <- convertReportFSDP(repReg, scengroup = c("FSECd"), subset = FALSE, varlist = NULL)
} else if (scenarioType == "e") {
  rep <- convertReportFSDP(repReg, scengroup = c("FSECe"), subset = FALSE, varlist = NULL)
} else if (scenarioType == "bundlesBAUFSDP") {
   repBau <- convertReportFSDP(repReg, scengroup = c("FSECc"), subset = FALSE, varlist = NULL)
   rep <- convertReportFSDP(repReg, scengroup = c("FSECb"), subset = FALSE, varlist = NULL)
   repFSDP <- convertReportFSDP(repReg, scengroup = c("FSECe"), subset = FALSE, varlist = NULL)
   rep <- rbind(repBau, rep, repFSDP)
} else if (scenarioType == "manuscript") {
   rep <- convertReportFSDP(repReg, scengroup = c("FSECa", "FSECb", "FSECc", "FSECd", "FSECe"), subset = FALSE, varlist = NULL)
   rep <- filter(rep, scenario %in% c("BAU",
                                      "Diet", "Livelihoods", "NatureSparing", "AgroMngmt",
                                      "ExternalPressures", "SSP2fsdp", "FSDP"))
   } else {
  stop("Scenario type does not exist")
}

if (!file.exists(file.path(outFolder, "supplPlots"))) {
  dir.create(file.path(outFolder, "supplPlots"))
}

# trytoplot <- function(tryplot) {
#   if (inherits(try(ggplot_build(tryplot)), "try-error")) {
#     warning("One of the map plot scripts failed")
#     return(NULL)
#   } else {
#     return(tryplot)
#   }
# }

LIR <- c("SSA", "IND")
HIR <- c("USA", "CAZ", "ANZ", "EUR", "JKO", "NEU")
ROW <- c("CHA", "BRA", "LAM", "MEA", "NEA", "OAS", "REF")

if (!is.null(caseRegion)) {
  scens <- rep %>%
    mutate(RegionG =  case_when( # add region groupings
      region %in% caseRegion ~ caseRegion))
} else {
scens <- rep %>%
mutate(RegionG =  case_when( # add region groupings
         region %in% LIR ~ "Low-Income Regions",
         region %in% HIR ~ "High-Income \n Regions",
         region %in% ROW ~ "Middle-Income \n Regions",
         region == "GLO" ~ "World",
         region == "World" ~ "World"),
       RegionG = factor(RegionG,
                        levels = c("Low-Income Regions",
                                   "Middle-Income \n Regions",
                                   "High-Income \n Regions",
                                   "World")))
}

scens <- filter(scens, !is.na(RegionG))

scenarios <- as.character(unique(scens$scenario)) # re-order scenario factors to put BAU first
scenarios <- c(scenarios[which(scenarios == "BAU")], scenarios[-which(scenarios == "BAU")])

# add official names
names <- getScenarios() %>%
             rename("scenario" = "modelrun")
scens <- inner_join(scens, names)


scens$scenarioname <- factor(scens$scenarioname,
                         levels = c("BASE_SSP2",
                                    "Diets", "Livelihoods", "Biosphere",
                                    "Agriculture", "CrossSector", "FST_SSP2", "FST_SDP"))

scens$scenario <- factor(scens$scenario,
                             levels = c("BAU",
                                        "Diet", "Livelihoods", "NatureSparing",
                                        "AgroMngmt", "ExternalPressures", "SSP2fsdp", "FSDP"))


# extract population to use as a separate column
pop <- filter(scens, variable == "Population") %>%
  rename("pop" = value) %>%
  select(model, scenario, scenarioname, region, RegionG, period, pop)


## assign colors
colors <- assignScenarioColors(levels(scens$scenario))
names(colors) <- levels(scens$scenarioname)


####### calorie supply
kli <- findset("kli")
kcr <- findset("kcr")
kfo <- findset("kfo")
ksd <- findset("ksd")

if (calorieSupply) {

cropDem <- "Nutrition|Calorie Supply|Crops|"
procDem <- "Nutrition|Calorie Supply|Secondary products|+|"
livDem <- "Nutrition|Calorie Supply|Livestock products|+|"

cereals <- paste0(cropDem, "Cereals|+|", reportingnames(c("maiz", "rice_pro", "tece", "trce")))
oilCrops <- paste0(cropDem, "Oil crops|+|", reportingnames(c("soybean", "rapeseed",  "sunflower", "groundnut")))
otherCrops <- paste0(cropDem, "Other Crops|+|", reportingnames(c("others", "potato", "cassav_sp",
                                                                 "puls_pro")))
sugarCrops <-  paste0(cropDem, "Sugar Crops|+|", reportingnames(c("sugr_beet", "sugr_cane")))
processed <- paste0(procDem, reportingnames(c("alcohol",
                                              "sugar", "oils",  "scp",  "molasses", "brans")))
animal <- paste0(livDem, reportingnames(kli))
fish <- "Nutrition|Calorie Supply|+|Fish"
} else {

cropInt <- "Nutrition|Calorie Intake|Crops|"
procInt <- "Nutrition|Calorie Intake|Secondary products|+|"
livInt <- "Nutrition|Calorie Intake|Livestock products|+|"

cereals <- paste0(cropInt, "Cereals|+|", reportingnames(c("maiz", "rice_pro", "tece", "trce")))
oilCrops <- paste0(cropInt, "Oil crops|+|", reportingnames(c("soybean", "rapeseed",  "sunflower", "groundnut")))
otherCrops <- paste0(cropInt, "Other Crops|+|", reportingnames(c("others", "potato", "cassav_sp",
                                                                 "puls_pro")))
sugarCrops <-  paste0(cropInt, "Sugar Crops|+|", reportingnames(c("sugr_beet", "sugr_cane")))
processed <- paste0(procInt, reportingnames(c("alcohol",
                                              "sugar", "oils",  "scp",  "molasses", "brans")))
animal <- paste0(livInt, reportingnames(kli))
fish <- "Nutrition|Calorie Intake|+|Fish"
}

# groupings
staples <- c(cereals, otherCrops[-grep("Fruit", otherCrops)],
             sugarCrops, processed[grep("Microbial|Brans", processed)],
             oilCrops[grep("Sunflower|Other oil|Soybean", oilCrops)])
animalProducts <- c(animal, fish)
emptyCalories <- processed[grep("Alcohol|Sugar|Oils|Molasses", processed)]
fruitVegNut <- c(otherCrops[grep("Fruit", otherCrops)], oilCrops[grep("Groundnut", oilCrops)])

food_df <- filter(scens, variable %in% c(cereals, oilCrops, otherCrops,
                                         sugarCrops, processed, animal, fish), # filter food products
                  region != "GLO",   # remove global
                  period %in% c(2020, 2050)) %>%
  mutate(FoodGroup = case_when( # recategorize products and regions
    variable %in% staples ~ "Staples",
    variable %in% animalProducts ~ "Animal Products",
    variable %in% emptyCalories ~ "Empty Calories",
    variable %in% fruitVegNut ~ "Fruits, Vegetables, and Nuts"),
    FoodGroup = factor(FoodGroup,
                       levels = c("Staples", "Animal Products",
                                  "Empty Calories", "Fruits, Vegetables, and Nuts"))) %>%
  group_by(scenarioname, scenario, period, FoodGroup, region, RegionG) %>%
  summarise(CalorieSupply = sum(value))  %>% # sum across food groups with above grouping
  inner_join(pop) %>%
  group_by(scenarioname, scenario, period, FoodGroup, RegionG) %>%  # weighted mean across regions w/ above grouping
  summarise(CalorieSupply = weighted.mean(CalorieSupply, w = pop), pop = sum(pop))

food_df <- food_df[-which(food_df$period == 2020 & food_df$scenarioname != "SSP2 BAU"), ] # remove nonBAU 2020 values
food_df$pop_barwidth <- rescale(food_df$pop, c(0.2, 0.5))   # scale pop for barwidths

if (!is.null(caseRegion)) {
  food_df <- food_df %>% filter(RegionG == selRegion)
  plotCalSupply <- ggplot() +
    facet_grid(cols = vars(period), scales = "free_x", space = "free_x",   switch = "x") +
    geom_col(data = food_df[order(food_df$FoodGroup), ],
             aes(x = scenarioname, y = CalorieSupply, group = scenarioname, fill = FoodGroup),
             position = "stack",
             width = filter(food_df[order(food_df$FoodGroup), ], RegionG == selRegion)$pop_barwidth) +
    themeSupplFood(base_size = 25) +
       ylab("Kcal/capita/day") +
    scale_fill_manual(values = c("#fcba03", "#a11523", "#66407a", "#40945a"),
                      guide = guide_legend(reverse = TRUE)) +
    if (calorieSupply) {
      labs(title = "Calorie Supply")
    } else {
      labs(title = "Calorie Intake")
    }

} else {

plotCalSupply <- ggplot() +
  facet_grid(cols = vars(period), scales = "free_x", space = "free_x",   switch = "x") +
  geom_col(data = filter(food_df[order(food_df$FoodGroup), ],
                       RegionG == "High-Income \n Regions"),
           aes(x = scenarioname, y = CalorieSupply, group = scenarioname, fill = FoodGroup),
           position = "stack",
width = filter(food_df[order(food_df$FoodGroup), ],
                                   RegionG == "High-Income \n Regions")$pop_barwidth
  ) +
  geom_col(data = filter(food_df[order(food_df$FoodGroup), ], RegionG == "Middle-Income \n Regions"),
           aes(x = as.numeric(scenarioname) + 0.25, y = CalorieSupply, group = scenarioname, fill = FoodGroup),
           position = "stack",
width = filter(food_df[order(food_df$FoodGroup), ],
                         RegionG == "Middle-Income \n Regions")$pop_barwidth
  ) +
  geom_col(data = filter(food_df[order(food_df$FoodGroup), ], RegionG == "Low-Income Regions"),
           aes(x = as.numeric(scenarioname) + 0.6, y = CalorieSupply, group = scenarioname, fill = FoodGroup),
width = filter(food_df[order(food_df$FoodGroup), ],
                          RegionG == "Low-Income Regions")$pop_barwidth
  ) +
  themeSupplFood(base_size = 24) +
  theme(strip.text.x = element_text(size = 20)) +
  ylab("Kcal/capita/day") +
  scale_fill_manual(values = c("#fcba03", "#a11523", "#66407a", "#40945a"),
                    guide = guide_legend(reverse = TRUE)) +
  if (calorieSupply) {
    labs(title = "a) Calorie Supply")
  } else {
    labs(title = "a) Calorie Intake")
  }

}

### product demand per capita
demandCats <- scens[grep("Demand\\|\\+\\|", scens$variable), ]$variable %>%
  unique() %>%
  as.vector()
demandCats <- demandCats[!grepl("Value", demandCats)]

demandCats <- demandCats[-which(demandCats %in% c("Demand|+|Roundwood", "Demand|+|Domestic Balanceflow"))]

agDem <- filter(scens, variable %in% demandCats,
                region != "GLO",
                period %in% c(2020, 2050)) %>%
  inner_join(pop) %>%
  mutate(variable = gsub("Demand\\|\\+\\|", "", variable),
         variable = factor(variable,
                           levels = c("Food", "Feed", "Material", "Bioenergy",
                                      "Processing", "Agricultural Supply Chain Loss",  "Seed")),
         perCap = value / pop) %>%
  group_by(scenarioname, scenario, period, variable, RegionG) %>%  # weighted mean across regions w/ above grouping
  summarise(value = weighted.mean(perCap, w = pop),
            pop = sum(pop))

agDem <- agDem[-which(agDem$period == 2020 & agDem$scenarioname != "SSP2 BAU"), ] # remove nonBAU 2010 values
agDem$pop_barwidth <- rescale(agDem$pop, c(0.2, 0.5))   # scale pop for barwidths

if (!is.null(caseRegion)) {
  plotAgDem <- ggplot() +
    geom_col(data = agDem,
             aes(x = scenarioname, y = value, group = scenarioname, fill = variable),
             position = "stack",
             width = filter(agDem,
                            RegionG == selRegion)$pop_barwidth) +
    themeSupplFood(base_size = 24) +
    labs(title = "Crop-Based Product Demand") +
    facet_grid(cols = vars(period), scales = "free_x", space = "free_x",   switch = "x") +
    ylab("t dm/capita") +
    scale_fill_manual(values = c("#60CB4A", "#0c2c84", "#7fcdbb", "#ED5835", "#1d91c0",  "#7150E4", "#F6AE0E" ),
                      guide = guide_legend(reverse = TRUE))
  } else {
plotAgDem <- ggplot() +
  geom_col(data = filter(agDem, RegionG == "High-Income \n Regions"),
           aes(x = scenarioname, y = value, group = scenarioname, fill = variable),
           position = "stack",
           width = filter(agDem,
                          RegionG == "High-Income \n Regions")$pop_barwidth) +
  geom_col(data = filter(agDem, RegionG == "Middle-Income \n Regions"),
           aes(x = as.numeric(scenarioname) + 0.25, y = value, group = scenarioname, fill = variable),
           position = "stack",
           width = filter(agDem,
                          RegionG == "Middle-Income \n Regions")$pop_barwidth) +
  geom_col(data = filter(agDem, RegionG == "Low-Income Regions"),
           aes(x = as.numeric(scenarioname) + 0.6, y = value, group = scenarioname, fill = variable),
           position = "stack",
           width = filter(agDem,
                          RegionG == "Low-Income Regions")$pop_barwidth) +
  themeSupplFood(base_size = 24) +
  theme(strip.text.x = element_text(size = 20)) +
  labs(title = "b) Crop-Based Product Demand") +
  facet_grid(cols = vars(period), scales = "free_x", space = "free_x",   switch = "x") +
  ylab("t dm/capita") +
  scale_fill_manual(values = c("#60CB4A", "#0c2c84", "#7fcdbb", "#ED5835", "#1d91c0",  "#7150E4", "#F6AE0E" ),
                    guide = guide_legend(reverse = TRUE, title = "Product"))
}

calAg <- plotCalSupply + plotAgDem +
         plot_layout(guides = "keep", ncol = 1, byrow = FALSE)

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "caloriesAgDemand.png"),
                  calAg, width = 10, height = 11, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "caloriesAgDemand.pdf"),
         calAg, width = 10, height =11, scale = 1.5, bg = "white")
}



###### emissions

varEmiss <- c("Emissions|CO2|Land|+|Land-use Change",
              "Emissions|CH4|Land|+|Agriculture",
              "Emissions|N2O|Land|+|Agriculture")
names(varEmiss) <- c("CO2", "CH4", "N2O")
unit <- expression(bold("Gt CO"[2] ~ "eq yr"^{
-1
})) # "Gt CO2eq per Year"

emiss <- filter(scens,
                variable %in% varEmiss,
                region == "GLO",
                period <= 2050,
                period > 1995) %>%
  droplevels() %>%
  mutate(variable = factor(variable, levels = rev(varEmiss), labels = names(rev(varEmiss))),
         value = case_when( # GWP
           variable == "CO2" ~ value * 1,
           variable == "CH4" ~ value * 27,
           variable == "N2O" ~ value * 273),
         value = value / 1000)

emiss_glo <- filter(emiss, region == "GLO")
emiss_glo$positive <- ifelse(emiss_glo$value >= 0, emiss_glo$value, 0)
emiss_glo$negative <- ifelse(emiss_glo$value < 0, emiss_glo$value, -1e-36)

plotEmissGlo <- ggplot(emiss_glo, aes(x = period)) +
  facet_wrap(~scenarioname, nrow = 3) +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab(unit) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = variable), position = "stack") +
  geom_area(aes(y = negative, fill = variable), position = "stack") +
  scale_fill_manual("AFOLU emission type",
                    values = rev(c("#1b9e77", "#d95f02", "#7570b3")),
                    labels = function(x) parse(text = x)) +
  stat_summary(fun = "sum", colour = "black", size = 1,
               geom = "line", mapping = aes(group = scenarioname, y = value)) +
  theme(legend.position = "bottom", axis.text = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 3, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab(NULL) +
  labs(title = "a) Global GHG Emissions")


# emis reg CUMULATIVE
emissReg <- filter(scens, RegionG != "World",
                    variable %in% varEmiss,
                    period >= 2020,
                     period <= 2050) %>%
  droplevels() %>%
  mutate(variable = factor(variable, levels = rev(varEmiss), labels = names(rev(varEmiss))),
         scenarioname = factor(scenarioname, levels = rev(levels(scenarioname))),
         value = case_when( # GWP
           variable == "CO2" ~ value * 1,
           variable == "CH4" ~ value * 27,
           variable == "N2O" ~ value * 273),
         value = value / 1000) %>%
  group_by(scenarioname, scenario, region, variable)


mapping <- distinct(emissReg[,c("region", "RegionG")])
# ch4 and n2o need to be interpolated for cumulative sum, quick as magclass

emissRegch4n2o <- filter(emissReg, variable %in% c("CH4", "N2O")) %>%
               select(region, period, variable, scenario, value) %>%
               as.magpie(tidy = TRUE) %>%
               time_interpolate(interpolated_year = c(2020:2050), integrate_interpolated_years = TRUE) %>%
               as.data.frame(rev = 2) %>%
               rename("value" = ".value") %>%
               filter(period != 2020) %>% #remove 2020 value for cumsum
               group_by(region, scenarioname, variable) %>%
               mutate(cum = cumsum((value))) %>%
               filter(period == 2050) # subset to 2050 value

# co2 is a stock accounting so it only needs to be multiplied by 5
emissRegco2 <- filter(emissReg, variable == "CO2") %>%
               mutate(cum = cumsum(value) * 5) %>%   # Gt and cumulative, multiply by the 5 year time steps
                 filter(period == 2050) %>%  # subset to 2050 value
                select(region, period, variable, scenario, value, cum)


emissReg <- rbind(emissRegco2, emissRegch4n2o)
emissReg <- inner_join(emissReg, mapping)


emissReg$positive <- ifelse(emissReg$value >= 0, emissReg$value, 0)
emissReg$negative <- ifelse(emissReg$value < 0, emissReg$value, -1e-36)

# emiss_reg <- emiss_reg[-which(emiss_reg$period == 2020 & emiss_reg$scenarioname!= "BAU"),] #remove nonBAU 2010 values
#unit <- "Gt CO2eq since 2020"
unit <- expression(bold("Gt CO"[2] ~ "eq yr"^{
  -1
})) # "Gt CO2eq per Year"

if (!is.null(caseRegion)) {
  emissReg <- emissReg %>% filter(RegionG == selRegion)
}

plotEmissReg <- ggplot(emissReg, aes(y = scenarioname)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 20, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  scale_fill_manual("AFOLU emission type", values = c("#1b9e77", "#d95f02", "#7570b3"),
                    labels = function(x) parse(text = x)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenarioname, x = cum)) +
  theme(legend.position = "bottom") +
  theme(strip.text.y = element_text(size = 14), axis.text.y = element_text(size = 12)) +
  guides(fill = guide_legend("AFOLU emission type", ncol = 4, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab(unit) + scale_x_continuous(guide = guide_axis(check.overlap = TRUE), expand = expansion(mult = c(0.05, 0.1)),
                                  breaks = pretty_breaks()) +
  labs(title = "b) Regional GHG Emissions")

plotEmiss <- plotEmissGlo + plotEmissReg +
  plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.6))

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotEmiss.png"),
         plotEmiss, width = 9, height = 12, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotEmiss.pdf"),
         plotEmiss, width = 9, height = 12, scale = 1.5, bg = "white")
}



########## Land Use
landVar <- c("Resources|Land Cover|+|Cropland",
             "Resources|Land Cover|+|Pastures and Rangelands",
             "Resources|Land Cover|Forest|Natural Forest|+|Primary Forest",
             "Resources|Land Cover|Forest|Natural Forest|+|Secondary Forest",
             "Resources|Land Cover|Forest|Managed Forest|+|Plantations",
             "Resources|Land Cover|Forest|Managed Forest|+|NPI/NDC",
             "Resources|Land Cover|Forest|Managed Forest|+|Afforestation",
             "Resources|Land Cover|Cropland|+|Bioenergy crops",
            "Resources|Land Cover|+|Other Land",
             "Resources|Land Cover|+|Urban Area")
names(landVar) <- c("Cropland", "Pasture", "Primary Forest", "Secondary Forest", "Timber",
                    "Aff NDC", "Aff CO2-Price",  "Bioenergy", "Other Natural", "Urban")

land_df <- filter(scens,
                  period <= 2050,
                  period > 2015,
                  variable %in% landVar) %>%
  droplevels() %>%
  group_by(model, scenarioname, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(landVar),
                           labels = names(rev(landVar))),
         value = case_when(variable == "Cropland" ~ value[variable == "Cropland"] - value[variable == "Bioenergy"], # get non-bioenergy cropland
                           variable != "Cropland" ~ value)) %>%
  group_by(model, scenarioname, scenario, region, variable) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping


land_df$positive <- ifelse(land_df$value >= 0, land_df$value, 0)
land_df$negative <- ifelse(land_df$value < 0, land_df$value, -1e-36)

landGlo <- filter(land_df, region == "GLO")

# plotLandGlo <- ggplot(landGlo, aes(x = period)) +
#   facet_wrap(~scenarioname, nrow = 2) +
#   themeSupplReg(base_size = 22, panel.spacing = 3, rotate_x = 90) +
#   ylab("Change in Mha compared to 2020") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   geom_area(aes(y = positive, fill = variable), position = "stack") +
#   geom_area(aes(y = negative, fill = variable), position = "stack") +
#   scale_fill_manual("Land type", values = rev(c("#FCED0F", "#E6AB02", "darkgreen", "#99cc00", "lightgreen",
#                                                "brown3", "#785318", "#9e9ac8", "#ae017e", "#08589e"))) +
#   theme(legend.position = "bottom", legend.text = element_text(size = 16)) +
#   guides(fill = guide_legend(ncol = 3, title.position = "left", byrow = TRUE, reverse = TRUE)) +
#   xlab(NULL) +
#   labs(title = "a) Global Land-Use Change")


landReg <- filter(land_df, region != "GLO", period == 2050) %>%
  group_by(model, scenarioname, scenario, variable, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenarioname = factor(scenarioname, levels = rev(levels(scenarioname))))

if (!is.null(caseRegion)) {
  landReg <- landReg %>% filter(RegionG == selRegion)
}
plotLandReg <- ggplot(landReg, aes(y = scenarioname)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 20, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual("Land type", values = rev(c("#FCED0F", "#E6AB02", "darkgreen", "#99cc00", "lightgreen",
                                                          "brown3", "#785318", "#9e9ac8", "#ae017e", "#08589e"))) +
   theme(legend.position = "bottom", legend.text = element_text(size = 16),
        strip.text.y = element_text(size = 14), axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 12)) +
  guides(fill = guide_legend("Land type", ncol = 3, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab("Change in Mha compared to 2020") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) +
  theme(panel.spacing = unit(100, "lines")) +
  labs(title = "Regional Land-Use Change 2050 rel. to 2020")
# breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


#plotLand <- plotLandGlo + plotLandReg +
 # plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.7))

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotLand.png"),
         plotLandReg, width = 9, height = 7, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotLand.pdf"),
         plotLandReg, width = 9, height = 7, scale = 1.5, bg = "white")
}


### Crop Area ##########

# Take soy and groundnut out of oil crops into pulses as legumes,
# sugar beet + sunflower/raps + potato as OTHER,
# Bioenergy + sugarcane + oilpalm = plantations (Bioenergy, sugar cane, Oilpalm)

cropVar <- c(scens[grep("Cropland\\|Crops\\|Cereals\\|\\+", scens$variable), ]$variable %>%  unique() %>% as.vector(),
             scens[grep("Cropland\\|Crops\\|Oil crops\\|\\+", scens$variable), ]$variable %>%  unique() %>% as.vector(),
             scens[grep("Cropland\\|Crops\\|Sugar crops\\|\\+", scens$variable), ]$variable %>%  unique() %>% as.vector(),
             scens[grep("Cropland\\|Crops\\|Other crops\\|\\+", scens$variable), ]$variable %>%  unique() %>% as.vector(),
             scens[grep("Cropland\\|\\+\\|Bioenergy crops", scens$variable), ]$variable %>%  unique() %>% as.vector(),
             "Resources|Land Cover|Cropland|+|Fallow Cropland")

cereals <- cropVar[grep("Cereals", cropVar)]
legumes <- cropVar[grep("Soy|Pulse|Groundnut", cropVar)]
plantations <- cropVar[grep("Bioenergy|cane|Oilpalm", cropVar)]
fruits <- cropVar[grep("Fruits", cropVar)]
other <- cropVar[grep("beet|Sunflower|rapeseed|Potato|Cotton|roots", cropVar)]
fallow <- "Resources|Land Cover|Cropland|+|Fallow Cropland"

crop_df <- filter(scens,
                  period <= 2050,
                  period > 2015,
                  variable %in% cropVar) %>%
  droplevels() %>%
  mutate(CropGroup = case_when( # recategorize products
    variable %in% cereals ~ "Cereals",
    variable %in% legumes ~ "Legumes",
    variable %in% plantations ~ "Plantations (Bioenergy, Oilpalm, Sugar cane)",
    variable %in% fruits ~ "Fruits, Vegetables, and Nuts",
    variable %in% other ~ "Other Crops",
    variable %in% fallow ~ "Fallow Cropland"),
    CropGroup = factor(CropGroup,
                       levels = c("Cereals", "Legumes",
                                  "Plantations (Bioenergy, Oilpalm, Sugar cane)", "Fruits, Vegetables, and Nuts",
                                  "Other Crops", "Fallow Cropland"))) %>%
  group_by(model, scenarioname, scenario, region, RegionG, period, CropGroup) %>%
  summarise(value = sum(value)) %>%
  group_by(model, scenarioname, scenario, region, CropGroup) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping

# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
crop_df$positive <- ifelse(crop_df$value >= 0, crop_df$value, 0)
crop_df$negative <- ifelse(crop_df$value < 0, crop_df$value, -1e-36)

cropGlo <- filter(crop_df, region == "GLO")

# plotCropGlo <- ggplot(cropGlo, aes(x = period)) +
#   facet_wrap(~scenarioname, nrow = 2) +
#   themeSupplReg(base_size = 18, panel.spacing = 3, rotate_x = 90) +
#   ylab("Cropland change in Mha \n compared to 2020") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   geom_area(aes(y = positive, fill = CropGroup), position = "stack") +
#   geom_area(aes(y = negative, fill = CropGroup), position = "stack") +
#   stat_summary(fun = "sum", colour = "black", size = 1,
#                geom = "line", mapping = aes(group = scenarioname, y = value)) +
#   scale_fill_manual("Cropland type", values = rev(c(
#     "#E6AB02", "lightblue", "#9e9ac8", "pink", "#4b6fb8", "darkgreen", "#fcba03"))) +
#                theme(legend.position = "bottom", legend.text = element_text(size = 14),
#                      strip.text.y = element_text(size = 16), axis.text.y = element_text(size = 16),
#                      axis.text.x = element_text(size = 16)) +
#   guides(fill = guide_legend(ncol = 2, title.position = "left", byrow = TRUE, reverse = FALSE)) +
#   xlab(NULL) +
#   labs(title = "a) Global Cropland Distribution")

cropReg <- filter(crop_df, region != "GLO", period == 2050) %>%
  group_by(model, scenarioname, scenario, CropGroup, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenarioname = factor(scenarioname, levels = rev(levels(scenarioname))))

# write.csv(b,file="SI_lu_reg_2100_bar.csv",row.names = FALSE)
 if (!is.null(caseRegion)) {
   cropReg <- cropReg %>% filter(RegionG == selRegion)
 }

 plotCropReg <- ggplot(cropReg, aes(y = scenarioname)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 16, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = CropGroup), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = CropGroup, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenarioname, x = value)) +
  scale_fill_manual("Cropland type", values = rev(c("#F2B701", "#E73F74", "#7F3C8D","#3969AC","#11A579","#A5AA99"))) +
  theme(legend.position = "bottom", legend.text = element_text(size = 14),
        strip.text.y = element_text(size = 16), axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 2, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Change in Mha compared to 2020") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) +
   labs(title = "Regional Cropland Distribution 2050 rel. to 2020")

 # plotCrop <- plotCropGlo + plotCropReg +
 #   plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.7))

 if (!is.null(outFolder)) {
   ggsave(filename = file.path(outFolder, "supplPlots",
                               "plotCrop.png"),
          plotCropReg, width = 8, height = 6, scale = 1.5, bg = "white")
   ggsave(filename = file.path(outFolder, "supplPlots",
                               "plotCrop.pdf"),
          plotCropReg, width = 8, height = 6, scale = 1.5, bg = "white")
 }


####### Nitrogen##############

nitrVar <- c("Resources|Nitrogen|Pollution|Surplus|+|Cropland",
             "Resources|Nitrogen|Pollution|Surplus|+|Pasture",
             "Resources|Nitrogen|Pollution|Surplus|+|Animal Waste Management",
             "Resources|Nitrogen|Pollution|Surplus|+|Non-agricultural land")

names(nitrVar) <- c("Cropland", "Pasture", "Manure Management", "Non-agricultural Land")

nitr_df <- filter(scens,
                  period <= 2050,
                  period > 2015,
                  variable %in% nitrVar) %>%
  droplevels() %>%
  group_by(model, scenarioname, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(nitrVar),
                           labels = names(rev(nitrVar)))) %>%
  group_by(model, scenarioname, scenario, region, variable)


# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
nitr_df$positive <- ifelse(nitr_df$value >= 0, nitr_df$value, 0)
nitr_df$negative <- ifelse(nitr_df$value < 0, nitr_df$value, -1e-36)

nitrGlo <- filter(nitr_df, region == "GLO")

plotNitrGlo <- ggplot(nitrGlo, aes(x = period)) +
  facet_wrap(~scenarioname, nrow = 2) +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab("Nitrogen Surplus (Mt Nr/yr)") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = variable), position = "stack") +
  geom_area(aes(y = negative, fill = variable), position = "stack") +
  scale_fill_manual("Land type", values = rev(c("#FCED0F", "#E6AB02", "#785318",  "darkgreen"))) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab(NULL) +
  labs(title = "a) Global Nitrogen Surplus")

nitrReg <- filter(nitr_df, region != "GLO", period %in% c(2020, 2050)) %>%
  group_by(model, scenarioname, scenario, variable, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenarioname = factor(scenarioname, levels = rev(levels(scenarioname))))
nitrReg <- nitrReg[-which(nitrReg$period == 2020 & nitrReg$scenarioname != "SSP2 BAU"), ] # remove nonBAU 2010 values

# write.csv(b,file="SI_lu_reg_2100_bar.csv",row.names = FALSE)

if (!is.null(caseRegion)) {
  nitrReg <- nitrReg %>% filter(RegionG == selRegion)
}

plotNitrReg <- ggplot(nitrReg, aes(y = scenarioname)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 18, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual("Land type", values = rev(c("#FCED0F", "#E6AB02", "#785318",  "darkgreen"))) +
  theme(legend.position = "bottom", strip.text.y = element_text(angle = 0, size = 14),
         axis.text = element_text(size = 16)) +
 guides(fill = guide_legend("Land type", ncol = 3, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Nitrogen Surplus (Mt Nr/yr)") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) +
  labs(title = "b) Regional Nitrogen Surplus 2050") # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

plotNitr <- plotNitrGlo + plotNitrReg +
  plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.7))

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotNitr.png"),
         plotNitr, width = 8, height = 10, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotNitr.pdf"),
         plotNitr, width = 8, height = 10, scale = 1.5, bg = "white")
}


############ Water #######

waterVar <- "Resources|Water|Withdrawal|Agriculture"
names(waterVar) <- c("Agricultural Water Withdrawals")

water_df <- filter(scens,
                   period <= 2050,
                   period > 2015,
                   variable %in% waterVar) %>%
  droplevels() %>%
  group_by(model, scenarioname, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(waterVar),
                           labels = names(rev(waterVar)))) %>%
         # scenarioname = factor(scenarioname, levels = c(
         #                             "BAU", "AgroMngmt", "NatureSparing",
         #                             "ExternalPressures", "Livelihoods", "Diet",
         #                             "FSDP"))) %>%
  group_by(model, scenarioname, scenario, region, variable) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping

# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
water_df$positive <- ifelse(water_df$value >= 0, water_df$value, 0)
water_df$negative <- ifelse(water_df$value < 0, water_df$value, -1e-36)

waterGlo <- filter(water_df, region == "GLO")

plotWaterGlo <- ggplot(waterGlo, aes(x = period)) +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab("Change in Water withdrawals \n from 2020 value, in km3") +
  geom_line(aes(y = value, color = scenarioname), position = "stack", size = 1.3) +
scale_color_manual(values = colors) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, title.position = "left", byrow = TRUE)) + xlab(NULL) +
  labs(title = "a) Global Water Withdrawals")


waterReg <- filter(water_df, region != "GLO", period == 2050) %>%
  group_by(model, scenarioname, scenario, variable, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenarioname = factor(scenarioname, levels = rev(levels(scenarioname))))

if (!is.null(caseRegion)) {
  waterReg <- waterReg %>% filter(RegionG == selRegion)
}

#facet_bounds <- data.frame("RegionG" = rep(c("High-Income \n Regions", "Middle-Income \n Regions", "Low-Income Regions"), 2),
 #                          "value" = c(-100, NA, NA, 150, NA, NA), "scenarioname" = waterReg$scenarioname[1])

plotWaterReg <- ggplot(waterReg, aes(y = scenarioname)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 18, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenarioname, x = value)) +
  scale_fill_manual(" ", values = rev(c("purple", "#2080EC", "lightblue"))) +
  theme(legend.position = "bottom", strip.text.y = element_text(angle = 0, size = 14),
        axis.text = element_text(size = 16)) +
  guides(fill = element_blank()) +
  # guide_legend("Cropland type",ncol=5,title.position = "left", byrow = TRUE,reverse=FALSE)) +
  xlab("Change in Water withdrawals from 2020 value, in km3") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) +
 labs(title = "b) Regional Water Withdrawals 2050 rel. to 2020") # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

#plotWaterReg <- plotWaterReg + geom_blank(data = facet_bounds, aes(x = value, y = scenarioname))

plotWater <- plotWaterGlo + plotWaterReg +
  plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.7))

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotWater.png"),
         plotWater, width = 9, height = 10, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotWater.pdf"),
         plotWater, width = 9, height = 10, scale = 1.5, bg = "white")
}

### Health

healthVar <-  scens[grep("Nutrition\\|Anthropometrics\\|People", scens$variable), ]$variable %>%  unique()
names(healthVar) <- c("Normal weight", "Obese", "Overweight", "Underweight")


health_df <- filter(scens,
                    period <= 2050,
                    period > 2015,
                    variable %in% healthVar) %>%
  droplevels() %>%
  group_by(model, scenarioname, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(healthVar),
                           labels = names(rev(healthVar))),
         variable = factor(variable, levels = rev(c("Underweight", "Normal weight",  "Overweight", "Obese"))))

health_df <- health_df[-which(health_df$period == 2020 & health_df$scenarioname != "SSP2 BAU"), ] # remove nonBAU 2020 values


#
# healthGlo <- filter(health_df, region == "GLO")
# plotHealthGlo <- ggplot(healthGlo, aes(x = period)) +
#   facet_wrap(~scenarioname, nrow = 2) +
#   themeSupplReg(base_size = 18, panel.spacing = 3, rotate_x = 90) +
#   ylab("Million People") +
#   geom_area(aes(y = value, fill = variable), position = "stack") +
#   scale_fill_manual("Nutrition Indicators",
#                     values = rev(c("lightblue", "#2080EC", "blue", "purple"))) +
#   theme(legend.position = "bottom", strip.text.y = element_text(angle = 0, size = 14),
#         axis.text = element_text(size = 14)) +
#   guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)
# # healthGloP

healthReg <- filter(health_df, region != "GLO", period %in% c(2020, 2050),
                    scenarioname %in% c("SSP2 BAU",  "Diets", "ExtTransformation", "FSDP")) %>%
  group_by(model, scenarioname, scenario, variable, period, RegionG) %>%
  summarise(value = sum(value)) %>%
  mutate(scenarioname = factor(scenarioname, levels = rev(levels(scenarioname))))

if (!is.null(caseRegion)) {
  healthReg <- healthReg %>% filter(RegionG == selRegion)
}

plotHealthReg <- ggplot(healthReg, aes(y = scenarioname)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 18, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = value, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual(element_blank(),
                    values = rev(c("lightblue", "#2080EC", "blue", "purple"))) +
  theme(legend.position = "bottom", strip.text.y = element_text(angle = 0, size = 14),
        axis.text = element_text(size = 14)) +
  guides(fill = guide_legend("Indicator", ncol = 4, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab("Million People") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

#plotHealth <- plotHealthGlo + plotHealthReg +
 # plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.7))

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotHealth.png"),
         plotHealthReg, width = 8.6, height = 6, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotHealth.pdf"),
         plotHealthReg, width = 8.6, height = 6, scale = 1.5, bg = "white")
}

## Marco health output indicators NOTE here the GLO region is "World", re-set appropriately if he fixes

unique(scens$variable)[grep("Health", unique(scens$variable))]

marcoVar <-  c("Health|Years of life lost|Disease",
                "Health|Years of life lost|Disease|+|Congenital Heart Disease",
                "Health|Years of life lost|Disease|+|Stroke",
                "Health|Years of life lost|Disease|+|Cancer" ,
                "Health|Years of life lost|Disease|+|Type-2 Diabetes",
                "Health|Years of life lost|Disease|+|Respiratory Disease"  )
names(marcoVar) <- c("Years of Life Lost",
                     "Congenital Heart Disease",
                     "Stroke",
                     "Cancer" ,
                     "Type-2 Diabetes",
                     "Respiratory Disease"  )

marcoDF <- filter(scens,
                  period <= 2050,
                  period > 2015,
                  variable %in% marcoVar) %>%
  droplevels() %>%
  group_by(model, scenarioname, scenario, region, variable)

marcoGLO <- filter(marcoDF, region == "World")

plotYLivesLostGLO <- ggplot(
  filter(marcoGLO, variable == "Health|Years of life lost|Disease"),
  aes(x = period)) +
  themeSupplReg(base_size = 18, panel.spacing = 3, rotate_x = 90) +
  ylab("Million Years") +
  geom_line(aes(y = value, color = scenarioname), lwd = 1.1) +
  theme(legend.position = "bottom", legend.text = element_text(size = 18),
        strip.text = element_text(size = 16), axis.text = element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = FALSE, reverse = FALSE)) +
  xlab(NULL) +
  scale_color_manual(values = colors) +
  labs(title = "Years of Life Lost")


marcoReg <- filter(marcoDF, region != "World", variable !=  "Health|Years of life lost|Disease") %>%
  group_by(model, scenarioname, scenario, variable, period, RegionG) %>%
  group_by(model, scenarioname, scenario, RegionG, variable) %>%
  mutate(variable = factor(variable, levels = rev(marcoVar),
                           labels = names(rev(marcoVar))),
         scenarioname = factor(scenarioname, levels = rev(c("SSP2 BAU", "Diets", "ExtTransformation", "allFSMs", "FSDP"))))

marcoReg <- marcoReg[-which(marcoReg$period == 2020 & marcoReg$scenarioname != "SSP2 BAU"), ] # remove nonBAU 2020 values


plotYLivesLostReg <- ggplot(filter(marcoReg,
                                 period %in% c(2020, 2050)),
                          aes(y = scenarioname)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 18, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = value, fill = variable), stat = "identity", width = 0.75) +
  theme(legend.position = "bottom", legend.text = element_text(size = 18),
        strip.text = element_text(size = 16), axis.text = element_text(size = 14))  +
  guides(fill = guide_legend("Indicator", ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab("Million Years") +
  labs(title = "Years of Life Lost") +
  scale_fill_manual(values = brewer.pal(n = 5, "Dark2")) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


plotYLivesLost <- plotYLivesLostGLO + plotYLivesLostReg +
  plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.7))

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotYLivesLost.png"),
         plotYLivesLost, width = 9, height = 10, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotYLivesLost.pdf"),
         plotYLivesLost, width = 9, height = 10, scale = 1.5, bg = "white")
}



######## EMPLOYMENT ##########

## Global: Absolute change in employment - global lineplot

empVars <- c("Agricultural employment|Crop and livestock products", # old variable name
             "Labor|Employment|Agricultural employment") # new variable name

emp_df <- filter(scens,
                  period <= 2050,
                  period > 2015,
                  variable %in% empVars) %>%
  droplevels() %>%
  group_by(model, scenarioname, scenario, region, RegionG, period) %>%
  filter(region == "GLO")

plotEmpGlo <- ggplot(emp_df, aes(x = period)) +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab("Million people") +
  geom_line(aes(y = value, color = scenarioname), linewidth = 1.1) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 8, title.position = "left", byrow = TRUE, reverse = FALSE)) + xlab(NULL) +
  scale_color_manual(values = colors) +
  labs(title = "a) Number of People Employed in Agriculture")


## Regional: abolute change 2020 to 2050 - barplot

empVar <- intersect(unique(scens$variable), c("Agricultural employment|Crop and livestock products", # old variable name
             "Labor|Employment|Agricultural employment")) # new variable name
names(empVar) <- c("Agricultural employment")

emp_df <- filter(scens,
                  period <= 2050,
                  period > 2015,
                  variable %in% empVar) %>%
  droplevels() %>%
  group_by(model, scenarioname, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(empVar),
                           labels = names(rev(empVar)))) %>%
  group_by(model, scenarioname, scenario, region, variable) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping

emp_df$positive <- ifelse(emp_df$value >= 0, emp_df$value, 0)
emp_df$negative <- ifelse(emp_df$value < 0, emp_df$value, -1e-36)

facet_bounds <- data.frame("RegionG" = rep(c("High-Income \n Regions", "Middle-Income \n Regions", "Low-Income Regions"), 2),
                           "value" = c(-100, NA, NA, 7, NA, NA), "scenarioname" = emp_df$scenarioname[1])

empReg <- filter(emp_df, region != "GLO", period == 2050) %>%
  group_by(model, scenarioname, scenario, variable, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenarioname = factor(scenarioname, levels = rev(levels(scenarioname))))

if (!is.null(caseRegion)) {
  empReg <- empReg %>% filter(RegionG == selRegion) %>% mutate(RegionG_f = selRegion)
} else {
  empReg$RegionG_f <- factor(empReg$RegionG, levels = c("High-Income \n Regions", "Middle-Income \n Regions", "Low-Income Regions"))
}

plotEmpReg <- ggplot(empReg, aes(y = scenarioname)) +
  facet_grid(vars(period), vars(RegionG_f), scales = "free", space = "free") +
  themeSupplReg(base_size = 20, rotate_x = FALSE) + ylab(NULL) +
  #geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75, alpha = 0.75) +
  #geom_bar(aes(x = negative, fill = variable), position = "stack", stat = "identity", width = 0.75, alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "#89c783", fill = "#89c783", geom = "bar", mapping = aes(group = scenarioname, x = value)) +
  scale_fill_manual(" ", values = "#89c783") +
  theme(legend.position = "bottom", strip.text = element_text(angle = 0, size = 14),
        axis.text = element_text(size = 14)) +
  guides(fill = element_blank()) +
  xlab("Regional agricultural employment compared to 2020 in mio. people") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) +
  labs(title = "b) Regional agricultural employment 2050 rel. to 2020")

plotEmpReg <- plotEmpReg + geom_blank(data = facet_bounds, aes(x = value, y = scenarioname))


plotEmp <- plotEmpGlo + plotEmpReg +
  plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.7))

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotEmp.png"),
         plotEmp, width = 9, height = 10, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotEmp.pdf"),
         plotEmp, width = 9, height = 10, scale = 1.5, bg = "white")
}


##### Hourly Labour Costs ####

laborVar <-  as.character(scens$variable[grep("Hourly labor costs$", scens$variable)] %>% unique())
names(laborVar) <- "Hourly Labor Costs"
totalHoursVar <- scens$variable[grep("Total Hours Worked", scens$variable)] %>% unique()

# extract total hours worked to use as a separate column
hoursWorked <- filter(scens, variable == totalHoursVar) %>%
  rename("hours" = value) %>%
  select(model, scenarioname, region, RegionG, period, hours)

labor_df <- filter(scens,
                 period <= 2050,
                 period > 2015,
                 variable %in% laborVar) %>%
  droplevels() %>%
  group_by(model, scenarioname, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(laborVar),
                           labels = names(rev(laborVar)))) %>%
  inner_join(hoursWorked) %>%
  group_by(model, scenarioname, scenario, region, variable)

labReg <- filter(labor_df, region != "GLO") %>%
  group_by(model, scenario, scenarioname, variable, period, RegionG) %>%
  summarise(value = weighted.mean(value, w = hours)) %>%
  # mutate(scenarioname = factor(scenarioname, levels = c(
  #  "BAU", "AgroMngmt", "NatureSparing",
  #  "ExternalPressures", "Livelihoods", "Diet",
  #  "FSDP"))) %>%
  group_by(model, scenarioname, scenario, RegionG, variable)

if (!is.null(caseRegion)) {
  labReg <- labReg %>% filter(RegionG == selRegion)
}

plotLabReg <- ggplot(labReg,
                     aes(x = period)) +
  facet_wrap(~RegionG, scales = "free") +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab("Hourly Labor Costs (USD05/h)") +
  geom_line(aes(y = jitter(value), color = scenarioname),  lwd = 1.1) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL) +
  scale_color_manual(values = colors) +
  labs(title = "Regional Labour Costs")


if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotLabReg.png"),
         plotLabReg, width = 9, height = 6, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotLabReg.pdf"),
         plotLabReg, width = 9, height = 6, scale = 1.5, bg = "white")
}


######## INEQUALITY INDICATORS ##########

ineqVar <-  c("Income|Gini Coefficient",
              "Income|Average Income of Lower 40% of Population",
              "Income|Fraction of Population below half of Median Income",
              "Income|Number of People Below 1p90 USDppp11/day",
              "Income|Number of People Below 3p20 USDppp11/day",
              "Income|Number of People Below 5p50 USDppp11/day")
names(ineqVar) <- ineqVar

ineq_df <- filter(scens,
                   period <= 2050,
                   period > 2015,
                   variable %in% ineqVar) %>%
  droplevels() %>%
  group_by(model, scenarioname, scenario, region, period) %>%
  # mutate(variable = factor(variable, levels = rev(ineqVar),
  #                          labels = names(rev(ineqVar))),
  #        scenarioname = factor(scenarioname, levels = c(
  #          "SSP2bau", "AgroMngmt", "NatureSparing",
  #          "ExternalPressures", "Livelihoods", "Diet",
  #          "FSDP"))) %>%
  group_by(model, scenarioname, scenario, region, variable)



ineqGlo <- filter(ineq_df, region == "GLO")

plotGiniGlo <- ggplot(filter(ineqGlo, variable == "Income|Gini Coefficient"),
                    aes(x = period)) +
  themeSupplReg(base_size = 20, panel.spacing = 3) +
  ylab("Gini Coefficient") +
  geom_line(aes(y = value, color = scenarioname), lwd = 1.5) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = FALSE, reverse = FALSE)) + xlab(NULL) +
scale_color_manual(values = colors) +
  labs(title = "Global Gini Coefficient")


 # scale_colour_manual(values = c("#1f78b4", "#33a02c", "#b2df8a", "#d95f02", "#7570b3", "#e7298a"))

ineqReg <- filter(ineq_df, region != "GLO") %>%
  group_by(model, scenarioname, scenario, variable, period, RegionG) %>%
  group_by(model, scenarioname, scenario, RegionG, variable)
# %>%
 # filter(period == 2050)
ineqReg <- ineqReg[-which(ineqReg$period == 2020 & ineqReg$scenarioname != "SSP2 BAU"), ] # remove nonBAU 2020 values


if (!is.null(caseRegion)) {
  ineqReg <- ineqReg %>% filter(RegionG == selRegion)
} else {

  ineqGini <- filter(ineqReg, variable == "Income|Gini Coefficient") %>%
              inner_join(pop) %>%
              group_by(model, scenario, scenarioname, RegionG, variable, period) %>%
              summarise(value = weighted.mean(value, w = pop))
}

plotGiniReg <- ggplot(ineqGini, aes(color = scenarioname)) +
  facet_wrap(~RegionG, scales = "free") +
  themeSupplReg(base_size = 20, rotate_x = FALSE) + ylab(NULL) +
  geom_line(aes(y = value, x = period), size = 1.5) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend("Indicator", ncol = 5, title.position = "left", byrow = FALSE, reverse = FALSE)) +
  xlab("Gini Coefficient") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) +
scale_color_manual(values = colors) +
  labs(title = "Regional Gini Coefficient 2050") # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


plotGini <- plotGiniGlo + plotGiniReg +
  plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.7))

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotGini.png"),
         plotGini, width = 9, height = 10, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotGini.pdf"),
         plotGini, width = 9, height = 10, scale = 1.5, bg = "white")
}


plotBelowPovGlo <- ggplot(
                  filter(ineqGlo, variable == "Income|Number of People Below 3p20 USDppp11/day"),
                   aes(x = period)) +
  themeSupplReg(base_size = 18, panel.spacing = 3, rotate_x = 90) +
  ylab("Million People") +
  geom_line(aes(y = value, color = scenarioname), lwd = 1.1) +
  theme(legend.position = "bottom", legend.text = element_text(size = 18),
        strip.text = element_text(size = 16), axis.text = element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = FALSE, reverse = FALSE)) +
  xlab(NULL) +
scale_color_manual(values = colors) +
  labs(title = "Number of People Below 3.20$/Day Poverty Line")


plotBelowPovReg <- ggplot(filter(ineqReg, variable == "Income|Number of People Below 3p20 USDppp11/day",
                                 RegionG != "High-Income \n Regions",
                                 period %in% c(2020, 2050)),
                          aes(y = scenarioname)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 18, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = value), stat = "identity", width = 0.75, fill = "#1E7CAF") +
  theme(legend.position = "bottom", legend.text = element_text(size = 18),
        strip.text = element_text(size = 16), axis.text = element_text(size = 14))  +
  guides(fill = guide_legend("Indicator", ncol = 5, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Million People") +
  labs(title = "Number of People Below 3.20$/Day Poverty Line") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


plotBelowPov <- plotBelowPovGlo + plotBelowPovReg +
  plot_layout(guides = "keep", ncol = 1, byrow = FALSE, heights = c(1, 0.7))

if (!is.null(outFolder)) {
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotBelowPov.png"),
         plotBelowPov, width = 9, height = 10, scale = 1.5, bg = "white")
  ggsave(filename = file.path(outFolder, "supplPlots",
                              "plotBelowPov.pdf"),
         plotBelowPov, width = 9, height = 10, scale = 1.5, bg = "white")
}

}


