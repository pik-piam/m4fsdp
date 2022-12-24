globalVariables(c("CalorieSupply", "CropGroup", "FoodGroup", "RegionG", "negative", "model", "percentage", "positive",
                  "hours", "Products", "RegionG_f", "selRegion"))

#' @title SupplPlotsFSDP
#' @description creates supplementary plots for FSDP MAgPIE runs
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param scenarioType options: all, or one of the a-e groupings
#' @param file  file name to save to
#' @param calorieSupply parameter to set if calorie figures needed for calories supply or calorie intake, set FALSE for Intake
#' @param caseRegion set to a particular region (i.e. "IND")
#'                    if plots are being used for country case studies
#' @return if file is NULL a ggplot2 object will be return
#' @author David M Chen, Vartika Singh
#' @import ggplot2 data.table scales magpiesets
#' @importFrom stats weighted.mean
#' @importFrom dplyr case_when filter group_by inner_join mutate summarise rename select %>%

SupplPlotsFSDP <- function(repReg, scenarioType = "manuscript", file = NULL, calorieSupply = TRUE, caseRegion = NULL) {

#repReg <- "C:/Users/IIMA/Dropbox (IFPRI)/PhD/FSEC/Data/v26_FSDP_reg.rds"
 # repReg <- "C:/PIK/SDPplot/v17_FSDP_reg.rds"

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
}  else if (scenarioType == "manuscript") {
   rep <- convertReportFSDP(repReg, scengroup = c("FSECa", "FSECb", "FSECc", "FSECd", "FSECe"), subset = FALSE, varlist = NULL)
   rep <- filter(rep, scenario %in% c("SSP2bau","ExternalPressures",
                                      "Sufficiency","Livelihoods","NatureSparing", "AgroMngmt", "FSDP"))
   } else {
  stop("Scenario type does not exist")
}

plots <- list()

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
         region %in% ROW ~ "Rest of World",
         region == "GLO" ~ "World",
         region == "World" ~ "World"),
       RegionG = factor(RegionG,
                        levels = c("High-Income \n Regions",
                                   "Rest of World", "Low-Income Regions", "World")))
}

scens <- filter(scens, !is.na(RegionG))

scenarios <- as.character(unique(scens$scenario)) # re-order scenario factors to put BAU first
scenarios <- c(scenarios[which(scenarios == "SSP2bau")], scenarios[-which(scenarios == "SSP2bau")])

scens$scenario <- factor(scens$scenario,
                        levels = scenarios)

# extract population to use as a separate column
pop <- filter(scens, variable == "Population") %>%
  rename("pop" = value) %>%
  select(model, scenario, region, RegionG, period, pop)


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
  group_by(scenario, period, FoodGroup, region, RegionG) %>%
  summarise(CalorieSupply = sum(value))  %>% # sum across food groups with above grouping
  inner_join(pop) %>%
  group_by(scenario, period, FoodGroup, RegionG) %>%  # weighted mean across regions w/ above grouping
  summarise(CalorieSupply = weighted.mean(CalorieSupply, w = pop), pop = sum(pop))

food_df <- food_df[-which(food_df$period == 2020 & food_df$scenario != "SSP2bau"), ] # remove nonBAU 2010 values
food_df$pop_barwidth <- rescale(food_df$pop, c(0.05, 0.45))   # scale pop for barwidths

if (!is.null(caseRegion)) {
  food_df <- food_df %>% filter(RegionG == selRegion)
  plotCalSupply <- ggplot() +
    facet_grid(cols = vars(period), scales = "free_x", space = "free_x",   switch = "x") +
    geom_col(data = food_df[order(food_df$FoodGroup), ],
             aes(x = scenario, y = CalorieSupply, group = scenario, fill = FoodGroup),
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
           aes(x = scenario, y = CalorieSupply, group = scenario, fill = FoodGroup),
           position = "stack",
width = filter(food_df[order(food_df$FoodGroup), ],
                                   RegionG == "High-Income \n Regions")$pop_barwidth
  ) +
  geom_col(data = filter(food_df[order(food_df$FoodGroup), ], RegionG == "Rest of World"),
           aes(x = as.numeric(scenario) + 0.23, y = CalorieSupply, group = scenario, fill = FoodGroup),
           position = "stack",
width = filter(food_df[order(food_df$FoodGroup), ],
                         RegionG == "Rest of World")$pop_barwidth
  ) +
  geom_col(data = filter(food_df[order(food_df$FoodGroup), ], RegionG == "Low-Income Regions"),
           aes(x = as.numeric(scenario) + 0.54, y = CalorieSupply, group = scenario, fill = FoodGroup),
width = filter(food_df[order(food_df$FoodGroup), ],
                          RegionG == "Low-Income Regions")$pop_barwidth
  ) +
  themeSupplFood(base_size = 24) +
  theme(strip.text.x = element_text(size = 20), axis.text.x = element_text(size = 25,
                                                                           vjust = 1.5))+
  ylab("Kcal/capita/day") +
  scale_fill_manual(values = c("#fcba03", "#a11523", "#66407a", "#40945a"),
                    guide = guide_legend(reverse = TRUE)) +
  if (calorieSupply) {
    labs(title = "Calorie Supply")
  } else {
    labs(title = "Calorie Intake")
  }

}

### product demand per capita
demandCats <- scens[grep("Demand\\|\\+\\|", scens$variable), ]$variable %>%
  unique() %>%
  as.vector()
demandCats <- demandCats[!grepl("Value", demandCats)]

demandCats <- demandCats[-which(demandCats %in% c("Demand|+|Roundwood", "Demand|+|Domestic Balanceflow"))]

agDem <- filter(scens, variable %in% demandCats,
                region != "World",
                period %in% c(2020, 2050)) %>%
  inner_join(pop) %>%
  mutate(variable = gsub("Demand\\|\\+\\|", "", variable),
         variable = factor(variable,
                           levels = c("Food", "Feed", "Material", "Bioenergy",
                                      "Processing", "Agricultural Supply Chain Loss",  "Seed")),
         perCap = value / pop) %>%
  group_by(scenario, period, variable, RegionG) %>%  # weighted mean across regions w/ above grouping
  summarise(value = weighted.mean(value, w = pop),
            pop = sum(pop))

agDem <- agDem[-which(agDem$period == 2020 & agDem$scenario != "SSP2bau"), ] # remove nonBAU 2010 values
agDem$pop_barwidth <- rescale(agDem$pop, c(0.2, 0.8))   # scale pop for barwidths

if (!is.null(caseRegion)) {
  plotAgDem <- ggplot() +
    geom_col(data = agDem,
             aes(x = scenario, y = value, group = scenario, fill = variable),
             position = "stack",
             width = filter(agDem,
                            RegionG == selRegion)$pop_barwidth) +
    themeSupplFood(base_size = 25) +
    labs(title = "Crop-Based Product Demand") +
    facet_grid(cols = vars(period), scales = "free_x", space = "free_x",   switch = "x") +
    ylab("Mt dm") +
    scale_fill_manual(values = c("#FCE900", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84"),
                      guide = guide_legend(reverse = TRUE))
  } else {
plotAgDem <- ggplot() +
  geom_col(data = filter(agDem, RegionG == "High-Income \n Regions"),
           aes(x = scenario, y = value, group = scenario, fill = variable),
           position = "stack",
           width = filter(agDem,
                          RegionG == "High-Income \n Regions")$pop_barwidth) +
  geom_col(data = filter(agDem, RegionG == "Rest of World"),
           aes(x = as.numeric(scenario) + 0.2, y = value, group = scenario, fill = variable),
           position = "stack",
           width = filter(agDem,
                          RegionG == "Rest of World")$pop_barwidth) +
  geom_col(data = filter(agDem, RegionG == "Low-Income Regions"),
           aes(x = as.numeric(scenario) + 0.5, y = value, group = scenario, fill = variable),
           position = "stack",
           width = filter(agDem,
                          RegionG == "Low-Income Regions")$pop_barwidth) +
  themeSupplFood(base_size = 24) +
  theme(strip.text.x = element_text(size = 20))+
  labs(title = "Crop-Based Product Demand") +
  facet_grid(cols = vars(period), scales = "free_x", space = "free_x",   switch = "x") +
  ylab("Mt dm") +
  scale_fill_manual(values = c("#FCE900", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84"),
                    guide = guide_legend(reverse = TRUE, title = "Product"))
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
  facet_wrap(~scenario, nrow = 3) +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab(unit) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = variable), position = "stack") +
  geom_area(aes(y = negative, fill = variable), position = "stack") +
  scale_fill_manual("AFOLU emission type",
                    values = rev(c("#1b9e77", "#d95f02", "#7570b3")),
                    labels = function(x) parse(text = x)) +
  stat_summary(fun = "sum", colour = "black", size = 1,
               geom = "line", mapping = aes(group = scenario, y = value)) +
  theme(legend.position = "bottom", axis.text = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 2, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab(NULL)


# emis reg CUMULATIVE
emissReg <- filter(scens, RegionG != "World",
                    variable %in% varEmiss,
                    period > 2020) %>%
  droplevels() %>%
  mutate(variable = factor(variable, levels = rev(varEmiss), labels = names(rev(varEmiss))),
         scenario = factor(scenario, levels = rev(levels(scenario))),
         value = case_when( # GWP
           variable == "CO2" ~ value * 1,
           variable == "CH4" ~ value * 27,
           variable == "N2O" ~ value * 273)) %>%
  group_by(scenario, region, variable) %>%
  mutate(value = cumsum(value / 1000) * 5) %>%   # Gt and cumulative, multiply by the 5 year time steps
  filter(period == 2050) # subset to 2050 value

emissReg$positive <- ifelse(emissReg$value >= 0, emissReg$value, 0)
emissReg$negative <- ifelse(emissReg$value < 0, emissReg$value, -1e-36)

# emiss_reg <- emiss_reg[-which(emiss_reg$period == 2020 & emiss_reg$scenario!= "BAU"),] #remove nonBAU 2010 values
unit <- expression(bold("Gt CO"[2] ~ "since 2020")) # "Gt CO2eq since 2020"

if (!is.null(caseRegion)) {
  emissReg <- emissReg %>% filter(RegionG == selRegion)
}

plotEmissReg <- ggplot(emissReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 20, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  scale_fill_manual("AFOLU emission type", values = c("#1b9e77", "#d95f02", "#7570b3"),
                    labels = function(x) parse(text = x)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenario, x = value)) +
  theme(legend.position = "bottom") +
  theme(strip.text.y = element_text(size = 14), axis.text.y= element_text(size = 12)) +
  guides(fill = guide_legend("AFOLU emission type", ncol = 4, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab(unit) + scale_x_continuous(guide = guide_axis(check.overlap = TRUE), expand = expansion(mult = c(0.05, 0.1)),
                                  breaks = pretty_breaks()) # breaks= function(x) seq(round(min(x)/0.5)*0.5, round(max(x)/0.5)*0.5, by = 1)#+scale_x_continuous(breaks = c(-2,0,2,4))# + labs(caption = paste(Sys.Date()))

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
names(landVar) <- c("Cropland", "Pasture", "Primary Forest","Secondary Forest", "Timber",
                    "Aff NDC", "Aff CO2-Price",  "Bioenergy", "Other Natural", "Urban")

land_df <- filter(scens,
                  period <= 2050,
                  period > 2005,
                  variable %in% landVar) %>%
  droplevels() %>%
  group_by(model, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(landVar),
                           labels = names(rev(landVar))),
         value = case_when(variable == "Cropland" ~ value[variable == "Cropland"] - value[variable == "Bioenergy"], # get non-bioenergy cropland
                           variable != "Cropland" ~ value)) %>%
  group_by(model, scenario, region, variable) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping


land_df$positive <- ifelse(land_df$value >= 0, land_df$value, 0)
land_df$negative <- ifelse(land_df$value < 0, land_df$value, -1e-36)

landGlo <- filter(land_df, region == "GLO")

plotLandGlo <- ggplot(landGlo, aes(x = period)) +
  facet_wrap(~scenario, nrow = 2) +
  themeSupplReg(base_size = 22, panel.spacing = 3, rotate_x = 90) +
  ylab("Change in Mha compared to 2020") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = variable), position = "stack") +
  geom_area(aes(y = negative, fill = variable), position = "stack") +
  scale_fill_manual("Land type", values = rev(c( "#FCED0F", "#E6AB02","darkgreen", "#99cc00", "lightgreen",
                                               "brown3", "#785318", "#9e9ac8", "#ae017e","#08589e"))) +
  theme(legend.position = "bottom", legend.text = element_text(size=16)) + guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)


landReg <- filter(land_df, region != "GLO", period == 2050) %>%
  group_by(model, scenario, variable, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))

if (!is.null(caseRegion)) {
  landReg <- landReg %>% filter(RegionG == selRegion)
}
plotLandReg <- ggplot(landReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 20, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual("Land type", values = rev(c( "#FCED0F", "#E6AB02","darkgreen", "#99cc00", "lightgreen",
                                                          "brown3", "#785318", "#9e9ac8", "#ae017e","#08589e"))) +
   theme(legend.position = "bottom", legend.text = element_text(size=16),
        strip.text.y = element_text(size = 14), axis.text.y= element_text(size = 18),
        axis.text.x = element_text(size = 12)) +
  guides(fill = guide_legend("Land type", ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab("Change in Mha compared to 2020") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) +
  theme(panel.spacing=unit(100,"lines"))
# breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


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
  mutate(CropGroup = case_when(           # recategorize products
    variable %in% cereals ~ "Cereals",
    variable %in% legumes ~ "Legumes",
    variable %in% plantations ~ "Plantations (Bioenergy, Oilpalm, Sugar cane)",
    variable %in% fruits ~ "Fruits, Vegetables, and Nuts",
    variable %in% other ~ "Other Crops",
    variable %in% fallow ~ "Fallow Cropland"),
    CropGroup = factor(CropGroup,
                       levels = c("Cereals", "Legumes",
                                  "Plantations (Bioenergy, Oilpalm, Sugar cane)", "Fruits, Vegetables, and Nuts",
                                  "Other Crops", "Fallow Cropland" ))) %>%
  group_by(model, scenario, region, RegionG, period, CropGroup) %>%
  summarise(value = sum(value)) %>%
  group_by(model, scenario, region, CropGroup) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping

# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
crop_df$positive <- ifelse(crop_df$value >= 0, crop_df$value, 0)
crop_df$negative <- ifelse(crop_df$value < 0, crop_df$value, -1e-36)

cropGlo <- filter(crop_df, region == "GLO")

plotCropGlo <- ggplot(cropGlo, aes(x = period)) +
  facet_wrap(~scenario, nrow = 2) +
  themeSupplReg(base_size = 18, panel.spacing = 3, rotate_x = 90) +
  ylab("Cropland change in Mha \n compared to 2020") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = CropGroup), position = "stack") +
  geom_area(aes(y = negative, fill = CropGroup), position = "stack") +
  stat_summary(fun = "sum", colour = "black", size = 1,
               geom = "line", mapping = aes(group = scenario, y = value)) +
  scale_fill_manual("Cropland type", values = rev(c(
    "#E6AB02", "lightblue", "#9e9ac8", "#ACC3A6", "#4b6fb8", "darkgreen", "#fcba03"))) +
  theme(legend.position = "bottom", legend.text = element_text(size=16),
      strip.text.y = element_text(size = 14), axis.text.y= element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 3, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab(NULL)

cropReg <- filter(crop_df, region != "GLO", period == 2050) %>%
  group_by(model, scenario, CropGroup, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))

# write.csv(b,file="SI_lu_reg_2100_bar.csv",row.names = FALSE)
 if (!is.null(caseRegion)) {
   cropReg <- cropReg %>% filter(RegionG == selRegion)
 }

 plotCropReg <- ggplot(cropReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 16, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = CropGroup), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = CropGroup, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenario, x = value)) +
  scale_fill_manual("Cropland type", values = rev(c(
    "#E6AB02", "lightblue", "#9e9ac8", "#ACC3A6", "#4b6fb8", "darkgreen", "#fcba03"))) +
  theme(legend.position = "bottom", legend.text = element_text(size=14),
        strip.text.y = element_text(size = 16), axis.text.y= element_text(size = 16),
        axis.text.x= element_text(size = 16)) +
  guides(fill = guide_legend("Land type", ncol = 3, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Change in Mha compared to 2020") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


####### Nitrogen##############

nitrVar <- scens[grep("Nutrient Surplus", scens$variable), ]$variable %>%  unique()
# scens[grep("Nitrogen", scens$variable),]$variable %>%  unique()
names(nitrVar) <- c("Cropland", "Pasture")


nitr_df <- filter(scens,
                  period <= 2050,
                  period > 2015,
                  variable %in% nitrVar) %>%
  droplevels() %>%
  group_by(model, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(nitrVar),
                           labels = names(rev(nitrVar)))) %>%
  group_by(model, scenario, region, variable)


# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
nitr_df$positive <- ifelse(nitr_df$value >= 0, nitr_df$value, 0)
nitr_df$negative <- ifelse(nitr_df$value < 0, nitr_df$value, -1e-36)

nitrGlo <- filter(nitr_df, region == "GLO")

plotNitrGlo <- ggplot(nitrGlo, aes(x = period)) +
  facet_wrap(~scenario, nrow = 2) +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab("Nitrogen Surplus (Mt Nr/yr)") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = variable), position = "stack") +
  geom_area(aes(y = negative, fill = variable), position = "stack") +
  scale_fill_manual("Land type", values = rev(c(
    "#E6AB02", "lightblue", "#9e9ac8", "#ACC3A6", "purple", "darkgreen", "yellow3"))) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)

nitrReg <- filter(nitr_df, region != "GLO", period %in% c(2020, 2050)) %>%
  group_by(model, scenario, variable, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))
nitrReg <- nitrReg[-which(nitrReg$period == 2020 & nitrReg$scenario != "SSP2bau"), ] # remove nonBAU 2010 values

# write.csv(b,file="SI_lu_reg_2100_bar.csv",row.names = FALSE)

if (!is.null(caseRegion)) {
  nitrReg <- nitrReg %>% filter(RegionG == selRegion)
}

plotnitrReg <- ggplot(nitrReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 18, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual("nitrland type", values = rev(c(
    "#E6AB02", "lightblue", "#9e9ac8", "#ACC3A6", "purple", "darkgreen", "yellow3"))) +
  theme(legend.position = "bottom", strip.text.y = element_text(angle = 0, size = 14),
         axis.text= element_text(size = 16)) +
 guides(fill = guide_legend("Land type", ncol = 3, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Nitrogen Surplus (Mt Nr/yr)") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


############ Water #######

waterVar <- "Resources|Water|Withdrawal|Agriculture"
names(waterVar) <- c("Agricultural Water Withdrawals")

water_df <- filter(scens,
                   period <= 2050,
                   period > 2015,
                   variable %in% waterVar) %>%
  droplevels() %>%
  group_by(model, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(waterVar),
                           labels = names(rev(waterVar))),
         scenario = factor(scenario, levels = c(
                                     "SSP2bau", "AgroMngmt", "NatureSparing",
                                     "ExternalPressures", "Livelihoods", "Sufficiency",
                                     "FSDP"))) %>%
  group_by(model, scenario, region, variable) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping

# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
water_df$positive <- ifelse(water_df$value >= 0, water_df$value, 0)
water_df$negative <- ifelse(water_df$value < 0, water_df$value, -1e-36)

waterGlo <- filter(water_df, region == "GLO")

plotWaterGlo <- ggplot(waterGlo, aes(x = period)) +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab("Change in Water withdrawals \n from 2020 value, in km3") +
  geom_line(aes(y = value, color = scenario), position = "stack", size = 1.3) +
  scale_color_manual(values = c("#009FFA", "#FFDC3D", "#008607", "#6A0213", "#9400E6", "#00DCB5", "#008169")) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 3, title.position = "left", byrow = FALSE)) + xlab(NULL)


waterReg <- filter(water_df, region != "GLO", period == 2050) %>%
  group_by(model, scenario, variable, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))

if (!is.null(caseRegion)) {
  waterReg <- waterReg %>% filter(RegionG == selRegion)
}

plotWaterReg <- ggplot(waterReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 18, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenario, x = value)) +
  scale_fill_manual(" ", values = rev(c("purple", "#2080EC", "lightblue"))) +
  theme(legend.position = "bottom", strip.text = element_text(angle = 0, size = 12),
        axis.text= element_text(size = 14)) +  guides(fill = element_blank()) +
  # guide_legend("Cropland type",ncol=5,title.position = "left", byrow = TRUE,reverse=FALSE)) +
  xlab("Change in Water withdrawals from 2020 value, in km3") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


### Health

healthVar <-  scens[grep("Nutrition\\|Anthropometrics\\|People", scens$variable), ]$variable %>%  unique()
names(healthVar) <- c("Normal weight", "Obese", "Overweight", "Underweight")


health_df <- filter(scens,
                    period <= 2050,
                    period > 2015,
                    variable %in% healthVar) %>%
  droplevels() %>%
  group_by(model, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(healthVar),
                           labels = names(rev(healthVar))),
         variable = factor(variable, levels = rev(c("Normal weight", "Obese", "Overweight", "Underweight"))))


healthGlo <- filter(health_df, region == "GLO")
plotHealthGlo <- ggplot(healthGlo, aes(x = period)) +
  facet_wrap(~scenario, nrow = 2) +
  themeSupplReg(base_size = 18, panel.spacing = 3, rotate_x = 90) +
  ylab("Million People") +
  geom_area(aes(y = value, fill = variable), position = "stack") +
  scale_fill_manual("Nutrition Indicators",
                    values = rev(c("#2080EC", "lightblue", "blue", "purple"))) +
  theme(legend.position = "bottom", strip.text.y = element_text(angle = 0, size = 14),
        axis.text= element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)
# healthGloP

healthReg <- filter(health_df, region != "GLO", period == 2050) %>%
  group_by(model, scenario, variable, period, RegionG) %>%
  summarise(value = sum(value)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))

if (!is.null(caseRegion)) {
  healthReg <- healthReg %>% filter(RegionG == selRegion)
}

plotHealthReg <- ggplot(healthReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 18, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = value, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual(element_blank(),
                    values = rev(c("#2080EC", "lightblue", "blue", "purple"))) +
  theme(legend.position = "bottom", strip.text.y = element_text(angle = 0, size = 14),
        axis.text = element_text(size = 14)) +
  guides(fill = guide_legend("Indicator", ncol = 3, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab("Million People") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


######## EMPLOYMENT ##########

## Global: Absolute change in employment - global areaplot

empVar <-  setdiff(scens[grep("Agricultural employment\\|", scens$variable), ]$variable %>%  unique(),
                        "Agricultural employment|Crop and livestock products")

emp_df <- filter(scens,
                  period <= 2050,
                  period > 2015,
                  variable %in% empVar) %>%
  droplevels() %>%
  mutate(Products = case_when(           # recategorize products
    variable == "Agricultural employment|+|Crop products" ~ "Crop products",
    variable == "Agricultural employment|+|Livestock products" ~ "Livestock products"),
    Products = factor(Products,
                       levels = c("Crop products", "Livestock products"))) %>%
  group_by(model, scenario, region, RegionG, period, Products) %>%
  filter(region == "GLO")

plotEmpGlo <- ggplot(emp_df, aes(x = period)) +
  facet_wrap(~scenario, nrow = 2) +
  themeSupplReg(base_size = 18, panel.spacing = 3, rotate_x = 90) +
  ylab("Number of People Employed in Agriculture \n (millions)") +
  # geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = value, fill = Products), position = "stack") +
  scale_fill_manual("Production", values = rev(c("#ffd256", "#489448"))) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend("Production", ncol = 3, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)


## Regional: abolute change 2020 to 2050 - barplot

empVar <-  setdiff(scens[grep("Agricultural employment\\|", scens$variable), ]$variable %>%  unique(),
                        "Agricultural employment|Crop and livestock products")

emp_df <- filter(scens,
                  period <= 2050,
                  period > 2015,
                  variable %in% empVar) %>%
  droplevels() %>%
  mutate(Products = case_when(           # recategorize products
    variable == "Agricultural employment|+|Crop products" ~ "Crop products",
    variable == "Agricultural employment|+|Livestock products" ~ "Livestock products"),
    Products = factor(Products,
                       levels = c("Crop products", "Livestock products"))) %>%
  group_by(model, scenario, region, RegionG, period, Products) %>%
  summarise(value = sum(value)) %>%
  group_by(model, scenario, region, Products) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping

emp_df$positive <- ifelse(emp_df$value >= 0, emp_df$value, 0)
emp_df$negative <- ifelse(emp_df$value < 0, emp_df$value, -1e-36)

facet_bounds <- data.frame("RegionG" = rep(c("High-Income \n Regions", "Rest of World", "Low-Income Regions"), 2),
                           "value" = c(-70, NA, NA, 7, NA, NA), "scenario" = "SSP2bau")

empReg <- filter(emp_df, region != "GLO", period == 2050) %>%
  group_by(model, scenario, Products, period, RegionG) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))


if (!is.null(caseRegion)) {
  empReg <- empReg %>% filter(RegionG == selRegion) %>% mutate(RegionG_f = selRegion)
} else {
  empReg$RegionG_f <- factor(empReg$RegionG, levels = c("High-Income \n Regions", "Rest of World", "Low-Income Regions"))
}


plotEmpReg <- ggplot(empReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(RegionG_f), scales = "free_y", space = "free") +
  themeSupplReg(base_size = 20, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = Products), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = Products, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenario, x = value)) +
  scale_fill_manual("Production", values = rev(c("#ffd256", "#489448"))) +
  theme(legend.position = "bottom", strip.text = element_text(angle = 0, size = 14),
        axis.text= element_text(size = 14)) +
  guides(fill = guide_legend("Production", ncol = 2, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Change in agricultural employment compared to 2020 (mio. people)") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

plotEmpReg <- plotEmpReg + geom_blank(data = facet_bounds, aes(x = value, y = scenario))


##### Hourly Labour Costs ####

laborVar <-  "Hourly labor costs"
names(laborVar) <- "Hourly Labor Costs"

# extract total hours worked to use as a separate column
hoursWorked <- filter(scens, variable == "Total Hours Worked|Crop and livestock products") %>%
  rename("hours" = value) %>%
  select(model, scenario, region, RegionG, period, hours)

labor_df <- filter(scens,
                 period <= 2050,
                 period > 2015,
                 variable %in% laborVar) %>%
  droplevels() %>%
  group_by(model, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(laborVar),
                           labels = names(rev(laborVar)))) %>%
  inner_join(hoursWorked) %>%
  group_by(model, scenario, region, variable)

labReg <- filter(labor_df, region != "GLO") %>%
  group_by(model, scenario, variable, period, RegionG) %>%
  summarise(value = weighted.mean(value, w = hours)) %>%
  mutate(scenario = factor(scenario, levels = c(
    "SSP2bau", "AgroMngmt", "NatureSparing",
    "ExternalPressures", "Livelihoods", "Sufficiency",
    "FSDP"))) %>%
  group_by(model, scenario, RegionG, variable)

if (!is.null(caseRegion)) {
  labReg <- labReg %>% filter(RegionG == selRegion)
}

plotLabReg <- ggplot(labReg,
                     aes(x = period)) +
  facet_wrap(~RegionG, scales = "free") +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab("Hourly Labor Costs (USD05/h)") +
  geom_line(aes(y = jitter(value), color = scenario),  lwd = 1.1) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL) +
  scale_color_manual(values = c("#009FFA", "#FFDC3D", "#008607", "#6A0213", "#9400E6", "#00DCB5", "#008169"))


######## INEQUALITY INDICATORS ##########

ineqVar <-  c("Income|Gini Coefficient",
              "Income|Average Income of Lower 40% of Population",
              "Income|Fraction of Population below half of Median Income",
              "Income|Number of People Below 1.90$/Day",
              "Income|Number of People Below 3.20$/Day",
              "Income|Number of People Below 5.50$/Day")
names(ineqVar) <- ineqVar

ineq_df <- filter(scens,
                   period <= 2050,
                   period > 2015,
                   variable %in% ineqVar) %>%
  droplevels() %>%
  group_by(model, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(ineqVar),
                           labels = names(rev(ineqVar))),
         scenario = factor(scenario, levels = c(
           "SSP2bau", "AgroMngmt", "NatureSparing",
           "ExternalPressures", "Livelihoods", "Sufficiency",
           "FSDP"))) %>%
  group_by(model, scenario, region, variable)

ineqGlo <- filter(ineq_df, region == "GLO")

plotGiniGlo <- ggplot(filter(ineqGlo, variable == "Income|Gini Coefficient"),
                    aes(x = period)) +
  themeSupplReg(base_size = 20, panel.spacing = 3, rotate_x = 90) +
  ylab("Gini Coefficient") +
  geom_line(aes(y = value, color = scenario), lwd = 1.1) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL) +
  scale_color_manual(values = c("#009FFA", "#FFDC3D", "#008607", "#6A0213", "#9400E6", "#00DCB5", "#008169"))


 # scale_colour_manual(values = c("#1f78b4", "#33a02c", "#b2df8a", "#d95f02", "#7570b3", "#e7298a"))

ineqReg <- filter(ineq_df, region != "GLO") %>%
  group_by(model, scenario, variable, period, RegionG) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario)))) %>%
  group_by(model, scenario, RegionG, variable) %>%
  filter(period == 2050)


if (!is.null(caseRegion)) {
  ineqReg <- ineqReg %>% filter(RegionG == selRegion)
} else {

  ineqGini <- filter(ineqReg, variable == "Income|Gini Coefficient") %>%
              inner_join(pop) %>%
              group_by(model, scenario, RegionG, variable, period) %>%
              summarise(value = weighted.mean(value, w = pop))
}

plotGiniReg <- ggplot(ineqGini, aes(y = scenario)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 18, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = value), stat = "identity", width = 0.75, fill = "#A455CF") +
  theme(legend.position = "bottom", legend.text = element_text(size=16),
        strip.text = element_text(size = 14), axis.text= element_text(size = 16))+
  guides(fill = guide_legend("Indicator", ncol = 5, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Gini Coefficient") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


plotBelowPovGlo <- ggplot(
                  filter(ineqGlo, variable == "Income|Number of People Below 3.20$/Day"),
                   aes(x = period)) +
  themeSupplReg(base_size = 16, panel.spacing = 3, rotate_x = 90) +
  ylab("Million People below 3.20$/Day Poverty Line") +
  geom_line(aes(y = value, color = scenario), lwd = 1.1) +
  theme(legend.position = "bottom", legend.text = element_text(size=16),
        strip.text = element_text(size = 14), axis.text= element_text(size = 12)) +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab(NULL) +
  scale_color_manual(values = c("#009FFA", "#FFDC3D", "#008607", "#6A0213", "#9400E6", "#00DCB5", "#008169"))


plotBelowPovReg <- ggplot(filter(ineqReg, variable == "Income|Number of People Below 3.20$/Day",
                                 RegionG != "High-Income \n Regions"),
                          aes(y = scenario)) +
  facet_grid(vars(period), vars(RegionG), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = value), stat = "identity", width = 0.75, fill = "#A455CF") +
  theme(legend.position = "bottom", legend.text = element_text(size=16),
        strip.text = element_text(size = 14), axis.text= element_text(size = 14))  +
  guides(fill = guide_legend("Indicator", ncol = 5, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Million People below 3.20$/Day Poverty Line") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


trytoplot <- function(tryplot) {
  if (inherits(try(ggplot_build(tryplot)), "try-error")) {
    warning("One of the map plot scripts failed")
    return(NULL)
  } else {
    return(tryplot)
  }
}
combined <- (trytoplot(plotCalSupply)
             + trytoplot(plotAgDem)
             + trytoplot(plotEmissGlo)
             + trytoplot(plotEmissReg)
             + trytoplot(plotLandGlo)
             + trytoplot(plotLandReg)
             + trytoplot(plotCropGlo)
             + trytoplot(plotCropReg)
             + trytoplot(plotNitrGlo)
             + trytoplot(plotnitrReg)
             + trytoplot(plotWaterGlo)
             + trytoplot(plotWaterReg)
             + trytoplot(plotHealthGlo)
             + trytoplot(plotHealthReg)
             + trytoplot(plotEmpGlo)
             + trytoplot(plotEmpReg)
             + trytoplot(plotLabReg)
             + trytoplot(plotGiniGlo)
             + trytoplot(plotGiniReg)
             + trytoplot(plotBelowPovGlo)
             + trytoplot(plotBelowPovReg)
    )

combined <- combined + plot_layout(guides = "keep", ncol = 2, byrow = FALSE)

if (is.null(file)) {
  return(combined)
} else {
  ggsave(filename = file, combined, width = 12, height = 7, scale = 1.5, bg = "white")
}


}
