globalVariables(c("CalorieSupply", "CropGroup", "FoodGroup", "RegionG", "negative", "model", "percentage", "positive",
                  "hours", "Products", "RegionG_f"))

#' @title SupplPlotsFSDPIndia
#' @description creates supplementary plots for FSDP MAgPIE runs for India case study
#'
#' @export
#'
#' @param repReg rds file or data.frame with all MAgPIE runs, produced with FSDP_collect.R output script.
#' @param scenarioType options: all, or one of the a-e groupings
#' @param save binary to save files to output directory
#' @param outputdir outputdirectory path
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author David M Chen, Vartika Singh
#' @import ggplot2 data.table scales magpiesets scales
#' @importFrom stats weighted.mean
#' @importFrom dplyr case_when filter group_by inner_join mutate summarise rename select %>%

SupplPlotsFSDPIndia <- function(repReg, scenarioType = "all", save = TRUE, outputdir = "/p/projects/magpie/users/beier/FSECmodeling/output") {

  if(!dir.exists(file.path(outputdir, "SupplPlots"))){
    dir.create(file.path(outputdir, "SupplPlots"))
  }
  savedir <- file.path(outputdir, "SupplPlots")
#  repReg <- paste0(outputdir,"/v16_FSDP_reg.rds")

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
} else {
  stop("Scenario type does not exist")
}

plots <- list()

india <- "IND"
#HIR <- c("USA", "CAN", "ANZ", "EUR", "JKO")
#ROW <- c("CHA", "BRA", "LAM", "MEA", "NEA", "NEU", "OAS", "REF")

scens <- filter(rep, region == "IND")

scenarios <- as.character(unique(scens$scenario)) # re-order scenario factors to put BAU first
scenarios <- c(scenarios[which(scenarios == "BAU")], scenarios[-which(scenarios == "BAU")])

scens$scenario <- factor(scens$scenario,
                        levels = scenarios)

# extract population to use as a separate column
pop <- filter(scens, variable == "Population") %>%
  rename("pop" = value) %>%
  select(model, scenario, region, period, pop)


####### calorie supply
kli <- findset("kli")
kcr <- findset("kcr")
kfo <- findset("kfo")
ksd <- findset("ksd")

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



# groupings
staples <- c(cereals, otherCrops[-grep("Fruit", otherCrops)],
             sugarCrops, processed[grep("Microbial|Brans", processed)],
             oilCrops[grep("Sunflower|Other oil|Soybean", oilCrops)])
animalProducts <- c(animal, fish)
emptyCalories <- processed[grep("Alcohol|Sugar|Oils|Molasses", processed)]
fruitVegNut <- c(otherCrops[grep("Fruit", otherCrops)], oilCrops[grep("Groundnut", oilCrops)])

food_df <- filter(scens, variable %in% c(cereals, oilCrops, otherCrops,
                                         sugarCrops, processed, animal, fish), # filter food products
          #        region != "GLO",   # remove global
                  period %in% c(2020, 2050)) %>%
  mutate(FoodGroup = case_when( # recategorize products and regions
    variable %in% staples ~ "Staples",
    variable %in% animalProducts ~ "Animal Products",
    variable %in% emptyCalories ~ "Empty Calories",
    variable %in% fruitVegNut ~ "Fruits, Vegetables, and Nuts"),
    FoodGroup = factor(FoodGroup,
                       levels = c("Staples", "Animal Products",
                                  "Empty Calories", "Fruits, Vegetables, and Nuts"))) %>%
  group_by(scenario, period, FoodGroup, region) %>%
  summarise(CalorieSupply = sum(value))  %>% # sum across food groups with above grouping
  inner_join(pop) %>%
  group_by(scenario, period, FoodGroup, region) %>%  # weighted mean across regions w/ above grouping
  summarise(CalorieSupply = weighted.mean(CalorieSupply, w = pop), pop = sum(pop))

food_df <- food_df[-which(food_df$period == 2020 & food_df$scenario != "BAU"), ] # remove nonBAU 2010 values
food_df$pop_barwidth <- rescale(food_df$pop, c(0.05, 0.45))   # scale pop for barwidths

calSupply <- ggplot() +
  facet_grid(cols = vars(period), scales = "free_x", space = "free_x",   switch = "x") +
  geom_col(data = food_df[order(food_df$FoodGroup), ],
           aes(x = scenario, y = CalorieSupply, group = scenario, fill = FoodGroup),
           position = "stack",
           width = filter(food_df[order(food_df$FoodGroup), ], region == "IND")$pop_barwidth) +
  themeSupplFood(base_size = 25) +
  labs(title = "Calorie Supply") +
  ylab("Kcal/capita/day") +
  scale_fill_manual(values = c("#fcba03", "#a11523", "#66407a", "#40945a"),
                    guide = guide_legend(reverse = TRUE))


if (save) {
ggsave(filename = file.path(savedir,"FigS1_CalSupply.pdf"), calSupply, width = 25, height = 10, scale = 1)
} else {
plots <- list(plots, calSupply)
}


### product demand per capita
demandCats <- scens[grep("Demand\\|\\+\\|", scens$variable), ]$variable %>%
  unique() %>%
  as.vector()
demandCats <- demandCats[!grepl("Value", demandCats)]

demandCats <- demandCats[-which(demandCats %in% c("Demand|+|Roundwood", "Demand|+|Domestic Balanceflow"))]

agDem <- filter(scens, variable %in% demandCats,
              #  region != "World",
                period %in% c(2020, 2050)) %>%
  inner_join(pop) %>%
  mutate(variable = gsub("Demand\\|\\+\\|", "", variable),
         variable = factor(variable,
                           levels = c("Food", "Feed", "Material", "Bioenergy",
                                      "Processing", "Agricultural Supply Chain Loss",  "Seed")),
         perCap = value / pop) %>%
  group_by(scenario, period, variable, region) %>%  # weighted mean across regions w/ above grouping
  summarise(value = weighted.mean(value, w = pop),
            pop = sum(pop))

agDem <- agDem[-which(agDem$period == 2020 & agDem$scenario != "BAU"), ] # remove nonBAU 2010 values
agDem$pop_barwidth <- rescale(agDem$pop, c(0.2, 0.8))   # scale pop for barwidths


agDemPlot <- ggplot() +
  geom_col(data = agDem,
           aes(x = scenario, y = value, group = scenario, fill = variable),
           position = "stack",
           width = filter(agDem,
                          region == "IND")$pop_barwidth) +
  themeSupplFood(base_size = 25) +
  labs(title = "Crop-Based Product Demand") +
  facet_grid(cols = vars(period), scales = "free_x", space = "free_x",   switch = "x") +
  ylab("Mt dm") +
  scale_fill_manual(values = c("#FCE900", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84"),
                    guide = guide_legend(reverse = TRUE))

if (save) {
  ggsave(filename = file.path(savedir, "FigS2_CropProductDemand.pdf"), agDemPlot, width = 25, height = 10, scale = 1)
} else {
plots <- list(plots, calSupply)
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
               # region == "GLO",
                period <= 2050,
                period > 1995) %>%
  droplevels() %>%
  mutate(variable = factor(variable, levels = rev(varEmiss), labels = names(rev(varEmiss))),
         value = case_when( # GWP
           variable == "CO2" ~ value * 1,
           variable == "CH4" ~ value * 27,
           variable == "N2O" ~ value * 273),
         value = value / 1000)

emiss_glo <- filter(emiss, region == "IND")
emiss_glo$positive <- ifelse(emiss_glo$value >= 0, emiss_glo$value, 0)
emiss_glo$negative <- ifelse(emiss_glo$value < 0, emiss_glo$value, -1e-36)

emissPlotGLO <- ggplot(filter(emiss_glo, scenario %in% c("BAU", "FSDP")), aes(x = period)) +
  facet_grid(vars(region), vars(scenario)) +
  themeSupplReg(base_size = 25, panel.spacing = 3, rotate_x = 90) + ylab(unit) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = variable), position = "stack") +
  geom_area(aes(y = negative, fill = variable), position = "stack") +
  scale_fill_manual("AFOLU emission type",
                    values = rev(c("#1b9e77", "#d95f02", "#7570b3")),
                    labels = function(x) parse(text = x)) +
  stat_summary(fun = "sum", colour = "black", size = 1,
               geom = "line", mapping = aes(group = scenario, y = value)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 4, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab(NULL)


if (save) {
  ggsave(filename = file.path(savedir,"FigS3a_EmissionsGLO.pdf"), emissPlotGLO, width = 20, height = 10, scale = 1)
} else {
plots <- list(plots, emissPlotGLO)
}


# emis reg CUMULATIVE
emiss_reg <- filter(scens, region == "IND",
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

emiss_reg$positive <- ifelse(emiss_reg$value >= 0, emiss_reg$value, 0)
emiss_reg$negative <- ifelse(emiss_reg$value < 0, emiss_reg$value, -1e-36)

# emiss_reg <- emiss_reg[-which(emiss_reg$period == 2020 & emiss_reg$scenario!= "BAU"),] #remove nonBAU 2010 values
unit <- expression(bold("Gt CO"[2] ~ "since 2020")) # "Gt CO2eq since 2020"

emissPlotREG <- ggplot(emiss_reg, aes(y = scenario)) +
  facet_grid(vars(period), vars(region), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  scale_fill_manual("AFOLU emission type", values = c("#1b9e77", "#d95f02", "#7570b3"),
                    labels = function(x) parse(text = x)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenario, x = value)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend("AFOLU emission type", ncol = 4, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab(unit) + scale_x_continuous(guide = guide_axis(check.overlap = TRUE), expand = expansion(mult = c(0.05, 0.1)),
                                  breaks = pretty_breaks()) # breaks= function(x) seq(round(min(x)/0.5)*0.5, round(max(x)/0.5)*0.5, by = 1)#+scale_x_continuous(breaks = c(-2,0,2,4))# + labs(caption = paste(Sys.Date()))


if (save) {
  ggsave(filename = file.path(savedir,"FigS3b_EmissionsREG.pdf"), emissPlotREG, width = 30, height = 15, scale = 1)
} else {
plots <- list(plots, emissPlotREG)
}


# ggsave(filename = "Fig3d_emis_2050_reg.pdf",p,width = 10,height = 4,scale=1)

# #p+theme(legend.text = element_text(margin = margin(r = 10), hjust = 0),legend.title = element_text(margin = margin(r = 10), hjust = 0))
#
# EmisCombined <- emissPlotGLO / emissPlotREG + plot_annotation(tag_levels = 'a')
# EmisCombined <- EmisCombined + plot_layout(guides = "collect",nrow = 2,heights = c(0.9,1.1)) & theme(legend.position = "bottom")
# ggsave(filename = "Fig3_emis_GHG.pdf",combined,width = 8,height = 7,scale=1.3)
# ggsave(filename = "Fig3_emis_GHG.png",combined,width = 8,height = 7,scale=1.3)
#

########## Land Use
landVar <- c("Resources|Land Cover|+|Cropland", "Resources|Land Cover|Cropland|+|Bioenergy crops",
             "Resources|Land Cover|+|Pastures and Rangelands", "Resources|Land Cover|Forest|Managed Forest|+|Plantations",
             "Resources|Land Cover|Forest|Managed Forest|+|NPI/NDC", "Resources|Land Cover|Forest|Managed Forest|+|Afforestation",
             "Resources|Land Cover|Forest|Natural Forest|+|Secondary Forest", "Resources|Land Cover|Forest|Natural Forest|+|Primary Forest", "Resources|Land Cover|+|Other Land",
             "Resources|Land Cover|+|Urban Area")
names(landVar) <- c("Cropland", "Bioenergy", "Pasture", "Timber", "Aff NDC", "Aff CO2-Price", "Secondary Forest", "Primary Forest", "Other Natural", "Urban")

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


# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
land_df$positive <- ifelse(land_df$value >= 0, land_df$value, 0)
land_df$negative <- ifelse(land_df$value < 0, land_df$value, -1e-36)

landGlo <- filter(land_df, region == "IND")

landGloP <- ggplot(filter(landGlo, scenario %in% c("BAU", "FSDP")), aes(x = period)) +
  facet_wrap(~scenario, nrow = 1) +
  themeSupplReg(base_size = 22, panel.spacing = 3, rotate_x = 90) +
  ylab("Change in Mha compared to 2020") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = variable), position = "stack") +
  geom_area(aes(y = negative, fill = variable), position = "stack") +
  scale_fill_manual("Land type", values = rev(c("chocolate4", "brown3", "#E6AB02", "#6a51a3", "#9e9ac8", "#ae017e", "#66A61E", "darkgreen", "#41b6c4", "#08589e"))) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)

if (save) {
  ggsave(filename = file.path(savedir,"FigS4a_LandGLO.pdf"), landGloP, width = 20, height = 10, scale = 1)
} else {
plots <- list(plots, landGloP)
}



landReg <- filter(land_df, region == "IND", period == 2050) %>%
  group_by(model, scenario, variable, period, region) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))

# write.csv(b,file="SI_lu_reg_2100_bar.csv",row.names = FALSE)

landRegP <- ggplot(landReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(region), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual("Land type", values = rev(c("chocolate4", "brown3", "#E6AB02", "#6a51a3", "#9e9ac8", "#ae017e", "#66A61E", "darkgreen", "#41b6c4", "#08589e"))) +
  theme(legend.position = "bottom") + guides(fill = guide_legend("Land type", ncol = 5, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Change in Mha compared to 2020") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

if (save) {
  ggsave(filename = file.path(savedir,"FigS4b_LandREG.pdf"), landRegP, width = 30, height = 15, scale = 1)
} else {
plots <- list(plots, landRegP)
}


# combined <- p1 / p2 + plot_annotation(tag_levels = 'a')
# combined <- combined + plot_layout(guides = "collect",nrow = 2,heights = c(1.1,0.9)) & theme(legend.position = "bottom")
# ggsave(filename = "Fig2_land2.pdf",combined,width = 8,height = 6,scale=1.3)
# ggsave(filename = "Fig2_land2.png",combined,width = 8,height = 6,scale=1.3)

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
  group_by(model, scenario, region, period, CropGroup) %>%
  summarise(value = sum(value)) %>%
  group_by(model, scenario, region, CropGroup) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping

# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
crop_df$positive <- ifelse(crop_df$value >= 0, crop_df$value, 0)
crop_df$negative <- ifelse(crop_df$value < 0, crop_df$value, -1e-36)

cropGlo <- filter(crop_df, region == "IND")

cropGloP <- ggplot(filter(cropGlo, scenario %in% c("BAU", "FSDP")), aes(x = period)) +
  facet_wrap(~scenario, nrow = 1) +
  themeSupplReg(base_size = 25, panel.spacing = 3, rotate_x = 90) +
  ylab("Cropland change in Mha compared to 2020") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = CropGroup), position = "stack") +
  geom_area(aes(y = negative, fill = CropGroup), position = "stack") +
  stat_summary(fun = "sum", colour = "black", size = 1,
               geom = "line", mapping = aes(group = scenario, y = value)) +
  scale_fill_manual("Cropland type", values = rev(c(
    "#E6AB02", "lightblue", "#9e9ac8", "#ACC3A6", "#4b6fb8", "darkgreen", "#fcba03"))) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = FALSE)) + xlab(NULL)


if (save) {
  ggsave(filename = file.path(savedir,"FigS5a_cropGLO.pdf"), cropGloP, width = 25, height = 10, scale = 1)
} else {
plots <- list(plots, cropGloP)
}

cropReg <- filter(crop_df, region == "IND", period == 2050) %>%
  group_by(model, scenario, CropGroup, period, region) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))

# write.csv(b,file="SI_lu_reg_2100_bar.csv",row.names = FALSE)

cropRegP <- ggplot(cropReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(region), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = CropGroup), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = CropGroup, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenario, x = value)) +
  scale_fill_manual("Cropland type", values = rev(c(
    "#E6AB02", "lightblue", "#9e9ac8", "#ACC3A6", "#4b6fb8", "darkgreen", "#fcba03"))) +
  theme(legend.position = "bottom") + guides(fill = guide_legend("Land type", ncol = 5, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Change in Mha compared to 2020") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

if (save) {
  ggsave(filename = file.path(savedir,"FigS5b_CropREG.pdf"), cropRegP, width = 30, height = 15, scale = 1)
} else {
plots <- list(plots, cropRegP)
}



# p2 <- p

# combined <- p1 / p2 + plot_annotation(tag_levels = 'a')
# combined <- combined + plot_layout(guides = "collect",nrow = 2,heights = c(1.1,0.9)) & theme(legend.position = "bottom")
# ggsave(filename = "Fig2_land2.pdf",combined,width = 8,height = 6,scale=1.3)
# ggsave(filename = "Fig2_land2.png",combined,width = 8,height = 6,scale=1.3)

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

# %>%
# mutate(value = cumsum(c(0,diff(value)))) #get diff wrt to 2020, based on above grouping

# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
nitr_df$positive <- ifelse(nitr_df$value >= 0, nitr_df$value, 0)
nitr_df$negative <- ifelse(nitr_df$value < 0, nitr_df$value, -1e-36)

nitrGlo <- filter(nitr_df, region == "IND")

nitrGloP <- ggplot(filter(nitrGlo, scenario %in% c("BAU", "FSDP")), aes(x = period)) +
  facet_wrap(~scenario, nrow = 1) +
  themeSupplReg(base_size = 25, panel.spacing = 3, rotate_x = 90) +
  ylab("Nitrogen Surplus (Mt Nr/yr)") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = positive, fill = variable), position = "stack") +
  geom_area(aes(y = negative, fill = variable), position = "stack") +
  scale_fill_manual("Land type", values = rev(c(
    "#E6AB02", "lightblue", "#9e9ac8", "#ACC3A6", "purple", "darkgreen", "yellow3"))) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)

if (save) {
  ggsave(filename = file.path(savedir,"FigS6a_NitrGLO.pdf"), nitrGloP, width = 25, height = 10, scale = 1)
} else {
plots <- list(plots, nitrGloP)
}

nitrReg <- filter(nitr_df, region == "IND", period %in% c(2020, 2050)) %>%
  group_by(model, scenario, variable, period, region) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))
nitrReg <- nitrReg[-which(nitrReg$period == 2020 & nitrReg$scenario != "BAU"), ] # remove nonBAU 2010 values

# write.csv(b,file="SI_lu_reg_2100_bar.csv",row.names = FALSE)

nitrRegP <- ggplot(nitrReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(region), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual("nitrland type", values = rev(c(
    "#E6AB02", "lightblue", "#9e9ac8", "#ACC3A6", "purple", "darkgreen", "yellow3"))) +
  theme(legend.position = "bottom") + guides(fill = guide_legend("Land type", ncol = 5, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Nitrogen Surplus (Mt Nr/yr)") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

if (save) {
  ggsave(filename = file.path(savedir,"FigS6b_NitrREG.pdf"), nitrRegP, width = 30, height = 15, scale = 1)
} else {
plots <- list(plots, nitrRegP)
}


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
                           labels = names(rev(waterVar)))) %>%
  group_by(model, scenario, region, variable) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping

# write.csv(b,file="Fig2_LandCoverChange.csv",row.names = FALSE)
water_df$positive <- ifelse(water_df$value >= 0, water_df$value, 0)
water_df$negative <- ifelse(water_df$value < 0, water_df$value, -1e-36)

waterGlo <- filter(water_df, region == "IND")
# waterGlop <-
waterGloP <- ggplot(filter(waterGlo, scenario %in% c("BAU", "FSDP", "WaterSoil", "LandSparing", "dietHealth")), aes(x = period)) +
  themeSupplReg(base_size = 25, panel.spacing = 3, rotate_x = 90) +
  ylab("Change in Water withdrawals from 2020 value, in km3") +
  geom_line(aes(y = value, color = scenario), position = "stack", size = 1.3) +
  scale_color_manual(values = c("#386cb0", "#e78ac3", "#beaed4", "#7fc97f", "#fdc086")) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)

if (save) {
  ggsave(filename = file.path(savedir,"FigS7a_WaterGLO.pdf"), waterGloP, width = 25, height = 10, scale = 1)
} else {
plots <- list(plots, waterGloP)
}



waterReg <- filter(water_df, region == "IND", period == 2050) %>%
  group_by(model, scenario, variable, period, region) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))

waterRegP <- ggplot(waterReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(region), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = variable, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenario, x = value)) +
  scale_fill_manual(" ", values = rev(c("purple", "#2080EC", "lightblue"))) +
  theme(legend.position = "bottom") +
  guides(fill = element_blank()) +
  # guide_legend("Cropland type",ncol=5,title.position = "left", byrow = TRUE,reverse=FALSE)) +
  xlab("Change in Water withdrawals from 2020 value, in km3") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

if (save) {
  ggsave(filename = file.path(savedir,"FigS7b_waterREG.pdf"), waterRegP, width = 30, height = 15, scale = 1)
} else {
plots <- list(plots, waterRegP)
}



#### Health

healthVar <-  scens[grep("Nutrition\\|Anthropometrics\\|People", scens$variable), ]$variable %>%  unique()
names(healthVar) <- c("Underweight", "Normal Weight", "Overweight", "Obese")


health_df <- filter(scens,
                    period <= 2050,
                    period > 2015,
                    variable %in% healthVar) %>%
  droplevels() %>%
  group_by(model, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(healthVar),
                           labels = names(rev(healthVar))),
         variable = factor(variable, levels = rev(c("Underweight", "Normal Weight",
                                                    "Overweight", "Obese"))))


healthGlo <- filter(health_df, region == "IND")
healthGloP <- ggplot(filter(healthGlo, scenario %in% c("BAU", "FSDP", "population", "gdp_educ_inequ")), aes(x = period)) +
  facet_wrap(~scenario, nrow = 1) +
  themeSupplReg(base_size = 25, panel.spacing = 3, rotate_x = 90) +
  ylab("Million People") +
  geom_area(aes(y = value, fill = variable), position = "stack") +
  scale_fill_manual("Nutrition Indicators",
                    values = rev(c("#2080EC", "lightblue", "blue", "purple"))) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)
# healthGloP

if (save) {
  ggsave(filename = file.path(savedir,"FigS8a_HealthGLO.pdf"), healthGloP, width = 25, height = 10, scale = 1)
} else {
plots <- list(plots, healthGloP)
}

healthReg <- filter(health_df, region == "IND", period == 2050) %>%
  group_by(model, scenario, variable, period, region) %>%
  summarise(value = sum(value)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))

healthRegP <- ggplot(healthReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(region), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = value, fill = variable), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_fill_manual(element_blank(),
                    values = rev(c("#2080EC", "lightblue", "blue", "purple"))) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend("Indicator", ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) +
  xlab("Million People") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

if (save) {
  ggsave(filename = file.path(savedir,"FigS8b_HealthREG.pdf"), healthRegP, width = 30, height = 15, scale = 1)
} else {
plots <- list(plots, healthRegP)
}


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
  group_by(model, scenario, region,period, Products) %>%
  filter(region == "IND")

empGloP <- ggplot(filter(emp_df, scenario %in% c("BAU", "FSDP")), aes(x = period)) +
  facet_wrap(~scenario, nrow = 1) +
  themeSupplReg(base_size = 25, panel.spacing = 3, rotate_x = 90) +
  ylab("Number of People Employed in Agriculture (millions)") +
  # geom_hline(yintercept = 0, linetype = "dotted") +
  geom_area(aes(y = value, fill = Products), position = "stack") +
  scale_fill_manual("Production", values = rev(c("#ffd256", "#489448"))) +
  theme(legend.position = "bottom") + guides(fill = guide_legend("Production", ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)

if (save) {
  ggsave(filename = file.path(savedir,"FigS9a_EmployGLO.pdf"), empGloP, width = 30, height = 15, scale = 1)
} else {
plots <- list(plots, empGloP)
}

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
  group_by(model, scenario, region, period, Products) %>%
  summarise(value = sum(value)) %>%
  group_by(model, scenario, region, Products) %>%
  mutate(value = cumsum(c(0, diff(value)))) # get diff wrt to 2020, based on above grouping

emp_df$positive <- ifelse(emp_df$value >= 0, emp_df$value, 0)
emp_df$negative <- ifelse(emp_df$value < 0, emp_df$value, -1e-36)

#facet_bounds <- data.frame("RegionG" = rep(c("High-Income Regions", "Rest of World", "Low-Income Regions"), 2),
#                           "value" = c(-70, NA, NA, 7, NA, NA), "scenario" = "BAU")

empReg <- filter(emp_df, region == "IND", period == 2050) %>%
  group_by(model, scenario, Products, period, region) %>%
  summarise(value = sum(value), positive = sum(positive), negative = sum(negative)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario))))

#empReg$RegionG_f <- factor(empReg$RegionG, levels = c("High-Income Regions", "Rest of World", "Low-Income Regions"))

empRegP <- ggplot(empReg, aes(y = scenario)) +
  facet_grid(vars(period), vars(region), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = positive, fill = Products), position = "stack", stat = "identity", width = 0.75) +
  geom_bar(aes(x = negative, fill = Products, ), position = "stack", stat = "identity", width = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  stat_summary(fun = "sum", colour = "black", size = 1, geom = "point", mapping = aes(group = scenario, x = value)) +
  scale_fill_manual("Production", values = rev(c("#ffd256", "#489448"))) +
  theme(legend.position = "bottom") + guides(fill = guide_legend("Production", ncol = 2, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Change in agricultural employment compared to 2020 (mio. people)") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

#empRegP <- empRegP + geom_blank(data = facet_bounds, aes(x = value, y = scenario))

if (save) {
  ggsave(filename = file.path(savedir,"FigS9b_EmployREG.pdf"), empRegP, width = 30, height = 15, scale = 1)
} else {
  plots <- list(plots, empRegP)
}

##### Hourly Labour Costs ####

laborVar <-  "Hourly labor costs"
names(laborVar) <- "Hourly Labor Costs"

# extract total hours worked to use as a separate column
hoursWorked <- filter(scens, variable == "Total Hours Worked|Crop and livestock products") %>%
  rename("hours" = value) %>%
  select(model, scenario, region, period, hours)

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

labReg <- filter(labor_df, region == "IND") %>%
  group_by(model, scenario, variable, period, region) %>%
  summarise(value = weighted.mean(value, w = hours)) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario)))) %>%
  group_by(model, scenario, region, variable)

labRegP <- ggplot(filter(labReg, scenario %in% c("BAU", "FSDP")), aes(x = period)) +
  facet_grid(cols = vars(region), scales = "free") +
  themeSupplReg(base_size = 25, panel.spacing = 3, rotate_x = 90) +
  ylab("Hourly Labor Costs (USD05/h)") +
  geom_line(aes(y = value, color = scenario), lwd = 1.1) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL) +
  scale_colour_manual(values = c("#1f78b4", "#33a02c", "#b2df8a", "#d95f02", "#7570b3", "#e7298a"))

if (save) {
  ggsave(filename = file.path(savedir,"FigS10a_LabREG.pdf"), labRegP, width = 30, height = 15, scale = 1)
} else {
  plots <- list(plots, labRegP)
}


######## INEQUALITY INDICATORS ##########

ineqVar <-  c(scens[grep("Gini", scens$variable), ]$variable %>%  unique()%>% as.vector(),
           scens[grep("Average Income", scens$variable), ]$variable %>%  unique()%>% as.vector(),
           scens[grep("Median Income", scens$variable), ]$variable %>%  unique()%>% as.vector(),
           scens[grep("People Below", scens$variable), ]$variable %>%  unique()%>% as.vector())

names(ineqVar) <- ineqVar

ineq_df <- filter(scens,
                   period <= 2050,
                   period > 2015,
                   variable %in% ineqVar) %>%
  droplevels() %>%
  group_by(model, scenario, region, period) %>%
  mutate(variable = factor(variable, levels = rev(ineqVar),
                           labels = names(rev(ineqVar)))) %>%
  group_by(model, scenario, region, variable)

ineqGlo <- filter(ineq_df, region == "IND")
giniGloP <- ggplot(filter(ineqGlo, variable == "Gini Coefficient"),
                    aes(x = period)) +
  themeSupplReg(base_size = 25, panel.spacing = 3, rotate_x = 90) +
  ylab("Gini Coefficient") +
  geom_line(aes(y = value, color = scenario), lwd = 1.1) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)

 # scale_colour_manual(values = c("#1f78b4", "#33a02c", "#b2df8a", "#d95f02", "#7570b3", "#e7298a"))

if (save) {
  ggsave(filename = file.path(savedir,"FigS11a_GiniGLO.pdf"), giniGloP, width = 25, height = 10, scale = 1)
} else {
  plots <- list(plots, giniGloP)
}

ineqReg <- filter(ineq_df, region == "IND") %>%
  group_by(model, scenario, variable, period, region) %>%
  mutate(scenario = factor(scenario, levels = rev(levels(scenario)))) %>%
  group_by(model, scenario, region, variable) %>%
  filter(period == 2050)

giniRegP <- ggplot(filter(ineqReg, variable == "Gini Coefficient"), aes(y = scenario)) +
  facet_grid(vars(period), vars(region), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = value), stat = "identity", width = 0.75, fill = "#A455CF") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend("Indicator", ncol = 5, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Gini Coefficient") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))

if (save) {
  ggsave(filename = file.path(savedir, "FigS11b_giniREG.pdf"), giniRegP, width = 30, height = 15, scale = 1)
} else {
  plots <- list(plots, giniRegP)
}

belowPovMglo <- ggplot(filter(ineqGlo, variable == "Number of People Below 3.20$/Day"),
                   aes(x = period)) +
  themeSupplReg(base_size = 25, panel.spacing = 3, rotate_x = 90) +
  ylab("Million People below 3.20$/Day Poverty Line") +
  geom_line(aes(y = value, color = scenario), lwd = 1.1) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 5, title.position = "left", byrow = TRUE, reverse = TRUE)) + xlab(NULL)


if (save) {
  ggsave(filename = file.path(savedir, "FigS12a_PovLineGLO.pdf"), belowPovMglo, width = 25, height = 10, scale = 1)
} else {
  plots <- list(plots, belowPovMglo)
}


belowPovMreg <- ggplot(filter(ineqReg, variable == "Number of People Below 3.20$/Day"), aes(y = scenario)) +
  facet_grid(vars(period), vars(region), scales = "free", space = "free") +
  themeSupplReg(base_size = 22, rotate_x = FALSE) + ylab(NULL) +
  geom_bar(aes(x = value), stat = "identity", width = 0.75, fill = "#A455CF") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend("Indicator", ncol = 5, title.position = "left", byrow = TRUE, reverse = FALSE)) +
  xlab("Million People below 3.20$/Day Poverty Line") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks = pretty_breaks()) # breaks = c(-400,-200,0,200,400) + labs(caption = paste(Sys.Date()))


if (save) {
  ggsave(filename = file.path(savedir, "FigS12b_PovLineGLO.pdf"), belowPovMreg, width = 30, height = 15, scale = 1)
} else {
  plots <- list(plots, belowPovMreg)
}




}
