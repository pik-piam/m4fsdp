#' @title milestoneTable
#' @description Creates csv with values for SI table on milestones
#'
#' @export
#'
#' @param scenarioFolder outputfolder of a specific MAgPIE run (should be an FSECe_FSDP run for SI table)
#' @param outFolder main outputfolder, where output csv will be saved
#' @param file name to save table as csv
#'

milestoneTable <- function(scenarioFolder, outFolder = NULL, file = NULL) {

    gdx <- file.path(scenarioFolder, "fulldata.gdx")

    # prepare table
    milestones <- c("Consumption of livestock products in HICs",
                    "Consumption of livestock products in MICs",
                    "Production of livestock products, global",
                    "Production of fruits, vegetables and nuts, global",
                    "Food waste in HICs",
                    "Food waste in MICs",
                    "Inorganic fertilizer, global",
                    "Soil Nitrogen Uptake Efficiency, global",
                    "Rewetting of drained peatlands, global",
                    "Anthropogenic LUC emissions, global",
                    "AFOLU emissions, global",
                    "Manure recycling quota, excluding field losses",
                    "Re/Afforestation, global, compared to 2020",
                    "Total Forest Area",
                    "Total other semi-natural vegetation",
                    "Alternative Livelihoods for people formerly working in agriculture",
                    "Timber plantation, global",
                    "Gini",
                    "Labor productivity in LICs",
                    "Labor productivity in MICs",
                    "Labor productivity in HICs",
                    "Mortality from dietary risks compared to 2020",
                    "Cropland areas without sufficient landscape habitats")

    units <- c("Percentage change",
               "Percentage change",
               "Percentage change",
               "Percentage change",
               "Percentage change",
               "Percentage change",
               "Mt",
               "Percent", 
               "Mha",
               "Gt",
               "Gt",
               "Share",
               "Mha",
               "Mha",
               "Mha",
               "Mio people",
               "Mha",
               "Index",
               "kUSD/worker",
               "kUSD/worker",
               "kUSD/worker",
               "Percentage change",
               "Mha")

    res <- data.frame(matrix(ncol = 6, nrow = 23))
    colnames(res) <- c("Milestone", "Indicator", "2020", "2030", "2040", "2050")

    res[, 1] <- milestones
    res[, 2] <- units

    # mapping to economic regions
    LIR <- c("SSA", "IND")
    HIR <- c("USA", "CAN", "ANZ", "EUR", "JKO", "NEU")
    MIR <- c("CHA", "BRA", "LAM", "MEA", "NEA", "OAS")

    mapping <- data.frame("aggregate" = c(rep("LIR", length(LIR)), rep("MIR", length(MIR)), rep("HIR", length(HIR))),
                        "reg" = c(LIR, MIR, HIR))

    # population for per capita values
    pop <- population(gdx)
    popReg <- toolAggregate(pop, rel = mapping, from = "reg", to = "aggregate")

    # report rds
    reportRds <- readRDS(file.path(scenarioFolder, "report.rds"))

    # 1. Consumption of livestock products (per capita)
    livst <- c("livst_rum", "livst_pig", "livst_chick", "livst_egg", "livst_milk")
    demand <- dimSums(demand(gdx)[, , list("food", livst), drop = T], dim = 3)
    demand <- toolAggregate(demand, rel = mapping, from = "reg", to = "aggregate")

    perCapitaDemand <- demand / popReg
    perCapitaDemandRel <- perCapitaDemand / collapseDim(perCapitaDemand[, 2020, ]) * 100 # as percentage

    res[res$Milestone == "Consumption of livestock products in HICs", 3:6] <- perCapitaDemandRel["HIR", c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Consumption of livestock products in MICs", 3:6] <- perCapitaDemandRel["MIR", c(2020, 2030, 2040, 2050), ]


    # 2. Production of livestock & nuts, fruits and vegetables
    livst <- c("livst_rum", "livst_pig", "livst_chick", "livst_egg", "livst_milk")
    production <- dimSums(production(gdx), dim = 1)
    productionLivst <- dimSums(production[, , livst], dim = 3)
    productionOthers <- production[, , "others"]

    prodLivstRel <- productionLivst / collapseDim(productionLivst[, 2020, ]) * 100 # as percentage
    prodOthersRel <- productionOthers / collapseDim(productionOthers[, 2020, ]) * 100 # as percentage

    res[res$Milestone == "Production of livestock products, global", 3:6] <- prodLivstRel[, c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Production of fruits, vegetables and nuts, global", 3:6] <- prodOthersRel[, c(2020, 2030, 2040, 2050), ]


    # 3. Food waste (per capita)
    waste <- reportSDG12(gdx)[, , "Food waste total", pmatch = TRUE]
    waste <- waste["GLO", , , invert = TRUE]
    waste <- toolAggregate(waste, rel = mapping, from = "reg", to = "aggregate")

    wastePerCapita <- waste / popReg
    wastePerCapitaRel <- wastePerCapita / collapseDim(wastePerCapita[, 2020, ]) * 100 # as percentage

    res[res$Milestone == "Food waste in HICs", 3:6] <- wastePerCapitaRel["HIR", c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Food waste in MICs", 3:6] <- wastePerCapitaRel["MIR", c(2020, 2030, 2040, 2050), ]


    # 4. Inorganic fertilizer
    fertilizer <- reportSDG6(gdx)["GLO", , "Fertilizer use", pmatch = TRUE]

    res[res$Milestone == "Inorganic fertilizer, global", 3:6] <- fertilizer[, c(2020, 2030, 2040, 2050), ]


    # 5. SNUPE
    snupe <- readGDX(gdx, "ov_nr_eff", react = "silent", format = "first_found", select = list(type = "level"))
    inputs <- readGDX(gdx, "ov50_nr_inputs", react = "silent", format = "first_found", select = list(type = "level"))
    snupe <- dimSums(snupe * inputs / dimSums(inputs, dim = 1), dim = 1) * 100 # as percentage

    res[res$Milestone == "Soil Nitrogen Uptake Efficiency, global", 3:6] <- snupe[, c(2020, 2030, 2040, 2050), ]

    # 6. Rewetting of peatlands
    peatland <- reportPeatland(gdx)["GLO", , "Rewetted", pmatch = TRUE]

    res[res$Milestone == "Rewetting of drained peatlands, global", 3:6] <- peatland[, c(2020, 2030, 2040, 2050), ]

    # 7. Anthropogenic LUC emissions & AFOLU emissions 
    emissions <- reportEmissions(gdx)["GLO", , ]
    afolu <- emissions[, , "Emissions|GWP100AR6|Land (Gt CO2e/yr)"]
    luc <- emissions[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"] / 1000 # to giga tonnes

    res[res$Milestone == "Anthropogenic LUC emissions, global", 3:6] <- luc[, c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "AFOLU emissions, global", 3:6] <- afolu[, c(2020, 2030, 2040, 2050), ]


    # 8. Manure recycling quota, excluding field losses
    # "what share of the manure excreted in confinements gets recycled to croplands"
    recycledManure <- reportNitrogenBudgetCropland(gdx)["GLO", , "Resources|Nitrogen|Cropland Budget|Inputs|+|Manure (Mt Nr/yr)"]
    excretedManure <- reportManure(gdx)["GLO", , "Resources|Nitrogen|Manure|++|Manure In Confinements (Mt Nr/yr)"]
    manureQuota <- recycledManure / excretedManure

    res[res$Milestone == "Manure recycling quota, excluding field losses", 3:6] <- manureQuota[, c(2020, 2030, 2040, 2050), ]    

    # 9. Re/Afforestation, global, compared to 2020?
    aff <- reportSDG15(gdx)["GLO", , "Afforestation", pmatch = TRUE]
    aff <- aff - collapseDim(aff[, 2020, ])

    res[res$Milestone == "Re/Afforestation, global, compared to 2020", 3:6] <- aff[, c(2020, 2030, 2040, 2050), ]    


    # 10. Total Forest Area & Total other semi-natural vegetation
    land <- dimSums(land(gdx), dim = 1)
    totalForest <- dimSums(land[, , c("forestry", "primforest", "secdforest")], dim = 3)
    other <- land[, , "other"]
    
    res[res$Milestone == "Total Forest Area", 3:6] <- totalForest[, c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Total other semi-natural vegetation", 3:6] <- other[, c(2020, 2030, 2040, 2050), ]


    # 11. Alternative Livelihoods for people formerly working in agriculture
    empl <- reportAgEmployment(gdx)["GLO", , ]
    empl <- collapseDim(empl[, 2020, ]) - empl

    res[res$Milestone == "Alternative Livelihoods for people formerly working in agriculture", 3:6] <- empl[, c(2020, 2030, 2040, 2050), ]


    # 12. Timber plantation, global 
    plantations <- reportLandUse(gdx)["GLO", , "Resources|Land Cover|Forest|Managed Forest|+|Plantations (million ha)"]
    
    res[res$Milestone == "Timber plantation, global", 3:6] <- plantations[, c(2020, 2030, 2040, 2050), ]

    # 13. Gini
    gini <- reportRds[reportRds$variable == "Income|Gini Coefficient", ]
    gini <- gini[gini$region == "GLO", ]

    res[res$Milestone == "Gini", 3:6] <- gini[gini$period %in% c(2020, 2030, 2040, 2050), ]$value


    # 14. Labor productivity
    wages <- readGDX(gdx, "pm_hourly_costs")[, , "baseline", drop = TRUE]
    employment <- reportAgEmployment(gdx)["GLO", , , invert = TRUE]
    totalHours <- totalHoursWorked(gdx, level = "reg")

    totalCosts <- wages * totalHours
    laborProd <- toolAggregate(totalCosts, rel = mapping, from = "reg", to = "aggregate") / 
                        toolAggregate(employment, rel = mapping, from = "reg", to = "aggregate")

    laborProd <- laborProd / 1000 # from USD/worker to k USD/worker

    res[res$Milestone == "Labor productivity in LICs", 3:6] <- laborProd["LIR", c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Labor productivity in MICs", 3:6] <- laborProd["MIR", c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Labor productivity in HICs", 3:6] <- laborProd["HIR", c(2020, 2030, 2040, 2050), ]


    # 15. Decrease in mortality
    health <- reportRds[reportRds$variable == "Health|Years of life lost|Disease" & reportRds$region == "World" & reportRds$period %in% c(2020, 2030, 2040, 2050), ]
    health$percChange <- health$value / health$value[health$period == 2020] * 100

    res[res$Milestone == "Mortality from dietary risks compared to 2020", 3:6] <- health$percChange

    # 16. Cropland areas without sufficient landscape habitats
    cropland <- land(gdx, level = "cell")[, , "crop"]
    avlCropland <- readGDX(gdx, "f30_avl_cropland")[, , "q33_marginal"]

    cropShr <- cropland[, c(2020, 2030, 2040, 2050), ] / avlCropland
    cropShr[cropShr > 1] <- 1

    cropCells <- cropland[, c(2020, 2030, 2040, 2050), ]
    cropCells[cropShr <= 0.8 + 1e-10] <- 0 # using a value slightly over 0.8 to avoid rounding issues

    quintShr <- dimSums(cropCells, dim = 1)

    res[res$Milestone == "Cropland areas without sufficient landscape habitats", 3:6] <- quintShr

    if (!is.null(file)) write.csv(res, file.path(outFolder, file), row.names = FALSE)

}