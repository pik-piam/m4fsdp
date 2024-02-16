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
    milestones <- c("Livestock product consumption per capita in current HIRs",
                    "Livestock product consumption per capita in current MIRs",
                    "Production of livestock products, global",
                    "Production of fruits, vegetables and nuts, global",
                    "Food waste per capita in current HIRs",
                    "Food waste per capita in current MIRs",
                    "Inorganic fertilizer, global",
                    "Soil Nitrogen Uptake Efficiency (SNUpE), global",
                    "Manure recycling quota, excluding field losses, global",
                    "Rewetted formerly drained peatlands, global",
                    "Anthropogenic LUC emissions, global",
                    "AFOLU emissions, global",
                    "Afforestation compared to 2020, global",
                    "Total forest area, global",
                    "Total other semi-natural vegetation, global",
                    "Timber plantations, global",
                    "Alternative livelihoods needed for people formerly working in agriculture, starting in 2020, global",
                    "Gini, global",
                    "Labor productivity in current LIRs",
                    "Labor productivity in current MIRs",
                    "Labor productivity in current HIRs",
                    "Mortality from dietary and metabolic risks compared to 2020, global"
                    # "Cropland landscapes with low natural habitats (<20% (semi)-natural vegetation in a 0.5*0.5° cell), global"
                    )

    units <- c("Index",
               "Index",
               "Index",
               "Index",
               "Index",
               "Index",
               "Mt N/yr",
               "Percent",
               "Percent",
               "Mha",
               "Gt CO2/yr",
               "Gt CO2eq/yr",
               "Mha",
               "Mha",
               "Mha",
               "Mha",
               "Mio people",
               "Index",
               "USD_05MER/worker",
               "USD_05MER/worker",
               "USD_05MER/worker",
               "Index"
               # "Mha"
               )

    res <- data.frame(matrix(ncol = 6, nrow = length(milestones)))
    colnames(res) <- c("Milestone", "Indicator", "2020", "2030", "2040", "2050")

    res[, 1] <- milestones
    res[, 2] <- units

    # mapping to economic regionsAfforestation compared to 2020, global
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
    consumption <- reportKcal(gdx, level = "reg")[, , "Livestock products", pmatch = TRUE]
    consumption <- toolAggregate(consumption, rel = mapping, weight = pop, from = "reg", to = "aggregate")

    consumptionRel <- consumption / collapseDim(consumption[, 2020, ])

    res[res$Milestone == "Livestock product consumption per capita in current HIRs", 3:6] <- consumptionRel["HIR", c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Livestock product consumption per capita in current MIRs", 3:6] <- consumptionRel["MIR", c(2020, 2030, 2040, 2050), ]


    # 2. Production of livestock & nuts, fruits and vegetables
    livst <- c("livst_rum", "livst_pig", "livst_chick", "livst_egg", "livst_milk")
    production <- dimSums(production(gdx), dim = 1)
    productionLivst <- dimSums(production[, , livst], dim = 3)
    productionOthers <- production[, , "others"]

    prodLivstRel <- productionLivst / collapseDim(productionLivst[, 2020, ])
    prodOthersRel <- productionOthers / collapseDim(productionOthers[, 2020, ])

    res[res$Milestone == "Production of livestock products, global", 3:6] <- prodLivstRel[, c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Production of fruits, vegetables and nuts, global", 3:6] <- prodOthersRel[, c(2020, 2030, 2040, 2050), ]


    # 3. Food waste (per capita)
    waste <- reportSDG12(gdx)[, , "Food waste total", pmatch = TRUE]
    waste <- toolAggregate(waste["GLO", , , invert = TRUE], rel = mapping, from = "reg", to = "aggregate")

    wastePerCapita <- waste / popReg
    wastePerCapitaRel <- wastePerCapita / collapseDim(wastePerCapita[, 2020, ])

    res[res$Milestone == "Food waste per capita in current HIRs", 3:6] <- wastePerCapitaRel["HIR", c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Food waste per capita in current MIRs", 3:6] <- wastePerCapitaRel["MIR", c(2020, 2030, 2040, 2050), ]


    # 4. Inorganic fertilizer
    fertilizer <- reportSDG6(gdx)["GLO", , "Fertilizer use", pmatch = TRUE]

    res[res$Milestone == "Inorganic fertilizer, global", 3:6] <- fertilizer[, c(2020, 2030, 2040, 2050), ]


    # 5. SNUPE
    snupe <- readGDX(gdx, "ov_nr_eff", react = "silent", format = "first_found", select = list(type = "level"))
    inputs <- readGDX(gdx, "ov50_nr_inputs", react = "silent", format = "first_found", select = list(type = "level"))
    snupe <- dimSums(snupe * inputs / dimSums(inputs, dim = 1), dim = 1) * 100 # as percentage

    res[res$Milestone == "Soil Nitrogen Uptake Efficiency (SNUpE), global", 3:6] <- snupe[, c(2020, 2030, 2040, 2050), ]


    # 6. Manure recycling quota, excluding field losses
    # "what share of the manure excreted in confinements gets recycled to croplands"
    recycledManure <- reportNitrogenBudgetCropland(gdx)["GLO", , "Resources|Nitrogen|Cropland Budget|Inputs|+|Manure Recycled from Confinements (Mt Nr/yr)"]
    excretedManure <- reportManure(gdx)["GLO", , "Resources|Nitrogen|Manure|++|Manure In Confinements (Mt Nr/yr)"]
    manureQuota <- recycledManure / excretedManure * 100 # as percentage

    res[res$Milestone == "Manure recycling quota, excluding field losses, global", 3:6] <- manureQuota[, c(2020, 2030, 2040, 2050), ]


    # 7. Rewetting of peatlands
    peatland <- reportPeatland(gdx)["GLO", , "Rewetted", pmatch = TRUE]

    res[res$Milestone == "Rewetted formerly drained peatlands, global", 3:6] <- peatland[, c(2020, 2030, 2040, 2050), ]


    # 8. Anthropogenic LUC emissions & AFOLU emissions
    emissions <- reportEmissions(gdx)["GLO", , ]
    luc <- emissions[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"] / 1000 # to giga tonnes
    afolu <- emissions[, , "Emissions|GWP100AR6|Land (Gt CO2e/yr)"]

    res[res$Milestone == "Anthropogenic LUC emissions, global", 3:6] <- luc[, c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "AFOLU emissions, global", 3:6] <- afolu[, c(2020, 2030, 2040, 2050), ]


    # 9. Afforestation compared to 2020, global
    aff <- dimSums(land(gdx, level = "glo",types = "forestry",subcategories = c("forestry"),sum = FALSE)[, , c("aff", "ndc")], dim = 3)
    aff <- aff - collapseDim(aff[, 2020, ])

    res[res$Milestone == "Afforestation compared to 2020, global", 3:6] <- aff[, c(2020, 2030, 2040, 2050), ]


    # 10. Total forest area & Total other semi-natural vegetation
    land <- dimSums(land(gdx), dim = 1)
    totalForest <- dimSums(land[, , c("forestry", "primforest", "secdforest")], dim = 3)
    other <- land[, , "other"]

    res[res$Milestone == "Total forest area, global", 3:6] <- totalForest[, c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Total other semi-natural vegetation, global", 3:6] <- other[, c(2020, 2030, 2040, 2050), ]


    # 11. Timber plantation, global
    plantations <- reportLandUse(gdx)["GLO", , "Resources|Land Cover|Forest|Managed Forest|+|Plantations (million ha)"]

    res[res$Milestone == "Timber plantations, global", 3:6] <- plantations[, c(2020, 2030, 2040, 2050), ]


    # 12. Alternative Livelihoods for people formerly working in agriculture (aka reduction in ag empl)
    empl <- reportAgEmployment(gdx)["GLO", , ]
    empl <- collapseDim(empl[, 2020, ]) - empl

    res[res$Milestone == "Alternative livelihoods needed for people formerly working in agriculture, starting in 2020, global", 3:6] <- empl[, c(2020, 2030, 2040, 2050), ]


    # 13. Gini index
    gini <- reportRds[reportRds$variable == "Income|Gini Coefficient", ]
    gini <- gini[gini$region == "GLO", ]

    res[res$Milestone == "Gini, global", 3:6] <- gini[gini$period %in% c(2020, 2030, 2040, 2050), ]$value


    # 14. Labor productivity
    # using revenues based on iniPrices (so minWage does not artificaially increase labor productivity, only slightly from labor-capital subst)
    prodQuantity <- mbind(production(gdx, products = "kcr"), production(gdx, products = "kli"))
    foodPriceIni <- readGDX(gdx, "f15_prices_initial")[, , getNames(prodQuantity)]
    employment <- toolAggregate(reportAgEmployment(gdx, level = "reg"), rel = mapping, from = "reg", to = "aggregate")

    totalRevenues <- toolAggregate(dimSums(prodQuantity * foodPriceIni, dim = 3), rel = mapping, from = "reg", to = "aggregate")
    laborProd <- totalRevenues / employment

    res[res$Milestone == "Labor productivity in current LIRs", 3:6] <- laborProd["LIR", c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Labor productivity in current MIRs", 3:6] <- laborProd["MIR", c(2020, 2030, 2040, 2050), ]
    res[res$Milestone == "Labor productivity in current HIRs", 3:6] <- laborProd["HIR", c(2020, 2030, 2040, 2050), ]


    # 15. Decrease in mortality
    health <- reportRds[reportRds$variable == "Health|Years of life lost|Disease" & reportRds$region == "World" & reportRds$period %in% c(2020, 2030, 2040, 2050), ]
    health$relChange <- health$value / health$value[health$period == 2020]

    res[res$Milestone == "Mortality from dietary and metabolic risks compared to 2020, global", 3:6] <- health$relChange


    # 16. Cropland landscapes with low natural habitats (<20% (semi)-natural vegetation in a 0.5*0.5° cell), global
    ## NOTE: The Landscape Habitats measure should lead to 0 cropland areas with insufficient landscape habitats in 2050
    ## (i.e. no cells with >80% of available cropland actually used as cropland area). This is true on cluster level
    ## (as enforced within MAgPIE), but no longer on grid-cell level after disaggregation (4.868673% of total cropland
    ## in 2050  are in cells in which more than 80% of available cropland is used as cropland). But on clusterlevel it
    ## is not a good indicator for biodiversity, as the resolution is too coarse --> we don't use this indicator for now.

    # # on cell level
    # cellCropland <- read.magpie(file.path(scenarioFolder, "cell.land_0.5.mz"))[, , "crop"]
    # avlCropland <- read.magpie(file.path(scenarioFolder, "avl_cropland_0.5.mz"))[, , "q33_marginal"]

    # cropShr <- cellCropland[, c(2020, 2030, 2040, 2050), ] / avlCropland
    # cropCells <- cellCropland[, c(2020, 2030, 2040, 2050), ]
    # cropCells[cropShr[, , "crop"] <= 0.8] <- 0

    # quintShr <- dimSums(cropCells, dim = 1)

    # # share of cropland that is violating the LandscapeHabitat constraint in 2050 (on cell level)
    # violation2050 <- quintShr[, 2050, ]
    # cropland2050 <- dimSums(cellCropland[, 2050, ], dim = 1)
    # shareViolating <- violation2050 / cropland2050
    # shareViolating * 100

    # # on cluster level
    # cropland <- land(gdx, level = "cell")[, , "crop"]
    # avlCropland <- readGDX(gdx, "f30_avl_cropland")[, , "q33_marginal"]

    # cropShr <- cropland[, c(2020, 2030, 2040, 2050), ] / avlCropland
    # cropShr[cropShr > 1] <- 1

    # cropCells <- cropland[, c(2020, 2030, 2040, 2050), ]
    # cropCells[cropShr <= 0.8 + 1e-10] <- 0 # using a value slightly over 0.8 to avoid rounding issues

    # quintShr <- dimSums(cropCells, dim = 1)

    # res[res$Milestone == "Cropland areas without sufficient landscape habitats", 3:6] <- quintShr


    # save results
    if (!is.null(file)) write.csv(res, file.path(outFolder, file), row.names = FALSE)


}
