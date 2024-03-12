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

  # initialize result table
  res <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(res) <- c("Milestone", "Indicator", "2020", "2030", "2040", "2050")

  .addRow <- function(df, indicator, unit, data, years = c(2020, 2030, 2040, 2050), lmh = FALSE, digits = 2) {
    if (!is.magpie(data)) {
      df[nrow(df) + 1, ] <- c(indicator, unit, round(data, digits))
      return(df)
    }

    if (lmh) {
      df[nrow(df) + 1, ] <- c(paste0(indicator, " in current LIRs"), unit, round(data["LIR", years, ], digits))
      df[nrow(df) + 1, ] <- c(paste0(indicator, " in current MIRs"), unit, round(data["MIR", years, ], digits))
      df[nrow(df) + 1, ] <- c(paste0(indicator, " in current HIRs"), unit, round(data["HIR", years, ], digits))
    } else {
      df[nrow(df) + 1, ] <- c(indicator, unit, round(data[, years, ], digits))
    }
    return(df)
  }

  # mapping to economic regions
  lir <- c("SSA", "IND")
  hir <- c("USA", "CAN", "ANZ", "EUR", "JKO", "NEU")
  mir <- c("CHA", "BRA", "LAM", "MEA", "NEA", "OAS")

  mapping <- data.frame("aggregate" = c(rep("LIR", length(lir)), rep("MIR", length(mir)), rep("HIR", length(hir))),
                        "reg" = c(lir, mir, hir),
                        "glo" = rep("GLO", length(c(lir, mir, hir))))

  # population for per capita values
  pop <- population(gdx)
  # popReg <- toolAggregate(pop, rel = mapping, from = "reg", to = "aggregate")

  # report rds
  reportRds <- readRDS(file.path(scenarioFolder, "report.rds"))

  ## Consumption of specific products + total consumption (per capita)
  consumption <- reportKcal(gdx, detail = TRUE, level = "reg")
  consumption <- toolAggregate(consumption, rel = mapping, weight = pop, from = "reg", to = "aggregate")

  totalConsumption <- consumption[, , "Calorie Supply (", pmatch = TRUE]

  cereals <- consumption[, , "Cereals ", pmatch = TRUE]
  legumes <- dimSums(consumption[, , c("Groundnuts", "Soybean", "Pulses"), pmatch = TRUE], dim = 3)
  fruitVegNuts <- consumption[, , "Fruits", pmatch = TRUE]
  # sugarCrops <- consumption[, , "Sugar crops ", pmatch = TRUE]

  alcohol <- consumption[, , "Alcoholic ", pmatch = TRUE]
  sugar <- consumption[, , "Sugar (", pmatch = TRUE]
  # otherSecondary <- dimSums(consumption[, , c("Brans", "Molasses", "Oils"), pmatch = TRUE], dim = 3)

  # fish <- consumption[, , "Fish ", pmatch = TRUE]
  monogastric <- consumption[, , c("Monogastric"), pmatch = TRUE]
  ruminant <- consumption[, , c("Ruminant"), pmatch = TRUE]
  dairy <- consumption[, , c("Dairy"), pmatch = TRUE]
  poultryEggs <- dimSums(consumption[, , c("Poultry",  "Eggs"), pmatch = TRUE], dim = 3)

  res <- .addRow(res, "Total per-capita food supply", "kcal/capita/day", totalConsumption, lmh = TRUE, digits = 0)

  res <- .addRow(res, "Per-capita supply of cereals", "kcal/capita/day", cereals, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita supply of legumes", "kcal/capita/day", legumes, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita supply of fruits, vegetables and nuts", "kcal/capita/day", fruitVegNuts, lmh = TRUE, digits = 0)
  # res <- .addRow(res, "Per-capita supply of sugar crops", "kcal/capita/day", sugarCrops, lmh = TRUE, digits = 0)

  res <- .addRow(res, "Per-capita supply of alcoholic beverages", "kcal/capita/day", alcohol, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita supply of sugar", "kcal/capita/day", sugar, lmh = TRUE, digits = 0)
  # res <- .addRow(res, "Per-capita supply of other secondary products", "kcal/capita/day", otherSecondary, lmh = TRUE, digits = 0)

  # res <- .addRow(res, "Per-capita supply of fish", "kcal/capita/day", fish, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita supply of monogastric meat", "kcal/capita/day", monogastric, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita supply of ruminant meat", "kcal/capita/day", ruminant, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita supply of dairy products", "kcal/capita/day", dairy, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita supply of poultry and eggs", "kcal/capita/day", poultryEggs, lmh = TRUE, digits = 0)


  ## Intake of specific products + total intake (per capita)
  intake <- reportIntakeDetailed(gdx, level = "reg")
  intake <- toolAggregate(intake, rel = mapping, weight = pop, from = "reg", to = "aggregate")

  totalIntake <- intake[, , "Calorie Intake (", pmatch = TRUE]

  cereals <- intake[, , "Cereals ", pmatch = TRUE]
  legumes <- dimSums(intake[, , c("Groundnuts", "Soybean", "Pulses"), pmatch = TRUE], dim = 3)
  fruitVegNuts <- intake[, , "Fruits", pmatch = TRUE]
  # sugarCrops <- intake[, , "Sugar crops ", pmatch = TRUE]

  alcohol <- intake[, , "Alcoholic ", pmatch = TRUE]
  sugar <- intake[, , "Sugar (", pmatch = TRUE]
  # otherSecondary <- dimSums(intake[, , c("Brans", "Molasses", "Oils"), pmatch = TRUE], dim = 3)

  # fish <- intake[, , "Fish ", pmatch = TRUE]
  monogastric <- intake[, , c("Monogastric"), pmatch = TRUE]
  ruminant <- intake[, , c("Ruminant"), pmatch = TRUE]
  dairy <- intake[, , c("Dairy"), pmatch = TRUE]
  poultryEggs <- dimSums(intake[, , c("Poultry",  "Eggs"), pmatch = TRUE], dim = 3)

  res <- .addRow(res, "Total per-capita food intake", "kcal/capita/day", totalIntake, lmh = TRUE, digits = 0)

  res <- .addRow(res, "Per-capita intake of cereals", "kcal/capita/day", cereals, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita intake of legumes", "kcal/capita/day", legumes, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita intake of fruits, vegetables and nuts", "kcal/capita/day", fruitVegNuts, lmh = TRUE, digits = 0)
  # res <- .addRow(res, "Per-capita intake of sugar crops", "kcal/capita/day", sugarCrops, lmh = TRUE)

  res <- .addRow(res, "Per-capita intake of alcoholic beverages", "kcal/capita/day", alcohol, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita intake of sugar", "kcal/capita/day", sugar, lmh = TRUE, digits = 0)
  # res <- .addRow(res, "Per-capita intake of other secondary products", "kcal/capita/day", otherSecondary, lmh = TRUE, digits = 0)

  # res <- .addRow(res, "Per-capita intake of fish", "kcal/capita/day", fish, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita intake of monogastric meat", "kcal/capita/day", monogastric, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita intake of ruminant meat", "kcal/capita/day", ruminant, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita intake of dairy products", "kcal/capita/day", dairy, lmh = TRUE, digits = 0)
  res <- .addRow(res, "Per-capita intake of poultry and eggs", "kcal/capita/day", poultryEggs, lmh = TRUE, digits = 0)


  ## Production of livestock & nuts, fruits and vegetables
  livst <- c("livst_rum", "livst_pig", "livst_chick", "livst_egg", "livst_milk")
  production <- dimSums(production(gdx), dim = 1)  # in Mt DM
  productionLivst <- dimSums(production[, , livst], dim = 3)
  productionOthers <- production[, , "others"]

  # prodLivstRel <- productionLivst / collapseDim(productionLivst[, 2020, ])
  # prodOthersRel <- productionOthers / collapseDim(productionOthers[, 2020, ])

  res <- .addRow(res, "Production of livestock products, global", "Mt DM", productionLivst, digits = 0)
  res <- .addRow(res, "Production of fruits, vegetables and nuts, global", "Mt DM", productionOthers, digits = 0)


  ## Food waste (per capita)
  # waste <- reportSDG12(gdx)[, , "Food waste total", pmatch = TRUE]
  # waste <- toolAggregate(waste["GLO", , , invert = TRUE], rel = mapping, from = "reg", to = "aggregate")

  # wastePerCapita <- waste / popReg
  # wastePerCapitaRel <- wastePerCapita / collapseDim(wastePerCapita[, 2020, ])

  wasteShare <- (1 - (totalIntake / totalConsumption)) * 100 # based on kcal

  res <- .addRow(res, "Share of food wasted", "Percentage", wasteShare, lmh = TRUE)


  ## Yields
  yields <- reportYields(gdx, detail = TRUE)["GLO", , , invert = TRUE]

  yCereal <- setNames(yields[, , "Cereals (", pmatch = TRUE], "cereal")
  yOilcrops <- setNames(yields[, , "Oil crops (", pmatch = TRUE], "oilcrops")
  yFruitsVegNuts <- setNames(yields[, , "Fruits Vegetables Nuts (", pmatch = TRUE], "fruitsvegnuts")
  yPasture <- setNames(yields[, , "Pasture (", pmatch = TRUE], "pasture")
  yBioenergy <- setNames(yields[, , "Bioenergy crops (", pmatch = TRUE], "bioenergy")

  areas <- reportCroparea(gdx, detail = TRUE)["GLO", , , invert = TRUE]
  aCereal <- setNames(areas[, 2020, "Cereals (", pmatch = TRUE], "cereal")
  aOilcrops <- setNames(areas[, 2020, "Oil crops (", pmatch = TRUE], "oilcrops")
  aFruitsVegNuts <- setNames(areas[, 2020, "Fruits Vegetables Nuts (", pmatch = TRUE], "fruitsvegnuts")
  aPasture <- setNames(reportLandUse(gdx)[, 2020, "Pastures and Rangelands", pmatch = TRUE], "pasture")["GLO", , , invert = TRUE]
  aBioenergy <- setNames(areas[, 2020, "Bioenergy crops (", pmatch = TRUE], "bioenergy")

  yCereal <- toolAggregate(yCereal, rel = mapping, weight = aCereal, from = "reg", to = "aggregate")
  yOilcrops <- toolAggregate(yOilcrops, rel = mapping, weight = aOilcrops, from = "reg", to = "aggregate")
  yFruitsVegNuts <- toolAggregate(yFruitsVegNuts, rel = mapping, weight = aFruitsVegNuts, from = "reg", to = "aggregate")
  yPasture <- toolAggregate(yPasture, rel = mapping, weight = aPasture, from = "reg", to = "aggregate")
  yBioenergy <- toolAggregate(yBioenergy, rel = mapping, weight = aBioenergy, from = "reg", to = "aggregate")

  res <- .addRow(res, "Cereal yields with fixed area weights from 2020, global", "t DM/ha", yCereal, lmh = TRUE, digits = 1)
  res <- .addRow(res, "Oil crop yields with fixed area weights from 2020, global", "t DM/ha", yOilcrops, lmh = TRUE, digits = 1)
  res <- .addRow(res, "Fruits, vergetables and nuts yields with fixed area weights from 2020, global", "t DM/ha", yFruitsVegNuts, lmh = TRUE, digits = 1)
  res <- .addRow(res, "Pasture yields with fixed area weights from 2020, global", "t DM/ha", yPasture, lmh = TRUE, digits = 1)
  res <- .addRow(res, "Short rotation bioenergy crop yields with fixed area weights from 2020, global", "t DM/ha", yBioenergy, lmh = TRUE, digits = 1)


  ## Feed conversion
  feed <- reportFeedConversion(gdx)["GLO", , , invert = TRUE]
  productionLivst <- dimSums(production(gdx)[, , livst], dim = 3) # in Mt DM

  feed <- toolAggregate(feed, rel = mapping, weight = productionLivst, from = "reg", to = "aggregate")

  rumDairy <- feed[, , "Productivity|Feed conversion|Ruminant meat and dairy (GE per GE)"]
  monogastric <- feed[, , "Productivity|Feed conversion|Monogastric meat (GE per GE)"]
  poultry <- feed[, , "Productivity|Feed conversion|Poultry meat and eggs (GE per GE)"]

  res <- .addRow(res, "Feed conversion for ruminant meat and dairy, global", "GE per GE", rumDairy, lmh = TRUE, digits = 1)
  res <- .addRow(res, "Feed conversion for monogastric meat, global", "GE per GE", monogastric, lmh = TRUE, digits = 1)
  res <- .addRow(res, "Feed conversion for poultry meat and eggs, global", "GE per GE", poultry, lmh = TRUE, digits = 1)


  ## Land use intensity Tau
  tau <- reportTau(gdx)["GLO", , , invert = TRUE]
  cr <- croparea(gdx, level = "reg", water_aggr = TRUE)

  tau <- toolAggregate(tau, rel = mapping, weight = cr, from = "reg", to = "aggregate")

  res <- .addRow(res, "Land use intensity", "Index", tau, lmh = TRUE)


  ## Area used for specific crops + total croparea
  areas <- reportCroparea(gdx, detail = TRUE)["GLO", , ]

  aBioenergy <- areas[, , "Bioenergy crops (", pmatch = TRUE]
  aFruitsVegNuts <- areas[, , "Fruits Vegetables Nuts (", pmatch = TRUE]
  aLegumes <- dimSums(areas[, , c("Pulses (", "Groundnuts (", "Soybean ("), pmatch = TRUE], dim = 3)
  aTotal <- areas[, , "Croparea (", pmatch = TRUE]

  res <- .addRow(res, "Area used for bioenergy crops, global", "Mha", aBioenergy, digits = 0)
  res <- .addRow(res, "Area used for fruits, vegetables and nuts, global", "Mha", aFruitsVegNuts, digits = 0)
  res <- .addRow(res, "Area used for legumes, global", "Mha", aLegumes, digits = 0)
  res <- .addRow(res, "Total cropland area, global", "Mha", aTotal, digits = 0)

  ## Irrigated area
  water <- reportAAI(gdx)["GLO", , ]

  res <- .addRow(res, "Irrigated cropland area, global", "Mha", water, digits = 0)


  ## Afforestation compared to 2020, global
  aff <- dimSums(land(gdx, level = "glo", types = "forestry", subcategories = c("forestry"), sum = FALSE)[, , c("aff", "ndc")], dim = 3)
  aff <- aff - collapseDim(aff[, 2020, ])

  res <- .addRow(res, "Afforestation compared to 2020, global", "Mha", aff, digits = 0)


  ## Total managed forest area
  managed <- reportLandUse(gdx)["GLO", , "Resources|Land Cover|Forest|+|Managed Forest (million ha)"]

  res <- .addRow(res, "Managed forest area, global", "Mha", managed, digits = 0)


  ## Natural forest area
  forest <- reportLandUse(gdx)["GLO", , "Resources|Land Cover|Forest|+|Natural Forest (million ha)"]

  res <- .addRow(res, "Natural forest area (primary + secondary), global", "Mha", forest, digits = 0)


  ## Total forest area & Total other semi-natural vegetation
  land <- dimSums(land(gdx), dim = 1)
  totalForest <- dimSums(land[, , c("forestry", "primforest", "secdforest")], dim = 3)
  other <- land[, , "other"]

  res <- .addRow(res, "Total forest area, global", "Mha", totalForest, digits = 0)
  res <- .addRow(res, "Total other semi-natural vegetation, global", "Mha", other, digits = 0)


  ## Timber plantation, global
  plantations <- reportLandUse(gdx)["GLO", , "Resources|Land Cover|Forest|Managed Forest|+|Plantations (million ha)"]

  res <- .addRow(res, "Timber plantations, global", "Mha", plantations, digits = 0)


  ## Timber demand for construction
  timber <- reportTimber(gdx)["GLO", , "Timber|Volumetric|Demand|Roundwood (Mm3/yr)"]

  res <- .addRow(res, "Demand for roundwood, global", "Mm3/yr", timber, digits = 0)


  ## Rewetting of peatlands
  peatland <- reportPeatland(gdx)["GLO", , "Rewetted", pmatch = TRUE]

  res <- .addRow(res, "Rewetted formerly drained peatlands, global", "Mha", peatland, digits = 0)


  ## Protected Areas
  protected <- dimSums(reportProtectedArea(gdx), dim = 3)["GLO", , ]
  protected <- protected - collapseDim(protected[, 2020, ])

  res <- .addRow(res, "Protected areas compared to 2020, global", "Mha", protected, digits = 0)


  ## Inorganic fertilizer
  nBudgetCrop <- reportNitrogenBudgetCropland(gdx)["GLO", , "Fertilizer", pmatch = TRUE]
  nBudgetPast <- reportNitrogenBudgetPasture(gdx)["GLO", , "Fertilizer", pmatch = TRUE]
  fertilizer <- nBudgetCrop + nBudgetPast

  res <- .addRow(res, "Inorganic fertilizer on cropland and pastures, global", "Mt N/yr", fertilizer, digits = 0)


  ## SNUPE
  snupe <- readGDX(gdx, "ov_nr_eff", react = "silent", format = "first_found", select = list(type = "level"))
  inputs <- readGDX(gdx, "ov50_nr_inputs", react = "silent", format = "first_found", select = list(type = "level"))
  snupe <- dimSums(snupe * inputs / dimSums(inputs, dim = 1), dim = 1) * 100 # as percentage

  res <- .addRow(res, "Soil Nitrogen Uptake Efficiency (SNUpE), global", "Percent", snupe, digits = 0)


  ## Manure recycling quota, excluding field losses
  # "what share of the manure excreted in confinements gets recycled to croplands"
  recycledManure <- reportNitrogenBudgetCropland(gdx)["GLO", , "Resources|Nitrogen|Cropland Budget|Inputs|+|Manure Recycled from Confinements (Mt Nr/yr)"]
  excretedManure <- reportManure(gdx)["GLO", , "Resources|Nitrogen|Manure|++|Manure In Confinements (Mt Nr/yr)"]
  manureQuota <- recycledManure / excretedManure * 100 # as percentage

  res <- .addRow(res, "Manure recycling quota, excluding field losses, global", "Percent", manureQuota, digits = 0)


  ## Nitrogen surplus
  nSurp <- reportNitrogenPollution(gdx, dir = scenarioFolder)["GLO", , ]

  res <- .addRow(res, "Nitrogen surplus from cropland, global", "Mt N/yr", nSurp[, , "Cropland", pmatch = TRUE], digits = 0)
  res <- .addRow(res, "Nitrogen surplus from pasture, global", "Mt N/yr", nSurp[, , "Pasture", pmatch = TRUE], digits = 0)
  res <- .addRow(res, "Nitrogen surplus from animal waste management, global", "Mt N/yr", nSurp[, , "Animal Waste", pmatch = TRUE], digits = 0)
  res <- .addRow(res, "Nitrogen surplus from semi-natural vegetation, global", "Mt N/yr", nSurp[, , "Non-agricultural land", pmatch = TRUE], digits = 0)


  ## Soil organic carbon
  soc <- reportSOM(gdx)["GLO", , "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)"]

  res <- .addRow(res, "Soil organic carbon in top 30 cm, global", "Mt C", soc, digits = 0)


  ## Anthropogenic LUC emissions & AFOLU emissions
  emissions <- reportEmissions(gdx)["GLO", , ]
  luc <- emissions[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"] / 1000 # to giga tonnes
  afolu <- emissions[, , "Emissions|GWP100AR6|Land (Gt CO2e/yr)"]

  res <- .addRow(res, "Anthropogenic LUC emissions, global", "Gt CO2/yr", luc, digits = 1)
  res <- .addRow(res, "AFOLU emissions, global", "Gt CO2eq/yr", afolu, digits = 1)


  ## Labor productivity
  # using revenues based on iniPrices (so minWage does not artificaially increase labor productivity, only slightly from labor-capital subst)
  prodQuantity <- mbind(production(gdx, products = "kcr"), production(gdx, products = "kli"))
  foodPriceIni <- readGDX(gdx, "f15_prices_initial")[, , getNames(prodQuantity)]
  employment <- toolAggregate(reportAgEmployment(gdx, level = "reg"), rel = mapping, from = "reg", to = "aggregate")

  totalRevenues <- toolAggregate(dimSums(prodQuantity * foodPriceIni, dim = 3), rel = mapping, from = "reg", to = "aggregate")
  laborProd <- totalRevenues / employment

  res <- .addRow(res, "Labor productivity", "USD_05MER/worker", laborProd, lmh = TRUE, digits = 0)


  ## Alternative Livelihoods for people formerly working in agriculture (aka reduction in ag empl)
  empl <- reportAgEmployment(gdx)["GLO", , , invert = TRUE]
  empl <- toolAggregate(empl, rel = mapping, from = "reg", to = "aggregate")
  emplRel <- collapseDim(empl[, 2020, ]) - empl

  res <- .addRow(res, "Alternative livelihoods needed for people formerly working in agriculture, difference to 2020", "Mio people", emplRel, lmh = TRUE)


  ## Ag. employment share
  workingAge <- c("15--19", "20--24", "25--29", "30--34", "35--39", "40--44",
                  "45--49", "50--54", "55--59", "60--64")
  popWa <- dimSums(population(gdx, level = "reg", age = TRUE, dir = outFolder)[, , workingAge], dim = 3)
  popWa <- toolAggregate(popWa, rel = mapping, from = "reg", to = "aggregate")

  emplShare <- (empl / popWa) * 100

  res <- .addRow(res, "Share of working age population employed in agriculture, global", "Percent", emplShare, lmh = TRUE, digits = 0)


  ## Gini index
  gini <- reportRds[reportRds$variable == "Income|Gini Coefficient", ]
  gini <- gini[gini$region == "GLO", ]

  res <- .addRow(res, "Gini, global", "Index", gini[gini$period %in% c(2020, 2030, 2040, 2050), ]$value)


  ##  Decrease in mortality
  yll <- reportRds[reportRds$variable == "Health|Years of life lost|Disease" & reportRds$region == "World" & reportRds$period %in% c(2020, 2030, 2040, 2050), ]

  popGlo <- as.data.frame(dimSums(pop, dim = 1)[, c(2020, 2030, 2040, 2050), ])[, c("Year", "Value")]
  popGlo$Year <- as.integer(levels(popGlo$Year))
  colnames(popGlo) <- c("Year", "Pop")
  yll <- dplyr::right_join(yll, popGlo[, c("Year", "Pop")], by = c("period" = "Year"))

  yll$YLLpc <- yll$value / yll$Pop * 365

  res <- .addRow(res, "Lifetime per capita lost due to dietary and metabolic risks, global", "Days per year", yll$YLLpc)


  ## Cropland landscapes with low natural habitats (<20% (semi)-natural vegetation in a 0.5*0.5° cell), global
  # NOTE: The Landscape Habitats measure should lead to 0 cropland areas with insufficient landscape habitats in 2050
  # (i.e. no cells with >80% of available cropland actually used as cropland area). This is true on cluster level
  # (as enforced within MAgPIE), but no longer on grid-cell level after disaggregation (4.868673% of total cropland
  # in 2050  are in cells in which more than 80% of available cropland is used as cropland). But on clusterlevel it
  # is not a good indicator for biodiversity, as the resolution is too coarse --> we don't use this indicator for now.

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


  # save results
  if (!is.null(file)) write.csv(res, file.path(outFolder, file), row.names = FALSE)

}
