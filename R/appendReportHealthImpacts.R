#' @title appendReportHealthImpacts
#' @description Reads in the results from Marco Springmann's health impacts model, filters them down to the
#' FSEC-relevant variables, and saves them to the selected scenarios' report.mif.
#'
#' @export
#'
#' @param healthImpacts_path full pathname of the health impacts dataset
#' @param scenario name of the scenario
#' @param dir scenario output directory
#'
#' @return NULL
#' @author Michael Crawford
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getSets read.report write.report getItems
#' @importFrom dplyr %>% mutate select pull filter arrange rename
#' @importFrom forcats fct_recode
#' @importFrom quitte as.quitte
#' @importFrom madrat toolGetMapping toolAggregate
#' @importFrom rlang .data

appendReportHealthImpacts <- function(healthImpacts_path, scenario, dir = ".") {

    # -----------------------------------------------------------------------------------------------------------------
    # Format Marco's health impacts dataset

    gdx <- suppressWarnings(readGDX(healthImpacts_path))[[2]]
    getSets(gdx) <- c("region", "year", "scenario", "unit",
                      "TMREL", "riskFactor", "causeOfDeath", "sex", "stat")

    # This script is run on a per-scenario basis, and so therefore we filter here (also keeping only the mean value).

    tryCatch(expr = {
        healthImpacts <- gdx[, , list(scenario, "mean", c("deaths_avd", "YLL_avd"))] %>%
            as.data.frame()
    }, error = function(error) {
        stop("This scenario's ID wasn't found in the healthImpacts dataset")
    })

    colnames(healthImpacts) <- c("cell", "region", "year", "scenario", "unit",
                                 "TMREL", "riskFactor", "causeOfDeath", "sex", "stat", "value")

    healthImpacts <- healthImpacts %>%
        mutate(model        = "MAgPIE",
               riskFactor   = as.factor(.data$riskFactor),
               causeOfDeath = as.factor(.data$causeOfDeath),
               sex          = as.factor(.data$sex)) %>%
        select(.data$model, .data$scenario, .data$region, .data$unit, .data$year,
               .data$sex, .data$riskFactor, .data$causeOfDeath, .data$value)

    # Remove Marco's regional aggregations
    regions_path <- system.file("extdata", "MarcoSpringmann_worldRegions.csv", package = "m4fsdp")

    marco_worldRegions <- read.csv(regions_path, check.names = FALSE) %>%
        as.data.frame() %>%
        pull(.data$`Regional abbreviation`)

    healthImpacts <- healthImpacts %>%
        filter(!(.data$region %in% marco_worldRegions))

    # Clarify the variable names
    healthImpacts <- healthImpacts %>%
        mutate(sex = fct_recode(.data$sex,
                                "Both sexes" = "BTH",
                                "Male"       = "MLE",
                                "Female"     = "FML"),
               unit = fct_recode(.data$unit,
                                 "Deaths avoided"          = "deaths_avd",
                                 "Year lives lost avoided" = "YLL_avd"),
               riskFactor = fct_recode(.data$riskFactor,
                                       "All risk factors" = "all-rf",
                                       Diet               = "diet",
                                       Weight             = "weight",
                                       Fruits             = "fruits",
                                       Vegetables         = "vegetables",
                                       "Nuts and seeds"   = "nuts_seeds",
                                       Legumes            = "legumes",
                                       "Red meat"         = "red_meat",
                                       Underweight        = "underweight",
                                       Overweight         = "overweight",
                                       Obese              = "obese"),
               causeOfDeath = fct_recode(.data$causeOfDeath,
                                         "All-cause mortality"      = "all-c",
                                         "Congenital Heart Disease" = "CHD",
                                         Stroke                     = "Stroke",
                                         Cancer                     = "Cancer",
                                         "Type-2 Diabetes"          = "T2DM",
                                         "Respiratory Disease"      = "Resp_Dis"))

    # For now we use only the top-level risk factor and cause of death
    healthImpacts <- healthImpacts %>%
        filter(.data$riskFactor   %in% c("All risk factors"),
               .data$causeOfDeath %in% c("All-cause mortality"))

    healthImpacts <- healthImpacts %>%
        mutate(variable = ifelse(.data$sex == "Both sexes",
                                 yes = paste0("Health|", .data$unit, "|Risk|Diet and anthropometrics"),
                                 no  = paste0("Health|", .data$unit, "|Risk|Diet and anthropometrics", "|+|", .data$sex)))

    healthImpacts <- healthImpacts %>%
        select(.data$model, .data$scenario, .data$region, .data$unit, .data$year, .data$variable, .data$value) %>%
        arrange(.data$model, .data$scenario, .data$region, .data$year, .data$variable)

    # Convert to millions people
    healthImpacts <- healthImpacts %>%
        mutate(value = .data$value / 1E6)

    # -----------------------------------------------------------------------------------------------------------------
    # Country-level data integration into "report_iso.rds"

    healthImpacts_country <- healthImpacts %>%
        rename(period = .data$year)

    rds_file <- file.path(dir, "report_iso.rds")
    if (file.exists(rds_file)) {
        report_iso_rds <- readRDS(rds_file)

        originalReport_scenarioVersion <- strsplit(x = as.character(report_iso_rds$scenario[[1]]), split = "_")[[1]][1]
        newReport_scenarioVersion      <- strsplit(x = as.character(healthImpacts_country$scenario[[1]]), split = "_")[[1]][1]

        if (originalReport_scenarioVersion != newReport_scenarioVersion) {
            message("Warning: Inconsistent version IDs. You are likely using an older version of the health impacts than the current runs.")
        }

        if (any(healthImpacts$variable %in% report_iso_rds$variable)) {
            message("Health impacts appear to already be included in the country-level .rds file. Let me remove those for you.")
            report_iso_rds <- report_iso_rds %>%
                filter(!(.data$variable %in% healthImpacts$variable))
        }

        report_iso_rds <- rbind(report_iso_rds, healthImpacts_country)
        toSaveAsRDS <- as.quitte(report_iso_rds)
        saveRDS(toSaveAsRDS, file = file.path(dir, "report_iso.rds"), version = 2)
    } else {
        message("Country-level report_iso.rds wasn't found in the scenario folder. Does the rds_report_iso.R output script need to be run?")
    }


    # -----------------------------------------------------------------------------------------------------------------
    # Regional-level data integration into "report.mif" and "report.rds"

    ## Append to regional report.rds

    healthImpacts_regional <- healthImpacts %>%
        as.magpie()

    mapping <- toolGetMapping("regionmappingH12.csv")
    mapping <- mapping %>% mutate(GlobalCode = "World")

    healthImpacts_regional <- toolAggregate(x       = healthImpacts_regional,
                                            rel     = mapping,
                                            from    = "CountryCode",
                                            to      = "RegionCode+GlobalCode",
                                            partrel = TRUE)

    healthImpacts_regional <- as.data.frame(healthImpacts_regional) %>%
        rename(model    = "Data1",
               scenario = "Data2",
               unit     = "Data3",
               variable = "Data4",
               region   = .data$Region,
               period   = .data$Year,
               value    = .data$Value) %>%
        select(.data$scenario, .data$model, .data$region, .data$variable, .data$unit, .data$period, .data$value)

    rds_file <- file.path(dir, "report.rds")
    if (file.exists(rds_file)) {
        report_rds <- readRDS(rds_file)

        originalReport_scenarioVersion <- strsplit(x = as.character(report_rds$scenario[[1]]), split = "_")[[1]][1]
        newReport_scenarioVersion      <- strsplit(x = as.character(healthImpacts_regional$scenario[[1]]), split = "_")[[1]][1]

        if (originalReport_scenarioVersion != newReport_scenarioVersion) {
            message("Warning: Inconsistent version IDs. You are likely using an older version of the health impacts than the current runs.")
        }

        if (any(healthImpacts$variable %in% report_rds$variable)) {
            message("Health impacts appear to already be included in the regional .rds file. Let me remove those for you.")
            report_rds <- report_rds %>%
                filter(!.data$variable %in% healthImpacts$variable)
        }

        report_rds <- rbind(report_rds, healthImpacts_regional)
        report_rds <- as.quitte(report_rds)
        saveRDS(report_rds, file = file.path(dir, "report.rds"))
    } else {
        stop("report.rds wasn't found. Have your `scenario` and `dir` variables been properly parameterized?")
    }

    ## Append to regional report.mif

    healthImpacts_regional <- healthImpacts_regional %>%
        mutate(region = fct_recode(.data$region,
                                   "GLO" = "World"))

    healthImpacts_regional <- as.magpie(healthImpacts_regional)

    mif_file <- file.path(dir, "report.mif")
    if (file.exists(mif_file)) {
        originalReport <- read.report(mif_file, as.list = FALSE)
        originalReportItems <- getItems(originalReport, dim = 3.3)
        newReportItems <- getItems(healthImpacts_regional, dim = 3.3)

        originalReport_scenarioVersion <- strsplit(x = getItems(originalReport, dim = 3.1), split = "_")[[1]][1]
        newReport_scenarioVersion      <- strsplit(x = getItems(healthImpacts_regional, dim = 3.1), split = "_")[[1]][1]

        if (originalReport_scenarioVersion != newReport_scenarioVersion) {
            message("Warning: Inconsistent version IDs. You are likely using an older version of the health impacts than the current runs.")
        }

        alreadyPresentItems <- Map(newReportItems, f = function(.x) grepl(pattern = .x, x = originalReportItems, fixed = TRUE))
        alreadyPresentItems <- Reduce(alreadyPresentItems, f = `|`)

        if (any(alreadyPresentItems)) {
            message("Health impacts appear to already be included in the .mif file. Let me remove those for you.")
            originalReport <- originalReport[, , !alreadyPresentItems]
        }

        write.report(x = originalReport, file = mif_file)
        write.report(x = healthImpacts_regional, file = mif_file, append = TRUE)
    } else {
        stop("report.mif wasn't found. Have your `scenario` and `dir` variables been properly parameterized?")
    }

}
