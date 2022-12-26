#' @title appendReportHealthImpacts
#' @description Reads in the results from Marco Springmann's health impacts model, filters them down to the
#' FSEC-relevant variables, and saves them to the selected scenarios' report.mif.
#' @author Michael Crawford
#'
#' @export
#'
#' @param healthImpacts_gdx full pathname of the health impacts dataset
#' @param scenario name of the scenario
#' @param dir scenario output directory
#'
#' @return NULL
#'
#' @importFrom gdx readGDX
#' @importFrom magclass getSets read.report write.report getItems
#' @importFrom dplyr %>% mutate select pull filter arrange rename
#' @importFrom stringr str_extract str_replace str_remove
#' @importFrom forcats fct_recode
#' @importFrom quitte as.quitte
#' @importFrom madrat toolAggregate
#' @importFrom rlang .data

appendReportHealthImpacts <- function(healthImpacts_gdx, scenario, dir = ".") {

    # -----------------------------------------------------------------------------------------------------------------
    # Format Marco's health impacts dataset

    gdx <- suppressWarnings(readGDX(healthImpacts_gdx, "report_health_s"))
    getSets(gdx) <- c("region", "year", "scenario", "unit",
                      "TMREL", "riskFactor", "causeOfDeath", "sex", "stat")

    # IMPORTANT NOTE: The variables are misnamed within Marco's datasets. Rather than "deaths avoided" (deaths_avd),
    # or "years of life lost avoided" (YLL_avd), they are actually "attributable deaths" and "years of life lost".
    # Ergo, the direction is opposite. Fewer = better. The variables are simply renamed for now.

    # Check to see if the scenario version number is the same as those held within the .gdx. If called
    # on divergent scenario IDs, the healthImpacts are nevertheless appended to the scenario reports.
    desiredVersionID <- str_extract(string = scenario,                    pattern = "^(.*?)(?=_)")
    gdxVersionID     <- str_extract(string = getItems(gdx, dim = 3.1)[1], pattern = "^(.*?)(?=_)")
    gdxScenario      <- NULL

    if (desiredVersionID != gdxVersionID) {
        gdxScenario <- str_replace(string = scenario, pattern = "^(.*?)(?=_)", replacement = gdxVersionID)

        if (!(gdxScenario %in% getItems(gdx, dim = 3.1))) {
            baseScenarioName <- str_remove(string = scenario, pattern = "^(.*?)_")
            stop("healthImpacts .gdx does not contain the scenario: ", baseScenarioName)
        }

        message("In appendReportHealthImpacts.R: Inconsistent version IDs. You are likely using
                 an older version of the health impacts data than the current runs.")
    } else {
        gdxScenario <- scenario
    }

    # This script is run on a per-scenario basis, and so therefore we filter here (also keeping only the mean value).
    healthImpacts <- gdx[, , list(gdxScenario, "mean", c("deaths_avd", "YLL_avd"))] %>%
        as.data.frame()

    colnames(healthImpacts) <- c("cell", "region", "year", "scenario", "unit",
                                 "TMREL", "riskFactor", "causeOfDeath", "sex", "stat", "value")

    healthImpacts <- healthImpacts %>%
        mutate(model        = "MAgPIE",
               scenario     = scenario, # Always use the desired scenario ID
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
                                 "mio persons" = "deaths_avd",
                                 "mio years" = "YLL_avd"),
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

    # Convert to millions people, millions years
    healthImpacts <- healthImpacts %>%
        mutate(value = .data$value / 1E6)

    # -----------------------------------------------------------------------------------------------------------------
    # Country-level data integration into "report_iso.rds"

    healthImpacts_country <- healthImpacts %>%
        rename(period = .data$year)

    rds_file <- file.path(dir, "report_iso.rds")
    if (file.exists(rds_file)) {
        report_iso_rds <- readRDS(rds_file)

        # If the variables are already included within the report, remove these older variables for replacement
        if (any(healthImpacts$variable %in% report_iso_rds$variable)) {
            report_iso_rds <- report_iso_rds %>%
                filter(!(.data$variable %in% healthImpacts$variable))
        }

        report_iso_rds <- rbind(report_iso_rds, healthImpacts_country)
        toSaveAsRDS <- as.quitte(report_iso_rds)
        saveRDS(toSaveAsRDS, file = file.path(dir, "report_iso.rds"), version = 2)
    } else {
        message("Country-level report_iso.rds wasn't found for scenario: ", scenario)
    }


    # -----------------------------------------------------------------------------------------------------------------
    # Regional-level data integration into "report.mif" and "report.rds"

    ## Append to regional report.rds

    healthImpacts_regional <- healthImpacts %>%
        as.magpie()

    gdx <- file.path(dir, "fulldata.gdx")
    mapping <- readGDX(gdx, "i_to_iso")
    mapping <- mapping %>% mutate(GlobalCode = "World")

    healthImpacts_regional <- toolAggregate(x       = healthImpacts_regional,
                                            rel     = mapping,
                                            from    = "iso",
                                            to      = "i+GlobalCode",
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

        # If the variables are already included within the report, remove these older variables for replacement
        if (any(healthImpacts$variable %in% report_rds$variable)) {
            report_rds <- report_rds %>%
                filter(!.data$variable %in% healthImpacts$variable)
        }

        report_rds <- rbind(report_rds, healthImpacts_regional)
        report_rds <- as.quitte(report_rds)
        saveRDS(report_rds, file = file.path(dir, "report.rds"))
    } else {
        stop("report.rds wasn't found for scenario: ", scenario)
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

        # If the variables are already included within the report, remove these older variables for replacement
        alreadyPresentItems <- Map(newReportItems,
                                   f = function(.x) grepl(pattern = .x, x = originalReportItems, fixed = TRUE))
        alreadyPresentItems <- Reduce(alreadyPresentItems, f = `|`)

        if (any(alreadyPresentItems)) {
            originalReport <- originalReport[, , !alreadyPresentItems]
        }

        write.report(x = originalReport, file = mif_file)
        write.report(x = healthImpacts_regional, file = mif_file, append = TRUE)
    } else {
        stop("report.mif wasn't found for scenario: ", scenario)
    }

}
