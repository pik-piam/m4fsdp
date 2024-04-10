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
#' @importFrom dplyr %>% mutate select pull filter arrange rename recode
#' @importFrom stringr str_extract str_replace str_remove str_detect
#' @importFrom quitte as.quitte
#' @importFrom madrat toolAggregate
#' @importFrom rlang .data

appendReportHealthImpacts <- function(healthImpacts_gdx, scenario, dir = ".") {

    # -----------------------------------------------------------------------------------------------------------------
    # Format Marco's health impacts dataset

    gdx <- suppressWarnings(readGDX(healthImpacts_gdx, "report_health_s"))
    getSets(gdx) <- c("region", "year", "scenario", "metric",
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

        message("Inconsistent version ID: Appending ", gdxVersionID, " to ", desiredVersionID, " scenarios")
    } else {
        gdxScenario <- scenario
    }

    # This script is run on a per-scenario basis, and so therefore we filter here (also keeping only the mean value).
    healthImpacts <- gdx[, , list(gdxScenario, "mean")] %>%
        as.data.frame()

    colnames(healthImpacts) <- c("cell", "region", "year", "scenario", "metric",
                                 "TMREL", "riskFactor", "causeOfDeath", "sex", "stat", "value")

    healthImpacts <- healthImpacts %>%
        mutate(model        = "MAgPIE",
               scenario     = scenario, # Always use the desired scenario ID
               riskFactor   = as.factor(.data$riskFactor),
               causeOfDeath = as.factor(.data$causeOfDeath),
               sex          = as.factor(.data$sex)) %>%
        select(.data$model, .data$scenario, .data$region, .data$metric, .data$year,
               .data$sex, .data$riskFactor, .data$causeOfDeath, .data$value)

    # Remove Marco's regional aggregations
    regions_path <- system.file("extdata", "MarcoSpringmann_worldRegions.csv", package = "m4fsdp")

    marco_worldRegions <- read.csv(regions_path, check.names = FALSE) %>%
        as.data.frame() %>%
        pull(.data$`Regional abbreviation`)

    healthImpacts <- healthImpacts %>%
        filter(!(.data$region %in% marco_worldRegions))

    # Clarify the variable names
    healthImpacts$sex <- recode(healthImpacts$sex,
                                `BTH` = "Both sexes",
                                `MLE` = "Male",
                                `FML` = "Female")

    healthImpacts$metric <- recode(healthImpacts$metric,
                                `deaths_avd`      = "Attributable deaths",
                                `YLL_avd`         = "Years of life lost",
                                `%deaths_avd/all` = "Percent change in Attributable deaths",
                                `%YLL_avd/all`    = "Percent change in Years of life lost")

    healthImpacts$riskFactor <- recode(healthImpacts$riskFactor,
                                    `all-rf`        = "All risk factors",
                                    `diet`          = "Diet",
                                    `weight`        = "Weight",
                                    `fruits`        = "Fruits",
                                    `vegetables`    = "Vegetables",
                                    `nuts_seeds`    = "Nuts and seeds",
                                    `legumes`       = "Legumes",
                                    `red_meat`      = "Red meat",
                                    `underweight`   = "Underweight",
                                    `overweight`    = "Overweight",
                                    `obese`         = "Obese")

    healthImpacts$causeOfDeath <- recode(healthImpacts$causeOfDeath,
                                        `all-c`     = "All-cause mortality",
                                        `CHD`       = "Congenital Heart Disease",
                                        `Stroke`    = "Stroke",
                                        `Cancer`    = "Cancer",
                                        `T2DM`      = "Type-2 Diabetes",
                                        `Resp_Dis`  = "Respiratory Disease")


    healthImpacts <- healthImpacts %>%
        filter(.data$metric %in% c("Years of life lost", "Attributable deaths")) %>%
        filter(.data$riskFactor == "All risk factors") %>%
        filter(.data$sex == "Both sexes")

    healthImpacts <- healthImpacts %>%
        mutate(variable = case_when(
            .data$causeOfDeath == "All-cause mortality" ~ paste0("Health|", .data$metric, "|Disease"),
            TRUE                                        ~ paste0("Health|", .data$metric, "|Disease|+|", .data$causeOfDeath)
        ))

    # Convert to millions people, millions years
    healthImpacts <- healthImpacts %>%
        mutate(value = case_when(
            .data$metric == "Attributable deaths" ~ .data$value / 1E6,
            .data$metric == "Years of life lost"  ~ .data$value / 1E6,
            TRUE ~ .data$value
        ))

    healthImpacts <- healthImpacts %>%
        mutate(unit = case_when(
                .data$metric == "Attributable deaths"                   ~ "mio deaths",
                .data$metric == "Percent change in Attributable deaths" ~ "percent change",
                .data$metric == "Years of life lost"                    ~ "mio years",
                .data$metric == "Percent change in Years of life lost"  ~ "percent change"
        ))

    healthImpacts <- healthImpacts %>%
        select(.data$model, .data$scenario, .data$region, .data$unit, .data$year, .data$variable, .data$value) %>%
        arrange(.data$model, .data$scenario, .data$region, .data$year, .data$variable)

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

    healthImpacts_regional$region[healthImpacts_regional$region == "World"] <- "GLO"

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
