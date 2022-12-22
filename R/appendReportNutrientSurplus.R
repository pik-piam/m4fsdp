#' @title appendReportNutrientSurplus
#' @description Calculates the global nutrient surplus from the grid-level dataset,
#' nutrientSurplus_total.mz. This must be calculated from the grid-level data because the
#' nutrient surplus for nonAg land is currently only available at the grid level.
#'
#' @export
#'
#' @param scenario name of the scenario
#' @param dir scenario output directory
#'
#' @return NULL
#' @author Michael Crawford
#'
#' @importFrom magclass read.report write.report getItems
#' @importFrom dplyr %>% mutate select arrange rename
#' @importFrom forcats fct_recode
#' @importFrom quitte as.quitte
#' @importFrom madrat toolAggregate
#' @importFrom rlang .data

appendReportNutrientSurplus <- function(scenario, dir = ".") {

    # -----------------------------------------------------------------------------------------------------------------
    # Calculate total global nutrient surplus

    NSgrid_path <- file.path(dir, paste0(scenario, "-nutrientSurplus_total.mz"))
    NSgrid      <- read.magpie(NSgrid_path)

    NScountry <- dimSums(NSgrid, dim = 1.2) # Grid -> country level

    gdx <- file.path(dir, "fulldata.gdx")
    mapping <- readGDX(gdx, "i_to_iso")
    mapping <- mapping %>% mutate(GlobalCode = "World")

    NSregWorld <- toolAggregate(x       = NScountry,
                                rel     = mapping,
                                from    = "iso",
                                to      = "i+GlobalCode",
                                partrel = TRUE)

    NSregWorld <- NSregWorld %>% as.data.frame()

    colnames(NSregWorld) <- c("cell", "region", "year", "variable", "value")

    NSregWorld <- NSregWorld %>%
        mutate(variable = "Resources|Nitrogen|Nutrient surplus incl natural vegetation") %>%
        mutate(model    = "MAgPIE",
               scenario = scenario,
               unit     = "Mt N") %>%
        select(.data$model, .data$scenario, .data$region, .data$variable, .data$unit, .data$year, .data$value) %>%
        arrange(.data$model, .data$scenario, .data$variable, .data$year, .data$region)


    # -----------------------------------------------------------------------------------------------------------------
    # Append report to report.rds

    NSregWorld_rds <- NSregWorld %>% rename(period = .data$year)

    rds_file <- file.path(dir, "report.rds")
    if (file.exists(rds_file)) {

        report_rds <- readRDS(rds_file)

        # If the variables are already included within the report, remove these older variables for replacement
        if (any(NSregWorld_rds$variable %in% report_rds$variable)) {
            report_rds <- report_rds %>% filter(!.data$variable %in% NSregWorld_rds$variable)
        }

        report_rds <- rbind(report_rds, NSregWorld_rds)
        toSaveAsRDS <- as.quitte(report_rds)
        saveRDS(toSaveAsRDS, file = file.path(dir, "report.rds"), version = 2)

    } else {
        stop("report.rds wasn't found for scenario: ", scenario)
    }


    # -----------------------------------------------------------------------------------------------------------------
    # Append report to report.mif

    NSregWorld_mif <- NSregWorld %>%
        mutate(region = fct_recode(.data$region,
                                   "GLO" = "World"))

    NSregWorld_mif <- as.magpie(NSregWorld_mif)

    mif_file <- file.path(dir, "report.mif")
    if (file.exists(mif_file)) {

        originalReport <- read.report(mif_file, as.list = FALSE)
        originalReportItems <- getItems(originalReport, dim = 3.3)
        newReportItems <- getItems(NSregWorld_mif, dim = 3.3)

        # If the variables are already included within the report, remove these older variables for replacement
        # This is currently unnecessarily complex, given that there is only one reporting variable (Nutrient surplus
        # incl natural vegetation), but will be helpful if further reporting names are added.
        alreadyPresentItems <- Map(newReportItems,
                                   f = function(.x) grepl(pattern = .x, x = originalReportItems, fixed = TRUE))
        alreadyPresentItems <- Reduce(alreadyPresentItems, f = `|`)

        if (any(alreadyPresentItems)) {
            originalReport <- originalReport[, , !alreadyPresentItems]
        }

        write.report(x = originalReport, file = mif_file)
        write.report(x = NSregWorld_mif, file = mif_file, append = TRUE)

    } else {
        stop("report.mif wasn't found for scenario: ", scenario)
    }

}
