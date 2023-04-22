#' @title appendReportMAGICC7
#' @description Reads in the results from the MAGICC7 AR6 probabilistic scenarios
#' @author Michael Crawford
#'
#' @export
#'
#' @param resultsPath full file path of the excel with the for MAGICC7 results
#' @param scenario name of the scenario
#' @param dir scenario output directory
#'
#' @return NULL
#'
#' @importFrom readxl read_excel
#' @importFrom magclass read.report write.report getItems
#' @importFrom dplyr %>% mutate filter rename_all
#' @importFrom stringr str_detect str_replace_all
#' @importFrom quitte as.quitte
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer

appendReportMAGICC7 <- function(resultsPath, scenario, dir = ".") {

  results <- read_excel(resultsPath) %>%
    filter(str_detect(.data$Scenario, scenario))

  # -----------------------------------------------------------------------------------------------------------------
  # Filter MAGICC7 dataset to only exceedance probability and median temperature

  patterns <- c("^(AR6 climate diagnostics\\|Exceedance Probability (\\d+(\\.\\d+)?C)\\|MAGICCv7\\.5\\.3)$",
                "^(AR6 climate diagnostics\\|Surface Temperature \\(GSAT\\)\\|MAGICCv7\\.5\\.3\\|50\\.0th Percentile)$")

  results <- results[Reduce("|", Map(function(pattern) grepl(pattern, results$Variable), patterns)), ]

  # -----------------------------------------------------------------------------------------------------------------
  # Reformatting dataset

  results <- results %>%
    pivot_longer(cols      = .data$`1995`:.data$`2100`, 
                 names_to  = "Period", 
                 values_to = "Value")

  results <- results %>%
    mutate(Variable = str_replace_all(.data$Variable, "\\.", "p")) %>%
    mutate(Model    = "MAgPIE") %>%
    mutate(Scenario = scenario) %>%
    mutate(Unit     = "C")

  results <- results %>%
    mutate(Variable = ifelse(.data$Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7p5p3|50p0th Percentile", #nolint
                             "Global Surface Temperature",
                             .data$Variable))

  results <- results %>%
    rename_all(tolower)

  # -----------------------------------------------------------------------------------------------------------------
  # Append to regional report.rds

  rds_file <- file.path(dir, "report.rds")
  if (file.exists(rds_file)) {

    report_rds <- readRDS(rds_file)

    # If the variables are already included within the report, remove these older variables for replacement
    if (any(results$variable %in% report_rds$variable)) {
      report_rds <- report_rds %>% filter(!.data$variable %in% results$variable)
    }

    report_rds <- rbind(report_rds, results)
    report_rds <- as.quitte(report_rds)
    saveRDS(report_rds, file = file.path(dir, "report.rds"))

  } else {
    stop("report.rds wasn't found for scenario: ", scenario)
  }

  # -----------------------------------------------------------------------------------------------------------------
  # Append to regional report.mif

  results$region[results$region == "World"] <- "GLO"

  results <- as.magpie(results)

  mif_file <- file.path(dir, "report.mif")
  if (file.exists(mif_file)) {

    originalReport <- read.report(mif_file, as.list = FALSE)
    originalReportItems <- getItems(originalReport, dim = 3.3)
    newReportItems <- getItems(results, dim = 3.3)

    # If the variables are already included within the report, remove these older variables for replacement
    alreadyPresentItems <- Map(newReportItems, f = function(.x) grepl(pattern = .x, x = originalReportItems, fixed = TRUE))
    alreadyPresentItems <- Reduce(alreadyPresentItems, f = `|`)

    if (any(alreadyPresentItems)) {
      originalReport <- originalReport[, , !alreadyPresentItems]
    }

    write.report(x = originalReport, file = mif_file)
    write.report(x = results, file = mif_file, append = TRUE)

  } else {
    stop("report.mif wasn't found for scenario: ", scenario)
  }

}
