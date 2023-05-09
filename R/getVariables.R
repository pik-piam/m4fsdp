#' @title getVariables
#' @description Variables for FSDP plotting
#'
#' @export
#'
#' @param reportVariables Variables in report file
#' @details includes information about unit, variable group, rounding etc
#' @return named vector
#' @author Florian Humpenoeder

getVariables <- function(reportVariables = NULL) {

  var <- c("SDG|SDG02|Prevalence of underweight",
           "SDG|SDG03|Prevalence of obesity",
           "Health|Years of life lost|Disease",
           "Biodiversity|BII",
           "Biodiversity|Cropland Landscapes BII",
           "Biodiversity|Biodiversity Hotspot BII",
           "Biodiversity|Shannon crop area diversity index",
           "Resources|Nitrogen|Nutrient surplus from land and manure management",
           "Water|Environmental flow violation volume",
           "Emissions|GWP100AR6|Land",
           "Global Surface Temperature",
           "Household Expenditure|Food|Expenditure",
           "Income|Number of People Below 3p20 USDppp11/day",
           "Labor|Employment|Agricultural employment",
           "Agricultural employment|Crop and livestock products",
           "Labor|Wages|Hourly labor costs relative to 2010",
           "Value|Bioeconomy Demand",
           "Costs Without Incentives")

  # vargroup|var#|variableName|variableUnit|DirectionImprovment|rounding|factor
  names(var) <- c("Health|1|Underweight|Mio people|decrease|0|1",
                  "Health|2|Obesity|Mio people|decrease|0|1",
                  "Health|3|Premature Mortality|Mio years of life lost|decrease|0|1",
                  "Environment|4.1|All Land Types|Biodiv. Intactness Index|increase|2|100",
                  "Environment|4.2|Cropland Landscapes|Biodiv. Intactness Index|increase|2|100",
                  "Environment|4.3|Hotspot Landscapes|Biodiv. Intactness Index|increase|2|100",
                  "Environment|5|Croparea Diversity|Shannon Index|increase|2|1",
                  "Environment|6|Nitrogen Surplus|Mt N/yr|decrease|0|1",
                  "Environment|7|Env. Water Flow Violations|km3/yr|decrease|0|1",
                  "Environment|8|AFOLU GHG Emissions|GtCO2eq/yr|decrease|1|1",
                  "Environment|9|Global Surface Warming|Degree Celsius|decrease|2|1",
                  "Inclusion|10|Ag. Expenditures|USD/person/yr|decrease|0|1",
                  "Inclusion|11|Poverty|Mio people below 3.20$/day|decrease|0|1",
                  "Inclusion|12|Ag. Employment|Mio people|increase|0|1",
                  "Inclusion|12|Ag. Employment|Mio people|increase|0|1",
                  "Inclusion|13|Ag. Wages|Index rel. to 2010|increase|2|1",
                  "Economy|14|Bioeconomy Supply|Billion US$05/yr|increase|0|0.001",
                  "Economy|15|Production Costs|Billion US$05/yr|decrease|0|0.001")

  if (is.null(reportVariables)) {
    return(var)
  } else {
    missingVars <- NULL
    keepVars <- NULL

    for (i in unique(names(var))) {
      tmp <- var[names(var) == i]
      tmp2 <- tmp[tmp == intersect(tmp, reportVariables)]
      if (length(tmp2) == 0) {
        missingVars <- c(tmp, missingVars)
      } else if (length(tmp2) == 1) {
        keepVars <- c(tmp2, keepVars)
        colnames(keepVars)
      } else if (length(tmp2) > 1) {
        keepVars <- c(tmp2[tmp2 == tmp[1]], keepVars)
      }
    }
    if (length(missingVars) > 0) {
      warning(paste(c("The following indicators are missing: \n", missingVars), collapse = "\n"))
    }
    return(keepVars)
  }
}
