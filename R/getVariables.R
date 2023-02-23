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
           "Health|Years of life lost|Risk|Diet and anthropometrics",
           "Biodiversity|BII",
           "Biodiversity|Agricultural landscape intactness",
           "Biodiversity|Biodiversity hotspot intactness",
           "Biodiversity|Shannon crop area diversity index",
           "Resources|Nitrogen|Nutrient surplus from land and manure management",
           "Water|Environmental flow violation volume",
           "Emissions|GWP100AR6|Land|Cumulative",
           "Global Surface Temperature",
           "Household Expenditure|Food|Expenditure",
           "Income|Number of People Below 3p20 USDppp11/day",
           "Labor|Employment|Agricultural employment",
           "Agricultural employment|Crop and livestock products",
           "Labor|Wages|Hourly labor costs relative to 2010",
           "Value|Bioeconomy Demand",
           "Costs Without Incentives")

  # vargroup|var#|variableName|variableUnit|DirectionImprovment|rounding|factor
  names(var) <- c("Health|1|Underweight|mio people|decrease|0|1",
                  "Health|2|Obesity|mio people|decrease|0|1",
                  "Health|3|Years of life lost|million years|decrease|0|1",
                  "Environment|4.1|All Land Types|BII|increase|2|100",
                  "Environment|4.2|Cropland Landscapes|BII|increase|2|100",
                  "Environment|4.3|Biodiversity Hotspots|BII|increase|2|100",
                  "Environment|5|Croparea Diversity|Shannon Index|increase|2|1",
                  "Environment|6|Nitrogen surplus|Mt N/yr|decrease|0|1",
                  "Environment|7|Water env. flow violations|km3/yr|decrease|0|1",
                  "Environment|8|Cumul. GHG emissions|GtCO2eq since 1995|decrease|0|1",
                  "Environment|9|Global Surface Warming|degree Celsius|decrease|2|1",
                  "Inclusion|10|Expenditures for agric.|USD/person|decrease|0|1",
                  "Inclusion|11|People Below 3.20$/Day|mio people|decrease|0|1",
                  "Inclusion|12|Agric. employment|mio people|increase|0|1",
                  "Inclusion|12|Agric. employment|mio people|increase|0|1",
                  "Inclusion|13|Agric. wages|Index rel. to 2010|increase|2|1",
                  "Economy|14|Bioeconomy Supply|billion US$05/yr|increase|0|0.001",
                  "Economy|15|Costs|billion US$05/yr|decrease|0|0.001")

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
