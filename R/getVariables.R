#' @title getVariables
#' @description Variables for FSDP plotting
#'
#' @export
#'
#' @details includes information about unit, variable group, rounding etc
#' @return named vector
#' @author Florian Humpenoeder

getVariables <- function() {

  var <- c("SDG|SDG02|Prevalence of underweight",
           "SDG|SDG03|Prevalence of obesity",
           "Health|Years of life lost|Risk|Diet and anthropometrics",
           "Biodiversity|BII",
           "Biodiversity|Shannon crop area diversity index",
           "Resources|Nitrogen|Nutrient surplus incl natural vegetation",
           "Water|Environmental flow violation volume",
           "Emissions|GWP100AR6|Land|Cumulative",
           "Global Surface Temperature",
           "Household Expenditure|Food|Expenditure",
           "Income|Number of People Below 3.20$/Day",
           "Agricultural employment|Crop and livestock products",
           "Hourly labor costs relative to 2000",
           "Value|Bioeconomy Demand",
           "Costs Without Incentives")

  #vargroup|var#|variableName|variableUnit|DirectionImprovment|rounding|factor
  names(var) <- c("Health|1|Underweight|mio people|decrease|0|1",
                  "Health|2|Obesity|mio people|decrease|0|1",
                  "Health|3|Years of life lost|million years|decrease|0|1",
                  "Environment|4|Biodiversity|BII|increase|2|100",
                  "Environment|5|Croparea diversity|Shannon Index (1)|increase|2|1",
                  "Environment|6|Nitrogen surplus|Mt N/yr|decrease|0|1",
                  "Environment|7|Water flow violations|km3/yr|decrease|0|1",
                  "Environment|8|Greenhouse Gases|GtCO2eq since 1995|decrease|0|1",
                  "Environment|9|Global Surface Temp.|deg C|decrease|2|1",
                  "Inclusion|10|Expenditures for agri.|USD/person|decrease|0|1",
                  "Inclusion|11|People Below 3.20$/Day|mio people|decrease|0|1",
                  "Inclusion|12|Agri. employment|mio people|increase|0|1",
                  "Inclusion|13|Agri. wages|Index rel. to 2000|increase|2|1",
                  "Economy|14|Bioeconomy Supply|billion US$05/yr|increase|0|0.001",
                  "Economy|15|Costs|billion US$05/yr|decrease|0|0.001")

  return(var)
}
