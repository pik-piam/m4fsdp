#' @title assignScenarioLinetype
#' @description Line Type for FSDP MAgPIE runs
#' @export
#'
#' @param scenario_names scenario names to be checked for their colorcode.
#' @return NULL
#' @author Florian Humpenoeder


assignScenarioLinetype <- function(scenario_names) {
  # Create a mapping from potential names to colors
  name_to_color <- c(
    "AgroMngmt" = "dashed",##FFDC3D
    "SSP2bau" = "solid",
    "BAU" = "solid",
    "NatureSparing" = "dashed",
#    "SSP3" = "#F60239",
    "AllInclusion" = "dashed",
    "emptycolorblind2" = "dashed",
    "FSDP" = "solid",
    "SSP5bau" = "dashed",
    "Diet" = "dashed",
    "Sufficiency" = "dashed",
    "Livelihoods" = "dashed",
    "emptycolorblind3" = "dashed",
    "emptycolorblind4" = "dashed",
    "emptycolorblind5" = "dashed",
    "ExternalPressures" = "dashed"
  )
  # Initialize an empty vector to store the colors for the scenario names
  scenario_colors <- character(length(scenario_names))
  # Iterate over the scenario names
  for (i in 1:length(scenario_names)) {
    # Look up the color for the current scenario name in the mapping
    scenario_color <- name_to_color[scenario_names[i]]
    # If the scenario name is in the mapping, store the corresponding color in the scenario_colors vector
    if (!is.na(scenario_color)) {
      scenario_colors[i] <- scenario_color
    } else {
      scenario_colors[i] <- "solid"
    }
  }
  return(scenario_colors)
}
