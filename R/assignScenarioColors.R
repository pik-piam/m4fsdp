#' @title validationFSDP
#' @description Validation for FSDP MAgPIE runs
#' written by openAI chatbot.
#' The command was: "Please write a function in R. It should receive a vector of scenario_names.
#' In the function, there is a mapping that assigns each element of potential_names to an individual color.
#' The function checks which elements of scenario_names are in potential_names and returns the
#' respective colors as a vector of colors.
#' @export
#'
#' @param scenario_names scenario names to be checked for their colorcode.
#' @return NULL
#' @author openAI, Benjamin Bodirsky


assignScenarioColors <- function(scenario_names) {
  # Create a mapping from potential names to colors
  name_to_color <- c(
    "AgroMngmt" = "#009FFA",##FFDC3D
    "SSP2bau" = "#F60239",
    "NatureSparing" = "#00E307",
#    "SSP3" = "#F60239",
    "AllInclusion" = "#9400E6",
    "emptycolorblind2" = "#68023F",
    "FSDP" = "#008169",
    "SSP5bau" = "#EF0096",
    "Diet" = "#00DCB5",
    "Sufficiency" = "#00DCB5",
    "Livelihoods" = "#FF71FD",
    "emptycolorblind3" = "#FFCFE2",
    "emptycolorblind4" = "#003C86",
    "emptycolorblind5" = "#7CFFFA",
    "ExternalPressures"="#6A0213"
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
      scenario_colors[i] <- "black"
    }
  }
  return(scenario_colors)
}
