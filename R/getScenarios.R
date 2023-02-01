#' @title getScenarios
#' @description Scenarios for FSDP plotting
#' @param tableType select the scenarios based on csv file figureTableType.csv in inst/extdata folder
#' @param oldformat return in format modelrun::bundle|scenarioname
#' @export
#'
#' @details includes scenarios and their grouping into bundles
#' @return named vector
#' @author Benjamin Bodirsky

getScenarios <- function(tableType="1", oldformat=FALSE) {
  pathX=system.file("extdata",mapping= "scenarionames.csv",package = "m4fsdp")
  #pathX="C:/bbb/MAgPIE SVN/libraries/m4fsdp/inst/extdata/scenarionames.csv"
  scenario_names <- read.csv(pathX, sep = ";")

  pathX=system.file("extdata",mapping= paste0("figure_",tableType,".csv"),package = "m4fsdp")
  scenario_select <- read.csv(pathX, sep = ";")

  scenario_select$names <- scenario_names[match(scenario_select[,1],table = scenario_names[,1]),2]

  if(oldformat){
    #"EnergyTrans::ExtTransformation|EnergyTrans"
    makebold <- which(scenario_select$bold==1)
    scenario_select$names[makebold] <- paste("<b>",scenario_select$names[makebold],"</b>",sep = "")

    scenario_select <- paste0(scenario_select$modelrun,"::",scenario_select$bundle,"|",scenario_select$names)
  }

  return(scenario_select)
}

