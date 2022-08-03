globalVariables(c("scenario", "scenset", "region", "period", "unit", "variable", "varunit", "value"))
#' @title convertReportFSDP
#' @description reads in FSDP reporting file
#'
#' @export
#'
#' @param rep reporting .rds file (produced by FDSP_collect.R output script)
#' @param scengroup options: FSECa, FSECb, FSECc, FSECd
#' @param subset TRUE returns "BAU 2020","BAU 2050","FSDP 2050". FALSE returns all scenarios
#' @param varlist file name for plain text variable list (e.g. "var_names.csv")
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author Florian Humpenoeder
#' @import data.table quitte
#' @importFrom utils write.csv

convertReportFSDP <- function(rep, scengroup = NULL, subset = FALSE, varlist = NULL) {

  if (!is.data.frame(rep)) rep <- readRDS(rep)
  rep <- as.data.table(rep)
  rep <- rep[!scenario %like% "calibration_FSEC", ]
  rep[, c("version", "scenset", "scenario") := tstrsplit(scenario, "_", fixed = TRUE)]
  rep[, version := NULL]
  if (!is.null(scengroup)) rep <- rep[scenset %in% scengroup]
  rep$scenario <- factor(rep$scenario)
  rep <- droplevels(rep)
  if (!is.null(varlist)) {
    if (!is.null(rep$unit)) {
      rep[, varunit := paste0(variable, " (", unit, ")")]
      write.csv(unique(rep$varunit), varlist, row.names = FALSE, quote = FALSE)
      rep[, varunit := NULL]
    } else {
      write.csv(paste0(unique(rep$variable)), varlist, row.names = FALSE, quote = FALSE)
    }
  }

  if (subset) {
    if (!is.null(rep$region)) {
      rep <- rep[region != "GLO", ]
      rep <- rep[region != "World", ]
    }

    rep <- rep[period %in% c(2020, 2050) & scenario %in% c("BAU", "FSDP"), ]
    rep <- rep[!(scenario == "FSDP" & period == 2020), ]
    rep[, scenario := paste(scenario, period)]
    rep$scenario <- factor(rep$scenario, levels = c("BAU 2020", "BAU 2050", "FSDP 2050"))
  }

  rep <- droplevels(rep)
  if (length(unique(rep$region)) == 249 || length(unique(rep$region)) == 179) {
    names(rep)[names(rep) == "region"] <- "iso_a3"
  }
  names(rep)[names(rep) == "country"] <- "iso_a3"
  return(rep)
}
