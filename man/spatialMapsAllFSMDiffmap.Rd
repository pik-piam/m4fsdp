% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatialMapsAllFSMDiffmap.R
\name{spatialMapsAllFSMDiffmap}
\alias{spatialMapsAllFSMDiffmap}
\title{spatialMapsAllFSMDiffmap}
\usage{
spatialMapsAllFSMDiffmap(
  repReg,
  repIso,
  repGrid,
  reg2iso,
  file = NULL,
  subset = c("FSTsdp_minus_BASEssp2")
)
}
\arguments{
\item{repReg}{reporting .rds file or data.frame with regional results (produced by FDSP_collect.R output script)}

\item{repIso}{reporting .rds file or data.frame with country level results (produced by FDSP_collect.R output script)}

\item{repGrid}{reporting .rds file or data.frame with grid level results (produced by FDSP_collect.R output script)}

\item{reg2iso}{mapping file or data.frame with regions and countries (produced by FDSP_collect.R output script)}

\item{file}{file name (e.g. FSDP_spatialmaps.pdf or FSDP_spatialmaps.pdf) or NULL}

\item{subset}{"FSTssp2_minus_BASEssp2" or "FSTsdp_minus_BASEssp2" returns diffmaps for allFSM or FSDP scenario minus the BAU scenario in 2050.}
}
\value{
if file is NULL a ggplot2 object will be return
}
\description{
creates a spatial diffmap between allFSM scneario and reference
}
\details{
blub
}
\author{
Florian Humpenoeder
}
