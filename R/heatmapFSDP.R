#' @title heatmapFSDP
#' @description creates a heatmap for FSDP MAgPIE runs
#'
#' @export
#'
#' @param rds_report rds file with all MAgPIE runs, produced with merge_report.R output script.
#' @param region_sel Region that should be plotted
#' @param file file name (e.g. FSDP_heatmap.pdf or FSDP_heatmap.jpg) or NULL
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author Florian Humpenoeder
#' @import ggplot2 ggiraph forcats data.table scales htmlwidgets
#' @importFrom utils write.csv

globalVariables(c("model", "scenario", "region","period","unit","variable","varunit","valuefill","value","label","vargroup",".",".label"))

heatmapFSDP <- function(rep_reg,region_sel="GLO",file=NULL) {

  #### read in data files
  rep <- convertReportFSDP(rep_reg,subset=FALSE)

  var <- c("SDG|SDG02|Prevalence of underweight",
           "SDG|SDG03|Prevalence of obesity",
           "Biodiversity|BII",
           "SDG|SDG06|Nitrogen surplus on cropland",
           "SDG|SDG06|Agricultural water use",
           "Emissions|CO2|Land|Cumulative|+|Land-use Change",
           "Prices|Food Expenditure Index corrected for ghg costs",
           "Agricultural employment",
           "Labor costs per worker relative to GDP pc",
           "Costs")

  names(var) <- c("Health|Prevalence of underweight (million people)",
                  "Health|Prevalence of obesity (million people)",
                  "Environment|Biodiversity Intactness (Index)",
                  "Environment|Nitrogen surplus on cropland (Mt N/yr)",
                  "Environment|Agricultural water use (km3/yr)",
                  "Environment|Cumulative CO2 emissions (GtCO2 since 1995)",
                  "Inclusion|Relative Poverty (Index 2020=100)",
                  "Inclusion|Agricultural employment (million people)",
                  "Inclusion|Agricultural wages (% of GDP)",
                  "Welfare|Costs (billion US$05/yr)")

  # names(var) <- c("Health|Prevalence of underweight\n(million people)",
  #                 "Health|Prevalence of obesity\n(million people)",
  #                 "Environment|Biodiversity Intactness\n(Index)",
  #                 "Environment|Nitrogen surplus on cropland\n(Mt N/yr)",
  #                 "Environment|Agricultural water use\n(km3/yr)",
  #                 "Environment|Cumulative CO2 emissions\n(GtCO2 since 1995)",
  #                 "Inclusion|Relative Poverty\n(Index 2020=100)",
  #                 "Inclusion|Agricultural employment\n(million people)",
  #                 "Inclusion|Agricultural wages\n(% of GDP)",
  #                 "Welfare|Costs\n(billion US$05/yr)")

  b <- rep[variable %in% var & region==region_sel & period == 2050,]
  b <- droplevels(b)

  bb <- rep[variable %in% var & region==region_sel & period == 2020 & scenario=="BAU",]
  bb <- droplevels(bb)
  #bb[,scenario:="2020"]
  b <- rbind(bb,b)

  b$variable <- factor(b$variable,levels = var,labels=names(var))
  b[, c("vargroup","variable") := tstrsplit(variable, "|", fixed=TRUE)]

  vargroup_order <- c("Health","Environment","Inclusion","Welfare")
  b$vargroup <- factor(b$vargroup,levels = vargroup_order)

  b[,valuefill := value-value[scenario=="BAU" & period=="2050"],by = .(variable)]
  b[variable %in% c("Biodiversity Intactness (Index)","Agricultural wages (% of GDP)","Agricultural employment (million people)"),valuefill:=-valuefill]
  b[valuefill>0, valuefill := rescale(valuefill, to = c(0, 1)),by = .(variable)]
  b[valuefill<0, valuefill := rescale(valuefill, to = c(-1,0)),by = .(variable)]

  b[variable %in% c("Costs (billion US$05/yr)"),value:=value/1000]
  b[variable %in% c("Biodiversity Intactness (Index)"),value:=value*100]

  b[,label:= fifelse(max(value) > 100, formatC(value,0,format="f"),formatC(value,2,format="f")),by = .(region,model,scenario,variable,unit,period)]

  b[scenario == "BAU",scenario:=paste(scenario,period)]
  b <- droplevels(b)
  scen_order <- levels(fct_reorder(b$scenario,b$valuefill,sum,.desc = F))
  scen_order <- c(scen_order[!scen_order %in% c("BAU 2020","BAU 2050")],"BAU 2050","BAU 2020")
  b$scenario <- factor(b$scenario,levels=scen_order)
  b <- droplevels(b)

  m <- ggplot(b, aes(y=scenario, x=variable)) + theme_minimal()+theme(panel.border = element_rect(colour = NA,fill=NA)) +
    geom_tile_interactive(aes(fill = valuefill, tooltip = paste0("Sceanrio: ",scenario,"\nIndicator: ",variable), data_id = interaction(variable)),colour = "white") +
    #  scale_fill_continuous(type = "viridis",breaks = c(-1, 0, 1), labels = c("low", "med", "high")) +
    scale_fill_gradient2_interactive(midpoint = 0,low = "#91cf60",mid = "#ffffbf",na.value = "grey80", high = "#fc8d59",breaks = c(-1,0,1), labels = c("positive","zero", "negative")) +
    geom_text_interactive(aes(label = label, tooltip = paste0("Sceanrio: ",scenario,"\nIndicator: ",variable), data_id = interaction(variable)),size=3,color="grey50") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(y="Scenario",x="Indicator",fill="Impact\nrelative to\nBAU 2050")+theme(legend.position = "right")+guides(fill= guide_colorbar_interactive(mapping=aes(data_id = interaction(variable)),reverse = F,title.hjust = 0,title.vjust=2,title.position = "top",barwidth = 1,barheight = 20,legend.direction = "vertical")) + theme(strip.background =element_rect(color="grey50"),axis.line = element_blank(),axis.ticks=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  m <- m+facet_grid(vars(period),vars(vargroup),scales="free",space = "free")+scale_x_discrete(position = "top")+theme(axis.text.x = element_text(angle = 30, hjust = 0))

  if(is.null(file)) return(m) else {
    ggsave(file,m,scale = 1.2,height=8,width = 7)
    p <- girafe(
      ggobj = m,
      options = list(
        opts_sizing(rescale = TRUE, width = .95),
        opts_hover(css = "fill:NULL;cursor:pointer;"),
        opts_hover_inv(css = "opacity:0.1;"),
        opts_selection(girafe_css(css = "fill:NULL;stroke:grey"), only_shiny = FALSE, type="multiple",selected = NULL),
        opts_tooltip(css = "background-color:white;padding:5px;border-radius:2px;border: black 1px solid;color:black;")
      ),
      width_svg = 10,
      height_svg = 10
    )
    saveWidget(p,paste0(substring(file,1,nchar(file)-3),"html"))
  }
}

