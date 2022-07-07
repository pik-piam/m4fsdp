#' @title spatialMapsFSDP
#' @description creates a spatial mapsfor FSDP MAgPIE runs
#'
#' @export
#'
#' @param rds_reg reporting .rds file or data.frame with regional results (produced by FDSP_collect.R output script)
#' @param rds_iso reporting .rds file or data.frame with country level results (produced by FDSP_collect.R output script)
#' @param rds_grid reporting .rds file or data.frame with grid level results (produced by FDSP_collect.R output script)
#' @param reg2iso mapping file or data.frame with regions and countries (produced by FDSP_collect.R output script)
#' @param file file name (e.g. FSDP_spatialmaps.pdf or FSDP_spatialmaps.pdf) or NULL
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author Florian Humpenoeder
#' @import ggplot2 data.table patchwork cartogram sf RColorBrewer rnaturalearth quitte
#' @importFrom utils write.csv read.csv

globalVariables(c("model", "scenario", "region","period","unit","variable","varunit","valuefill","value","label","vargroup",".",".label",".value","iso_a3","x","y"))

spatialMapsFSDP <- function(rep_reg,rep_iso,rep_grid,reg2iso,file=NULL) {
  ### projections
  #https://semba-blog.netlify.app/01/26/2020/world-map-and-map-projections/

  #### read in data files
  rep_reg <- convertReportFSDP(rep_reg,subset=TRUE)
  rep_iso <- convertReportFSDP(rep_iso,subset=TRUE)
  rep_grid <- convertReportFSDP(rep_grid,subset=TRUE)
  if(!is.data.frame(reg2iso)) reg2iso <- read.csv(reg2iso,header = TRUE,row.names = 1)

  #get country layer
  countries <- ne_countries(returnclass = "sf", scale = "small")
  countries <- subset(countries,iso_a3 %in% levels(rep_iso$iso_a3))
  countries <- countries[-grep('Antarctica',countries$name),]
  #map_ext <- terra::ext(countries)
  ylim <- c(-55.61183,83.64513)
  xlim <- c(-180,180)

  ### calc pop polygon for cartogram maps
  pop <- rep_iso[variable=="Population",]
  pop$unit <- NULL
  names(pop)[names(pop)=="value"] <- "pop"
  BAU_2020 <- droplevels(pop[scenario=="BAU 2020",])
  BAU_2050 <- droplevels(pop[scenario=="BAU 2050",])
  SDP_2050 <- droplevels(pop[scenario=="SDP 2050",])
  BAU_2020 <- merge(countries,BAU_2020,by="iso_a3")
  BAU_2050 <- merge(countries,BAU_2050,by="iso_a3")
  SDP_2050 <- merge(countries,SDP_2050,by="iso_a3")
  BAU_2020 <- st_transform(BAU_2020, crs = 3857)#('+proj=robin')try 3857
  BAU_2050 <- st_transform(BAU_2050, crs = 3857)
  SDP_2050 <- st_transform(SDP_2050, crs = 3857)
  BAU_2020 <- cartogram_cont(BAU_2020, "pop",itermax = 7)
  BAU_2050 <- cartogram_cont(BAU_2050, "pop",itermax = 7)
  SDP_2050 <- cartogram_cont(SDP_2050, "pop",itermax = 7)
  pop <- rbind(BAU_2020,BAU_2050,SDP_2050)
  pop$variable <- NULL

  #theme for maps
  my_theme <- theme_minimal()+theme(plot.margin=grid::unit(c(5,0,0,0), "mm"))+theme(legend.justification = "left",legend.title = element_text(size=12),plot.title = element_text(hjust=0.5,vjust = 0,size=12,face = "bold"),strip.text = element_text(size=12),panel.background = element_rect(fill="black"),panel.grid = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

  ## regional data
  title <- "Welfare: Production cost agriculture per capita"
  unit <- "USD/cap/yr"
  b <- rep_reg[,.(value=value[variable=="Costs"]/value[variable=="Population"]),by=.(model,scenario,region,period)]
  all <- merge(reg2iso,b)
  all <- merge(countries,all)
  #all <- merge(pop,all)
  p1 <- ggplot(all) + facet_wrap(vars(scenario),ncol = 3) +
    geom_sf(aes(fill=value),show.legend = TRUE,color="white",size=0.2)+#geom_sf_text(aes(label= I(ifelse(iso_a3 %in% c("USA","IND","NGA","BRA","CHN"),iso_a3,"")),color=I(ifelse(value < 0.1, "white","white"))),size=2)+#+scale_fill_gradientn("Share",colors=brewer.pal(9,"RdPu"),na.value = "grey90") +
    scale_fill_gradientn(unit,colors=brewer.pal(9,"RdPu")[-1],na.value = "grey90",limits=c(0,2000)) + my_theme + #scale_fill_viridis_b(name="Population (M)",direction = -1) +
    labs(title = title,caption = "Projection: Equal Earth")+coord_sf(crs = st_crs("+proj=eqearth"))

  ## Country level data
  title <- "Health: Spatial distribution of population underweight (Cartogram - size reflects population)"
  b <- rep_iso[,.(value=value[variable=="Nutrition|Anthropometrics|People underweight"]/value[variable=="Population"]),by=.(model,scenario,iso_a3,period)]
  all <- merge(pop,b)
  p2 <- ggplot(all) + facet_wrap(vars(scenario),ncol = 3) +
    geom_sf(aes(fill=value),show.legend = TRUE,color="white",size=0.2)+geom_sf_text(aes(label= I(ifelse(iso_a3 %in% c("USA","IND","NGA","BRA","CHN"),iso_a3,"")),color=I(ifelse(value < 0.1, "white","white"))),size=2)+#+scale_fill_gradientn("Share",colors=brewer.pal(9,"RdPu"),na.value = "grey90") +
    scale_fill_gradientn("Share",colors=brewer.pal(9,"RdPu")[-1],na.value = "grey90",limits=c(0,0.4)) + my_theme + #scale_fill_viridis_b(name="Population (M)",direction = -1) +
    labs(title = title,caption = "Projection: Cartogram based on population")

  title <- "Health: Spatial distribution of population obese (Cartogram - size reflects population)"
  b <- rep_iso[,.(value=value[variable=="Nutrition|Anthropometrics|People obese"]/value[variable=="Population"]),by=.(model,scenario,iso_a3,period)]
  all <- merge(pop,b)
  p2 <- ggplot(all) + facet_wrap(vars(scenario),ncol = 3) +
    geom_sf(aes(fill=value),show.legend = TRUE,color="white",size=0.2)+geom_sf_text(aes(label= I(ifelse(iso_a3 %in% c("USA","IND","NGA","BRA","CHN"),iso_a3,"")),color=I(ifelse(value < 0.1, "white","white"))),size=2)+#+scale_fill_gradientn("Share",colors=brewer.pal(9,"RdPu"),na.value = "grey90") +
    scale_fill_gradientn("Share",colors=brewer.pal(9,"RdPu")[-1],na.value = "grey90",limits=c(0,0.4)) + my_theme + #scale_fill_viridis_b(name="Population (M)",direction = -1) +
    labs(title = title,caption = "Projection: Cartogram based on population")

  ## Grid cell data
  title <- "Biodiversity Intactness Index"
  unit <- "index"
  bb <- droplevels(rep_grid[variable=="BII (index)",])
  p3 <- ggplot(bb) +
    geom_raster(aes(x=x,y=y,fill = .value)) + coord_equal(xlim = ) + facet_wrap(~scenario) + ggtitle(title) +
    geom_sf(data = countries,color = "white",fill = NA,size = 0.2) + coord_sf(ylim=ylim,xlim=xlim) +
    scale_fill_gradient2(unit,low = "darkred",high = "darkgreen",mid = "yellow",midpoint = 0.76,na.value = "grey90") + my_theme

  combined <- p1 + p2 +p3
  combined <- combined + plot_layout(guides = "keep",ncol = 1)

  if(is.null(file)) return(combined) else {
    ggsave(filename = file,combined,width = 8,height = 5,scale=1.5)
  }
}

