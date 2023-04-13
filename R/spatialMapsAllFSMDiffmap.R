globalVariables(c("model", "scenario", "region", "period", "unit", "variable", "varunit", "valuefill",
                  "value", "label", "vargroup", ".", ".label", ".value", "iso_a3", "x", "y"))
#' @title spatialMapsAllFSMDiffmap
#' @description creates a spatial diffmap between allFSM scneario and reference
#'
#' @export
#'
#' @param repReg reporting .rds file or data.frame with regional results (produced by FDSP_collect.R output script)
#' @param repIso reporting .rds file or data.frame with country level results (produced by FDSP_collect.R output script)
#' @param repGrid reporting .rds file or data.frame with grid level results (produced by FDSP_collect.R output script)
#' @param reg2iso mapping file or data.frame with regions and countries (produced by FDSP_collect.R output script)
#' @param file file name (e.g. FSDP_spatialmaps.pdf or FSDP_spatialmaps.pdf) or NULL
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author Florian Humpenoeder
#' @import ggplot2 data.table patchwork cartogram sf RColorBrewer rnaturalearth quitte
#' @importFrom utils write.csv read.csv
#' @importFrom terra project rast

spatialMapsAllFSMDiffmap <- function(repReg, repIso, repGrid, reg2iso, file = NULL) {
  ### projections
  # https://semba-blog.netlify.app/01/26/2020/world-map-and-map-projections/

  #### read in data files
  repReg  <- convertReportFSDP(repReg, subset = "allFSM_minus_ref")
  repIso  <- convertReportFSDP(repIso, subset = "allFSM_minus_ref")
  repGrid <- convertReportFSDP(repGrid, subset = "allFSM_minus_ref")
  if (!is.data.frame(reg2iso)) {
    reg2iso <- read.csv(reg2iso, header = TRUE, row.names = 1)
  }

  # get country layer
  countries <- ne_countries(returnclass = "sf", scale = "medium")
  countries <- countries[-grep("Antarctica", countries$name), ]
  countries <- countries[-grep("Fr. S. Antarctic Lands", countries$name), ]
  countries$iso_a3[countries$name == "France"] <- "FRA"
  countries$iso_a3[countries$name == "Norway"] <- "NOR"
  #countries <- countries[countries$pop_rank >= 10,]
  #iso_a3 does not include all countries: FRA and NOR are missing!

  # https://gist.github.com/rCarto/34c7599d7d89a379db02c663c2e333ee
  # Only keep the largest polygons of multipart polygons for a few countries
  # (e.g. display only continental US)
  # countries <- st_transform(countries, crs = st_crs("+proj=moll"))
  # frag_ctry <- c("US", "RU", "FR", "IN", "ES", "NL", "CL", "NZ", "ZA")
  # largest_ring = function(x) {
  #   x$ids <- 1:nrow(x)
  #   pols = st_cast(x, "POLYGON", warn = FALSE)
  #   spl = split(x = pols, f = pols$ids)
  #   do.call(rbind, (lapply(spl, function(y) y[which.max(st_area(y)),])))
  # }
  # st_geometry(countries[countries$iso_a2 %in% frag_ctry,]) <-
  #   st_geometry(largest_ring(countries[countries$iso_a2 %in% frag_ctry,]))


  countries <- countries[, c("iso_a3", "name", "geometry")]
  # countries$iso_a3 <- factor(countries$iso_a3)
  # countries$name <- factor(countries$name)
  # setdiff(levels(b$iso_a3),levels(countries$iso_a3))
  #names(countries)[names(countries) == "gu_a3"] <- "iso_a3"

  # function to calc polygons for cartogram
  calcPolygon <- function(pop, name) {
    z <- NULL
    for (i in levels(pop$scenario)) {
      x <- subset(pop, scenario == i)
      x <- st_transform(x, crs = 3857)
      x <- cartogram_cont(x, name, itermax = 15)
      z <- rbind(z, x)
    }
    z$variable <- NULL
    return(z)
  }

  # load polygons
  pop <- readRDS(system.file(package = "m4fsdp", "extdata", "pop.rds"))
  pop <- pop[which(pop$scenario=="BAU 2050"), ]
  pop$scenario <- "allFSM - reference"
  pop$scenario <- factor(pop$scenario)
  agEmpl <- readRDS(system.file(package = "m4fsdp", "extdata", "agEmpl.rds"))
  agEmpl <- agEmpl[which(agEmpl$scenario=="BAU 2050"), ]
  agEmpl$scenario <- factor(agEmpl$scenario)
  agEmpl$scenario <- "allFSM - reference"

  # theme for maps
  myTheme <- theme_minimal(base_size = 12) +
    theme(plot.margin = grid::unit(c(5, 5, 10, 5), "pt"),
          strip.text = element_blank(), strip.background = element_blank(),
          panel.background = element_rect(fill = "black"),
          panel.grid = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "top",
          legend.justification = "center",
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(-10, -10, -20, -10),
          legend.spacing.y = unit(2, "pt"),
          plot.title = element_text(hjust = 0, margin = unit(c(0, 0, 0, 0), "pt"), size = 12, face = "bold"),
          legend.title = element_text(size = 12, hjust = 1, margin = unit(c(0, 0, 0, 0), "pt")),
          plot.caption = element_text(hjust = 1, margin = unit(c(0, 0, 0, 0), "pt")))

  ## regional/country data
  labelX <- -5000000
  labelY <- -6500000
  cropAll <- function(all) {
    all <- st_crop(all, xmin = -13500000, xmax = 18796780,ymin = -6990985, ymax = 13296170)
    return(all)
  }

  ## Grid cell data
  countries2 <- st_transform(countries, crs = st_crs("+proj=moll"))
  labelXGrid <- -3500000
  labelYGrid <- -6500000
  xlimMoll   <- c(-11007870, 16007870)
  asRaster   <- function(x, countries2) {
    z <- rast()
    for (i in levels(x$scenario)) {
      y <- rast(droplevels(x[scenario == i, ])[, c("x", "y", ".value")], crs = "+proj=latlon")
      names(y) <- i
      z <- c(z, y, warn = FALSE)
    }
    z <- terra::project(z, st_crs(countries2)$proj4string)
    z <- as.data.frame(z, xy = TRUE)
    z <- melt(setDT(z), id.vars = c("x", "y"), variable.name = "scenario")
    return(z)
  }

  # plotDUMMY:
  title <- "DUMMY - data seems missing"
  unit <- "DUMMY"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  b[, value := 0]
  all <- merge(pop, b, all.x = TRUE)
  all <- cropAll(all)

  plotDUMMY <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2, ) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "PuBuGn")[-1], na.value = "grey90") +
    myTheme + labs(title = title, caption = caption) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  #ggsave("test25.png",plotDUMMY)

  # Health: Spatial distribution of population underweight
  title <- "a) Underweight"
  unit <- "Population share per country"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = value[variable == "Nutrition|Anthropometrics|People underweight"] /
                    value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b, all.x = TRUE)
  all <- cropAll(all)

  plotUNDERWEIGHT <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    #scale_fill_gradientn(unit, colors = brewer.pal(9, "PuBu")[-1], na.value = "grey90", limits = c(-0.2, 0.2), oob = scales::squish, breaks = c(-0.2,-0.15,-0.1,-0.05,0,0.05,0.1,0.15,0.2), labels = c("< -0.2",-0.15,-0.1,-0.05,0,0.05,0.1,0.15,">0.2")) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-0.2, 0.2), oob = scales::squish, breaks = c(-0.2,-0.1,0,0.1,0.2), labels = c("< -0.2",-0.1,0,0.1,">0.2")) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("testUNDERWEIGHT.png",plotUNDERWEIGHT)


  # Health: Spatial distribution of population obese
  title <- "b) Obesity"
  unit <- "Population share per country"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = value[variable == "Nutrition|Anthropometrics|People obese"] /
                    value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b, all.x = TRUE)
  all <- cropAll(all)

  plotOBESE <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-0.2, 0.2), oob = scales::squish, breaks = c(-0.2,-0.1,0,0.1,0.2), labels = c("< -0.2",-0.1,0,0.1,">0.2")) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("testOBESE.png",plotOBESE)

  # Years of lost life
  title <- "c) Years of life lost"
  unit <- "Years per person"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = (value[variable == "Health|Years of life lost|Disease"]) /
                        value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b, all.x = TRUE)
  all <- cropAll(all)

  plotYOLL <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-0.1, 0.1), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("testYOLL.png",plotYOLL)

  # Inclusion: Expenditure for agr. products per capita
  title <- "d) Expenditure for agricultural products"
  unit <- "USD per capita"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = value[variable == "Household Expenditure|Food|Expenditure"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b, all.x = TRUE)
  all <- cropAll(all)

  plotEXPENDITURE <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-400, 400), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE))," to ",round(max(b$value,na.rm = TRUE))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("testExpenditures.png",plotEXPENDITURE)

  # Inclusion: Share of Population with Incomes less than 3.20$/Day
  title <- "e) Income below 3.20$ per day"
  unit <- "Population share per world region"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = value[variable == "Income|Number of People Below 3p20 USDppp11/day"] /
                    value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b, all.x = TRUE)
  all <- cropAll(all)

  plotPOVERTY <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    # scale_fill_manual(values = c("#FFFFFF", "#fee8c8", "#fdbb84", "#d7301f", "#7f0000", "#54278f"),
    #                  breaks = seq(0, 0.4, by x  = 0.1)) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-0.02, 0.02), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX + 1000000, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotPOVERTY.png",plotPOVERTY)

  # # Inclusion: Gini Coefficient
  # title <- "f) Gini Coefficient"
  # unit <- "(0-1) Gini Coefficient between 0 and 1"
  # caption <- "Cartogram projections with areas proportional to population"
  # b     <- repIso[, .(value = value[variable == "Income|Gini Coefficient"] /
  #                       value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  # all <- merge(pop, b)
  # plotGINI <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
  #   geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
  #   geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
  #                    color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
  #   # scale_fill_manual(values = c("#FFFFFF", "#fee8c8", "#fdbb84", "#d7301f", "#7f0000", "#54278f"),
  #   #                  breaks = seq(0, 0.4, by = 0.1)) +
  #   scale_fill_gradientn(unit, colors = rev(brewer.pal(11, "RdYlGn")[-1]), na.value = "grey90", limits = c(0, 1), oob = scales::squish) +
  #   myTheme + labs(title = title, caption = caption) +
  #   guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
  #   geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX + 1000000, y = labelY,
  #             hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)


  # Inclusion: Share of working age population employed in agriculture
  title <- "f) Agricultural employment"
  unit <- "Population share per country"
  caption <- "Cartogram projections with areas proportional to population"

  b     <- repReg[, .(value = value[variable %in% c("Share of working age population employed in agriculture|Crop and livestock products",
                             "Labor|Employment|Share of working age population employed in agriculture")]/100), # new var name
                  by = .(model, scenario, region, period)]
  all   <- merge(reg2iso, b)
  all   <- merge(pop, all, all.x = TRUE)
  all <- cropAll(all)

  plotEMPLOYMENT <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "RdYlGn"), na.value = "grey90", limits = c(-0.05, 0.05), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotEMPLOYMENT.png",plotEMPLOYMENT)


  # Inclusion: Hourly labor costs in agriculture
  title <- "g) Agricultural wages"
  unit <- "USD per hour"
  caption <- "Cartogram projections with areas proportional to agricultural employment"
  b     <- repReg[, .(value = value[variable %in% c("Hourly labor costs", "Labor|Wages|Hourly labor costs")]),
                    by = .(model, scenario, region, period)]
  all   <- merge(reg2iso, b)
  all   <- merge(agEmpl, all, all.x = TRUE)
  all <- cropAll(all)

  plotWAGE <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "RdYlGn"), na.value = "grey90",
                         limits = c(-5, 5), oob = scales::squish)+
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE))," to ",round(max(b$value,na.rm = TRUE))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotWAGE.png",plotWAGE)


  # Environment: Biodiversity Intactness Index
  title <- "h) Biodiversity Intactness Index"
  unit  <- "index"
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "BII (index)", ])
  bb    <- asRaster(b, countries2)

  plotBII <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "RdYlGn"), na.value = "grey90", oob = scales::squish, limits = c(-0.2,0.2)) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value,na.rm = TRUE),2)," to ",round(max(b$.value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotBII.png",plotBII)

  # Environment: Croparea diversity
  title <- "i) Shannon crop diversity"
  unit  <- "index"
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "Shannon crop diversity (index)", ])
  bb    <- asRaster(b, countries2)

  plotCROPDIV <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    #scale_fill_gradientn(unit, colors = brewer.pal(9, "YlOrBr")[-1], na.value = "grey90", limits = c(1, 3), oob = scales::squish, breaks = c(1,1.5,2,2.5,3), labels = c("<1",1.5,2,2.5,">3")) + #trans = "log1p"
    scale_fill_gradientn(unit, colors = brewer.pal(9, "RdYlGn"), na.value = "grey90", limits = c(-1, 1), oob = scales::squish, breaks = c(-1,-0.5,0,0.5,1), labels = c("<-1",-0.5,0,0.5,">1")) + #trans = "log1p"
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value,na.rm=T),2)," to ",round(max(b$.value,na.rm=T),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotCROPDIV.png",plotCROPDIV)

  # Environment: Nutrient Surplus
  title <- "j) Nutrient Surplus"
  unit  <- "kg N per ha"
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "nutrientSurplus (kg N per ha)", ])
  bb    <- asRaster(b, countries2)

  plotNITROGEN <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-100, 100), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value))," to ",round(max(b$.value))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotNITROGEN.png",plotNITROGEN)

  # Environment: Water Withdrawal to Availability Ratio
  title <- "k) Water Withdrawal to Availability Ratio"
  unit  <- ""
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "water stress and violations", ])
  bb    <- asRaster(b, countries2)

  plotWATER <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-1, 1), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value))," to ",round(max(b$.value))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotWATER.png",plotWATER)

  #Environment: Greenhouse Gases --- IS THIS INCORRECT?!
  title <- "l) Greenhouse Gas Emissions"
  unit  <- "ton CO2eq per ha (cumulative since 2000)"
  caption <- "Projection: Mollweide"

  b     <- repReg[, .(value = value[variable == "Emissions|GWP100AR6|Land|Cumulative"] * 1000 /
                        value[variable == "Resources|Land Cover"]), by = .(model, scenario, region, period)]
  #b   <- droplevels(repReg[variable == "Emissions|GWP100AR6|Land|Cumulative", ]) # Will become ISO level, eventually
  all <- merge(reg2iso, b)
  all <- merge(countries2, all)

  # title <- "l) Cumulative Greenhouse Gas Emissions"
  # unit  <- "ton CO2eq per capita"
  # caption <- "Projection: Mollweide"
  # b     <- repReg[, .(value = value[variable == "Emissions|GWP100AR6|Land|Cumulative"] /
  #                       value[variable == "Population"]), by = .(model, scenario, region, period)]
  # all <- merge(reg2iso, b)
  # all <- merge(pop, all, all.x = TRUE)

  plotGHG <- ggplot(all) +
    facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) + coord_sf(xlim = xlimMoll) +
    # geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
    #                  color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-50, 50), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE))," to ",round(max(b$value,na.rm = TRUE))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotGHG.png",plotGHG)

  # Environment: Global Surface Temp
  title <- "m) Global Surface Temp"
  unit  <- "deg C"
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "Global Surface Temperature (C)", ])
  bb    <- asRaster(b, countries2)

  plotTEMP <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-3, 3), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value),2)," to ",round(max(b$.value),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotTEMP.png",plotTEMP)

  # Cost: Bioeconomy
  title <- "n) Value of Bioeconomy Supply"
  unit  <- "US$05/capita/yr"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repReg[, .(value = value[variable == "Value|Bioeconomy Demand"] /
                        value[variable == "Population"]), by = .(model, scenario, region, period)]
  all <- merge(reg2iso, b)
  all <- merge(pop, all, all.x = TRUE)
  all <- cropAll(all)

  plotBIOECON <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) + # coord_sf(xlim = xlimMoll) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "RdYlGn"), na.value = "grey90", limits = c(-5, 5), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value))," to ",round(max(b$value))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotBIOECON.png",plotBIOECON)

  # Cost:Production cost agriculture per capita
  title <- "o) Production cost agriculture"
  unit  <- "US$05/capita/yr"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repReg[, .(value = value[variable == "Costs Without Incentives"] /
                        value[variable == "Population"]), by = .(model, scenario, region, period)]
  all <- merge(reg2iso, b)
  all <- merge(pop, all, all.x = TRUE)
  all <- cropAll(all)

  plotCOSTS <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) + # coord_sf(xlim = xlimMoll) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) + coord_sf(expand = FALSE) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(9, "RdYlGn")), na.value = "grey90", limits = c(-250, 250), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value))," to ",round(max(b$value))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 15, barheight = 0.4)) +
    geom_text(aes(label = sub("", "", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)
  ggsave("plotCOSTS.png",plotCOSTS)



  trytoplot <- function(tryplot) {
    if (!is.null(tryplot)) {
      if (inherits(try(ggplot_build(tryplot)), "try-error")) {
        warning(paste0(bquote(tryplot), " failed"))
        return(plotDUMMY)
      } else {
        return(tryplot)
      }
    } else {
      warning(paste0(bquote(tryplot), " is NULL"))
      return(plotDUMMY)
    }
  }

  group1 <- wrap_plots(trytoplot(plotUNDERWEIGHT), trytoplot(plotOBESE), trytoplot(plotYOLL), widths = 1, ncol = 1, heights = 1) +
    plot_annotation(title = "Health", theme = theme(title = element_text(face = "bold", size = 20), plot.margin = margin(10, 5, 5, 5, "pt"), plot.background = element_rect(colour = "black", fill = NA, linewidth = 2))) +
    plot_layout(guides = "keep")
  group2 <- wrap_plots(trytoplot(plotEXPENDITURE), trytoplot(plotPOVERTY), trytoplot(plotEMPLOYMENT), trytoplot(plotWAGE), widths = 1, ncol = 1, heights = 1) +
    plot_annotation(title = "Inclusion", theme = theme(title = element_text(face = "bold", size = 20), plot.margin = margin(10, 5, 5, 5, "pt"), plot.background = element_rect(colour = "black", fill = NA, linewidth = 2))) +
    plot_layout(guides = "keep")
  groupEmpty <- wrap_plots(plot_spacer(), widths = 1, ncol = 1, heights = 1) +
    plot_annotation(title = NULL, theme = theme(title = element_text(face = "bold"), plot.background = element_rect(colour = NA, fill = NA, linewidth = 0)))
  group3 <- wrap_plots(trytoplot(plotBII), trytoplot(plotCROPDIV), trytoplot(plotNITROGEN), trytoplot(plotWATER), trytoplot(plotGHG), trytoplot(plotTEMP), widths = 1, ncol = 1, heights = 1) +
    plot_annotation(title = "Environment", theme = theme(title = element_text(face = "bold", size = 20), plot.margin = margin(10, 5, 5, 5, "pt"), plot.background = element_rect(colour = "black", fill = NA, linewidth = 2))) +
    plot_layout(guides = "keep")
  group4 <- wrap_plots(trytoplot(plotBIOECON), trytoplot(plotCOSTS), widths = 1, ncol = 1, heights = 1) +
    plot_annotation(title = "Economy", theme = theme(title = element_text(face = "bold", size = 20), plot.background = element_rect(colour = "black", fill = NA, linewidth = 2), plot.margin = margin(10, 5, 5, 5, "pt"))) +
    plot_layout(guides = "keep")

  col1 <- wrap_plots(wrap_elements(group1), wrap_elements(group2), wrap_elements(groupEmpty), ncol = 1, nrow = 3, heights = c(0.375, 0.5, 0.125)) & theme(plot.margin = margin(0, 0, 10, 0, "pt"))
  col2 <- wrap_plots(wrap_elements(group3), wrap_elements(group4), ncol = 1, nrow = 2, heights = c(0.75, 0.25)) & theme(plot.margin = margin(0, 0, 10, 0, "pt"))
  combined <- wrap_plots(wrap_elements(col1), wrap_elements(col2))

  if (is.null(file)) {
    return(combined)
  } else {
    ggsave(filename = file, combined, width = unit(8, "cm"), height = unit(26.3, "cm"), scale = 1, bg = "white")
    ggsave(filename = paste0(substring(file, 1, nchar(file) - 3), "pdf"), combined, width = unit(8, "cm"), height = unit(26.3, "cm"), scale = 1, bg = "white")
  }
}
