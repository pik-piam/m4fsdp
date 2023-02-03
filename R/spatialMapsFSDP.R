globalVariables(c("model", "scenario", "region", "period", "unit", "variable", "varunit", "valuefill",
                  "value", "label", "vargroup", ".", ".label", ".value", "iso_a3", "x", "y"))
#' @title spatialMapsFSDP
#' @description creates a spatial mapsfor FSDP MAgPIE runs
#'
#' @export
#'
#' @param repReg reporting .rds file or data.frame with regional results (produced by FDSP_collect.R output script)
#' @param repIso reporting .rds file or data.frame with country level results (produced by FDSP_collect.R output script)
#' @param repGrid reporting .rds file or data.frame with grid level results (produced by FDSP_collect.R output script)
#' @param reg2iso mapping file or data.frame with regions and countries (produced by FDSP_collect.R output script)
#' @param file file name (e.g. FSDP_spatialmaps.pdf or FSDP_spatialmaps.pdf) or NULL
#' @param recalcPolygons recalculate polygons for cartogram projections (FALSE / TRUE)
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author Florian Humpenoeder
#' @import ggplot2 data.table patchwork cartogram sf RColorBrewer rnaturalearth quitte
#' @importFrom utils write.csv read.csv
#' @importFrom terra project rast

spatialMapsFSDP <- function(repReg, repIso, repGrid, reg2iso, file = NULL, recalcPolygons = FALSE) {
  ### projections
  # https://semba-blog.netlify.app/01/26/2020/world-map-and-map-projections/

  #### read in data files
  repReg  <- convertReportFSDP(repReg, subset = TRUE)
  repIso  <- convertReportFSDP(repIso, subset = TRUE)
  repGrid <- convertReportFSDP(repGrid, subset = TRUE)
  if (!is.data.frame(reg2iso)) {
    reg2iso <- read.csv(reg2iso, header = TRUE, row.names = 1)
  }

  # get country layer
  countries <- ne_countries(returnclass = "sf", scale = "small")
  #countries <- subset(countries, iso_a3 %in% levels(repIso$iso_a3))
  countries <- countries[-grep("Antarctica", countries$name), ]
  countries <- countries[-grep("Fr. S. Antarctic Lands", countries$name), ]

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

  if (recalcPolygons) {
    ### calc pop polygon for cartogram maps
    pop <- repIso[variable == "Population", ]
    pop$unit <- NULL
    names(pop)[names(pop) == "value"] <- "pop"
    pop <- merge(countries[, c("iso_a3", "geometry")], pop)
    pop <- calcPolygon(pop, "pop")

    # calc ag. empl. polygon for cartogram maps
    agEmpl <- repIso[variable %in% c("Agricultural employment|Crop and livestock products", # old variable name
                                     "Labor|Employment|Agricultural employment"), ] # new variable name
    agEmpl$unit <- NULL
    names(agEmpl)[names(agEmpl) == "value"] <- "agEmpl"
    agEmpl <- merge(countries[, c("iso_a3", "geometry")], agEmpl)
    agEmpl <- calcPolygon(agEmpl, "agEmpl")
  } else {
    pop <- readRDS(system.file(package = "m4fsdp", "extdata", "pop.rds"))
    agEmpl <- readRDS(system.file(package = "m4fsdp", "extdata", "agEmpl.rds"))
  }

  # theme for maps
  myTheme <- theme_minimal(base_size = 19) +
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
          plot.title = element_text(hjust = 0, margin = unit(c(0, 0, 0, 0), "pt"), size = 19, face = "bold"),
          legend.title = element_text(size = 19, hjust = 1, margin = unit(c(0, 0, 0, 0), "pt")),
          plot.caption = element_text(hjust = 1, margin = unit(c(0, 0, 0, 0), "pt")))

  ## regional/country data
  labelX <- -19500000
  labelY <- -5400000

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
  all   <- merge(pop, b)
  plotDUMMY <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "PuBuGn")[-1], na.value = "grey90", limits = c(0, 0.4), oob = scales::squish) +
    myTheme + labs(title = title, caption = caption) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)


  # Health: Spatial distribution of population underweight
  title <- "a) Underweight"
  unit <- "Population share per country"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = value[variable == "Nutrition|Anthropometrics|People underweight"] /
                    value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all   <- merge(pop, b)
  plotUNDERWEIGHT <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "BuPu")[-1], na.value = "grey90", limits = c(0, 0.2), oob = scales::squish, breaks = c(0,0.05,0.1,0.15,0.2), labels = c(0,0.05,0.1,0.15,">0.2")) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)


  # Health: Spatial distribution of population obese
  title <- "b) Obesity"
  unit <- "Population share per country"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = value[variable == "Nutrition|Anthropometrics|People obese"] /
                    value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all     <- merge(pop, b)
  plotOBESE <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "PuBu")[-1], na.value = "grey90", limits = c(0, 0.2), oob = scales::squish, breaks = c(0,0.05,0.1,0.15,0.2), labels = c(0,0.05,0.1,0.15,">0.2")) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Years of lost life
  title <- "c) Years of life lost"
  unit <- "Years per person"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = (value[variable == "Health|Years of life lost|Risk|Diet and anthropometrics"]) /
                        value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all     <- merge(pop, b)
  plotYOLL <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "YlOrBr")[-1], na.value = "grey90", limits = c(0, 0.10), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Inclusion: Expenditure for agr. products per capita
  title <- "d) Expenditure for agricultural products"
  unit <- "USD per capita"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = value[variable == "Household Expenditure|Food|Expenditure"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b)
  plotEXPENDITURE <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "YlOrRd")[-1], na.value = "grey90", limits = c(0, 1000), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE))," to ",round(max(b$value,na.rm = TRUE))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Inclusion: Share of Population with Incomes less than 3.20$/Day
  title <- "e) Income below 3.20$ per day"
  unit <- "Population share per country"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repIso[, .(value = value[variable == "Income|Number of People Below 3p20 USDppp11/day"] /
                    value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b)
  plotPOVERTY <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    # scale_fill_manual(values = c("#FFFFFF", "#fee8c8", "#fdbb84", "#d7301f", "#7f0000", "#54278f"),
    #                  breaks = seq(0, 0.4, by = 0.1)) +
    scale_fill_gradientn(unit, colors = rev(brewer.pal(11, "RdYlGn")[-1]), na.value = "grey90", limits = c(0, 1), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX + 1000000, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)


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
  #   guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
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
  all   <- merge(pop, all)
  plotEMPLOYMENT <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "Purples")[-1], na.value = "grey90", limits = c(0, 0.3), oob = scales::squish, breaks = c(0,0.1,0.2,0.3)) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE),2)," to ",round(max(b$value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)


  # Inclusion: Hourly labor costs in agriculture
  title <- "g) Agricultural wages"
  unit <- "USD per hour"
  caption <- "Cartogram projections with areas proportional to agricultural employment"
  b     <- repReg[, .(value = value[variable %in% c("Hourly labor costs", "Labor|Wages|Hourly labor costs")]),
                    by = .(model, scenario, region, period)]
  all   <- merge(reg2iso, b)
  all   <- merge(agEmpl, all)
  plotWAGE <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "Purples")[-c(1, 3, 4, 6)], na.value = "grey90",
                         limits = c(0, 30), oob = scales::squish, trans = "log1p", breaks = c(0, 1.5, 5, 13, 30), labels = c(0, 1.5, 5, 13, ">30")) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value,na.rm = TRUE))," to ",round(max(b$value,na.rm = TRUE))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)


  # Environment: Biodiversity Intactness Index
  title <- "h) Biodiversity Intactness Index"
  unit  <- "index"
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "BII (index)", ])
  bb    <- asRaster(b, countries2)

  plotBII <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "RdYlGn"), na.value = "grey90", oob = scales::squish, limits = c(0.4,1)) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value,na.rm = TRUE),2)," to ",round(max(b$.value,na.rm = TRUE),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Environment: Croparea diversity
  title <- "i) Shannon crop diversity"
  unit  <- "index"
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "Shannon crop diversity (index)", ])
  bb    <- asRaster(b, countries2)

  plotCROPDIV <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "YlGn")[-1], na.value = "grey90") +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value),2)," to ",round(max(b$.value),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Environment: Nutrient Surplus
  title <- "j) Nutrient Surplus"
  unit  <- "kg N per ha"
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "nutrientSurplus (kg N per ha)", ])
  bb    <- asRaster(b, countries2)

  plotNITROGEN <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "RdPu")[-1], na.value = "grey90", limits = c(0, 100), oob = scales::squish, breaks = c(0,25,50,75,100), labels = c(0,25,50,75,">100")) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value))," to ",round(max(b$.value))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Environment: Water Withdrawal to Availability Ratio
  title <- "k) Water Withdrawal to Availability Ratio"
  unit  <- ""
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "water stress and violations", ])
  bb    <- asRaster(b, countries2)

  plotWATER <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = cut(value,
                                             breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 100)))) +
    facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "grey90", fill = NA, size = 0.2) +
    coord_sf(xlim = xlimMoll) +
    scale_fill_manual(unit,
                      values = c("#ffffd4", "#fee391", "#fdbb84", "#d7301f", "#7f0000", "#54278f"),
                      label = c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1", "EFR violation"),na.value = "grey50") +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value))," to ",round(max(b$.value))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 1, nrow = 1)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Environment: Greenhouse Gases --- IS THIS INCORRECT?!
  title <- "l) Greenhouse Gas Emissions"
  unit  <- "Gt CO2eq"
  caption <- "Projection: Mollweide"
  b   <- droplevels(repReg[variable == "Emissions|GWP100AR6|Land|Cumulative", ]) # Will become ISO level, eventually
  all <- merge(reg2iso, b)
  all <- merge(countries2, all)

  plotGHG <- ggplot(all) +
    facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) + coord_sf(xlim = xlimMoll) +
    # geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
    #                  color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradient2(unit, low = "darkgreen", high = "darkred", mid = "white", midpoint = 0, na.value = "grey90") +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value))," to ",round(max(b$.value))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Environment: Global Surface Temp
  title <- "m) Global Surface Temp"
  unit  <- "deg C"
  caption <- "Projection: Mollweide"
  b     <- droplevels(repGrid[variable == "Global Surface Temperature (C)", ])
  bb    <- asRaster(b, countries2)

  plotTEMP <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradient2(unit, low = "darkgreen", high = "darkred", mid = "yellow", midpoint = 0, na.value = "grey90", limits = c(-1, 4), oob = scales::squish, breaks = c(-1,0,1,2,3,4), labels = c(-1,0,1,2,3,">4")) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$.value),2)," to ",round(max(b$.value),2)),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = scenario), x = labelXGrid, y = labelYGrid,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Cost: Bioeconomy
  title <- "n) Value of Bioeconomy Demand"
  unit  <- "US$05/capita/yr"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repReg[, .(value = value[variable == "Value|Bioeconomy Demand"] /
                        value[variable == "Population"]), by = .(model, scenario, region, period)]
  all <- merge(reg2iso, b)
  all <- merge(pop, all)

  plotBIOECON <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) + # coord_sf(xlim = xlimMoll) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "YlGnBu")[-1], na.value = "grey90", limits = c(0, 500), oob = scales::squish, breaks = c(0,100,200,300,400,500), labels = c(0,100,200,300,400,">500")) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value))," to ",round(max(b$value))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)

  # Cost:Production cost agriculture per capita
  title <- "o) Production cost agriculture"
  unit  <- "US$05/capita/yr"
  caption <- "Cartogram projections with areas proportional to population"
  b     <- repReg[, .(value = value[variable == "Costs"] /
                        value[variable == "Population"]), by = .(model, scenario, region, period)]
  all <- merge(reg2iso, b)
  all <- merge(pop, all)

  plotCOSTS <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) + # coord_sf(xlim = xlimMoll) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "YlOrBr")[-1], na.value = "grey90", limits = c(0, 2000), oob = scales::squish) +
    myTheme +
    labs(title = title, caption = c(paste0("Data range: ",round(min(b$value))," to ",round(max(b$value))),caption)) + theme(plot.caption = element_text(hjust=c(0, 1))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 1, barwidth = 44, barheight = 0.4)) +
    geom_text(aes(label = sub(" ", "\n", scenario)), x = labelX, y = labelY,
              hjust = 0, vjust = 0, color = "white", size = 18 / .pt, lineheight = 0.7)



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
    ggsave(filename = file, combined, width = unit(20, "cm"), height = unit(26.3, "cm"), scale = 1, bg = "white")
    ggsave(filename = paste0(substring(file, 1, nchar(file) - 3), "pdf"), combined, width = unit(20, "cm"), height = unit(26.3, "cm"), scale = 1, bg = "white")
  }
}
