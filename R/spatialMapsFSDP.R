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
#' @details blub
#' @return if file is NULL a ggplot2 object will be return
#' @author Florian Humpenoeder
#' @import ggplot2 data.table patchwork cartogram sf RColorBrewer rnaturalearth quitte
#' @importFrom utils write.csv read.csv
#' @importFrom terra project rast

spatialMapsFSDP <- function(repReg, repIso, repGrid, reg2iso, file = NULL) {

  ### projections
  # https://semba-blog.netlify.app/01/26/2020/world-map-and-map-projections/

  #### read in data files
  repReg <- convertReportFSDP(repReg, subset = TRUE)
  repIso <- convertReportFSDP(repIso, subset = TRUE)
  repGrid <- convertReportFSDP(repGrid, subset = TRUE)
  if (!is.data.frame(reg2iso)) reg2iso <- read.csv(reg2iso, header = TRUE, row.names = 1)

  # get country layer
  countries <- ne_countries(returnclass = "sf", scale = "small")
  countries <- subset(countries, iso_a3 %in% levels(repIso$iso_a3))
  countries <- countries[-grep("Antarctica", countries$name), ]

  ### calc pop polygon for cartogram maps
  pop <- repIso[variable == "Population", ]
  pop$unit <- NULL
  names(pop)[names(pop) == "value"] <- "pop"
  pop <- merge(countries, pop)
  calcPolygon <- function(pop) {
    z <- NULL
    for (i in levels(pop$scenario)) {
      x <- subset(pop, scenario == i)
      x <- st_transform(x, crs = 3857)
      x <- cartogram_cont(x, "pop", itermax = 7)
      z <- rbind(z, x)
    }
    z$variable <- NULL
    return(z)
  }
  pop <- calcPolygon(pop)

  # theme for maps
  myTheme <- theme_minimal() + theme(plot.margin = grid::unit(c(5, 0, 0, 0), "mm")) +
    theme(legend.justification = "left", legend.title = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, vjust = 0, size = 12, face = "bold"),
          strip.text = element_text(size = 12), panel.background = element_rect(fill = "black"),
          panel.grid = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank())

  ## regional data
  title <- "Costs: Production cost agriculture per capita"
  unit <- "USD/cap/yr"
  b <- repReg[, .(value = value[variable == "Costs"] /
                    value[variable == "Population"]), by = .(model, scenario, region, period)]
  all <- merge(reg2iso, b)
  all <- merge(pop, all)
  p_costs <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "Blues")[-1], na.value = "grey90", limits = c(0, 2000)) +
    myTheme + labs(title = title, caption = "Projection: Cartogram based on population. Country size indicates the number of people potentially affected.")


  title <- "Inclusion: Share of working age population employed in agriculture"
  b <- repReg[, .(value = value[variable == "Share of working age population employed in agriculture"]),
              by = .(model, scenario, region, period)]
  all <- merge(reg2iso, b)
  all <- merge(pop, all)
  p_employment_share <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn("Share", colors = brewer.pal(9, "Purples")[-1], na.value = "grey90", limits = c(0, 30)) +
    myTheme + labs(title = title, caption = "Projection: Cartogram based on population. Country size indicates the number of people potentially affected.")

  title <- "Inclusion: Hourly labor costs in agriculture"
  b <- repReg[, .(value = value[variable == "Hourly labor costs"]), by = .(model, scenario, region, period)]
  all <- merge(reg2iso, b)
  all <- merge(pop, all)
  p_wage <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn("USD/h", colors = brewer.pal(9, "Purples")[-c(1,3,4,6)], na.value = "grey90",
                         limits = c(0, 30), trans = "log1p", breaks=c(0, 1.5, 5, 13, 30)) +
    myTheme + labs(title = title, caption = "Projection: Cartogram based on population. Country size indicates the number of people potentially affected.")


  ## Country level data
  title <- "Health: Spatial distribution of population underweight"
  b <- repIso[, .(value = value[variable == "Nutrition|Anthropometrics|People underweight"] /
                    value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b)
  p_underweight <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn("Share", colors = brewer.pal(9, "PuBuGn")[-1], na.value = "grey90", limits = c(0, 0.4)) +
    myTheme + labs(title = title, caption = "Projection: Cartogram based on population. Country size indicates the number of people potentially affected.")


  title <- "Health: Spatial distribution of population obese"
  b <- repIso[, .(value = value[variable == "Nutrition|Anthropometrics|People obese"] /
                    value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b)
  p_obese <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn("Share", colors = brewer.pal(9, "PuBuGn")[-1], na.value = "grey90", limits = c(0, 0.4)) +
    myTheme + labs(title = title, caption = "Projection: Cartogram based on population. Country size indicates the number of people potentially affected.")

  title <- "Inclusion: Expenditure for agr. products per capita"
  b <- repIso[, .(value = value[variable == "Household Expenditure|Food|Expenditure"] /
                    value[variable == "Population"]), by = .(model, scenario, iso_a3, period)]
  all <- merge(pop, b)
  p_expenditure <- ggplot(all) + facet_wrap(vars(scenario), ncol = 3) +
    geom_sf(aes(fill = value), show.legend = TRUE, color = "white", size = 0.2) +
    geom_sf_text(aes(label = I(ifelse(iso_a3 %in% c("USA", "IND", "NGA", "BRA", "CHN"), iso_a3, "")),
                     color = I(ifelse(value < 0.1, "white", "white"))), size = 2) +
    scale_fill_gradientn("Share", colors = brewer.pal(9, "Reds")[-1], na.value = "grey90", limits = c(0, 0.4)) +
    myTheme + labs(title = title, caption = "Projection: Cartogram based on population. Country size indicates the number of people potentially affected.")


  ## Grid cell data
  countries2 <- st_transform(countries, crs = st_crs("+proj=moll"))
  xlimMoll <- c(-11007870, 16007870)
  asRaster <- function(x, countries2) {
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

  title <- "Environment: Biodiversity Intactness Index"
  unit <- "index"
  b <- droplevels(repGrid[variable == "BII (index)", ])
  bb <- asRaster(b, countries2)

  p_bii <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradient2(unit, low = "darkred", high = "darkgreen", mid = "yellow",
                         midpoint = 0.76, na.value = "grey90") + myTheme +
    labs(title = title, caption = "Projection: Mollweide")

  title <- "Environment: Nutrient Surplus"
  unit <- "kg N per ha"
  b <- droplevels(repGrid[variable == "nutrientSurplus (kg N per ha)", ])
  bb <- asRaster(b, countries2)

  p_nitrogen <- ggplot(bb) +
    geom_raster(aes(x = x, y = y, fill = value)) + facet_wrap(~scenario) +
    geom_sf(data = countries2, color = "white", fill = NA, size = 0.2) + coord_sf(xlim = xlimMoll) +
    scale_fill_gradientn(unit, colors = brewer.pal(9, "RdPu")[-1], na.value = "grey90", limits = c(0, 400)) + myTheme +
    labs(title = title, caption = "Projection: Mollweide")

  trytoplot = function(tryplot){
    if (inherits(try(ggplot_build(tryplot)), "try-error")){
      warning("One of the map plot scripts failed")
      return(NULL)
    } else {
      return(tryplot)
    }
  }

  combined <- (trytoplot(p_costs)
               + trytoplot(p_employment_share)
               + trytoplot(p_wage)
               + trytoplot(p_expenditure)
               + trytoplot(p_underweight)
               + trytoplot(p_obese)
               + trytoplot(p_bii)
               + trytoplot(p_nitrogen)
               )
  combined <- combined + plot_layout(guides = "keep", ncol = 2,byrow = FALSE)

  if (is.null(file)) {
    return(combined)
  } else {
    ggsave(filename = file, combined, width = 12, height = 7, scale = 1.5, bg = "white")
  }
}
