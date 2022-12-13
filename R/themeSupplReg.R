#' @title themeSupplFood
#' @description creates supplementary themes for FSDP MAgPIE runs
#'
#' @export
#'
#' @param base_size plot settings
#' @param base_family plot settings
#' @param rotate_x plot settings
#' @param panel.spacing plot settings


themeSupplReg <- function(base_size = 11, base_family = "",rotate_x=FALSE, panel.spacing=3) {
  txt <- element_text(size = base_size, colour = "black", face = "plain")
  bold_txt <- element_text(size = base_size, colour = "black", face = "bold")

  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_blank(),
      strip.background = element_rect(color="black",fill="grey95"),
      axis.text.x = if(rotate_x) element_text(angle = 90, hjust = 1, vjust = 0.5) else element_text(angle = 0),
      axis.title.x = element_text(vjust=0),
      axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 5)),
      #panel.grid.minor.x = element_blank(),
      #panel.grid.major.x = element_blank(),

      panel.spacing.x = unit(panel.spacing, "mm"),
      panel.spacing.y = unit(panel.spacing, "mm"),

      text = txt,
      plot.title = bold_txt,

      axis.title = bold_txt,
      axis.text = txt,

#      legend.title = bold_txt,
#      legend.text = txt,
      legend.text = element_text(margin = margin(r = 10), hjust = 0,size = base_size, colour = "black", face = "plain"),
      legend.title = element_text(margin = margin(r = 10), hjust = 0,size = base_size, colour = "black", face = "bold")
      )
  # +
  #    theme(legend.position="bottom",legend.box = "vertical",legend.title.align = 0)
}
