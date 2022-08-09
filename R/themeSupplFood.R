themeSupplFood = function(base_size = 16) {
  theme_classic(base_size=base_size) +
  theme(axis.ticks.x=element_blank(),
        #legend.position = "none",
        axis.text.x=element_text(angle=90, size=base_size - 4, hjust = 1),
        strip.placement = "outside",                      # Place facet labels outside x axis labels.
        #  strip.background = element_rect(fill = "white"),  # Make facet label background white.
        axis.title.x  = element_blank(),
        strip.text.y.right = element_text(angle = 0),
        strip.text.x = element_text(size= 16),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
}
  