theme_custom <- function(base_size=16, base_family="", legend_title=16, plot_title=18, plot_caption=8){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
  ) %+replace%
    theme(axis.ticks = element_line(color = "grey92"),
          axis.ticks.length = unit(.5, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_text(size=legend_title),
          legend.text = element_text(color = "grey30"),
          plot.title = element_text(size = plot_title, face = "bold", hjust=0),
          plot.subtitle = element_text(size = base_size, color = "grey30"),
          plot.caption = element_text(size = plot_caption, margin = margin(t = 15)),
          strip.text.x = element_text(margin = margin(0.5,0,0.5,0, "cm")),
          strip.text=element_text(size=16),
          plot.background = element_rect(
            fill = "white",
            colour = "white")
    )
}
