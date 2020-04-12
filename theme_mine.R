theme_mine <- function (base_size = 12, base_family = "") {
  require(grid)
  theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
        rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
        text = element_text(family = base_family,face = "plain", colour = "green",
                            size = base_size, hjust = 0.5, angle = 0, lineheight = 0.1),
        axis.text = element_text(size = rel(0.8)),
        strip.text = element_text(size = rel(0.8)),
        axis.line = element_blank(),
        axis.text.x = element_text(vjust = 1,colour = "black"),
        axis.text.y = element_text(hjust = 1,colour="black"),
        axis.ticks = element_line(colour = "grey50"),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(angle = 90,colour = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks.margin = unit(0.1, "cm"),
        
        legend.background = element_rect(colour = NA),
        legend.margin = unit(0.2, "cm"),
        legend.key = element_rect(colour = "grey80"),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.8)),
        legend.text.align = NULL,
        legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
        legend.title.align = NULL,
        legend.position = "right",
        legend.direction = NULL,
        legend.justification = "center",
        legend.box = NULL,
  #grey90      
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA, colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = "grey98", size = 0.5),
        panel.spacing = unit(0.25, "lines"),
        
        strip.background = element_rect(fill = "grey80", colour = "grey50"),
        strip.text.x = element_text(),
        strip.text.y = element_text(angle = -90),
        
        plot.background = element_rect(colour = "white"),
        plot.title = element_text(size = rel(1.2)),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"), complete = TRUE)
}






