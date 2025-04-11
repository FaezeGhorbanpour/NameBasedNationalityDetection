plot_prop_return_bilateral <- function(data){
  ggplot(data) + 
    geom_line(
      aes(x=year_group, y = value, group = name, color= name), 
      linewidth = 1
    ) + 
    geom_point(
      aes(x=year_group, y = value, shape = name, color= name),
      size = 2
    ) +
    scale_y_continuous("Proportion",limits = c(0,1), breaks = seq(0,1,.2)) + 
    scale_x_discrete(NULL, expand = c(.1,.1)) + 
    scale_color_brewer(NULL, type = "qual") + 
    scale_shape(NULL) + 
    theme(legend.position = "bottom")
  
  ggsave(
    file.path("outputs","prop_return_us_to_china.pdf"),
    width = 8.5, height = 6, units = "cm"
  )
}

