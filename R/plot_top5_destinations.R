plot_top5_destinations <- function(data_file, plot_path, from_country_code_iso3){
  data <- read_parquet(data_file)

  top_academic <- data |> 
    filter(type_origin == "Academic Origin", 
           from_country_iso3 == from_country_code_iso3) |> 
    ggplot(
      aes(x = year_group, y = prop, fill = to_country, stratum = to_country, 
          alluvium= to_country, label = to_country)
    ) +
    facet_grid(cols = vars(return)) +
    labs(title = "Academic Origin") +  
    stat_stratum(
      decreasing = FALSE,
      width = .9,
      color = "gray90",
      linewidth = .1,
    ) +
    geom_text(
      stat = "stratum", decreasing = FALSE, color = "white",lineheight = 0.85,
      size = 6, size.unit = "pt"
    ) + 
    scale_fill_manual(values = palettes$selected_countries) + 
    scale_y_continuous(
      "Proportion", 
      limits = c(0,.65), breaks = seq(0,.6,.2), expand = c(0,0)
    ) + 
    scale_x_discrete(NULL) + 
    theme(
      legend.position = "none",
      panel.spacing.x = unit(.5, "lines"),
      panel.spacing.y = unit(.5, "lines"),
      strip.background = element_rect(
        color="black", fill="white"
      ),
      strip.text = element_text(size = 7),
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      text = element_text(size = 7),
      plot.margin = margin(rep(2,4))
    )
  
  
  top_name <- data |> 
    filter(type_origin == "ML Name Origin", 
           from_country_iso3 == from_country_code_iso3) |> 
    ggplot(
      aes(x = year_group, y = prop, fill = to_country, stratum = to_country, 
          alluvium= to_country, label = to_country)
    ) +
    labs(title = "ML Name Origin")  +  
    facet_grid(cols = vars(return)) +
    stat_stratum(
      decreasing = FALSE, width = .9, color = "gray90", linewidth = .1,
    ) +
    geom_text(
      stat = "stratum", decreasing = FALSE, color = "white", lineheight = 0.85,
      size = 6, size.unit = "pt"
    ) + 
    scale_fill_manual(values = palettes$selected_countries) + 
    scale_y_continuous(
      "Proportion", 
      limits = c(0,.65), breaks = seq(0,.6,.2), expand = c(0,0)
    ) + 
    scale_x_discrete(NULL) + 
    theme(
      legend.position = "none",
      panel.spacing.x = unit(.5, "lines"),
      panel.spacing.y = unit(.5, "lines"),
      strip.background = element_rect(
        color="black", fill="white"
      ),
      strip.text = element_text(size = 7),
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      text = element_text(size = 7),
      plot.margin = margin(rep(2,4))
    )
  
  top_destinations_plot <- ggdraw() +
    draw_plot(top_academic, x = 0, y = 0.5, width = 1, height=.5) + 
    draw_plot(top_name, x = 0, y = 0, width = 1, height=.5) 
  
  ggsave(
    plot_path,
    top_destinations_plot,
    width = 17,height = 9, units = "cm",
  )
  
  plot_path
}


