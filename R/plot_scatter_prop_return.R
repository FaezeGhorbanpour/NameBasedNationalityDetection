plot_scatter_prop_return <- function(data_file){
  # library(arrow)
  # library(tidyverse)
  # data_file <- file.path("inputs","outmigration_by_country_gender_and_origin.parquet")
  outmigration_counts <- read_parquet(data_file)
  continent_mean <- outmigration_counts |>   
    summarise(
      across(c(return_academic:emig), sum),
      .by = c(from_continent, gender)
    )  |> 
    mutate(
      prop_return_academic = return_academic/emig,
      prop_return_name = return_name/emig,
    )
  
  scatter_mig_plot_theme <- function(gg) {
    list(
      coord_cartesian(xlim = c(0,.55)),
      scale_y_discrete(
        "",
        labels = function(x) ifelse(
          x == "North America", "North \nAmerica",
          str_wrap(x, width = 15)
        )
      ),
      scale_x_continuous("", breaks = seq(0,1,.1), expand = c(0,0)),
      scale_fill_brewer("Period", palette = "Greens"),
      scale_shape_manual("Gender", values=c(21,23)),
      scale_linetype_manual(values = c("solid","dotted")),
      scale_size(
        "Scholar's \nPopulation \n(thousands)", range = c(.5, 6),
        breaks = c(25, 50, 100, 200)
      ),
      guides(
        fill = guide_legend(
          override.aes = list(size=4, shape=21, alpha = .5),
          title.position="top"),
        size = guide_legend(
          override.aes = list(shape=21),
          title.position="top"),
        shape = guide_legend(
          title.position="top", title.hjust = 0.5,
          override.aes = list(size=5))
      )
    )
  } 
  
  g_academic <- outmigration_counts |>
    ggplot() +
    labs(title = "Proportion based on Academic Origin") +
    geom_point(
      aes(y = from_continent, x = prop_return_academic,
          size = exposure_thousands, shape = gender, fill = year_group),
      position = position_jitterdodge(
        jitter.width = 0.12,
        dodge.width = .7,
        seed = 42
      ),
      alpha = .5, color = "gray50"
    ) +
    geom_crossbar(
      aes(y = from_continent, x = prop_return_academic,
          xmin = prop_return_academic, xmax = prop_return_academic,
          linetype = gender, group=gender),
      show.legend = FALSE,
      data = continent_mean,
      linewidth = .5
    ) +
    scatter_mig_plot_theme() +
    theme(legend.position = "none")
  
  g_academic <-
    g_academic +
    annotate(
      geom="curve", x = .12, y = 5.25, xend = .19, yend = 6,
      arrow = arrow(length = unit(0.07, "inch")), linewidth = 0.5,
      color = "gray20", curvature = -0.4
    ) +
    annotate(
      "text", x = .12, 5.15, label = "Female mean", 
      size = 7, size.unit = "pt", fontface = "bold", hjust = 1
    ) + 
    annotate(
      geom="curve", x = .285, y = 6.3, xend = .225, yend = 6,
      arrow = arrow(length = unit(0.07, "inch")), linewidth = 0.5,
      color = "gray20", curvature = -0.4
    )  +
    annotate(
      "text", x = .285, y = 6.45, label = "Male mean", hjust = 0, 
      size = 7, size.unit = "pt", fontface = "bold"
    )
  
  g_name <- outmigration_counts |> 
    ggplot(aes(y = from_continent, x = prop_return_name)) +
    labs(title = "Proportion based on ML Name Origin") +
    geom_point(
      aes(size = exposure_thousands, shape = gender, fill = year_group),
      position = position_jitterdodge( 
        jitter.width = 0.12,
        dodge.width = .7,
        seed = 42
      ),
      alpha = .5, color = "gray50"
    ) +
    geom_crossbar(
      aes(xmin = prop_return_name, xmax = prop_return_name, 
          linetype = gender, group=gender),
      data = continent_mean,
      show.legend = FALSE
    ) + 
    scatter_mig_plot_theme()
  
  
  # Rotating a rectangle
  polygon_coords <- tibble(
    xpol = c(.44, .545, .525, .425),
    ypol = c(4.2, 4.5, 5.9, 5.6)
  )
  
  g_name <- g_name + 
    geom_polygon(
      data = polygon_coords, aes(x = xpol, y = ypol), 
      color = "darkred", fill = "transparent", linetype = "dashed", 
      linewidth = .75 
    ) + 
    annotate(
      geom="curve", x = .47, y = 3.7, xend = .485, yend = 4.25,
      arrow = arrow(length = unit(0.07, "inch")), linewidth = 0.5,
      color = "darkred", curvature = 0.2
    ) +
    annotate(
      "text", x = .45, y = 3.65, size = 7, vjust = 1, size.unit = "pt", 
      label = paste0(
        "Almost half of migration from ",
        "\nUnited States of America is a ",
        "\nreturn migration, considering ", 
        "\nthe ML name origin"
      )
    ) 
  
  scatter_plot <- grid_arrange_shared_legend(
    g_academic,
    g_name,
    nrow = 2, ncol = 1, position= "right"
  )
  
  ggsave(
    file.path("outputs","fig4_prop_return_by_continent.pdf"),
    scatter_plot, 
    width = 20, height = 16, units = "cm"
  )
}