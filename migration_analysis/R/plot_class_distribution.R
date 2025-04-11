plot_class_distribution <- function(
    data_path, plot_path, 
    th_filter = .01, title.size = 9, label.size = 7.5,
    width = 10, height = 13
  )
  {
  # Uncomment to debug
  # targets::tar_load_globals()
  # lapply(c("tidyverse","arrow", "cowplot"), library, character.only= T)
  # data_path <- file.path("inputs","wikipedia_distribution_train_dataset.parquet")
  # plot_path <- file.path("outputs","fig1_class_distribution_wikidata.pdf")
  # th_filter = .015
  # title.size = 9
  # width = 10
  # height = 13
  data <- read_parquet(data_path) |>
    mutate(fill_color = palettes$level1[level1_region])
  
  class1_plot <- data |>
    summarise(count = sum(Count), .by = c(level1_region, fill_color)) |>
    mutate(
      level1_region = factor(
        level1_region, 
        levels = level1_region[order(- count)]
      ),
      perc = count / sum(count),
      perc_label = paste0(
        level1_region, " ", sprintf("%2.0f", perc * 100), "%"
      )
    )
  
  level1_order <- levels(class1_plot$level1_region)
  
  class1_plot <- class1_plot |>
    arrange(desc(level1_region)) |>
    mutate(ylabel = (cumsum(count)-count)+count/2)
  
  class2_plot <- data |>
    summarise(
      count = sum(Count), .by = c(level1_region, level2_region, fill_color)
    ) |>
    mutate(
      level1_region = factor(level1_region, levels = level1_order),
      level2_region = case_when(
        level2_region=="Russian" & level1_region == "Muslim" ~ "Russian (Muslin)",
        level2_region=="EastAfrica" & level1_region == "Muslim" ~ "EastAfrica (Muslin)",
        level2_region=="SouthernAfrica" & level1_region == "Hispanic" ~ "SouthernAfrica (Hispanic)",
        TRUE ~ level2_region
      ),
      level2_region = factor(
        level2_region, 
        levels = unique(level2_region[order(level1_region, -count)])
      ),
      perc = count / sum(count),
      perc_label = paste0(
        level2_region, " ", sprintf("%2.0f", perc * 100), "%"
      ),
      perc_label = factor(
        perc_label, 
        levels = unique(perc_label[order(level1_region, count)])
      )
    )
  
  class2_plot <- class2_plot |>
    arrange(desc(level2_region)) |>
    mutate(ylabel = (cumsum(count)-count)+count/2)
  
  level2_order <- levels(class2_plot$level2_region)
  
  class3_plot <- data |>
    summarise(
      count = sum(Count), .by = c(level1_region, level2_region, level3_region, fill_color)
    ) |>
    mutate(
      level1_region = factor(level1_region, levels = level1_order),
      level2_region = case_when(
        level2_region=="Russian" & level1_region == "Muslim" ~ "Russian (Muslin)",
        level2_region=="EastAfrica" & level1_region == "Muslim" ~ "EastAfrica (Muslin)",
        level2_region=="SouthernAfrica" & level1_region == "Hispanic" ~ "SouthernAfrica (Hispanic)",
        TRUE ~ level2_region
      ),
      level2_region = factor(
        level2_region, 
        levels = level2_order
      )
    )
  
  country_order <- class3_plot |>
    arrange(level1_region, level2_region, -count) |>
    pull(level3_region) 
  
  class3_plot <- class3_plot |>
    mutate(
      level3_region = factor(level3_region, levels = country_order),
      perc = count / sum(count),
      perc_label = paste0(
        level3_region, " ", sprintf("%2.0f", perc * 100), "%"
      ),
      perc_label = factor(
        perc_label, 
        levels = unique(perc_label[order(level1_region, count)])
      )
    )
  
  class3_plot <- class3_plot |>
    arrange(desc(level3_region)) |>
    mutate(ylabel = (cumsum(count)-count)+count/2)
  
  fill_label <- alpha(c("white"),0.75)
  cols <- set_names(class1_plot$fill_color, class1_plot$level1_region)
  
  
  barplot_level1 <- 
    ggplot(
      class1_plot, 
      aes(x = "Level 1", y = count, fill = level1_region, order = level1_region)
    ) + 
    geom_bar(stat = "identity", width = 1, color = "gray80") + 
    scale_fill_manual(values = cols) + 
    theme_void() + 
    coord_cartesian(expand = FALSE) + 
    theme(legend.position = "none")
  
  
  label_level1 <-
    ggplot(
      class1_plot, 
      aes(x = "Level 1", y = count, order = level1_region)
    ) + 
    geom_bar(
      stat = "identity", width = 1, fill = "transparent", color = "transparent"
    ) + 
    geom_label(
      data = filter(class1_plot, perc >= th_filter), 
      aes(x = "Level 1",  y = ylabel, label = perc_label),
      size = label.size, fontface = "bold", size.unit = "pt",
      fill = fill_label, label.size = 0
    ) + 
    theme_void() + 
    coord_cartesian(expand = FALSE) + 
    theme(legend.position = "none")
  
  barplot_level1_border <- 
    ggplot(
      class1_plot, 
      aes(x = "Level 1", y = count, order = level1_region)
    ) + 
    geom_bar(
      stat = "identity", width = 1, fill = "transparent", linewidth = 1.2,
      color = "white") + 
    theme_void() + 
    coord_cartesian(expand = FALSE) + 
    theme(legend.position = "none")
  
  
  cols <- set_names(class2_plot$fill_color, class2_plot$level2_region)
  
  barplot_level2 <- 
    ggplot(
      class2_plot, 
      aes(x = "Level 2", y = count, fill = level2_region, order = level2_region)
    ) + 
    geom_bar(stat = "identity", width = 1, color = "gray80") + 
    scale_fill_manual(values = cols) + 
    theme_void() + 
    coord_cartesian(expand = FALSE) + 
    theme(legend.position = "none")
  
  label_level2 <-
    ggplot(
      class2_plot, 
      aes(x = "Level 2", y = count, fill = level2_region, order = level2_region)
    ) + 
    geom_bar(
      stat = "identity", width = 1, fill = "transparent", color = "transparent"
    ) + 
    geom_label(
      data = filter(class2_plot, perc >= th_filter), 
      aes(x = "Level 2",  y = ylabel, label = perc_label),
      size = label.size, fontface = "bold", size.unit = "pt", 
      fill = fill_label, label.size = 0
    ) + 
    theme_void() + 
    coord_cartesian(expand = FALSE) + 
    theme(legend.position = "none")
  
  cols <- set_names(class3_plot$fill_color, class3_plot$level3_region)
  
  barplot_level3 <- 
    ggplot(
      class3_plot, 
      aes(x = "Level 3", y = count, fill = level3_region, order = level3_region)
    ) + 
    geom_bar(stat = "identity", width = 1, color = "gray80") + 
    scale_fill_manual(values = cols) + 
    theme_void() + 
    coord_cartesian(expand = FALSE) + 
    theme(legend.position = "none")
  
  label_level3 <-
    ggplot(
      class3_plot, 
      aes(x = "Level 3", y = count, fill = level3_region, order = level3_region)
    ) + 
    geom_bar(
      stat = "identity", width = 1, fill = "transparent", color = "transparent"
    ) + 
    geom_label(
      data = filter(class3_plot, perc >= th_filter), 
      aes(x = "Level 3",  y = ylabel, label = perc_label),
      size = label.size, fontface = "bold", size.unit = "pt", 
      fill = fill_label, label.size = 0
    ) + 
    theme_void() + 
    coord_cartesian(expand = FALSE) + 
    theme(legend.position = "none")
  
  barplot_titles <- ggplot(tibble(level = c("Level 1", "Level 2", "Level 3"))) + 
    geom_text(
      aes(label = level, y = 0, x = level),
      size = title.size, fontface = "bold", size.unit = "pt",
    ) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme_void()
  
  final_plot <- ggdraw() + 
    draw_plot(barplot_level1, x = 0.005, y = 0, width = .33, height=.98) + 
    draw_plot(barplot_level2, x = 0.335, y = 0, width = .33, height=.98) + 
    draw_plot(barplot_level3, x = 0.665, y = 0, width = .33, height=.98) + 
    draw_plot(barplot_level1_border, x= 0, y = 0, width = 1, height = .98) +
    draw_plot(barplot_titles, x= 0, y = 0.98, width = 1, height = .02) +
    draw_plot(label_level1, x = 0.005, y = 0, width = .33, height=.98) + 
    draw_plot(label_level2, x = 0.335, y = 0, width = .33, height=.98) + 
    draw_plot(label_level3, x = 0.665, y = 0, width = .33, height=.98) 
  
  ggsave(
    plot_path, 
    final_plot,
    width = width, height = height, units = "cm"
  )
  
  plot_path
}