plot_evaluation_measures <- function(level){
  class_map <- arrow::read_parquet(file.path("inputs","class_map.parquet"))
  if (level == 1){
    eval_level1 <- read.xlsx(
      file.path("inputs","model_evaluation.xlsx"), 
      sheet = "level1"
    )
    
    eval_level1 <- eval_level1 |> 
      pivot_longer(precision:`f1-score`) |> 
      mutate(value = as.numeric(value))
    
    order <- eval_level1 |> 
      filter(name == "f1-score") |> 
      arrange(value) |> 
      mutate(X1 = factor(X1))
    
    eval_level1 |> 
      mutate(X1 = factor(X1, levels = order$X1, ordered = TRUE)) |> 
      ggplot() + 
      geom_bar(
        aes(x=value, y=X1, fill=name), 
        stat = "identity", width = .75,
        position = position_dodge2(width = 2, padding = -0.3, reverse = TRUE)
      ) + 
      scale_x_continuous(
        NULL, limits = c(0,1), breaks = seq(0,1,.2), expand = c(0,0)
      ) + 
      scale_y_discrete(NULL, expand = c(0.045,0.01))+ 
      scale_fill_brewer(NULL, type = "qual") +
      theme(
        plot.margin = margin(2,8,3,3), 
        axis.text.x = element_text(size = 7.5),           
        legend.key.height = unit(.75, "lines"),    
        legend.key.width = unit(1., "lines"),  
        legend.text.position = "right",
        legend.text = element_text(size = 7, vjust = .5),
        legend.margin = margin(rep(2, 1)),
        legend.box.margin = margin(rep(2, 1)),
        legend.box.spacing = margin(rep(2, 1))
      )
    
    file_name <- file.path("outputs","fig6_evaluation_measures_level_1.pdf")
    
    ggsave(
      file_name,
      width = 7.5, height = 7, units = "cm"
    )   
  } else if (level == 2){
    eval_level2 <- read.xlsx(
      file.path("inputs", "model_evaluation.xlsx"), sheet = "level2"
    )
    
    eval_level2 <- eval_level2 |> 
      pivot_longer(precision:`f1-score`) |> 
      mutate(value = as.numeric(value))
    
    order <- eval_level2 |> 
      filter(name == "f1-score") |> 
      arrange(value) |> 
      mutate(level2 = factor(level2))
    
    eval_level2 |> 
      mutate(level2 = factor(level2, levels = order$level2, ordered = TRUE)) |> 
      ggplot() + 
      geom_bar(
        aes(x=value, y=level2, fill=name), stat = "identity", width = .75,
        position = position_dodge2(width = 2, padding = -0.3, reverse = TRUE)) + 
      scale_x_continuous(
        NULL, limits = c(0,1), breaks = seq(0,1,.2), expand = c(0,0)
      ) + 
      scale_y_discrete(NULL) + 
      scale_fill_brewer(NULL, type = "qual") +
      theme(
        plot.margin = margin(2,8,3,3), 
        axis.text.x = element_text(size = 7.5),           
        legend.key.height = unit(.75, "lines"),    
        legend.key.width = unit(1., "lines"),  
        legend.text.position = "right",
        legend.text = element_text(size = 7, vjust = .5),
        legend.margin = margin(rep(2, 1)),
        legend.box.margin = margin(rep(2, 1)),
        legend.box.spacing = margin(rep(2, 1))
      )
    
    file_name <- file.path("outputs","fig7_evaluation_measures_level_2.pdf")
    
    ggsave(
      file_name,
      width = 7.5, height = 12, units = "cm"
    ) 
  } else if (level == 3) {
    eval_level3 <- read.xlsx(
      file.path("inputs", "model_evaluation.xlsx"), sheet = "level3"
    )
    
    n_countries <- 10
    
    eval_level3 <- eval_level3 |> 
      filter(support >= 200) |> 
      select(-support) |> 
      pivot_longer(precision:`f1-score`) |> 
      mutate(value = as.numeric(value))
    
    order <- eval_level3 |> 
      filter(name == "f1-score") |> 
      arrange(value) |> 
      mutate(level3 = factor(level3))
    
    bottom_countries <- eval_level3 |> 
      filter(name == "f1-score") |> 
      arrange(value) |> 
      slice_head(n = n_countries) |> 
      pull(level3)
    
    top_countries <- eval_level3 |> 
      filter(name == "f1-score") |> 
      arrange(value) |> 
      slice_tail(n = n_countries) |> 
      pull(level3)
    
    top_countries <- eval_level3 |>  
      filter(level3 %in% top_countries) |>
      mutate(group = paste0("Top ",n_countries," Countries"))
    
    bottom_countries <- eval_level3 |>  
      filter(level3 %in% bottom_countries) |>
      mutate(group = paste0("Bottom ",n_countries," Countries"))
    
    eval_level3 <- rbind(top_countries, bottom_countries) |> 
      mutate(
        level3 = factor(level3, levels = order$level3, ordered = TRUE),
        group = factor(
          group, 
          levels = c(
            paste0("Top ",n_countries," Countries"),
            paste0("Bottom ",n_countries," Countries"))
        )
      )
    
    eval_level3 <- eval_level3 |> 
      left_join(class_map, by = join_by(level3==level3_region))
    
    filter(eval_level3,name=="f1-score") |>
      mutate(
        level3 = factor(level3, levels = order$level3, ordered = TRUE)
      ) |> 
      ggplot() + 
      geom_bar(
        aes(x=value, y=level3, fill=level1_region), 
        stat = "identity", 
        width = .8,
        position = position_dodge2(width = 2, padding = -0.3, reverse = TRUE)
      ) +
      scale_x_continuous(
        "F1-Score", 
        limits = c(0,1), breaks = seq(0,1,.20), expand = c(0,0)
      ) + 
      scale_y_discrete(NULL) + 
      facet_wrap(~group, nrow = 2, scales = "free_y") + 
      scale_fill_manual("First Level", values=palettes$level1) +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(color="transparent", fill="white"),
        panel.spacing = unit(.25, "lines"),
        plot.margin = margin(2,9, 0, 3), 
        axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8), 
        axis.title.x = element_text(size = 8), 
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7), 
        legend.margin = margin(rep(0,4)), 
        legend.box.margin = margin(rep(0,4)),    
        legend.key.height = unit(.75, "lines"),    
        legend.key.width = unit(.75, "lines"),  
        legend.text.position = "right",
        legend.title.position = "left",
        legend.background = element_rect(fill="transparent",color="transparent")
      ) +
      guides(
        fill = guide_legend(nrow = 2) # Arrange legend in 2 rows
      )
    
    file_name <- file.path("outputs","fig8_evaluation_measures_level_3.pdf")
    
    ggsave(
      file_name,
      width = 8.5, height = 10, units = "cm"
    ) 
  }
  
  file_name
}