plot_confusion_matrix <- function(confusion_matrix_file){
  confusion_matrix <- read_parquet(confusion_matrix_file)
  
  confusion_matrix_lv1 <- confusion_matrix |> 
    summarise(n = sum(n), .by = c(level_1,level_1_pred)) |> 
    mutate(perc = n/sum(n), .by = level_1)
  
  level_1_map <- list(
    "SouthAsian"    ~ "SA",
    "Slavic"        ~ "SL",
    "Romance"       ~ "RO",
    "Nordic-Baltic" ~ "NB",
    "Muslim"        ~ "MU",
    "Jewish"        ~ "JE",
    "Hispanic"      ~ "HI",
    "Greek"         ~ "GR",
    "German"        ~ "GE",
    "English"       ~ "EN",
    "EastAsian"     ~ "EA",
    "African"       ~ "AF"
  )
  
  confusion_matrix_lv1 <- confusion_matrix_lv1 |> 
    mutate(
      level_1_label = case_match(level_1, !!!level_1_map), 
      level_1_pred_label = case_match(level_1_pred, !!!level_1_map)
    )
  
  level_1_order <- confusion_matrix_lv1 |>
    filter(level_1 == level_1_pred) |> 
    arrange(desc(perc)) |> 
    pull(level_1_label)
  
  confusion_matrix_lv1 <- confusion_matrix_lv1 |> 
    mutate(
      level_1_label = factor(level_1_label, levels = rev(level_1_order)),
      level_1_pred_label = factor(level_1_pred_label, levels = level_1_order),
      perc_cat = cut(
        perc,
        c(0, .001, .01, seq(.1,.7,.4), .8, 1), 
        include.lowest = TRUE
      ),
      perc_label = paste0(sprintf("%2.0f", perc * 100))
    )
  
  # Custom breaks and labels
  breaks <- c(0,.001, .01, .5, .9, 1)
  labels <- c(
    "< 0.1%", "0.1 to 1%", "1 to 10%", "10 to 50%","50% to 80%","> 80%"
  )
  
  confusion_matrix_lv1_plot <- 
    ggplot(confusion_matrix_lv1,aes(x=level_1_pred_label,y=level_1_label)) +   
    scale_fill_manual(
      NULL,
      values = c(
        "white", "gray95", 
        brewer.pal(4, "Blues")[c(2,4)], 
        brewer.pal(3, "Reds")[2:3]
      )
    ) + 
    scale_x_discrete(expand = c(0,0), position = "top") + 
    scale_y_discrete(expand = c(0,0)) + 
    geom_tile(aes(fill = perc_cat))  +  
    geom_text(aes(label = perc_label), size = 10, size.unit = "pt") + 
    geom_tile(
      data =  filter(confusion_matrix_lv1, level_1_label==level_1_pred_label),
      fill = "transparent", color = "#22B473", linewidth = 1
    ) + 
    theme_void() + 
    theme(
      plot.margin = margin(2,2,2,2), 
      legend.position = "bottom",
      axis.text.x = element_text(
        angle = 90, size = 12, 
        vjust = 0.5, hjust = 0, margin = margin(0, 0, 4, 0)
      ),
      axis.text.y = element_text(
        size = 12, vjust = .5, hjust = 1, 
        margin = margin(0, 4, 0, 0)
      ),
      panel.border = element_rect(fill= "transparent", color = "gray25")
    ) +
    theme(legend.position = "none") 
  
  ## |- CM Level 2 ---------------------------------------------------------------
  level_2_map <- list(
    # African
    "EastAfrica" ~ "EAF", "SouthernAfrica" ~ "SAF", "WestAfrica" ~ "WAF",
    # EastAsian
    "Chinese" ~ "CHI", "Indochina" ~ "INC", "Japanese" ~ "JPN", "Korean" ~ "KOR",
    "Malay" ~ "MAL",
    # English
    "English" ~ "ENG",
    # German
    "Dutch" ~ "DUT", "German" ~ "GER",
    # Greek
    "Greek" ~ "GRE",
    # Hispanic
    "Portuguese" ~ "POR", "Spanish" ~ "SPA", 
    # Jewish
    "Jewish" ~ "JEW",
    # Muslin
    "Arab" ~ "ARB", "NorthAfrica" ~ "NAF", "Pakistanis" ~ "PAK",
    "Persian" ~ "PER", "Russian" ~ "RUS", "Turk" ~ "TUR",
    # Nordic-Baltic
    "Baltic" ~ "BAL", "Scandinavian" ~ "SCA",
    # Romance
    "French" ~ "FRE", "Italian" ~ "ITA",
    # Slavic
    "EastEuropean" ~ "EUE", "SouthSlavs" ~ "SSL",
    # South Asian
    "Indian" ~ "IND", "Oceania" ~ "OCE"
  )
  
  confusion_matrix_lv2 <- confusion_matrix |> 
    mutate(
      level_2_label = case_match(level_2, !!!level_2_map), 
      level_2_pred_label = case_match(level_2_pred, !!!level_2_map),
      level_2_label = case_when(
        level_1 == "African" &  level_2_label == "EAF" ~ "AEA", 
        level_1 == "Muslim" &  level_2_label == "EAF" ~ "MEA", 
        level_1 == "Muslim" &  level_2_label == "RUS" ~ "MRU", 
        level_1 == "Slavic" &  level_2_label == "RUS" ~ "SRU", 
        level_1 == "African" &  level_2_label == "SAF" ~ "ASA", 
        level_1 == "Hispanic" &  level_2_label == "SAF" ~ "HSA", 
        TRUE ~ level_2_label
      ), 
      level_2_pred_label = case_when(
        level_1_pred == "African" &  level_2_pred_label == "EAF" ~ "AEA", 
        level_1_pred == "Muslim" &  level_2_pred_label == "EAF" ~ "MEA", 
        level_1_pred == "Muslim" &  level_2_pred_label == "RUS" ~ "MRU", 
        level_1_pred == "Slavic" &  level_2_pred_label == "RUS" ~ "SRU", 
        level_1_pred == "African" &  level_2_pred_label == "SAF" ~ "ASA", 
        level_1_pred == "Hispanic" &  level_2_pred_label == "SAF" ~ "HSA", 
        TRUE ~ level_2_pred_label
      ), 
      level_1_label = case_match(level_1, !!!level_1_map), 
      level_1_pred_label = case_match(level_1_pred, !!!level_1_map),
      level_1_label = factor(level_1_label, levels = rev(level_1_order)),
      level_1_pred_label = factor(level_1_pred_label, levels = level_1_order)
    )
  
  confusion_matrix_lv2 <- confusion_matrix_lv2 |> 
    summarise(
      n = sum(n), 
      .by = c(level_1_label,level_1_pred_label, level_2_label, level_2_pred_label)
    ) |> 
    mutate(perc = n/sum(n), .by = level_2_label)
  
  level_2_order <- confusion_matrix_lv2 |>
    filter(level_2_label == level_2_pred_label) |> 
    arrange(desc(level_1_label), desc(perc)) |> 
    pull(level_2_label) |> 
    unique()
  
  confusion_matrix_lv2 <- confusion_matrix_lv2 |> 
    mutate(
      level_2_label = factor(level_2_label, levels = rev(level_2_order)),
      level_2_pred_label = factor(level_2_pred_label, levels = level_2_order),
      perc_cat = cut(
        perc, 
        c(0,.001,.01, seq(.1,.7,.4), .8, 1), 
        include.lowest = TRUE
      ),
      perc_label = paste0(sprintf("%2.0f", perc * 100), "%")
    )
  
  intercepts_lv1_cmlv2 <- confusion_matrix_lv2 |> 
    select(level_1_label, level_2_label) |> 
    distinct() |> 
    arrange(desc(level_2_label)) |> 
    mutate(i = row_number()-.5) |> 
    summarise(xstart = min(i), n = n(), .by = c(level_1_label)) |> 
    mutate(
      xend = xstart+n,
      ystart = max(xend)-xend+.5,
      yend = ystart+n)
  
  confusion_matrix_lv2_plot <- 
    ggplot(confusion_matrix_lv2) +   
    scale_fill_manual(
      NULL,
      values = c(
        "white", "gray95", 
        brewer.pal(4, "Blues")[c(2,4)], 
        brewer.pal(3, "Reds")[2:3]
      )
    ) + 
    scale_x_discrete(expand = c(0,0), position = "top") + 
    scale_y_discrete(expand = c(0,0)) + 
    geom_tile(aes(x=level_2_pred_label,y=level_2_label, fill = perc_cat)) + 
    geom_tile(
      aes(x=level_2_pred_label,y=level_2_label), 
      data = filter(confusion_matrix_lv2, level_2_pred_label==level_2_label), 
      color = "gray5", linetype = "dashed", fill = "transparent"
    ) +  
    geom_rect(
      aes(xmin = xstart, xmax = xend, ymin = ystart, ymax= yend),
      data =intercepts_lv1_cmlv2, fill = "transparent",
      color = "#22B473", linewidth = 1.25) +
    theme_void() + 
    theme(
      plot.margin = margin(2,2,2,2), 
      legend.position = "bottom",
      axis.text.x = element_text(
        angle = 90, size = 10, vjust = 0.5, hjust = 0, 
        margin = margin(0, 0, 4, 0)
      ),
      axis.text.y = element_text(
        size = 10, vjust = .5, hjust = 1,  
        margin = margin(0, 4, 0, 0)
      ),
      panel.border = element_rect(fill= "transparent", color = "gray25")
    ) +
    theme(legend.position = "none") 
  
  ## |- CM Level 3 -------------------------------------------------------------
  confusion_matrix_lv3 <- confusion_matrix |> 
    mutate(
      level_2_label = case_match(level_2, !!!level_2_map), 
      level_2_pred_label = case_match(level_2_pred, !!!level_2_map),
      level_2_label = case_when(
        level_1 == "African" &  level_2_label == "EAF" ~ "AEA", 
        level_1 == "Muslim" &  level_2_label == "EAF" ~ "MEA", 
        level_1 == "Muslim" &  level_2_label == "RUS" ~ "MRU", 
        level_1 == "Slavic" &  level_2_label == "RUS" ~ "SRU", 
        level_1 == "African" &  level_2_label == "SAF" ~ "ASA", 
        level_1 == "Hispanic" &  level_2_label == "SAF" ~ "HSA", 
        TRUE ~ level_2_label
      ), 
      level_2_pred_label = case_when(
        level_1_pred == "African" &  level_2_pred_label == "EAF" ~ "AEA", 
        level_1_pred == "Muslim" &  level_2_pred_label == "EAF" ~ "MEA", 
        level_1_pred == "Muslim" &  level_2_pred_label == "RUS" ~ "MRU", 
        level_1_pred == "Slavic" &  level_2_pred_label == "RUS" ~ "SRU", 
        level_1_pred == "African" &  level_2_pred_label == "SAF" ~ "ASA", 
        level_1_pred == "Hispanic" &  level_2_pred_label == "SAF" ~ "HSA", 
        TRUE ~ level_2_pred_label
      ), 
      level_1_label = case_match(level_1, !!!level_1_map), 
      level_1_pred_label = case_match(level_1_pred, !!!level_1_map),
      level_1_label = factor(level_1_label, levels = rev(level_1_order)),
      level_1_pred_label = factor(level_1_pred_label, levels = level_1_order),
      level_2_label = factor(level_2_label, levels = rev(level_2_order)),
      level_2_pred_label = factor(level_2_pred_label, levels = level_2_order)
    ) |> 
    mutate(perc = n/sum(n), .by = level_3)
  
  order_level_3 <- confusion_matrix_lv3 |>
    filter(level_3 == level_3_pred) |> 
    arrange(level_1_label, level_2_label, perc) |> 
    pull(level_3) |> 
    unique()
  
  countries_not_predicted <- confusion_matrix_lv3 |> 
    summarise(
      perc = sum(perc), 
      .by = c(level_1_pred, level_2_pred, level_3_pred)
    ) |> 
    filter(perc == 0)
  
  confusion_matrix_lv3 <- confusion_matrix_lv3 |> 
    mutate(
      level_3 = factor(level_3, levels = order_level_3), 
      level_3_pred = factor(level_3_pred, levels = rev(order_level_3)), 
    ) 
  
  confusion_matrix_lv3 <- confusion_matrix_lv3 |>
    mutate(
      perc_cat = cut(
        perc,c(0,.001,.01,seq(.1,.7,.4),.8,1), 
        include.lowest = TRUE)
    )
  
  intercepts_lv1_line <- confusion_matrix_lv3 |> 
    select(level_1_pred_label, level_2_pred_label, level_3_pred) |> 
    distinct() |> 
    arrange(level_3_pred) |> 
    mutate(i = row_number()) |> 
    summarise(xintercept = max(i)+.5, .by = c(level_1_pred_label)) |> 
    mutate(yintercept = (max(xintercept)-xintercept)+.5)
  
  intercepts_lv1 <- confusion_matrix_lv3 |> 
    select(level_1_label, level_2_label, level_3) |> 
    distinct() |> 
    arrange(desc(level_3)) |> 
    mutate(i = row_number()-.5) |> 
    summarise(xstart = min(i), n = n(), .by = c(level_1_label)) |> 
    mutate(
      xend = xstart+n,
      ystart = max(xend)-xend+.5,
      yend = ystart+n)
  
  intercepts_lv2 <- confusion_matrix_lv3 |> 
    select(level_1_label, level_2_label, level_3) |> 
    distinct() |> 
    arrange(desc(level_3)) |> 
    mutate(i = row_number()-.5) |> 
    summarise(
      xstart = min(i), n = n(), 
      .by = c(level_1_label, level_2_label)
    ) |>
    mutate(
      xend = xstart+n,
      ystart = max(xend)-xend+.5,
      yend = ystart+n)
  
  confusion_matrix_lv3_plot <- ggplot(confusion_matrix_lv3) +  
    scale_fill_manual(
      NULL,
      values = c(
        "white", "gray95", 
        brewer.pal(4, "Blues")[c(2,4)], 
        brewer.pal(3, "Reds")[2:3]
      )
    ) + 
    scale_x_discrete(expand = c(0,0)) + 
    scale_y_discrete(expand = c(0,0)) + 
    theme_void() +
    geom_tile(aes(x=level_3_pred,y=level_3, fill = perc_cat)) +  
    geom_rect(
      aes(xmin = xstart, xmax = xend, ymin = ystart, ymax= yend),
      data =intercepts_lv2, fill = "transparent",
      color = "gray30", linewidth = .5, linetype = "dashed") +
    geom_vline(
      aes(xintercept=xintercept), data =intercepts_lv1_line,
      color = "gray25", linewidth = .5) +
    geom_hline(
      aes(yintercept=yintercept),data =intercepts_lv1_line,
      color = "gray25", linewidth = .5) +
    geom_rect(
      aes(xmin = xstart, xmax = xend, ymin = ystart, ymax= yend),
      data =intercepts_lv1, fill = "transparent",
      color = "#22B473", linewidth = 1.2) +
    guides(fill = guide_coloursteps(bar_width=10)) +
    theme(
      plot.margin = margin(2,2,2,2), 
      legend.position = "None",
      panel.border = element_rect(fill= "transparent", color = "gray25")
    )
  
  confusion_matrix_lv3_legend <- ggplot(confusion_matrix_lv3) +  
    scale_fill_manual(
      NULL,
      values = c(
        "white", "gray95", 
        brewer.pal(4, "Blues")[c(2,4)], 
        brewer.pal(3, "Reds")[2:3]
      ),
      labels = labels
    ) + 
    geom_tile(
      aes(x=level_3_pred,y=level_3, fill = perc_cat), color = "gray75"
    ) +
    theme(
      legend.position = "bottom",                
      legend.key.height = unit(1, "lines"),    
      legend.key.width = unit(5, "lines"),  
      legend.text.position = "bottom",
      legend.text = element_text(size = 11, vjust = .5)
    ) +
    guides(
      fill = guide_legend(nrow = 1)        
    ) 
  
  confusion_matrix_lv3_legend <- g_legend(confusion_matrix_lv3_legend)
  
  intercepts_lv1 <- intercepts_lv1 |> 
    mutate(
      xlabel = case_when(
        level_1_label=="JE" ~ xstart+n/2-1, 
        level_1_label=="GR" ~ xstart+n/2, 
        TRUE                ~ xstart+n/2
      ), 
      ylabel = case_when(
        level_1_label=="JE" ~ ystart+n/2+1, 
        level_1_label=="GR" ~ ystart+n/2-.5, 
        TRUE                ~ ystart+n/2
      )
    )
  
  x_labels <- ggplot() + 
    geom_text(
      aes(x = xlabel, y = 0, label = level_1_label), 
      data = intercepts_lv1, 
      size = 12, size.unit = "pt", angle = 90, vjust = 1, hjust=0
    ) + 
    scale_x_discrete(limits = factor(0:176), expand=c(0,0)) + 
    theme_void()
  
  y_labels <- ggplot() + 
    geom_text(
      aes(x = 1, y = ylabel, label = level_1_label), 
      data = intercepts_lv1, 
      size = 12, size.unit = "pt", vjust = 0, hjust = 1
    ) + 
    scale_y_discrete(limits = factor(0:176), expand=c(0,0)) + 
    theme_void()
  
  ## |- CM Final plot ----------------------------------------------------------
  ggdraw() + 
    draw_plot(
      confusion_matrix_lv1_plot,  
      x = 0.010, y = 0.650, width = .475, height=.35
    ) +
    draw_plot(
      confusion_matrix_lv2_plot,   
      x = 0.510, y = 0.650, width = .475, height=.35
    ) + 
    draw_plot(
      confusion_matrix_lv3_plot,  
      x = 0.040, y = 0.040, width = .950, height=.585
    ) +
    draw_plot(
      y_labels,                    
      x = 0.010, y = 0.040, width = .050, height=.585
    ) + 
    draw_plot(
      x_labels,                    
      x = 0.040, y = 0.600, width = .950, height=.050
    ) + 
    draw_plot(
      confusion_matrix_lv3_legend, 
      x = 0.000, y = 0.000, width = 1, height=.045
    )
  
  
  ggsave(
    file.path("outputs","fig9_confusion_matrix_level_3.pdf"),
    width = 30, height = 36, units = "cm"
  ) 
  
  file.path("outputs","fig9_confusion_matrix_level_3.pdf")
}