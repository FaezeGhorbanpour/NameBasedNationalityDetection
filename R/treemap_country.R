treemap_country <- function(data, country_filter){
  data <- data |> 
    filter(country == country_filter) |> 
    arrange(desc(n))
  
  top_lv2 <- data |> 
    summarise(
      n = sum(n),
      .by = c(country, pred_level_2)
    ) |> 
    arrange(desc(n)) |> 
    # slice(1:10) |> 
    pull(pred_level_2)
  
  data <- data |> 
    mutate(
      pred_level_2 = if_else(
        pred_level_2 %in% top_lv2, 
        pred_level_2, 
        "others"
      ),
      pred_level_3 = if_else(
        pred_level_2 %in% top_lv2, 
        pred_level_3, 
        "others"
      )
    ) |> 
    summarise(
      n = sum(n),
      .by = c(country, pred_level_2, pred_level_3)
    ) |> 
    arrange(desc(n)) 
  
  data_tree <- treemap(
    data,
    index=c("pred_level_2","pred_level_3"),
    vSize="n",
    type="index",
    aspRatio = 1/1
  )       
  
  treemap_lv1 <- data_tree[["tm"]] |> 
    as_tibble() |> 
    filter(level == 1) |> 
    arrange(desc(vSize)) |> 
    mutate(
      rank = row_number(),
      xmax = x0 + w,
      ymax = y0 + h,
      perc = 100*vSize/sum(vSize),
      label_lv1 = str_glue("{pred_level_2} ({sprintf('%.0f%%', perc)})")
    )
  
  ggplot() +
    geom_rect(
      data = treemap_lv1,
      aes(xmin = x0, ymin = y0, 
          xmax = xmax, ymax= ymax, 
          fill = pred_level_2),
      linewidth = .75,
      colour = "gray20",
    ) +
    geom_fit_text(
      data = treemap_lv1,
      aes(xmin = x0, xmax = xmax, ymin = y0, ymax = ymax,
          label = label_lv1), 
      colour = "white",
      size = 16,
      min.size = 3.5,
      reflow = TRUE) + 
    coord_equal(expand = FALSE) + 
    scale_fill_manual(values = palettes$level2) +
    theme_void() + 
    theme(legend.position = "none")  + 
    labs(title = country_filter) +
    theme(plot.title = element_text(hjust=0.5, size=18, margin=margin(0,0,4,0)))
}
