plot_country_composition <- function(
    file, 
    included = c("China", "Germany", "India"), 
    excluded = c("Australia", "Canada", "United States")
  )
  {
  data <- read_parquet(file) 
  
  countries_excluded <- lapply(excluded, \(x) treemap_country(data, x))
  countries_included <- lapply(included, \(x) treemap_country(data, x))
  
  n_inc <- length(countries_included)
  n_exc <- length(countries_excluded)
  
  total_width <- 1
  space <- 0.01
  
  plot_width_inc <- (total_width - (n_inc + 1) * space) / n_inc
  plot_width_exc <- (total_width - (n_exc + 1) * space) / n_exc
  
  x_inc <- cumsum(c(space, rep(plot_width_inc + space, n_inc - 1)))
  x_exc <- cumsum(c(space, rep(plot_width_exc + space, n_exc - 1)))
  
  countries_composition_plot <- ggdraw() +
    imap(
      countries_included, 
      ~ draw_plot(
        .x, x = x_inc[.y], y = 0.5, 
        width = plot_width_inc, height = 0.5
      )
    ) +
    imap(
      countries_excluded, 
      ~ draw_plot(
        .x, x = x_exc[.y], y = 0, 
        width = plot_width_exc, height = 0.5
      )
    )
  
  ggsave(
    file.path("outputs","fig2_composition_of_scholars.pdf"),
    countries_composition_plot,
    width = 15,
    height = 12,
    units = "cm",
    bg = "transparent"
  )
  
  file.path("outputs","fig2_composition_of_scholars.pdf")
}