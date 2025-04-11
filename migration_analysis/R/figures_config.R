## |- Visualization config -----------------------------------------------------
ggplot2::theme_set(ggplot2::theme_bw())

ggplot2::theme_update(
  text = ggplot2::element_text(size = 10),
  panel.spacing = ggplot2::unit(2, "lines"),
  panel.background = ggplot2::element_rect(fill = "transparent",
                                           colour = NA_character_), 
  plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
  plot.margin = ggplot2::margin(rep(5,4)),
  plot.background = ggplot2::element_rect(fill = "transparent",
                                          colour = NA_character_),
  legend.position = 'bottom',
  legend.margin = ggplot2::margin(rep(5, 4)),
  legend.box.margin = ggplot2::margin(rep(5, 4)),
  legend.box.spacing = ggplot2::margin(rep(5, 4))
) 

palettes <- list(
  level1 = c(
    "African"= "#006C66", "EastAsian" ="#D2DC51", "English"="#348A85", 
    "German" = "#9AC5C2", "Greek"="#EF7C00", "Hispanic" = "#F5B167", 
    "Jewish"="#00413D", "Muslim" ="#8E961E", "Nordic-Baltic" = "#1F5350",
    "Romance"="#4D8682", "Slavic" = "#8F4A00", "SouthAsian" = "#C46C0D"
  ),
  level2 = c(
    "EastAfrica"="#006C66", "SouthernAfrica"="#006C66", "WestAfrica"="#006C66",
    "Chinese"="#D2DC51", "Indochina"="#D2DC51", "Japanese"="#D2DC51", 
    "Korean"="#D2DC51", "Malay"="#D2DC51",
    "English"="#348A85", 
    "Dutch"="#9AC5C2", "German"="#9AC5C2",
    "Greek"="#EF7C00", 
    "Portuguese"="#F5B167", "SouthernAfrica"="#F5B167", "Spanish"="#F5B167",
    "Jewish"="#00413D",
    "Arab"="#8E961E", "EastAfrica"="#8E961E", "NorthAfrica"="#8E961E",
    "Pakistanis"="#8E961E", "Persian"="#8E961E",  "Russian"="#8E961E",
    "Turk"="#8E961E",
    "Baltic"="#1F5350", "Scandinavian"="#1F5350",
    "French"="#4D8682", "Italian"="#4D8682",
    "EastEuropean"="#8F4A00", "Russian"="#8F4A00", "SouthSlavs"="#8F4A00",
    "Indian"="#C46C0D", "Oceania"="#C46C0D"
  ),
  selected_countries = c(
    "Canada" = "#348A85",  
    "Australia" = "#348A85",  
    "Germany" = "#9AC5C2",  
    "Italy" = "#4D8682",  
    "United Kingdom" = "#348A85",  
    "Switzerland" = "#9AC5C2",   
    "China" = "#D2DC51",  
    "South Korea" = "#bd6f30",  
    "Japan" = "#D2DC51",  
    "India" = "#C46C0D"
  )
)