library(targets)

## |= Config ===================================================================
## |- Targets config -----------------------------------------------------------
tar_option_set(
  format = "qs"
  )

tar_source()

## |= Analysis =================================================================
list(
  tar_target(
    name = fig1_class_distribution_wikidata,
    plot_class_distribution(
      file.path("inputs","wikipedia_distribution_train_dataset.parquet"),
      file.path("outputs","fig1_class_distribution_wikidata.pdf"),
      th_filter = .015
    ),
    packages = c("tidyverse","arrow", "cowplot"),
    format = "file"
  ),
  tar_target(
    name = fig2_country_composition,
    command = plot_country_composition(
      file.path("inputs","country_composition.parquet"),
      included = c("China", "Germany", "India"), 
      excluded = c("Australia", "Canada", "United States")
      ),
    packages = c("tidyverse", "arrow", "treemap", "cowplot", "ggfittext"),
    format = "file"
  ),
  tar_target(
    name = fig3_top5_destinations_from_us,
    command = plot_top5_destinations(
      file.path("inputs","outmig_to_top5_destinations.parquet"),
      file.path("outputs","fig3_top5_destinations_from_us.pdf"),
      from_country_code_iso3 = "USA"
    ),
    packages = c("tidyverse", "arrow", "ggalluvial", "cowplot"),
    format = "file"
  ),
  tar_target(
    name = fig4_prop_return_by_continent,
    command = plot_scatter_prop_return(
      file.path("inputs","outmigration_by_country_gender_and_origin.parquet")
    ),
    packages = c("tidyverse", "arrow", "lemon"),
    format = "file"
  ),
  tar_target(
    name = fig5a_class_distribution_athletes,
    command = plot_class_distribution(
      file.path("inputs","athletes_distribution_test_dataset.parquet"),
      file.path("outputs","fig5a_class_distribution_athletes.pdf"),
      th_filter = .015
    ),
    packages = c("tidyverse","arrow", "cowplot"),
    format = "file"
  ),
  tar_target(
    name = fig5b_class_distribution_iussp,
    command = plot_class_distribution(
      file.path("inputs","iussp_distribution_test_dataset.parquet"),
      file.path("outputs","fig5b_class_distribution_iussp.pdf"),
      th_filter = .015
    ),
    packages = c("tidyverse","arrow", "cowplot"),
    format = "file"
  ),
  tar_target(
    name = fig6_evaluation_measures_level1,
    command = plot_evaluation_measures(level = 1),
    packages = c("tidyverse","arrow", "openxlsx"),
    format = "file"
  ),
  tar_target(
    name = fig7_evaluation_measures_level2,
    command = plot_evaluation_measures(level = 2),
    packages = c("tidyverse","arrow", "openxlsx"),
    format = "file"
  ),
  tar_target(
    name = fig8_evaluation_measures_level3,
    command = plot_evaluation_measures(level = 3),
    packages = c("tidyverse","arrow", "openxlsx"),
    format = "file"
  ),
  tar_target(
    name = fig9_confusion_matrix,
    command = plot_confusion_matrix(
      file.path("inputs","confusion_matrix_at_country_level.parquet")
    ),
    packages = c("tidyverse","arrow", "cowplot", "RColorBrewer", "lemon"),
    format = "file"
  )
)
