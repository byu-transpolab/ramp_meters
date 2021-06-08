library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/data_functions.R")
source("R/analysis_functions.R")
source("R/plot_functions.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "readxl", "lubridate"))

# End this file with a list of target objects.
list(
  # data cleaning and grouping pipeline
  tar_target(raw_detector_data, read_raw_data(c("data/layton_detector.xlsx"))),
  tar_target(raw_manual_data, read_raw_data(c("data/layton_manual.xlsx"))),
  tar_target(adjusted_data, adjust_timebins(raw_detector_data, raw_manual_data)),
  tar_target(df, clean_data(adjusted_data)),
  tar_target(nested_data, nest_data(df)),
  tar_target(correlation_data, get_correlation_data(nested_data)),
  
  # data analysis and plotting functions
  tar_target(default_k_rmse, rmse_kalman(0.22, df)),
  tar_target(group_k, group_optimize_k(nested_data)),
  tar_target(removed_outliers, remove_outliers(group_k)),

  # make plot data
  tar_target(plot_data, make_plot_data(group_k)),
  # (example) tar_target(AM_plot,make_plot(group_k,"AM")),
  # (example) tar_target(PM_plot,make_plot(group_k,"PM"))
  
  

  
  
  # Put correlation data and optimal groupings on the same df
  tar_target(model_data, join_correlation_data(removed_outliers, correlation_data)), 
  
  # make data analysis tables
  tar_target(linearmodels, linear_models(model_data)),
  tar_target(modelsummary, model_summary(linearmodels))
  
  

)

