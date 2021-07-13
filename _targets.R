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
tar_option_set(packages = c("tidyverse", "readxl", "lubridate", "modelsummary", "kableExtra"))

# End this file with a list of target objects.
list(
  # data cleaning and grouping pipeline
  tar_target(raw_detector_data, read_raw_data(c("data/layton_detector.xlsx", "data/bangerter_detector.xlsx"),
                                              c("Layton", "Bangerter"))),
  tar_target(raw_manual_data, read_raw_data(c("data/layton_manual.xlsx", "data/bangerter_manual.xlsx"),
                                            c("Layton", "Bangerter"))),
  tar_target(adjusted_bgt_data, adjust_timebins(raw_detector_data$Bangerter, raw_manual_data$Bangerter)),
  tar_target(adjusted_lyt_data, adjust_timebins(raw_detector_data$Layton, raw_manual_data$Layton)),
  tar_target(df_bgt, clean_data(adjusted_bgt_data)),
  tar_target(df_lyt, clean_data(adjusted_lyt_data)),
  tar_target(nested_data, nest_data(list(df_bgt, df_lyt))),
  tar_target(correlation_data, get_correlation_data(nested_data)),
  
  # data analysis and plotting functions
  tar_target(group_k, group_optimize_k(nested_data)),

  # make plot data
  tar_target(plot_data, make_plot_data(group_k)),
  tar_target(plot_model, plot_predicted_queues(linearmodels, model_data)),
  # (example) tar_target(AM_plot,make_plot(group_k,"AM")),
  # (example) tar_target(PM_plot,make_plot(group_k,"PM"))
  
  # Put correlation data and optimal groupings on the same df
  tar_target(model_data, join_correlation_data(group_k, correlation_data)), 
  
  # make data analysis tables
  tar_target(linearmodels, linear_models(model_data)),
  tar_target(modelsummary, model_summary(linearmodels))
  
  

)

