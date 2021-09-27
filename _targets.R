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
tar_option_set(packages = c("tidyverse", "readxl", "lubridate", "zoo", 
                            "modelsummary", "kableExtra", "RColorBrewer"))

# debugging:  uncomment tar_option_set with the name of the target to debug
# Then run tar_make with callr_function = NULL
# tar_option_set(debug = "adjusted_univ_data") # uncomment to debug
# tar_make(callr_function = NULL) # leave commented! run this in your console

# End this file with a list of target objects.
list(
  # data cleaning and grouping pipeline
  tar_target(raw_detector_data, read_raw_data(c("data/layton_detector.xlsx", "data/bangerter_detector.xlsx", "data/university_detector.xlsx"),
                                              c("Layton", "Bangerter", "University"))),
  tar_target(raw_manual_data, read_raw_data(c("data/layton_manual.xlsx", "data/bangerter_manual.xlsx", "data/university_manual.xlsx"),
                                            c("Layton", "Bangerter", "University"))),
 
  tar_target(adjusted_bgt_data,  adjust_timebins(raw_detector_data$Bangerter, raw_manual_data$Bangerter)),
  tar_target(adjusted_lyt_data,  adjust_timebins(raw_detector_data$Layton, raw_manual_data$Layton)),
  tar_target(adjusted_univ_data, adjust_timebins(raw_detector_data$University, raw_manual_data$University)),
  
  tar_target(df_bgt,  clean_data(adjusted_bgt_data)),
  tar_target(df_lyt,  clean_data(adjusted_lyt_data)),
  tar_target(df_univ, clean_data(adjusted_univ_data)),
  
  tar_target(nested_data,   nest_data(list(df_bgt, df_lyt, df_univ))),
  tar_target(nested_data30, nest_data(list(df_bgt, df_lyt, df_univ), bin_length = 30)),
  tar_target(nested_data60, nest_data(list(df_bgt, df_lyt, df_univ), bin_length = 60)),
  
  tar_target(correlation_data,   get_correlation_data(nested_data)),
  tar_target(correlation_data30, get_correlation_data(nested_data30)),
  tar_target(correlation_data60, get_correlation_data(nested_data60)),
  
  # data analysis and plotting functions
  tar_target(group_k,   group_optimize_k(nested_data)),
  tar_target(group_k30, group_optimize_k(nested_data30)),
  tar_target(group_k60, group_optimize_k(nested_data60)),
  
  # make plot data
  tar_target(plot_data,   make_plot_data(group_k)),
  tar_target(plot_data30, make_plot_data(group_k30)),
  tar_target(plot_data60, make_plot_data(group_k60)),
  
  # Put correlation data and optimal groupings on the same df
  tar_target(model_data,   join_correlation_data(group_k,   correlation_data)),
  tar_target(model_data30, join_correlation_data(group_k30, correlation_data30)), 
  tar_target(model_data60, join_correlation_data(group_k60, correlation_data60)), 
  
  # make data analysis tables
  tar_target(linearmodels, linear_models(model_data)),
  tar_target(modelsummary, model_summary(linearmodels)),
  
  # cluster analysis
  tar_target(cluster_data,      build_clusters(model_data)),
  tar_target(cluster_data30,    build_clusters(model_data30)),
  tar_target(cluster_data60,    build_clusters(model_data60)),
  
  tar_target(cluster_plot,       plot_clusters(cluster_data)),
  tar_target(cluster_plot30,     plot_clusters(cluster_data30)),
  tar_target(cluster_plot60,     plot_clusters(cluster_data60)),
  
  tar_target(cluster_estimates,   estimate_cluster_k(cluster_data)),
  tar_target(cluster_estimates30, estimate_cluster_k(cluster_data30)),
  tar_target(cluster_estimates60, estimate_cluster_k(cluster_data60)),
  
  # Queue length estimates and RMSE
  tar_target(pdata, predicted_queues(linearmodels, model_data)),
  tar_target(pdata_plot, plot_predicted_queues(pdata)),
  
  tar_target(rmse_bgt,   rmse_data(pdata, "Bangerter")),
  tar_target(rmse_lyt,   rmse_data(pdata, "Layton")),
  tar_target(rmse_univ,  rmse_data(pdata, "University")),
  tar_target(rmse_table, rmse_data(pdata)),
  
  # Wait time estimates and RMSE
  tar_target(wttime_plot, wait_times(pdata)),
  
  tar_target(rmse_wt_bgt,      rmse_waittime_data(pdata, "Bangerter")),
  tar_target(rmse_wt_lyt,      rmse_waittime_data(pdata, "Layton")),
  tar_target(rmse_wt_univ,     rmse_waittime_data(pdata, "University")),
  tar_target(rmse_wt,          rmse_waittime_data(pdata))
  )

