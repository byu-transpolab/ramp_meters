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

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "readxl", "lubridate"))

# End this file with a list of target objects.
list(
  # data cleaning and grouping pipeline
  tar_target(raw_data, read_raw_data(c("data/layton_counts.xlsx"))),
  tar_target(df, clean_data(raw_data)), 
  
  
  # data analysis and plotting functions
  tar_target(default_k, rmse_kalman(0.22, df)),
  tar_target(group_k , group_optimize_k(df))
  
  
)
