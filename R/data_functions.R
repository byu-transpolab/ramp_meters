#' Read spreadsheet(s) into a dataframe
#' 
#' @param files A vector of paths to excel spreadsheets containing the data we need
#' @return A tibble with the necessary data
#'
read_raw_data <- function(files){
  
  lapply(files, function(f){
    read_excel(f)
  }) %>%
    bind_rows(.id = "file")
  
}

#' Clean the data
#' 
#' @param raw_data The tibble returned by read_raw_data
#' @return A tibble with cleaned data
#' 

clean_data <- function(raw_data){
  raw_data %>%
    select(Start_time, Queue_size, contains("Manual"),
           contains("Auto"), IQ_1_Occ, IQ_2_Occ, Meter_rate_vph, Q_out)  %>%
    mutate(
      v_in = Auto_EQ_1 + Auto_EQ_2 + Auto_EQ_3,
      v_out = Auto_Pass_1 + Auto_Pass_2,
      occupancy = (IQ_1_Occ + IQ_2_Occ)/ 2,
      Manual_EQ_Tot = Manual_EQ_1 + Manual_EQ_2 + Manual_EQ_3,
      Detector_EQ_Tot = Auto_EQ_1 + Auto_EQ_2 + Auto_EQ_3
    )
}


#' Get data columns for correlation analysis
#' 
#' @param df
#' 
#' 


