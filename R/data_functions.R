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

#' Adjust the timebins
#' 
#' @param cleaned_data
#' @return A dataframe with the time bins corrected
adjust_timebins <- function(raw_detector_data, raw_manual_data){
  
  # TODO: Adjust time stamps, join together
  cleaned_data
  
}

#' Nest the data
#' 
#' @param cleaned_data
nest_data <- function(cleaned_data, ...){
  cleaned_data %>%
    mutate(
      day = lubridate::day(`Start_time`),
      hour = lubridate::hour(`Start_time`),
      minute = lubridate::minute(`Start_time`),
      period = minute %/% 30
    ) %>%
    group_by(day, hour, period) %>%
    nest() 
}


#' Get data columns for correlation analysis
#' 
#' @param df
#' 
#' 
get_correlation_data <- function(nested_data) {
  nested_data %>%
    mutate(
      pq_occ = map_dbl(data, mean_occ),
      meter_rate_vpm = map_dbl(data, meter_rate_vpm),
      density = map_dbl(data, density)
    ) %>% 
    select(-data)
  
}

mean_occ <- function(df){
  mean(df$pq_occ, na.rm = TRUE)
}

meter_rate_vpm <- function(df){
  mean(df$meter_rate_vpm, na.rm = TRUE)
}

density <- function(df){
  mean(df$density, na.rm = TRUE)
}


