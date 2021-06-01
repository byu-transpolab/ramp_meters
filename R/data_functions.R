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


#' Adjust the time bins and combine detector and manual counts into one data frame
#' 
#' @param cleaned_data
#' @return A data frame with the time bins corrected
#'

raw_manual_data <- read_excel("data/layton_manual.xlsx")
raw_detector_data <- read_excel("data/layton_detector.xlsx")

adjust_timebins <- function(raw_detector_data, raw_manual_data){
  # lead/lag adjustment to align the manual and detector counts based on rmse
  rmse_lag <- function(detector, manual, nlags){
    n <- 0
    manual_lag <- manual
    if(nlags > 0){
      while(n < nlags){
        manual_lag <- lag(manual_lag) 
        n <- n + 1
      }
    } else if(nlags < 0){
      #if nlags is negative, use lead
      while(n < -1 * nlags){
        manual_lag <- lead(manual_lag) 
        n <- n + 1
      }
    }
    # calculate rmse
    sqrt(mean((detector - manual)^2, na.rm = TRUE))
  }  
  
  # calculate lead/lag rmse three minutes either direction
  lag_value <- vector()
  for(i in c(-3:3)){
    lag_value[i+4] <- rmse_lag(raw_detector_data$det_eq_1,raw_manual_data$man_eq_1,i)  
  }
  # report the best lead/lag  
  correct_lag <- which.min(lag_value) - 4
  
  raw_manual_data %>% 
    mutate(
      start_new = if(correct_lag > 0) {
        lag(start_time, n = correct_lag) 
      } else {
        lead(start_time, n = abs(correct_lag)) },
      # figure out what to do if correct_lag is negative, if statement of if correct_lag is positive or negative
      end_new = if(correct_lag > 0) {
        lag(end_time, n = correct_lag) 
      } else {
        lead(end_time, n = abs(correct_lag)) } 
      # figure out what the joining is actually doing when shifting the data (does it move the data up or down)
    ) %>% 
    # combine the detector and manual count spreadsheets together
    left_join(raw_detector_data, by = c("start_new" = "start_time", "end_new" = "end_time"))
}


#' Clean the data
#' 
#' @param raw_data The tibble returned by read_raw_data
#' @return A tibble with cleaned data
#' 
clean_data <- function(adjusted_data){
  adjusted_data %>%
    select(start_time, contains("man"),
           contains("det"), iq_1_occ, iq_2_occ, meter_rate_vph)  %>%
    mutate(
      v_in = det_eq_1 + det_eq_2 + det_eq_3,
      v_out = det_pq_1 + det_pq_2,
      occupancy = (iq_1_occ + iq_2_occ) / 2,
      man_eq_tot = man_eq_1 + man_eq_2 + man_eq_3,
      det_eq_tot = det_eq_1 + det_eq_2 + det_eq_3,
      meter_rate_vpm = meter_rate_vph / 60
    )
}


#' Nest the data
#' 
#' @param cleaned_data
nest_data <- function(cleaned_data, ...){
  cleaned_data %>%
    mutate(
      day = lubridate::day(`start_time`),
      hour = lubridate::hour(`start_time`),
      minute = lubridate::minute(`start_time`),
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


