#' Read spreadsheet(s) into a dataframe
#' 
#' @param files A vector of paths to excel spreadsheets containing the data we need
#' @return A tibble with the necessary data
#'
read_raw_data <- function(files, names){
  file_names <- tibble(
    ramp = as.character(1:length(names)),
    name = names
  )
  
  lapply(files, function(f){
    read_excel(f)
  }) %>%
    bind_rows(.id = "ramp") %>%
    left_join(file_names, by = "ramp") %>%
    mutate(ramp = name) %>% select(-name) %>%
    split(.$ramp)
}


#' Adjust the time bins and combine detector and manual counts into one data frame
#' 
#' @param clean_data
#' @return A data frame with the time bins corrected
#'


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
    sqrt(mean((detector - manual_lag)^2, na.rm = TRUE))
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
    left_join(raw_detector_data, by = c("ramp", "start_new" = "start_time", "end_new" = "end_time"))
}

#' Configure ramp properties
#' 
ramp_properties <- function(){
  tribble(
    ~ramp_name, ~direction, ~ramp_length, ~n_lanes,
    "Bangerter", "SB", 1000, 3,
    "Layton", "NB", 537, 2
    )
}


#' Clean the data
#' 
#' @param raw_data The tibble returned by read_raw_data
#' @return A tibble with cleaned data
#' 
clean_data <- function(adjusted_data){
  adjusted_data %>%
    select(start_time, contains("man"), ramp, direction, ramp_length, n_lanes,
           contains("det"), contains("iq_occ"), contains("pq_occ"), meter_rate_vph)  %>%
    mutate(
      v_in = det_eq_1 + det_eq_2 + det_eq_3,
      v_out = det_pq_1 + det_pq_2 + det_pq_3,
      man_eq_tot = man_eq_1 + man_eq_2 + man_eq_3,
      det_eq_tot = det_eq_1 + det_eq_2 + det_eq_3,
      iq_occ = (iq_occ_1 + iq_occ_2 + iq_occ_3) / n_lanes,
      pq_occ = (pq_occ_1 + pq_occ_2 + pq_occ_3) / n_lanes,
      density = man_tot_on_ramp/((ramp_length/5280)*n_lanes), # Total veh on ramp divided by (ramp length (mi) * number of lanes)
      meter_rate_vpm = meter_rate_vph / 60,
      flow = v_out * (60/1) # Total vehicles exiting the ramp times (60min/1hr) divided by 1 min period
    ) %>%
  filter(!is.na(det_eq_1))
}


#' Nest the data
#' 
#' @param clean_data
nest_data <- function(clean_data){
  
  bind_rows(clean_data) %>%
    mutate(
      day = lubridate::day(`start_time`),
      hour = lubridate::hour(`start_time`),
      minute = lubridate::minute(`start_time`),
      period = minute %/% 30
    ) %>%
    group_by(ramp, day, hour, period) %>%
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
      iq_occ = map_dbl(data, iq_occ),
      pq_occ = map_dbl(data, pq_occ),
      meter_rate_vpm = map_dbl(data, meter_rate_vpm),
      density = map_dbl(data, density),
      flow = map_dbl(data, flow)
    ) %>% 
    select(-data)
  
}

iq_occ <- function(df){
  mean(df$iq_occ, na.rm = TRUE)
}

pq_occ <- function(df){
  mean(df$pq_occ, na.rm = TRUE)
}

meter_rate_vpm <- function(df){
  mean(df$meter_rate_vpm, na.rm = TRUE)
}

density <- function(df){
  mean(df$density, na.rm = TRUE)
}

flow <- function(df){
  mean(df$flow, na.rm = TRUE)
}

