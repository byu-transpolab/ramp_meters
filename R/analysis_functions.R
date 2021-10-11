#' Function to compute estimated queue length via a Kalman filter
#'
#' @param v_in vector of vehicles entering queue in time 
#' @param v_out vector of vehicles entering queue in time 
#' @param K Coefficient of 
#' @param period_length in minutes
#' @param ramp_length Length of the ramp in feet
#' @param n_lanes Number of lanes on ramp.
#' @param veh_length Average assumed vehicle length with safety buffer
#' 
kalman_filter <- function(v_in, v_out, iq_occ, K = 0.22, C = NA, period_length = 1,
                          ramp_length, n_lanes, veh_length = 24){
  
  
  # estimate of queue based on density
  qhat <- (iq_occ/100) * ramp_length * n_lanes / veh_length
  
  # conservation model of queue length - C is a factor of the total error between
  # in and out
  if(is.na(C)) {
    C <- sum(v_in) / sum(v_out)
  }
  
  conservation <- period_length * (v_in - C * v_out)
  
  # loop through periods
  queue <- rep(0, length(v_in)) # create a vector of zeros to store everything
  for (i in 1:length(v_in)){
    lastqueue <- if(i == 1) 0 else queue[i-1]
    queue[i] <- lastqueue + conservation[i] + K * (qhat[i] - lastqueue)
    if(queue[i]<0) queue[i] <- 0
  }
  
  # queue must be non-negative
  queue
}


#' Root mean squared error (not in R by default)
#' 
#' @param x Comparison variable
#' @param y Baseline variable
rmse <- function(x, y){
  sqrt(mean(( x - y )^2, na.rm = TRUE))
}


#' Return RMSE between Kalman filter prediction and queue
#' 
#' @param p A vector of parameters
#' @param df Cleaned dataset, must have columns
#'   - v_in
#'   - v_out
#'   - iq_occ
rmse_kalman <- function(p, df){
  queue <- kalman_queue(p, df)
  rmse(df$man_q_len, queue)
}


kalman_queue <- function(p, df){
  kalman_filter(df$v_in, df$v_out, df$iq_occ, K = p[1], ramp_length = df$ramp_length, n_lanes = df$n_lanes)
}


rmse022 <- function(df){
  rmse_kalman(0.22, df)}


#' Find the value of k that minimizes the RMSE between observed and modeled
#' 
#' @param df dataset
#' 
find_optimum_k <- function(df){
  # start at the default value of 0.22
  o <- optim(c(0.22), rmse_kalman, df = df)
  
  q <- kalman_filter(df$v_in, df$v_out, df$iq_occ, K = o$par, ramp_length = df$ramp_length, n_lanes = df$n_lanes)
  
  list(optim = o, queue = q)
}



#' Group and optimize RMSE
#' 
#' @param df
#' @return A tibble with optimized k values for each period
#' 
group_optimize_k <- function(df){
  df %>%
    mutate(
      rmse_at_22 = map_dbl(data, rmse022),
      optim = map(data, find_optimum_k),
      optim_k = map_dbl(optim, function(x) x$optim$par),
      rmse_at_k  = map_dbl(optim, function(x) x$optim$value),
      queue_at_k = map(optim, function(x) x$queue)
    )
}

#' Join
#'
join_correlation_data <- function(group_k, correlation_data){
  left_join(group_k, correlation_data)
  
}

#' Create linear models
#' 
linear_models <- function(model_data){
  models <- list(
    #"Base" = lm(optim_k ~ iq_occ + density, data = model_data),
    #"Log Density" = lm(optim_k ~ iq_occ + log(density + 1), data = model_data),
    #"log_k" = lm(log(optim_k) ~ iq_occ + density, data = model_data),
    #"Flow" = lm(optim_k ~ iq_occ + log(density + 1) + flow, data = model_data),
    #"Bangerter" = lm(optim_k ~ iq_occ + flow + log(density + 1), data = model_data %>% filter(ramp == "Bangerter")),
    #"Layton" = lm(optim_k ~ iq_occ + flow + log(density + 1), data = model_data %>% filter(ramp == "Layton")),
    "Ramp Control [Density]" = lm(optim_k ~ iq_occ  + log(density + 1) + ramp, data = model_data),
    #"Mean" = lm(optim_k ~ iq_occ  + log(density + 1) , data = model_data),
    #"85th %" = lm(optim_k ~ iq_occ  + log(density_85 + 1) , data = model_data),
    #"Median" = lm(optim_k ~ iq_occ  + log(med_density + 1) , data = model_data),
    #"SD" = lm(optim_k ~ iq_occ  + log(sd_density + 1) , data = model_data),
    #"Minimum" = lm(optim_k ~ iq_occ  + log(min_density + 1) , data = model_data),
    #"Maximum" = lm(optim_k ~ iq_occ  + log(max_density + 1) , data = model_data),
    "Ramp Control [Occ]" = lm(optim_k ~ iq_occ  + eq_occ + pq_occ + ramp, data = model_data),
    "Ramp Control [Log Occ]" = lm(optim_k ~ log(iq_occ + .01) + log(eq_occ + .01) + log(pq_occ + .01) + ramp, data = model_data),
    "Log Occ" = lm(optim_k ~ log(iq_occ + .01) + log(eq_occ + .01) + log(pq_occ + .01), data = model_data)
  )
}

#' Create modelsummary table
#'
model_summary <- function(linear_models){
  modelsummary(
    linear_models, 
    estimate = "{estimate} ({statistic}){stars}",
    statistic = NULL, title = "Optim_K Linear Regression Estimates",
    notes = c("t-statistics in parentheses, * p < 0.1, ** p < 0.05, *** p < 0.01"),
    escape = FALSE
    
  )
}


