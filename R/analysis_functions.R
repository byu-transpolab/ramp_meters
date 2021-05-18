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
kalman_filter <- function(v_in, v_out, occupancy, K = 0.22, C = NA, period_length = 1,
                          ramp_length, n_lanes = 2, veh_length = 24){
  
  
  # estimate of queue based on density
  qhat <- (occupancy/100) * ramp_length * n_lanes / veh_length
  
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


#' Objective function to optimize
#' 
#' @param p A vector of parameters
#' @param df Cleaned dataset
rmse_kalman <- function(p, df){
  
  queue <- kalman_filter(df$v_in, df$v_out, df$occupancy, K = p[1], ramp_length = 537)
  # compute moving average: might want to do this ahead of time?
  #MAQueue <- (lag(queue) + queue + lead(queue))/3
  rmse(df$Queue_size, queue)
}


rmse022 <- function(df){rmse_kalman(0.22, df)}

optim_k <- function(df){
  optim_results <- optim(c(0.22), rmse_kalman, df = df)
  optim_results$par
}


optim_rmse <- function(df){
  optim_results <- optim(c(0.22), rmse_kalman, df = df)
  optim_results$value
}
#' Group and optimize RMSE
#' 
#' @param df
#' @return A tibble with optimized k values for each period
#' 
group_optimize_k <- function(df){
  df %>%
    mutate(
      day = lubridate::day(`Start_time`),
      hour = lubridate::hour(`Start_time`),
      minute = lubridate::minute(`Start_time`),
      period = minute %/% 30
    ) %>%
    group_by(day, hour, period) %>%
    nest() %>%
    mutate(
      rmse_at_22 = map_dbl(data, rmse022),
      optim_k = map_dbl(data, optim_k),
      optim_rmse = map_dbl(data, optim_rmse)
    )
}


#plot_optim_default_rmse(group_k){
 # ggplot(group_k, aes(x = optim_rmse, y = rmse_at_22, color = log(optim_k + 0.1))) + 
  #  geom_point() + 
   # scale_color_viridis_c()
#}

#adjust times 



