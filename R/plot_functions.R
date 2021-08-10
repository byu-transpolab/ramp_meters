#' Function to plot model results
#' 
#' @param optimal_values A dataframe with 
#' 
#' 
make_plot_data <- function(group_k) {
  group_k  %>%
    mutate(
      time = map(data, function(x) x$'start_time'),
      queue_observed = map(data, function(x) x$man_q_len),
    )  %>%
    rename(queue_modeled = queue_at_k) %>%
    unnest(cols = c(time, queue_modeled, queue_observed))  %>%
    pivot_longer(starts_with("queue"), names_prefix = "queue_", values_to = "queue_length")
}





plot_optim_default_rmse <- function(group_k){
  ggplot(group_k, aes(x = optim_rmse, y = rmse_at_22, color = log(optim_k + 0.1))) + 
    geom_point() + 
    scale_color_viridis_c()
}


#' Function to computer predicted queues among multiple models
#' 
#' @param linearmodels A list of regression models, including one named `Ramp Control`
#' @param model_data A tibble with data used to estimate the linear regression models
#' 
#' @return A tibble with model prediction data by multiple methods.
#' 
predicted_queues <- function(linearmodels, model_data){
  model_data %>%
    ungroup() %>%
    mutate(
      model_k = predict(linearmodels[['Ramp Control']]), # change which model?
      # heuristics
      heur_k = case_when(
        density > 50 ~ 0,
        density > 30 & density <= 40 ~ 0.1,
        density > 20 & density <= 30 ~ 0.2,
        density > 10 & density <= 20 ~ 0.3,
        density > 0 & density <= 10 ~ 0.4,
        TRUE ~ 0.22
      )
    ) %>%
    select(ramp, day,  data, optim_k, queue_at_k, model_k, heur_k, meter_rate_vpm, density, flow) %>%
    mutate(
      timestamp = map(data, function(x) x$start_time),
      queue_model = map2(model_k, data, kalman_queue),
      queue_observed = map(data, function(x) x$man_q_len),
      queue_horowitz = map2(0.22, data, kalman_queue),
      queue_conservation = map2(0, data, kalman_queue),
      queue_heuristic = map2(heur_k, data, kalman_queue)
    ) %>%
    select(-data) %>%
    rename(queue_optim_k = queue_at_k) %>%
    unnest(cols = c(timestamp, contains("queue"))) %>%
    pivot_longer(cols = contains("queue"), names_to = "Series", values_to = "Queue") %>%
    mutate(
      observed = ifelse(grepl("observed", Series), "observed", "modeled")
    )
}

#' Plot predicted queues by multiple models
#' 
#' @param pdata A tibble of model predictions created by predicted_queues()
#' 
#' @return A ggplot object.
plot_predicted_queues <- function(pdata){
  
  ggplot(pdata %>% filter(day %in% c(14)), 
         aes(x = timestamp, y = Queue, color = Series, lty = observed)) + 
    geom_line() +
    scale_linetype_manual("Queue Determination", values = c(5, 1)) +
    facet_grid(ramp ~ day, scales = "free") + theme_bw()
  
}

#' Build k-means clusters
build_clusters <- function(model_data){
  
  x = model_data$iq_occ
  y = model_data$pq_occ
  z = model_data$eq_occ
  
  # standardize x and y
  d <- cbind((x-mean(x))/sd(x), (y-mean(y))/sd(y)) #, (z-mean(z))/sd(z))
  clusters <- kmeans(d, 3)
  model_data$cluster <- clusters$cluster 
  
  model_data
}

#' Estimate the average k by clusters.
estimate_cluster_k <- function(cluster_data){
  cluster_data %>% 
    group_by(cluster) %>%
    summarise(
      mean_k = mean(optim_k), n = n(), sd = sd(optim_k),
      max_pq_occ = max(pq_occ), min_pq_occ = min(pq_occ)
    ) %>%
    arrange(mean_k)
}


plot_clusters <- function(cluster_data){
  ggplot(cluster_data, 
         aes(x=iq_occ, y = pq_occ, col = factor(cluster), shape = factor(ramp))) + 
    geom_point()
}


plot_rmse <- function(rmse, predicted_queues){

  #create rmse for each model (still need to figure out how to create columns of each queue estimate -
  # should I do it within the same predicted_queues function?)
  rmse_conservation = rmse(predicted_queues$queue_observed, predicted_queues$queue_conservation)
  rmse_heuristic = rmse(predicted_queues$queue_observed, predicted_queues$queue_heuristic)
  rmse_horowitz = rmse(predicted_queues$queue_observed, predicted_queues$queue_horowitz)
  rmse_model = rmse(predicted_queues$queue_observed, predicted_queues$queue_model)
  rmse_optim_k = rmse(predicted_queues$queue_observed, predicted_queues$queue_optim_k)

  #plot/report rmse results
  
  
}
