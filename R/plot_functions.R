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

old_plot <- function(group_k){
  ggplot(model_data, aes(x = density, y = optim_k, color = ramp, fill = factor(ramp))) + 
    geom_point() + 
    stat_poly_eq(formula = log(y) ~ x, aes(label = paste(..eq.label..,..rr.label..,sep="~~~")), parse = TRUE) +
    #geom_smooth(method="lm",se=FALSE)
    geom_smooth(method = "glm", formula = y ~ x, se = FALSE,
                method.args = list(family = gaussian(link = 'log')))
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
        density > 40 ~ 0,
        density > 30 & density <= 40 ~ 0.1,
        flow < 500 ~ .5,
        flow > 1200 ~ 0.2,
        flow > 900 & flow <= 1200 ~ 0.15,
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
  x = model_data$flow
  y = model_data$density
  
  # standardize x and y
  d <- cbind((x-mean(x))/sd(x), (y-mean(y))/sd(y))
  clusters <- kmeans(d, 5)
  model_data$cluster <- clusters$cluster
  
  model_data
}

#' Estimate the average k by clusters.
estimate_cluster_k <- function(cluster_data){
  cluster_data %>% 
    group_by(cluster) %>%
    summarise(
      mean_k = mean(optim_k), n = n(), sd = sd(optim_k),
      max_density = max(density), min_density = min(density)
    ) %>%
    arrange(mean_k)
}


plot_clusters <- function(cluster_data){
  ggplot(cluster_data, aes(x=density, y = flow, col = factor(cluster))) + 
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
