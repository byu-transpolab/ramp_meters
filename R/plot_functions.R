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


plot_predicted_queues <- function(linearmodels, model_data){
  pdata <- model_data %>%
    ungroup() %>%
    mutate(
      estimated_k = predict(linearmodels[['Ramp Control']]), # change which model?
      # heuristics
      heur_k = case_when(
        flow > 1200 ~ 0.6,
        density > 25 ~ 0.8,
        TRUE ~ 0.22
      )
    ) %>%
    select(ramp, day,  data, optim_k, queue_at_k, estimated_k, heur_k) %>%
    mutate(
      timestamp = map(data, function(x) x$start_time),
      queue_model = map2(estimated_k, data, kalman_queue),
      queue_observed = map(data, function(x) x$man_q_len),
      queue_horowitz = map2(0.22, data, kalman_queue),
      queue_heuristic = map2(heur_k, data, kalman_queue)
    ) %>%
    select(-data) %>%
    rename(queue_optimal = queue_at_k) %>%
    unnest(cols = c(timestamp, contains("queue"))) %>%
    pivot_longer(cols = contains("queue"), names_to = "Series", values_to = "Queue")
  
  ggplot(pdata %>% filter(day %in% c(9, 12)), aes(x = timestamp, y = Queue, color = Series)) + 
    geom_line() +
    facet_grid(ramp ~ day, scales = "free_x") + theme_bw()
  
}
