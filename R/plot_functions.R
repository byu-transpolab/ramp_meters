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

# Old
plot_optim_default_rmse <- function(group_k){
  ggplot(group_k, aes(x = optim_rmse, y = rmse_at_22, color = log(optim_k + 0.1))) + 
    geom_point() + 
    scale_color_viridis_c()
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
      min_pq_occ = min(pq_occ), max_pq_occ = max(pq_occ)
    ) %>%
    arrange(mean_k)
}

# Plot cluster data
plot_clusters <- function(cluster_data){
  ggplot(cluster_data, 
         aes(x=iq_occ, y = pq_occ, col = factor(cluster), shape = factor(ramp))) + 
    geom_point(size = 3) +
    labs(x = "IQ Occupancy (%)", y = "PQ Occupancy (%)") + 
    theme_bw() +
    theme(legend.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 15)) +
  scale_color_brewer(palette = "Dark2", name = "Cluster") + 
  scale_shape_discrete(name = "Ramp")
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
      model_k = predict(linearmodels[['Ramp Control [Density]']]), # change which model?
      # heuristics
      heur_k15 = case_when(
        iq_occ > 16 ~ 0.170,
        iq_occ <= 16 & pq_occ > 13.5 ~ 0.337,
        iq_occ <= 16 & pq_occ <= 13.5 ~ 0.189,
        TRUE ~ 0.22
      ),
      heur_k30 = case_when(
        pq_occ > 13.5 ~ 0.20,
        iq_occ <= 8 & pq_occ > 10 ~ 0.13,
        iq_occ <= 8 & pq_occ <= 10 ~ 0.173,
        TRUE ~ 0.22
      ),
      heur_k60 = case_when(
        pq_occ > 13.5 ~ 0.123,
        iq_occ <= 7 & pq_occ > 8.5 ~ 0.115,
        iq_occ > 7 & pq_occ <= 12 ~ 0.077,
        TRUE ~ 0.22
      )
    ) %>%
    select(ramp, month, day, hour, dow, data, optim_k, queue_at_k, model_k, 
           heur_k15, heur_k30, heur_k60, meter_rate_vpm) %>%
    mutate(
      timestamp = map(data, function(x) x$start_time),
      queue_model = map2(model_k, data, kalman_queue),
      queue_observed = map(data, function(x) x$man_q_len),
      queue_022 = map2(0.22, data, kalman_queue),
      queue_conservation = map2(0, data, kalman_queue),
      queue_heuristic15 = map2(heur_k15, data, kalman_queue),
      queue_heuristic30 = map2(heur_k30, data, kalman_queue),
      queue_heuristic60 = map2(heur_k60, data, kalman_queue)
    ) %>%
    select(-data) %>%
    rename(queue_optim_k = queue_at_k) %>%
    unnest(cols = c(timestamp, contains("queue"))) %>%
    pivot_longer(cols = contains("queue"), names_to = "Series", values_to = "Queue") %>%
    mutate(
      observed = ifelse(grepl("observed", Series), "observed", "modeled"),
      wait_time = Queue/meter_rate_vpm
    )
}

first_value <- function(x){ x[1]}

# RMSE of Models Table
rmse_data <- function(pdata, rampname = NULL){
  new <- pdata %>%
    select(-observed, -wait_time) %>%
    pivot_wider(names_from = Series, values_from = Queue, values_fn = first_value) %>%
    select(ramp, day, timestamp, contains("queue")) %>%
    mutate(
      across(contains("queue"), ~ rollmean(.x, 3, na.pad = TRUE))
    ) 
  if(!is.null(rampname)){
    new <- new %>% filter(ramp == rampname)
  }
  l <- list()
  l[["Conservation"]] <- rmse(new$queue_conservation, new$queue_observed)
  l[["Heuristic_15"]] <- rmse(new$queue_heuristic15,  new$queue_observed)
  l[["Heuristic_30"]] <- rmse(new$queue_heuristic30,  new$queue_observed)
  l[["Heuristic_60"]] <- rmse(new$queue_heuristic60,  new$queue_observed)
  l[["0.22"]]         <- rmse(new$queue_022,          new$queue_observed)
  l[["Linear Model"]] <- rmse(new$queue_model,        new$queue_observed)
  l[["Optim_K"]]      <- rmse(new$queue_optim_k,      new$queue_observed)
  
  l %>%
    bind_rows(.id = "Models") %>%
    pivot_longer(cols = everything(), names_to = "Models", values_to = "RMSE") %>%

    arrange(RMSE)
}


# RMSE of Wait Times Table
rmse_waittime_data <- function(pdata, rampname = NULL){
  new2 <- pdata %>%
    select(-observed, -contains("queue")) %>%
    mutate(Series = gsub("queue","waittime", Series)) %>%
    pivot_wider(names_from = Series, values_from = wait_time, values_fn = first_value) %>%
    select(ramp, day, timestamp, contains("waittime")) %>%
    mutate(
      across(contains("waittime"), ~ rollmean(.x, 3, na.pad = TRUE))
    ) 
  if(!is.null(rampname)){
    new2 <- new2 %>% filter(ramp == rampname)
  }
  l <- list()
  l[["Conservation"]] <- rmse(new2$waittime_conservation, new2$waittime_observed)
  l[["Heuristic_15"]] <- rmse(new2$waittime_heuristic15,  new2$waittime_observed)
  l[["Heuristic_30"]] <- rmse(new2$waittime_heuristic30,  new2$waittime_observed)
  l[["Heuristic_60"]] <- rmse(new2$waittime_heuristic60,  new2$waittime_observed)
  l[["0.22"]]         <- rmse(new2$waittime_022,          new2$waittime_observed)
  l[["Linear Model"]] <- rmse(new2$waittime_model,        new2$waittime_observed)
  l[["Optim_K"]]      <- rmse(new2$waittime_optim_k,      new2$waittime_observed)
  
  l %>%
    bind_rows(.id = "Models") %>%
    pivot_longer(cols = everything(), names_to = "Models", values_to = "RMSE") %>%
    arrange(RMSE)
}


#' Queue Length Plot
#' 
#' @param pdata A tibble of model predictions created by predicted_queues()
#' 
#' @return A ggplot object.
plot_predicted_queues <- function(pdata){
  d <- pdata %>% filter(month %in% c(8) &
                        day %in% c(6) & 
                        #hour %in% c(16) & 
                        ramp %in% c("University") &  
                        !Series %in% c("queue_heuristic30","queue_heuristic60" #,
                                       #"queue_optim_k", "queue_heuristic15", "queue_model", 
                                       #"queue_conservation"
                                       )) %>%
    group_by(Series) %>%
    arrange(timestamp, .by_group = TRUE) %>%
    mutate(rollqueue = rollmean(Queue, 3, na.pad = TRUE))
  
  ggplot(ungroup(d),
         aes(x = timestamp, color = Series, size = observed)) + 
    geom_line(aes(y = rollqueue)) +
    scale_color_brewer(palette = "Dark2", name = "Model",
                       breaks = c("queue_observed","queue_optim_k","queue_022","queue_heuristic15","queue_model","queue_conservation"),
                       labels = c("Observed","Optimized K","Vigos","Heuristic","Linear Model","Conservation")) +
    scale_size_manual(values = c(0.5,1.5), name = "Type",
                      labels = c("Modeled", "Observed")) + 
    labs(x = "Time of Day", y = "Queue Length (veh)") +    
    theme_bw() +
    theme(legend.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 15)) + 
    guides(color = guide_legend(override.aes = list(size=2)))
    #facet_grid(ramp ~ month + day, scales = "free")   
  }

# Wait Times Plot
wait_times <- function(pdata){
  d <- pdata %>% filter(month %in% c(8) & 
                        day %in% c(6) & 
                        #hour %in% c(17) & 
                        ramp %in% c("University") &
                        !Series %in% c("queue_heuristic30","queue_heuristic60")) %>%
    group_by(Series) %>%
    arrange(timestamp, .by_group = TRUE) %>%
    mutate(rollwait = rollmean(wait_time, 3, na.pad = TRUE))
  ggplot(ungroup(d), 
         aes(x = timestamp, color = Series, size = observed)) + 
    geom_line(aes(y = rollwait)) +
    scale_color_brewer(palette = "Dark2", name = "Model",
                       breaks = c("queue_observed","queue_optim_k","queue_022","queue_heuristic15","queue_model","queue_conservation"),
                       labels = c("Observed","Optimized K","Vigos","Heuristic","Linear Model","Conservation")) +
    scale_size_manual(values = c(0.5,1.5), name = "Type",
                      labels = c("Modeled", "Observed")) + 
    labs(x = "Time of Day", y = "Wait Time (min)") + 
    theme_bw() +
    theme(legend.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 15)) + 
    guides(color = guide_legend(override.aes = list(size=2)))
    #facet_grid(ramp ~ month + day, scales = "free")
}

#' Heuristics Plot
plot_heuristic_queues <- function(pdata){
  d <- pdata %>% filter(day %in% c(29) & month %in% c(7) & hour %in% c(18) &
                        Series %in% c("queue_observed","queue_heuristic15",
                                      "queue_heuristic30","queue_heuristic60")) %>%
    group_by(Series) %>%
    arrange(timestamp, .by_group = TRUE) %>%
    mutate(rollqueue = rollmean(Queue, 3, na.pad = TRUE))
  ggplot(ungroup(d),
         aes(x = timestamp, color = Series, size = observed)) + 
    geom_line(aes(y = rollqueue)) +
    scale_color_brewer(palette = "RdYlBu") +
    scale_size_manual(values = c(0.5,1.5)) +
    scale_linetype_manual("Queue Determination", values = c(5, 1)) +
    facet_grid(ramp ~ day, scales = "free") + theme_bw()
  
}

# Layton Wait Times Comparison
wt_comparison <- function(pdata){
  lyt_wt <- read_excel('data/lyt_man_wt.xlsx') %>%
    mutate(rollwait = wt/60,
           observed = "actual",
           Series = "manual")
  d <- pdata %>% filter(month %in% c(4) & 
                          day %in% c(15) & 
                          ramp %in% c("Layton") &
                          !Series %in% c("queue_heuristic30", "queue_heuristic60",
                                         "queue_022", "queue_conservation",
                                         "queue_heuristic15", "queue_model", 
                                         "queue_optim_k")) %>%
    group_by(Series) %>%
    arrange(timestamp, .by_group = TRUE) %>%
    mutate(rollwait = rollmean(wait_time, 3, na.pad = TRUE)) %>%
    select(timestamp, observed, rollwait, Series)
    combined <- rbind(d,lyt_wt[c("timestamp", "observed", "rollwait", "Series")])
    
  # Line
  ggplot(ungroup(combined), 
         aes(x = timestamp, color = Series)) + 
    geom_line(aes(y = rollwait, size = observed)) +
    scale_color_brewer(palette = "Dark2", name = "Wait Time",
                       breaks = c("manual", "queue_observed"),
                       labels = c("Actual", "Calculated")) +
    scale_size_manual(values = c(1.5,0.5),
                      name = "Type",
                      labels = c("Observed", "Calculated")) +
    guides(size = FALSE) +
    labs(x = "Time of Day", y = "Wait Time (min)") + theme_bw() + 
    theme(legend.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 15)) + 
    guides(color = guide_legend(override.aes = list(size=2)))
  
  # Boxplot
  ggplot(ungroup(combined),
         aes(x = Series, y = rollwait)) + 
           geom_boxplot()
  
  # Scatterplot
  combined2 <- cbind(d, lyt_wt[c("timestamp", "observed", "rollwait", "Series")])

  formula1 <- y ~ x
  ggplot(combined2,
    aes(x = rollwait...3, y = rollwait...7)) +
    geom_jitter() +
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula=formula1,aes(label=paste(..eq.label..,..rr.label..,sep="~~~")),parse=TRUE) +
    xlab("Calculated Wait Time (min)") + ylab("Actual Wait Time (min)")
  # RMSE       
  rmse(combined2$rollwait...3, combined2$rollwait...7)
}

# Test Diff in Queue Length Plot
queue_diff <- function(pdata){
  d <- pdata %>% filter(month %in% c(7) &
                          day %in% c(28) & 
                          #hour %in% c(17) & 
                          ramp %in% c("University") &  
                          !Series %in% c("queue_heuristic30","queue_heuristic60" ,
                                         "queue_optim_k", "queue_heuristic15", "queue_model", 
                                         "queue_conservation"
                          )) %>%
    group_by(Series) %>%
    arrange(timestamp, .by_group = TRUE) %>%
    select(timestamp, Series, Queue, observed) %>%
    mutate(rollqueue = rollmean(Queue, 3, na.pad = TRUE),
           queue_diff = 1) # HELP
  
  ggplot(ungroup(d),
         aes(x = timestamp, color = Series, size = observed)) + 
    geom_line(aes(y = rollqueue)) +
    scale_color_brewer(palette = "Dark2", name = "Model",
                       breaks = c("queue_observed","queue_optim_k","queue_022","queue_heuristic15","queue_model","queue_conservation"),
                       labels = c("Observed","Optimized K","Vigos","Heuristic","Linear Model","Conservation")) +
    scale_size_manual(values = c(0.5,1.5), name = "Type",
                      labels = c("Modeled", "Observed")) + 
    labs(x = "Time of Day", y = "Queue Length (veh)") +    
    theme_bw() +
    theme(legend.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 15)) + 
    guides(color = guide_legend(override.aes = list(size=2)))
  #facet_grid(ramp ~ month + day, scales = "free")   
  
}
