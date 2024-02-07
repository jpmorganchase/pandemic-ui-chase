# summer_expiration_funcs.R
# Author: Rupsha Debnath

# summer set up functions--
estimate <- function(date, donut = 0) {
  
  weekly_summary_natl <- df_ui_cust_week %>% 
    filter(exit_labor == 1,
           cust_state %in% states_rep_rate_timeseries,
           week_start_date >= as.Date("2019-01-01")) %>%
    group_by(week_start_date) %>%
    weekly_summary(last_date_ = last_date_exit_timeseries) %>%
    filter(week_start_date >= as.Date("2020-01-19"),
           week_start_date <= as.Date("2021-12-31"))
  
  (weekly_summary_natl %>% 
      filter(week_start_date >= date + 7 + donut,
             week_start_date <= date + 28 + donut) %>% 
      summarise(mean(`Exit rate not to recall`)) %>% 
      pull()) - 
    (weekly_summary_natl %>% 
       filter(week_start_date >= date - 7,
              week_start_date <= date) %>% 
       summarise(mean(`Exit rate not to recall`)) %>% 
       pull())
}

make_aggregate_summer <- function(df, ctrls = ""){
  df %>%
    lm(str_c("exit_ui ~  as.factor(vin_all) / post", ctrls), .) %>% 
    #it is easy to redo this analysis with controls (e.g. state / post) by adding them above
    broom::tidy() %>%
    filter(str_detect(term, ":")) %>%
    filter(str_detect(term, "vin_all")) %>%
    mutate(vin_all = as.numeric(str_extract(term, "[0-9]{1,2}"))) %>%
    left_join(df %>% filter(!post) %>% 
                group_by(vin_all) %>% 
                summarise(per_change = mean(per_change),
                          weekly_ben = mean(weekly_ben)), by = "vin_all")
}

make_binscatter_aggregate_summer <- function(df, grand_mean = 0) {
  
  plot_mean <- df %>%
    ungroup() %>%
    summarise(x = mean(per_change),
              y = mean(estimate))
  
  df <- df %>%
    mutate(estimate = estimate - plot_mean$y + grand_mean)
  
  temp_df <<- df
  
  slope <<- slope_4weeks_nodonut
  
  ggplot(df) +
    aes(per_change, estimate) +
    scale_x_continuous(name = "Change in benefits") +
    scale_y_continuous() +
    geom_point(size = 2, colour = navy0) +
    labs(subtitle = "Change in average exit rate to new job") +
    geom_abline(slope = slope,
                intercept = grand_mean - slope * for_intercept$x) +
    fte_theme()
}

exhibit_4_df <- function(data, duration_cutoff, donut){
  all_states_sepexpire_expand_nopeuc <- data %>%
    group_by(cust_number) %>%
    mutate(dur_at_window_start=min(weeks_active_run_sum))  %>%
    filter(dur_at_window_start < duration_cutoff) %>%
    ungroup()
  
  if(donut){
    all_states_sepexpire_full <- 
      all_states_sepexpire_expand_nopeuc %>%
      mutate(spike = (week_start_date > as.Date("2021-09-05") & week_start_date<as.Date("2021-09-24"))) %>%
      filter(!spike,
             week_start_date <= end_window_reg_sepexpire)
  } else {
    all_states_sepexpire_full <- 
      all_states_sepexpire_expand_nopeuc %>%
      filter(week_start_date <= end_window_reg_sepexpire)
  }
  
  all_states_sepexpire <- 
    all_states_sepexpire_full %>%
    mutate(SuppAvail = !post)
  
  reg_data <- all_states_sepexpire %>%
    filter(exit_labor == 1) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
    rename(PctChange = per_change)
  return(reg_data)
}

exhibit4_reg_output <- function(reg_list, output_type){
  cluster_count <- function(x){
    #this extracts the customer counts for the regression table
    formatC(length(unique(x$clustervar$cust_number)), format="d", big.mark=",")
  }
  output <- reg_list %>%
    mod_stargazer(type = output_type, 
                  keep =  "SuppAvailTRUE\\:",
                  digits = 4,
                  keep.stat = "n",
                  notes.label = "",
                  column.labels = c(rep("4 weeks", 2), rep("8 weeks", 2)),
                  dep.var.labels = c("Exit to New Job"),
                  dep.var.labels.include = TRUE,
                  add.lines = list(c("Donut", rep(c("Yes","No"), 2)),
                                   c("Number of Households", cluster_count(.$donut_4weeks),
                                     cluster_count(.$nodonut_4weeks), cluster_count(.$donut_8weeks),
                                     cluster_count(.$nodonut_8weeks))),
                  notes.align = "l",
                  float = FALSE,
                  omit.table.layout = "n",
                  model.numbers=FALSE) %>%
    str_replace("SuppAvailTRUE\\:", "SuppAvailTRUE*")
  return(output)
}