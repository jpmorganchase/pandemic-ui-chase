# rep_rate_figs.R
# Author: Peter Ganong / Katie Zhang
# This script produces plots for event study by above/below median rep rate as 
# well as binscatter plots.

# INS
# must run pgm/rep_rate_prep.R first, produces following dataframes:
# 1. all_states_december,
# 2. all_states_december_full,
# 3. all_states_pandemic,
# 4. all_states_pandemic_long
# must run pgm/rep_rate_tables.R first, produces: regressions_output

# OUTS
# outputted in pgm/R_analytics_driver_jan22.R
rep_rate_figs_list <-
  c(
    "binscatter_onset_new_job",
    "timeseries_onset_all",
    "timeseries_onset_new",
    "timeseries_onset_new_relative",
    "timeseries_onset_recall",
    "binscatter_expiry_new_job",
    "timeseries_expiry_all",
    "timeseries_expiry_new_trunc",
    "timeseries_expiry_new_relative_trunc",
    "timeseries_expiry_recall"
    )

################################################################################

make_median_split_plot <- function(df){
  
  if (min(df$week_start_date) <= as.Date("2020-06-07")) {
    ## EXPIRATION OPTIONS
    # geom_rect date range
    date_1 <- as.Date("2020-06-01")
    date_2 <- as.Date("2020-07-26")
    # annotation options
    label_text <- "$600 supplement\navailable"
    x_val <- date_2 - 3
    y_val <- 0.06*max(df$exit_rate)
    hjust_opt <- "right"
    # legend options
    lowrr <- "Lower PctChange"
    highrr <- "Higher PctChange"
    leg_pos_x <- 1
    leg_jus_x <- 1
  } else {
    ## ONSET OPTIONS
    # geom_rect date range
    date_1 <- as.Date("2021-01-01")
    date_2 <- as.Date("2021-03-18")
    # annotation options
    label_text <- "$300 supplement\navailable"
    x_val <- date_1 + 3
    y_val <- 0.96*max(df$exit_rate)
    hjust_opt <- "left"
    # legend options
    lowrr <- "Lower PctChange"
    highrr <- "Higher PctChange"
    leg_pos_x <- 0.95
    leg_jus_x <- 0.95
  }

  df %>%
  { max_exit_rate <<- max(.$exit_rate)
  .} %>%
    ggplot() + 
    aes(as.Date(week_start_date), exit_rate,
        colour = as.factor(above_median),
        group = as.factor(above_median),
        shape = as.factor(above_median)) +
    geom_point(data = . %>% filter(!spike)) +
    geom_line(data = . %>% filter(week_start_date < as.Date(window_dates[1]))) +
    geom_line(data = . %>% filter(week_start_date > as.Date(window_dates[2]))) +
    # geom_line(data = . %>% filter(week_start_date %in% c(ymd("2020-12-27"), ymd("2021-01-17"))),
    #           linetype = "longdash", alpha = 0.5) +
    scale_x_date(date_labels = "%b  '%y",
                 breaks = "1 month",
                 expand = c(0, 6)) +
    scale_shape_manual(name = NULL,
                       labels = c(lowrr,
                                  highrr),
                       values = c(`1` = 19,
                                  `2` = 15),
                       guide = guide_legend(reverse = TRUE)) +
    scale_colour_manual(name = NULL,
                        labels = c(lowrr,
                                   highrr),
                        values = c(`1` = "#ffae5f",
                                   `2` = "#004577"),
                        guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(limits =  c(0, NA),
                       labels = scales::percent_format(accuracy = 1L),
                       expand = c(0.003, 0)) + 
    labs(subtitle = "Exit rate from unemployment benefits", 
         y = NULL, x = NULL) + 
    fte_theme() +
    theme(legend.position =  c(leg_pos_x, 0),
          legend.justification = c(leg_jus_x, 0),
          legend.title = element_text(size = rel(1.3)),
          legend.text = element_text(size = 12),
          legend.key = element_rect(colour = NA, fill = "transparent"),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA)) +
    annotate("rect", xmin = date_1, xmax = date_2,
             ymin = 0, ymax = 1.05*max_exit_rate, alpha = 0.1) +
    annotate("text", x = x_val, y = y_val, hjust = hjust_opt,
             colour = greys[8],
             label = label_text)
}

make_aggregate <- function(df, ctrls = ""){
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
                          pan_ben = mean(pan_ben)), by = "vin_all")
}

make_binscatter_aggregate <- function(df, grand_mean = 0) {
  
  plot_mean <- df %>%
    ungroup() %>%
    summarise(x = mean(per_change),
              y = mean(estimate))
  
  df <- df %>%
    mutate(estimate = estimate - plot_mean$y + grand_mean)
  
  temp_df <<- df
  
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

# onset plots ----
onset_new_job_data <- all_states_december %>% 
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  make_aggregate() 

for_intercept <- onset_new_job_data %>%
  ungroup() %>%
  summarise(x = mean(per_change),
            y = mean(estimate))

grand_mean <- all_states_december %>% 
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  lm(exit_ui ~ post, .) %>%
  broom::tidy() %>%
  slice(2) %>%
  pull(estimate)

slope <- regressions_output$coefficients[[2]]$not_recall[1]
new_job_se <- regressions_output$new_job_se[[2]]

binscatter_onset_new_job <- onset_new_job_data %>%
  make_binscatter_aggregate(grand_mean) +
  scale_x_continuous(name = "Change in benefits",
                     labels = scales::percent_format(1L),
                     limits = c(0.3, 1.15),
                     breaks = seq(0.3, 1.1, 0.2)) +
  scale_y_continuous(name = NULL,
                     limits = c(-0.015, 0.003)) +
  annotate("text", y = -0.005, x = 0.31, hjust = "left",
           colour = "#727272",
           label = str_c("Slope:", scales::number(slope, accuracy = 0.001),
                         "\nStandard error: ", round(new_job_se, 3))) +
  geom_segment(x = 1,  xend = 1.05,
               y = -0.013, yend = -0.013,
               lineend = "square",
               linejoin = "bevel",
               colour = "#727272",
               arrow = arrow(length = unit(0.03, "npc"))) +
  annotate("text", x = 0.92, y = -0.013,
           colour = "#727272",
           label = "Larger increase\nin benefits") +
  geom_segment(x = 0.35, xend = 0.30,
               y = -0.013, yend = -0.013,
               lineend = "square",
               linejoin = "bevel",
               colour = "#727272",
               arrow = arrow(length = unit(0.03, "npc"))) +
  annotate("text", x = 0.43, y = -0.013,
           colour = "#727272",
           label = "Smaller increase\nin benefits")

binscatter_onset_new_job_df <- temp_df

timeseries_onset_df_all <- all_states_december_full %>%
  group_by(above_median, spike, week_start_date) %>%
  summarise(exit_rate = mean(exit_ui)) %>%
  filter(week_start_date <= last_date)

timeseries_onset_all <- timeseries_onset_df_all %>%
    make_median_split_plot()

timeseries_onset_df_new <- all_states_december_full %>%
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  group_by(above_median, spike, week_start_date) %>%
  summarise(exit_rate = mean(exit_ui)) %>%
  filter(week_start_date <= last_date)

timeseries_onset_new <- timeseries_onset_df_new %>%
  make_median_split_plot() + 
  labs(subtitle = "Exit rate to new job from unemployment benefits")

timeseries_onset_new_relative <- timeseries_onset_df_new %>%
  group_by(above_median, pre = week_start_date < as.Date("2020-12-31")) %>%
  mutate(average_search = mean(exit_rate)) %>%
  group_by(above_median) %>%
  mutate(exit_rate = exit_rate / first(average_search)) %>%
  make_median_split_plot() + 
  labs(subtitle = "Exit rate to new job relative to November/December group average")

timeseries_onset_recall <- all_states_december_full %>%
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 1)) %>%
  filter(week_start_date <= last_date) %>%
  group_by(above_median, spike, week_start_date) %>%
  summarise(exit_rate = mean(exit_ui)) %>%
  make_median_split_plot() + 
  labs(subtitle = "Exit rate to recall from unemployment benefits") +
  scale_y_continuous(limits =  c(0, NA),
                     labels = scales::percent_format(accuracy = 0.1),
                     expand = c(0.003, 0))

# expiration plots ----
expiry_new_job_data <- all_states_pandemic %>% 
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  make_aggregate()

for_intercept <- expiry_new_job_data %>%
  ungroup() %>%
  summarise(x = mean(-per_change),
            y = mean(estimate))

grand_mean <- all_states_pandemic %>% 
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  lm(exit_ui ~ post, .) %>%
  broom::tidy() %>%
  slice(2) %>%
  pull(estimate)

slope <- regressions_output$coefficients[[1]]$not_recall[1]
new_job_se <- regressions_output$new_job_se[[1]]

binscatter_expiry_new_job <- expiry_new_job_data %>%
  mutate(per_change = -per_change) %>%
  make_binscatter_aggregate(grand_mean) +
  scale_x_continuous(name = "Change in benefits",
                     labels = scales::percent_format(1L),
                     limits = c(NA, -0.45)) +
  scale_y_continuous(name = NULL, limits = c(0.002, NA)) +
  annotate("text", y = 0.013, x = -1.3, hjust = "left",
           colour = "#727272",
           label = str_c("Slope:", scales::number(slope, accuracy = 0.001),
                         "\nStandard error: ", round(new_job_se, 3))) +
  geom_segment(x = -1.1, xend = -1.17,
               y = 0.004, yend = 0.004,
               lineend = "square",
               linejoin = "bevel",
               colour = "#727272",
               arrow = arrow(length = unit(0.03, "npc"))) +
  annotate("text", x = -1, y = 0.004,
           colour = "#727272",
           label = "Larger decrease\nin benefits") +
  geom_segment(x = -0.6, xend = -0.53,
               y = 0.004, yend = 0.004,
               lineend = "square",
               linejoin = "bevel",
               colour = "#727272",
               arrow = arrow(length = unit(0.03, "npc"))) +
  annotate("text", x = -0.7, y = 0.004,
           colour = "#727272",
           label = "Smaller decrease\nin benefits")

binscatter_expiry_new_job_df <- temp_df

timeseries_expiry_all <- all_states_pandemic_long %>%
    group_by(above_median, spike, week_start_date) %>%
    summarise(exit_rate = mean(exit_ui)) %>%
    make_median_split_plot()

expiry_new_data <- all_states_pandemic_long %>%
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  group_by(above_median, spike, week_start_date) %>%
  summarise(exit_rate = mean(exit_ui))

timeseries_expiry_new <- expiry_new_data %>%
  make_median_split_plot() +
    labs(subtitle = "Exit rate to new job from unemployment benefits") +
    scale_y_continuous(limits =  c(0, NA),
                       labels = scales::percent_format(accuracy = 0.1),
                       expand = c(0.003, 0))

timeseries_expiry_new_trunc <- timeseries_expiry_new +
    scale_x_date(limits = c(as.Date(c(NA, "2020-09-30"))),
                 date_labels = "%b '%y") +
    theme(legend.text = element_text(size = 12))


timeseries_expiry_new_relative <- expiry_new_data %>% 
  mutate(period = if_else(week_start_date < as.Date("2020-08-01"),
                          "pre",
                          "later")) %>%
  group_by(above_median, period) %>%
  mutate(average_search = mean(exit_rate)) %>%
  group_by(above_median) %>%
  mutate(exit_rate = exit_rate / average_search[first(which(period == "pre"))]) %>%
  make_median_split_plot() + 
  labs(subtitle = "Exit rate to new job relative to June/July group average")

timeseries_expiry_new_relative_trunc <- timeseries_expiry_new_relative +
    scale_x_date(limits = c(as.Date(c(NA, "2020-09-30"))),
                 date_labels = "%b '%y") +
    theme(legend.text = element_text(size = 12))

timeseries_expiry_recall <- all_states_pandemic_long %>%
    filter(exit_labor == 1) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 1)) %>%
    group_by(above_median, spike, week_start_date) %>%
    summarise(exit_rate = mean(exit_ui)) %>%
    make_median_split_plot() +
    labs(subtitle = "Exit rate to recall from unemployment benefits") +
    theme(legend.position = c(1,1), legend.justification = c(1,1))

