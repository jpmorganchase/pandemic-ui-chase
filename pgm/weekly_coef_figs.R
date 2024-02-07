# weekly_coef_figs.R
# Author: Katie Zhang
# This runs regressions with weekly coefficients to new job for binary (above
# vs below median) and weekly DID, then plots the coefficients.

# INS
# must run pgm/rep_rate_prep.R first, produces following dataframes:
# 1. all_states_pandemic_long,
# 2. all_states_december,
# 3. all_states_pandemic

# OUTS
# outputted in pgm/R_analytics_driver_jan22.R
weekly_coef_figs_list <-
  c(
    "weekly_beta_new_job_expire",
    "weekly_beta_new_job_onset"
  )

################################################################################
# expiry, beta on continuous treatment ----
lm_by_week_not_recall_expire <-
  lm(exit_ui ~ week * per_change,
     all_states_pandemic %>%
       filter(exit_labor == 1) %>%
       mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
       mutate(week = as.factor(week_start_date)))

weekly_beta_new_job_expire_df <- lm_by_week_not_recall_expire %>%
  tidy() %>%
  filter(str_detect(term, "per_change")) %>%
  mutate(term = if_else(term == "per_change",
                        "week2020-06-07:per_change",
                        term),
         week = as.Date(str_sub(term, 5)),
         period = case_when(week < as.Date("2020-08-01") ~ "pre",
                            TRUE ~ "later")) %>%
  filter(week <= as.Date("2020-09-30")) %>% # match time horizon of cts treatment plot
  group_by(period) %>%
  mutate(average_coef = mean(estimate)) %>%
  ungroup() %>%
  mutate(estimate = estimate - average_coef[first(which(period == "pre"))]) %>%
  data.frame() %>%
  select(week, estimate, std.error)

weekly_beta_new_job_expire <- weekly_beta_new_job_expire_df %>%
  ggplot() + 
  geom_pointrange(aes(x = as.Date(week), 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 1)) +
  annotate("rect", xmin = as.Date("2020-06-01"), xmax = as.Date(cutoff_dates[2] - 4),
           ymin = -0.01, ymax = 0.035, alpha = 0.1) +
  annotate("text", x = cutoff_dates[2] - 5, y = 0.01, hjust = "right",
           colour = greys[8], label = "$600 supplement\navailable") +
  fte_theme() +
  labs(subtitle = "Continuous treatment effect estimate (difference from June/July average)",
       x = NULL, y = NULL) +
  scale_y_continuous(limits = c(-0.01, 0.035),
                     expand = c(0.01, 0)) +
  scale_x_date(date_labels = "%b '%y",
               limits = c(as.Date("2020-06-01"), as.Date("2020-10-03")))

# onset, beta on continuous treatment ----
lm_by_week_not_recall_onset <-
  lm(exit_ui ~ week * per_change,
     all_states_december %>%
       filter(exit_labor == 1) %>%
       mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
       mutate(week = as.factor(week_start_date)))

weekly_beta_new_job_onset_df <- lm_by_week_not_recall_onset %>%
  tidy() %>%
  filter(str_detect(term, "per_change")) %>%
  mutate(term = if_else(term == "per_change",
                        "week2020-11-01:per_change",
                        term),
         week = as.Date(str_sub(term, 5)),
         period = if_else(year(week) == 2020,
                          "nov_dec",
                          "other")) %>%
  group_by(period) %>%
  mutate(average_coef = mean(estimate)) %>%
  ungroup() %>%
  mutate(estimate = estimate - average_coef[first(which(period == "nov_dec"))]) %>%
  data.frame() %>%
  select(week, estimate, std.error)

weekly_beta_new_job_onset <- weekly_beta_new_job_onset_df %>%
  ggplot() + 
  geom_pointrange(aes(x = as.Date(week), 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 1)) +
  annotate("rect", xmin = as.Date(cutoff_dates[3]), xmax = as.Date("2021-03-20"),
           ymin = -0.036, ymax = 0.02, alpha = 0.1) +
  annotate("text", x = cutoff_dates[3] + 2, y = 0.014, hjust = "left",
           colour = greys[8], label = "$300 supplement\navailable") +
  fte_theme() +
  labs(subtitle = "Continuous treatment effect estimate (difference from November/December average)",
       x = NULL, y = NULL) +
  scale_y_continuous(limits = c(-0.036, 0.022),
                     expand = c(0.003, 0)) +
  scale_x_date(date_labels = "%b '%y")
