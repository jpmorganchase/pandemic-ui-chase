# 
# plot_weekly_coefs <- function(lm_recall, lm_not_recall) {
#   (bind_rows(
#     lm_recall %>% tidy() %>% mutate(group = "Recall"),
#     lm_not_recall %>% tidy() %>% mutate(group = "New Job")
#   )
#   %>%
#     filter(str_detect(term, "week"), #XXX this is lazy writing, need instead to combine to single pattern
#            str_detect(term, "TRUE")) %>%
#     mutate(week = as.Date(str_sub(term, 5))) %>% 
#     data.frame() %>%
#     ggplot() + 
#     geom_pointrange(aes(x = week, 
#                         y = estimate, 
#                         ymin = estimate - 1.96*std.error, 
#                         ymax = estimate + 1.96*std.error,
#                         color = group),
#                     position = position_dodge(width = 1)) +
#     labs(subtitle = "Regression coefficients on week*(symmetric_pct_change > median)")) %>%
#     gg_walk_save()
# }


lm_by_week_not_recall_expire <-
  lm(exit_ui ~ week * per_change,
     all_states_pandemic %>%
       filter(exit_labor == 1) %>%
       filter(!(exit_ui == 1 & start_recall == 1)) %>%
       mutate(week = as.factor(week_start_date)))

lm_by_week_not_recall_onset <-
  lm(exit_ui ~ week * per_change,
     all_states_december %>%
       filter(exit_labor == 1) %>%
       filter(!(exit_ui == 1 & start_recall == 1)) %>%
       mutate(week = as.factor(week_start_date)))

lm_by_week_not_recall_binary_onset <- 
  lm(exit_ui ~ week + week * above_median,
     all_states_december %>%
       filter(exit_labor == 1) %>%
       filter(!(exit_ui == 1 & start_recall == 1)) %>%
       mutate(week = as.factor(week_start_date),
              above_median = above_median == 2))

lm_by_week_not_recall_binary_expire <- 
  lm(exit_ui ~ week + week * above_median,
     all_states_pandemic_long %>%
       filter(exit_labor == 1) %>%
       filter(!(exit_ui == 1 & start_recall == 1)) %>%
       mutate(week = as.factor(week_start_date),
              above_median = above_median == 2))

weekly_coef_new_job_expire <- lm_by_week_not_recall_binary_expire %>%
  tidy() %>%
  filter(str_detect(term, "TRUE"),
         str_detect(term, "week")) %>%
  mutate(week = as.Date(str_sub(term, 5)),
         period = case_when(week < as.Date("2020-08-01") ~ "pre",
                            between(week, as.Date("2020-08-01"), as.Date("2020-09-30")) ~ "aug_sep",
                            TRUE ~ "later")) %>%
  group_by(period) %>%
  mutate(average_coef = mean(estimate)) %>%
  ungroup() %>%
  mutate(estimate = estimate - average_coef[first(which(period == "aug_sep"))]) %>%
  data.frame() %>%
  ggplot() + 
  geom_pointrange(aes(x = week, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 1)) +
  fte_theme() +
  labs(subtitle = "Regression coefficients on week*(above median) relative to August/September average",
       x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b '%y")

weekly_coef_new_job_onset <- lm_by_week_not_recall_binary_onset %>%
  tidy() %>%
  filter(str_detect(term, "TRUE"),
         str_detect(term, "week")) %>%
  mutate(week = as.Date(str_sub(term, 5))) %>% 
  data.frame() %>%
  ggplot() + 
  geom_pointrange(aes(x = week, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 1)) +
  fte_theme() +
  labs(subtitle = "Regression coefficients on week (above median)",
       x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b '%y")

weekly_beta_new_job_expire <- lm_by_week_not_recall_expire %>%
  tidy() %>%
  filter(str_detect(term, "per_change")) %>%
  slice(-1) %>%
  mutate(week = as.Date(str_sub(term, 5)),
         period = case_when(week < as.Date("2020-08-01") ~ "pre",
                            between(week, as.Date("2020-08-01"), as.Date("2020-09-30")) ~ "aug_sep",
                            TRUE ~ "later")) %>%
  group_by(period) %>%
  mutate(average_coef = mean(estimate)) %>%
  ungroup() %>%
  mutate(estimate = estimate - average_coef[first(which(period == "aug_sep"))]) %>%
  data.frame() %>%
  ggplot() + 
  geom_pointrange(aes(x = week, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 1)) +
  fte_theme() +
  labs(subtitle = "Regression coefficients on week*per_change relative to August/September average",
       x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b '%y")

weekly_beta_new_job_onset <- lm_by_week_not_recall_onset %>%
  tidy() %>%
  filter(str_detect(term, "per_change")) %>%
  slice(-1) %>%
  mutate(week = as.Date(str_sub(term, 5))) %>% 
  data.frame() %>%
  ggplot() + 
  geom_pointrange(aes(x = week, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 1)) +
  fte_theme() +
  labs(subtitle = "Regression coefficients on week*per_change",
       x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b '%y")


pdf(str_c(path_out, Sys.Date(), "_weekly_coefs_new_job.pdf"), width = 8, height = 4.5, onefile = TRUE)
weekly_coef_new_job_expire
weekly_beta_new_job_expire
weekly_coef_new_job_onset
weekly_beta_new_job_onset
dev.off()
