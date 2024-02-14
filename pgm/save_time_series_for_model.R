# SPDX-License-Identifier: MIT
# save_time_series_for_model.R
# Author: Peter Ganong (?)
# This script produces and outputs two dataframes (expiry and onset sample) that
# contain per_change, regular_benefits, income, exit_ui_rate, exit_to_recall,
# exit_not_to_recall, cumulative_recall, cumulative_exit by rep rate median and
# quintile for use in modelling on outside.

define_regression_sample_pandemic <- all_states_pandemic_full %>%
  ungroup() %>%
  select(week_start_date, spike, post) %>%
  mutate(pre = !spike & !post,
         post = !spike & !pre) %>%
  distinct()

define_regression_sample_december <- all_states_december_full %>%
  ungroup() %>%
  select(week_start_date, spike, post) %>%
  mutate(pre = !spike & !post,
         post = !spike & !pre) %>%
  distinct()

exit_summary <- grouped_exit_rates(week_start_date)

# Export aggregates
aggregates_pandemic <- tmp_for_hazard_plot_expanded %>%
  mutate(week_start_date = week_start_date + 7) %>%
  filter(cust_state %in% states_rep_rate_timeseries) %>%
  mutate(cut = "All recipients (8 states)") %>%
  exit_summary(cut) %>%
  mutate(type = "All recipients (8 states)")

with_cuts_pandemic <- tmp_for_hazard_plot_expanded %>%
  mutate(week_start_date = week_start_date + 7) %>%
  inner_join(calculate_per_change(median_benefits_pandemic, 600, 600) %>%
               mutate(dec_all = ntile(per_change, 10),
                      quin_all = ntile(per_change, 5),
                      med_all = ntile(per_change, 2)) %>%
               ungroup()) %>%
  left_join(incomes_2019) %>%
  filter(week_start_date <= last_reliable_date_payment_observed)

#these three statements throw warnings that I don't understand and haven't had
#time to troubleshoot yet
aggregates_with_cuts_pandemic <- with_cuts_pandemic %>%
  mutate(cut = "Changes available (8 states + MI)") %>%
  exit_summary(cut) %>%
  mutate(type = "Changes available (8 states + MI)")

by_quintile_pandemic <- with_cuts_pandemic %>%
  mutate(cut = as.character(quin_all)) %>%
  exit_summary(cut) %>%
  mutate(type = "By rep rate quintile")


by_median_pandemic <- with_cuts_pandemic %>%
  mutate(cut = as.character(med_all)) %>%
  exit_summary(cut) %>%
  mutate(type = "By rep rate median")

expiry_sample <-
    bind_rows(aggregates_pandemic,
              aggregates_with_cuts_pandemic,
              by_quintile_pandemic,
              by_median_pandemic) %>%
      left_join(define_regression_sample_pandemic) %>%
  select(-regular_benefits, -cumulative_recall, -cumulative_exit)

# Split out by subsample

with_cuts_december <- tmp_for_hazard_plot_expanded %>%
  mutate(week_start_date = week_start_date + 7) %>%
  inner_join(calculate_per_change(median_benefits_december, 0, 300) %>% 
               mutate(dec_all = ntile(per_change, 10),
                      quin_all = ntile(per_change, 5),
                      med_all = ntile(per_change, 2))) %>%
  filter(week_start_date <= end_window_reg_onset)


aggregates_with_cuts_december <- with_cuts_december %>%
  mutate(cut = "Changes available (8 states + TX)") %>%
  exit_summary(cut, start = "2020-11-01") %>%
  mutate(type = "Changes available (8 states + TX)")

by_quintile_december <- with_cuts_december %>%
  mutate(cut = as.character(quin_all)) %>%
  exit_summary(cut, start = "2020-11-01") %>%
  mutate(type = "By rep rate quintile")

by_median_december <- with_cuts_december %>%
  mutate(cut = as.character(med_all)) %>%
  exit_summary(cut, start = "2020-11-01") %>%
  mutate(type = "By rep rate median")

onset_sample <-
    bind_rows(aggregates_with_cuts_december,
              by_quintile_december,
              by_median_december) %>%
      left_join(define_regression_sample_december)%>%
  select(-regular_benefits, -income_2019, -cumulative_recall, -cumulative_exit)
