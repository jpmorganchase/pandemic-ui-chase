# SPDX-License-Identifier: MIT
# jobfind_stats_export_jan22.R
# Author: Katie Zhang
# Date: 2022-01-20
# Purpose: (for early 2022 draft) generates minimum aggregation standards &
# a chartbook that exports stats for text and useful dataframes and statistics
# for modelling on outside

#INS
dfs_used_throughout_jobfind_stats_export_jan22 <-
  # dataframe name + script where it was last generated in the current form
  tribble(~df_name, ~script_where_generated,
          "weekly_summary_natl", "timeseries_plots.R",
          "df_ui_cust_week", "jobfind_build_2_of_2.R",
          "peuc_recip_to_drop", "rep_rate_prep.R",
          "regs_1", "rep_rate_tables.R",
          "all_states_pandemic", "rep_rate_prep.R",
          "all_states_december", "rep_rate_prep.R")

#OUTS
# Creates stats for text for export
# Creates the minimum aggregation standards tables
# Creates other model input that Joe uses on the outside 
# Outputting a workbook (called ‘[date]_ui_jobfind_for_export.xls’) which includes 
#     all of the above and any other data frames that are wantedon the outside
#     e.g. the weekly coefficient plots


#### 1. STATS FOR TEXT ####
# average value of four red horizontal bars ----
segment_val <- function(dates) {
  dates <- as.Date(dates)
  weekly_summary_natl %>% 
    filter(week_start_date >= dates[1],
           week_start_date <= dates[2]) %>% 
    summarise(mean(`Exit rate not to recall`)) %>% 
    pull()
}

mean_red_bars <-
  mean(c(segment_val(c("2020-07-19", "2020-07-26")),
         segment_val(c("2020-08-02", "2020-08-23")),
         segment_val(c("2020-12-20", "2020-12-27")),
         segment_val(c("2021-01-17", "2021-02-07"))))

# number of placebo estimates ----
n_placebo <-
  c(seq(as.Date("2020-04-12"), as.Date("2020-06-28"), 7),
    seq(as.Date("2020-08-02"), as.Date("2020-11-29"), 7)) %>% 
  map_dbl(estimate) %>%
  length()

# Number of unique benefit spells ----
n_unique_spells_all <-
  df_ui_cust_week %>%
  filter(ui_spell_number != 0) %>%
  distinct(cust_number, ui_spell_number) %>%
  nrow()

n_unique_spells_sample <-
  df_ui_cust_week %>%
  filter(ui_spell_number != 0) %>%
  filter(cust_state %in% states_rep_rate_expand,
         cust_state != "FL") %>%
  distinct(cust_number, ui_spell_number) %>%
  nrow()

# N likely PEUC ----
n_likely_peuc <- nrow(peuc_recip_to_drop)

# N customers with spell active for time horizon of plot_exit_new_job_drop_peuc ----
n_spells_horizon <- df_ui_cust_week %>% 
  filter(exit_labor == 1,
         cust_state %in% states_rep_rate_timeseries, 
         week_start_date >= as.Date("2020-10-01"),
         week_start_date <= as.Date("2021-12-31")) %>%
  group_by(cust_number) %>%
  summarise(rec_ui_weeks = sum(spell_active)) %>% # 317395 rows here
  filter(rec_ui_weeks > 0) %>%
  nrow() # 165200, so >50% have at least one week active spell?

# table 1 avg per change ----
# average_per_change
avg_per_change_600 <- regs_1$average_per_change[1]
avg_per_change_300 <- regs_1$average_per_change[2]

# averge pct change ($100)
chg_per_100_expiry <- regs_1$chg_per_100[1]
chg_per_100_onset  <- regs_1$chg_per_100[2]

# n
n_avg_per_change_600 <- all_states_pandemic %>% 
  filter(exit_labor == 1) %>%
  filter(!(exit_ui == 1) | !(start_recall == 1)) %>%
  nrow()

n_avg_per_change_300 <- all_states_december %>% 
  filter(exit_labor == 1) %>%
  filter(!(exit_ui == 1) | !(start_recall == 1)) %>%
  nrow()

# avg inc in balance + x-sectional sd of balances (emp vs unemp from March 2020 to Jul 2020) ----
unemp_vs_emp_custs <- # from pgm/spend_build.R which might not be run every time we run job-finding side
  read_csv(str_c(data_path, "tmp/spend/", "unemp_vs_emp_cust_list.csv"))

winsorized_balances_for_sample <- df_demo_src_wins %>%
  filter(periodid %in% c(202003, 202007)) %>%
  transmute(cust_number, periodid, cust_type,
            checking_acct_balance) %>%
  inner_join(unemp_vs_emp_custs)

chk_bal_stats <- winsorized_balances_for_sample %>%
  group_by(periodid, group) %>%
  summarise(mean_chk_bal = mean(checking_acct_balance),
            med_chk_bal = xtile_ten(checking_acct_balance),
            sd_chk_bal = sd(checking_acct_balance)) %>%
  arrange(periodid, group)

# stats for stats_for_text
chg_bal_mean_unemp <- chk_bal_stats$mean_chk_bal[4] - chk_bal_stats$mean_chk_bal[2]

chg_bal_mean_emp <- chk_bal_stats$mean_chk_bal[3] - chk_bal_stats$mean_chk_bal[1]

chg_bal_mean_diff <- chg_bal_mean_unemp - chg_bal_mean_emp

sd_bal_unemp_mar <- chk_bal_stats$sd_chk_bal[2]

# result for coef in absence of liquidity accumulation ----
coef_no_liq_accum <- (reg_output_liq_ctrls$disincentive_coef[[1]] - 
                        ((chg_bal_mean_diff / sd_bal_unemp_mar) * reg_output_liq_ctrls$triple_diff_coef[[1]]))

# benefit changes (by ctrl var) ----
calc_ben_change <- function(df, group_var) {
  if (min(df$week_start_date) <= as.Date("2020-06-07")) {
    amount_supplement <- 600
    event <- "expiration"
  } else {
    amount_supplement <- 300
    event <- "onset"
  }
  
  df <- df %>%
    filter(exit_labor == 1,
           !is.na(!!sym(group_var))) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 0))
  
  non_sym_per_change <- df %>%
    mutate(non_sym_per_change = amount_supplement / no_sup_stat_benefits) %>%
    group_by(cust_number) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(!!sym(group_var)) %>%
    summarise(mean(non_sym_per_change))
  
  names(non_sym_per_change)[2] <- str_c(event, "_non_sym_per_change")
  
  sym_per_change <- df %>%
    group_by(cust_number) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(!!sym(group_var)) %>%
    # Note that the way per_change is defined in rep_rate_prep.R is symmetric
    summarise(mean(per_change))
  
  names(sym_per_change)[2] <- str_c(event, "_sym_per_change")
  
  temp_df <- sym_per_change %>%
    left_join(non_sym_per_change, by = group_var)
  
  return(temp_df)
} # KZ note to self ZZZ: remove the write_csv when merging to develop - we only want a version in big workbook or it gets too messy

# industry
per_change_by_industry_exp <- all_states_pandemic_ctrls %>% 
  calc_ben_change(group_var = "naics_industry")

per_change_by_industry_ons <- all_states_december_ctrls %>% 
  calc_ben_change(group_var = "naics_industry")

per_change_by_industry <-
  per_change_by_industry_exp %>%
  left_join(per_change_by_industry_ons, by = "naics_industry")

# age
per_change_by_agebin_exp <- all_states_pandemic_ctrls %>%
  calc_ben_change(group_var = "age_bin")

per_change_by_agebin_ons <- all_states_december_ctrls %>% 
  calc_ben_change(group_var = "age_bin")

per_change_by_agebin <- per_change_by_agebin_exp %>%
  left_join(per_change_by_agebin_ons, by = "age_bin")

# kids (for <= 44 years old)
per_change_by_kids_exp <- all_states_pandemic_ctrls %>%
  filter(age <= 44) %>%
  calc_ben_change(group_var = "has_kids")

per_change_by_kids_ons <- all_states_december_ctrls %>% 
  filter(age <= 44) %>%
  calc_ben_change(group_var = "has_kids")

per_change_by_kids <- per_change_by_kids_exp %>%
  left_join(per_change_by_kids_ons, by = "has_kids")

# just for pre-pandemic group
per_change_by_early_ui_exp <- all_states_pandemic_ctrls %>%
  filter(between(start_ui_date, as.Date("2020-01-01"), as.Date("2020-06-30"))) %>%
  mutate(early_ui = if_else(start_ui_date <= as.Date("2020-03-15"),
                            "pre-pandemic",
                            "other")) %>%
  calc_ben_change(group_var = "early_ui")

per_change_by_early_ui_ons <- all_states_december_ctrls %>%
  filter(between(start_ui_date, as.Date("2020-01-01"), as.Date("2020-06-30"))) %>%
  mutate(early_ui = if_else(start_ui_date <= as.Date("2020-03-15"),
                            "pre-pandemic",
                            "other")) %>%
  calc_ben_change(group_var = "early_ui")

per_change_by_early_ui <- per_change_by_early_ui_exp %>%
  left_join(per_change_by_early_ui_ons, by = "early_ui")

# benefit change overall ----
calc_ben_change_overall <- function(df, size) {
  symm_per_change <- df %>%
    group_by(cust_number) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(mean(per_change)) %>%
    pull()
  
  non_sym_per_change <- df %>%
    mutate(non_sym_per_change = size / no_sup_stat_benefits) %>%
    group_by(cust_number) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(mean(non_sym_per_change)) %>%
    pull()
  
  tibble(event = size,
         symm_per_change = symm_per_change,
         non_sym_per_change = non_sym_per_change)
}

per_change_overall <- list(
  list(all_states_pandemic %>% 
         filter(exit_labor == 1) %>%
         mutate(exit_ui = (exit_ui == 1 & start_recall == 0)),
       all_states_december %>% 
         filter(exit_labor == 1) %>%
         mutate(exit_ui = (exit_ui == 1 & start_recall == 0)),
       sepexpire %>% 
         filter(exit_labor == 1) %>%
         mutate(exit_ui = (exit_ui == 1 & start_recall == 0))),
  c(600, 300, 300)) %>%
  pmap_dfr(calc_ben_change_overall)%>%
  mutate(event = str_c(event, c(" expire", " onset", " expire")))

# stats_for_text sheet ----
stats_for_text <-
  tribble(
    ~stat, ~value,
    "Average value of red bars", mean_red_bars,
    "Number of placebo estimates", n_placebo,
    "Number of unique benefit spells (overall)", n_unique_spells_all,
    "Number of unique benefit spells (10 states)", n_unique_spells_sample,
    "Number of customers dropped as PEUC recipients", n_likely_peuc,
    "Number of customers with spell active for time horizon in 8 states", n_spells_horizon,
    "Average percent change in benefits ($600)", avg_per_change_600,
    "Average percent change in benefits for $100 (expiry)", chg_per_100_expiry,
    "Average percent change in benefits ($300)", avg_per_change_300,
    "Average percent change in benefits for $100 (onset)", chg_per_100_onset,
    "Change in mean balance from Mar to Jul (unemployed)", chg_bal_mean_unemp,
    "Change in mean balance from Mar to Jul (employed)", chg_bal_mean_emp,
    "Difference in change of mean balance", chg_bal_mean_diff,
    "Cross-sectional SD of March balances (unemployed)", sd_bal_unemp_mar,
    "Disincentive coefficient (Expiration)", reg_output_liq_ctrls$disincentive_coef[[1]],
    "Disincentive coefficient (Onset)", reg_output_liq_ctrls$disincentive_coef[[2]],
    "Triple diff coefficient (Expiration)", reg_output_liq_ctrls$triple_diff_coef[[1]],
    "Triple diff coefficient (Onset)", reg_output_liq_ctrls$triple_diff_coef[[2]],
    "Result for coefficient in absence of liquidity accumulation", coef_no_liq_accum
  ) %>%
  mutate(value = round(value, 6))

#### 2. MIN AGGREGATION ####
# figures ----
minagg_plots_min <-
  tribble(
    ~exhibit_name, ~min_n, ~fig_no, ~script,
    "exit_no_recall_shading",
    weekly_summary_natl %>% transmute(exit_ui - start_recall_if_exit) %>% min(),
    "3", "pgm/timeseries_plots.R",
    "exit_recall",
    weekly_summary_natl %>% transmute(start_recall_if_exit) %>% min(),
    "A-12a", "pgm/timeseries_plots.R",
    "exit_new_job_means",
    weekly_summary_natl %>% transmute(exit_ui - start_recall_if_exit) %>% min(),
    "A-13a", "pgm/timeseries_plots.R",
    "timeseries_placebo_test",
    weekly_summary_natl %>% transmute(exit_ui - start_recall_if_exit) %>% min(),
    "A-13b","pgm/timeseries_plots.R",
    "timeseries_expiry_new_relative_trunc",
    all_states_pandemic_long %>%
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
      group_by(week_start_date, above_median) %>%
      summarise(exit_without_recall = sum(exit_ui)) %>%
      ungroup() %>%
      select(exit_without_recall) %>% min(),
    "4a","pgm/rep_rate_figs.R",
    "timeseries_onset_new_relative",
    all_states_december_full %>%
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
      group_by(week_start_date, above_median) %>%
      summarise(exit_without_recall = sum(exit_ui)) %>%
      ungroup() %>%
      select(exit_without_recall) %>% min(),
    "4b","pgm/rep_rate_figs.R",
    "binscatter_expiry_new_job",
    all_states_pandemic %>% 
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
      group_by(vin_all) %>%
      count() %>%
      pull(n) %>%
      min(),
    "5a","pgm/rep_rate_figs.R",
    "binscatter_onset_new_job",
    all_states_december %>% 
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
      group_by(vin_all) %>% count() %>% pull(n) %>% min(),
    "5b", "pgm/rep_rate_figs.R",
    "active_spells_shade",
    weekly_summary_natl %>% transmute(spell_active) %>% min(),
    "A-9a", "pgm/timeseries_plots.R",
    "n_ui_churn_recall",
    weekly_summary_if_sep %>%
      filter(as.Date(week_start_date) <= last_for_exits) %>%
      select(`N exit with recall`, `N exit without recall`) %>%
      min(),
    "A-9b", "pgm/timeseries_plots.R",
    "scatter_d_ind_mix",
    jpmci_ind %>% group_by(ind_period, jpmci_naics) %>% count() %>% pull(n) %>% min(),
    "A-18", "pgm/industry_mix_change.R",
    "total_exits",
    weekly_summary_natl %>%
      filter(week_start_date <= last_for_exits) %>%
      select(exit_ui) %>%
      min(),
    "A-2b", "pgm/jobfind_plots.R",
    "hero_by_start",
    weekly_summary_by_start %>% 
      mutate(week_start_date = week_start_date + 7) %>%
      filter(week_start_date >= as.Date("2020-02-09"),
             week_start_date <= as.Date("2021-01-01"),
             (start_month_cohort != "Jun" | week_start_date > as.Date("2020-07-01")),
             (start_month_cohort != "May" | week_start_date > as.Date("2020-06-01"))) %>%
      pull(exit_ui) %>%
      min(),
    "A-10", "pgm/timeseries_plots.R",
    "plot_exit_new_job_drop_peuc",
    bind_rows(
      weekly_summary_natl %>% mutate(key = "All"),
      weekly_summary_natl_ex_peuc %>% mutate(key = "Drop PEUC in 4 states")
    ) %>%
      filter(week_start_date >= as.Date("2020-10-01"),
             week_start_date <= as.Date("2021-03-01")) %>%
      transmute(exit_ui - start_recall_if_exit) %>% min(),
    "A-11a", "pgm/timeseries_plots.R",
    "timeseries_expiry_new_trunc",
    all_states_pandemic_long %>%
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
      group_by(week_start_date, above_median) %>%
      summarise(exit_without_recall = sum(exit_ui)) %>%
      ungroup() %>%
      select(exit_without_recall) %>% min(),
    "A-15a", "pgm/rep_rate_figs.R",
    "timeseries_onset_new",
    all_states_december_full %>%
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
      group_by(week_start_date, above_median) %>%
      summarise(exit_without_recall = sum(exit_ui)) %>%
      ungroup() %>%
      select(exit_without_recall) %>% min(),
    "A-15b", "pgm/rep_rate_figs.R",
    "timeseries_expiry_all",
    all_states_pandemic_long %>%
      group_by(above_median, spike, week_start_date) %>%
      summarise(exits = sum(exit_ui)) %>%
      ungroup() %>%
      select(exits) %>% min(),
    "A-15c", "pgm/rep_rate_figs.R",
    "timeseries_onset_all",
    all_states_december_full %>%
      group_by(above_median, spike, week_start_date) %>%
      summarise(exits = sum(exit_ui)) %>%
      ungroup() %>%
      select(exits) %>% min(),
    "A-15d", "pgm/rep_rate_figs.R",
    "timeseries_expiry_recall",
    all_states_pandemic_long %>%
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 1)) %>%
      group_by(above_median, spike, week_start_date) %>%
      summarise(exit_to_recall = sum(exit_ui)) %>%
      ungroup() %>%
      select(exit_to_recall) %>%
      min(),
    "A-15e", "pgm/rep_rate_figs.R",
    "timeseries_onset_recall", 
    all_states_december_full %>%
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 1)) %>%
      group_by(above_median, spike, week_start_date) %>%
      summarise(exit_to_recall = sum(exit_ui)) %>%
      ungroup() %>%
      select(exit_to_recall) %>%
      min(),
    "A-15f", "pgm/rep_rate_figs.R",
    "weekly_beta_new_job_expire",
    all_states_pandemic %>%
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
      group_by(week_start_date) %>%
      summarise(exit_without_recall = sum(exit_ui)) %>%
      ungroup() %>%
      select(exit_without_recall) %>%
      min(),
    "A-14a", "pgm/weekly_coef_figs.R",
    "weekly_beta_new_job_onset",
    all_states_december_full %>%
      filter(exit_labor == 1) %>%
      mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
      group_by(week_start_date) %>%
      summarise(exit_without_recall = sum(exit_ui)) %>%
      ungroup() %>%
      select(exit_without_recall) %>%
      min(),
    "A-22b", "pgm/jobfind_liquidity.R") %>%
  mutate(exhibit_type = "figure")

minagg_plots <- minagg_plots_min %>%
  mutate(fig_no = str_c("Figure ", fig_no),
         exhibit_name = str_c("out/pub/", exhibit_name))

# tables ----
minagg_table_min <-
  tribble(~exhibit_name, ~fig_no, ~script, ~min_n,
          "table_effects_summary", "2", "pgm/rep_rate_tables.R",
          min(weekly_summary_natl %>% transmute(exit_ui - start_recall_if_exit) %>% min(),
              reg_xsec_expire$N, reg_xsec_expire$N), 
          "tbl_ui_flows", "A-8", "pgm/jobfind_tables.R",
          min_tbl_ui_flows %>% min(),
          "table_main_coefficients", "A-4", "pgm/rep_rate_tables.R",
          min(reg_xsec_expire$N,  reg_xsec_expire$N),
          "table_regression_alternative_outcome_expiry", "A-6b", "pgm/rep_rate_tables.R",
          regressions_output$min_n[[1]],
          "table_regression_alternative_outcome_onset", "A-6a", "pgm/rep_rate_tables.R",
          regressions_output$min_n[[2]],
          "table_ctrl_expiration", "A-9a", "pgm/rep_rate_tables.R",
          all_states_pandemic_ctrls %>% 
            mutate(post_bin = SuppAvail) %>%
            filter(exit_labor == 1) %>%
            mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
            rename(PctChange = per_change) %>%
            lm("exit_ui ~ PctChange * SuppAvail + state * post_bin + age_bin * post_bin + ind_factor * post_bin", data = .) %>%
            nobs(.),
          "table_ctrl_onset", "A-9b", "pgm/rep_rate_tables.R",
          all_states_december_ctrls %>%
            filter(week_start_date <= end_window_reg_onset) %>% 
            mutate(post_bin = SuppAvail) %>%
            filter(exit_labor == 1) %>%
            mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
            rename(PctChange = per_change) %>%
            lm("exit_ui ~ PctChange * SuppAvail + state * post_bin + age_bin * post_bin + ind_factor * post_bin", data = .) %>%
            nobs(.),
          "table_liq_ctrls_exp_wins5", "A-10", "pgm/jobfind_liquidity_tables.R",
          n_table_liq_ctrls_exp_wins5,
          "table_liq_ctrls_ons_wins5", "A-11", "pgm/jobfind_liquidity_tables.R",
          n_table_liq_ctrls_ons_wins5) %>%
  mutate(exhibit_type = "table")

# export ----
# just for jobfind_stats_export sheet
minagg_table <- minagg_table_min %>%
  # left_join(table_code_lines, by = "exhibit_name") %>%
  mutate(fig_no = str_c("Table ", fig_no),
         exhibit_name = str_c("out/pub/", exhibit_name))

ag_standards <- bind_rows(minagg_plots, minagg_table)

global_ag_min <- bind_rows(ag_standards %>%
                             arrange(min_n) %>%
                             filter(row_number() == 1),
                           minagg_plots,
                           minagg_table)

if (!small_samp){test_that("N >= 10",
                           expect_gte(global_ag_min %>% pull(min_n) %>% min(),
                                      10 ))}


#### 3. OTHER DFS FOR OUTPUT ####
# weekly summary df
weekly_summary_natl_export <- weekly_summary_natl_full %>%
  select(week_start_date, spell_active, exit_ui, start_recall_if_exit,
         contains("Exit rate")) %>%
  filter(week_start_date >= ymd("2019-02-03"))

if (!small_samp){
test_that("N >= 10",
          expect_gte(weekly_summary_natl_export %>% pull(start_recall_if_exit) %>% min(),
                     10 ))}

# weekly summary df by industry
weekly_summary_industry <- #NOTE: doesn't pass min agg requirements, might be dropping this
  df_ui_cust_week %>%
  left_join(attributes_covariates,
            by = c("cust_number", "ui_spell_number", "start_ui_date")) %>%
  filter(!is.na(naics_industry),
         exit_labor == 1,
         cust_state %in% states_rep_rate_timeseries,
         week_start_date >= as.Date("2020-01-01")) %>%
  group_by(week_start_date, naics_industry) %>%
  weekly_summary(last_date_ = last_date_exit_timeseries)

#### 4. OUTPUT EVERYTHING ####
sheet_names <-
  c("ag_standards",
    "stats_for_text",
    "tbl_ui_flows",
    "typical_benefits_state",
    "n_wks_paid_state",
    "hero_universe_df",
    "expiry_sample",
    "onset_sample" ,
    "weekly_beta_new_job_expire_df",
    "weekly_beta_new_job_onset_df",
    "binscatter_expiry_new_job_df",
    "binscatter_onset_new_job_df",
    "weekly_summary_natl_export",
    "weekly_summary_industry",
    "per_change_overall",
    "per_change_by_industry"
  )

sheet_names %>%
  WriteXLS::WriteXLS(., str_c(path_out, Sys.Date(), "_ui_jobfind_for_export.xls"))
 
