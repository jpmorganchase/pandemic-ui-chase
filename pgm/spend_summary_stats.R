# SPDX-License-Identifier: MIT
#spend_summary_stats.R
#Author: Max Liebeskind
#Objective: calculate some summary stats on spending and the spend samples.

#INS

#OUTS
xls_spend_aggregation_tables <- str_c(path_out, 'ui_spend_for_export.xls')

#spending summary stats for groups----
##get summary stats for two groups: employed, receive benefits since Apr 2020
cust_for_sumstats <-
  bind_rows(cust_nonui,
            #restrict to customers who have gotten UI since April
            cust_ui_onset %>%
              dplyr::filter(first_ui_week %in% c(as.Date('2020-04-05'), as.Date('2020-04-12')),
                            exit_ui_date >= as.Date('2020-08-01'))) %>% 
  distinct(cust_number, .keep_all = TRUE)

tmp_sumstats_data <-
  df_demo_src_wins %>% 
  inner_join(cust_for_sumstats, by = 'cust_number') %>% 
  distinct(cust_number, periodid, .keep_all = TRUE) %>% 
  mutate(period = ifelse(periodid %in% c(202001, 202002), 'jan_feb_2020',
                         ifelse(between(periodid, 202004, 202007), 'apr_jul_2020', 'other')),
         inflows_ex_transfers = total_inflows - transfer_inflows) %>%
  rename(spend_cardcash = total_spend_narrow,
         spend_total = total_spend_expanded) %>% 
  dplyr::filter(period %in% c('jan_feb_2020', 'apr_jul_2020')) %>%
  mutate(period = if_else(period == "jan_feb_2020", " (Jan-Feb 2020)", " (Apr-Jul 2020)"),
         group = if_else(group == "UI onset",
                         "Unemployed pandemic",
                         group))

spend_sumstats <- 
  tmp_sumstats_data %>% 
  group_by(group, period) %>% 
  summarise_at(.vars = vars(inflows_ex_transfers, spend_cardcash, spend_total, total_ui_inflows, checking_acct_balance),
               .funs = list(mean = mean, median = xtile_ten, sd = sd)) %>% 
  left_join(tmp_sumstats_data %>% group_by(group, period) %>% summarise(ct_cust = n()))

spend_sumstats_for_paper <-
  spend_sumstats %>%
  select(group, period, contains("_median")) %>%
  ungroup() %>%
  transmute(`Group (months)` = paste0(group, period),
            `Income` = inflows_ex_transfers_median,
            `Unemployment benefits` = total_ui_inflows_median,
            `Spending (card and cash)` = spend_cardcash_median,
            `Spending (total)` = spend_total_median,
            `Checking account balance` = checking_acct_balance_median)

#Aside: need to load in `table_iv_apcs_combined` from the mpc_robustness_funcs
table_iv_apcs_combined <- read.csv(str_c(path_out, "apcs_table_ivreg.csv"))

#Aggregation standards----
table_agg_standards_spend <- 
  tribble(~exhibit_name, ~min_cell_size, ~fig_no, ~script, ~method,
          ### PLOTS
          # spending timeseries
          "spending_ts_medians", 10,
          "ZZZ", "pgm/spend_plots.R", "xtile_ten",
          "spending_ts_means",
          df_monthly_collapsed_thru_feb %>% pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          "spending_ts_means_cardcash",
          df_monthly_collapsed_thru_feb %>% pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          #waiting plots
          "waiting_levels_bothspend",
          df_ui_onset_collapsed %>% 
            dplyr::filter(first_ui_week %in% weeks_to_plot) %>%
            pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          "waiting_levels_spendtotal",
          df_ui_onset_collapsed %>% 
            dplyr::filter(first_ui_week %in% weeks_to_plot) %>%
            pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          "waiting_diffs_spendtotal",
          df_ui_onset_collapsed %>% 
            dplyr::filter(first_ui_week %in% weeks_to_plot) %>%
            pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          "waiting_diffs_cardcash",
          df_ui_onset_collapsed %>% 
            dplyr::filter(first_ui_week %in% weeks_to_plot) %>%
            pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          # FPUC expiration plots
          "fpuc_exp_diff_ui_cardcash",
          min(c(df_employed_collapsed_expiration %>% pull(n_cust) %>% min(),
                df_weekly_ui_expiration_collapsed %>% pull(ct_cust) %>% min())),
          "ZZZ", "pgm/spend_plots.R", "mean",
          "fpuc_exp_diff_inc_spendtotal",
          min(c(df_employed_collapsed_expiration %>% pull(n_cust) %>% min(),
                df_weekly_ui_expiration_collapsed %>% pull(ct_cust) %>% min())),
          "ZZZ", "pgm/spend_plots.R", "mean",
          "fpuc_exp_diff_inc_spendtotal_norm",
          min(c(df_employed_collapsed_expiration %>% pull(n_cust) %>% min(),
                df_weekly_ui_expiration_collapsed %>% pull(ct_cust) %>% min())),
          "ZZZ", "pgm/spend_plots.R", "mean",
          # lwa plots
          "lwa_main",
          state_treatment_control_lwa_collapsed %>% pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          "lwa_exp",
          state_treatment_control_lwa_collapsed %>% pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          # onset of $300 plots
          "jan_uionset_ui_cardcash",
          df_weekly_ui_onset_collapsed %>% pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          "jan_uionset_inc_spentotal",
          df_weekly_ui_onset_collapsed %>% pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          "jan_uionset_inc_spentotal_norm",
          df_weekly_ui_onset_collapsed %>% pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          # checking account balance
          "chkbal", 10,
          "ZZZ", "pgm/spend_plots.R", "xtile_ten",
          "chkbal_mean",
          df_monthly_collapsed_thru_feb %>% pull(ct_cust) %>% min(),
          "ZZZ", "pgm/spend_plots.R", "mean",
          # APCS - "all"
          "table_apcs_combined", 10,
          "ZZZ", "pgm/apc_calculations.R", "xtile_ten",
          "table_iv_apcs_combined",
          table_iv_apcs_combined %>% pull(n_obs) %>% min(),
          "ZZZ", "pgm/apc_calculations.R", "regression",
          # APC - subsample
          "table_apcs_combined_no_other_cc", 10,
          "ZZZ", "pgm/mpc_robustness.R", "xtile_ten",
          "table_apcs_combined_debt_ach", 10,
          "ZZZ", "pgm/mpc_robustness.R", "xtile_ten",
          # APC - robustness
          "mpc_robust_df", 10,
          "ZZZ", "pgm/mpc_robustness.R", "xtile_ten",
          "mpc_elas_all_means",
          bind_rows(spend_sumstats_all,
                    spend_sumstats_no_other_cc,
                    spend_sumstats_debt_ach) %>%
            pull(ct_cust) %>%
            min(),
          "ZZZ", "pgm/mpc_robustness.R", "mean",
          # other tables & dfs 
          "matched_model_data_table", 10, "ZZZ", "pgm/spend_build.R", "xtile_ten",
          "spend_sumstats", 10, "ZZZ", "pgm/spend_summary_stats.R", "xtile_ten",
          "spend_sumstats_for_paper", 10, "ZZZ", "pgm/spend_summary_stats.R", "xtile_ten",
          "liquidity_distribution_stats", 10, "Not used as table", "pgm/liquidity_distribution_stats.R", "xtile_ten", 
          "inc_spend_avg", 10, "not used as table", "pgm/mpc_robustness.R", "xtile_ten",
          "Ct non-UI customers (stat for text)", cust_nonui %>% nrow(), "Not used as table", "pgm/spend_summary_stats.R", "count" #This is the one stat for text that we compute in the spend section of the code
  )

test_that('Min cell size should be at least 10',
          {expect_gte(table_agg_standards_spend %>% pull(min_cell_size) %>% min(), 
                      ifelse(small_samp==TRUE, 1, 10))})

#combine all stats into XLSX ----
c('table_agg_standards_spend',
  'table_apcs_combined_all', #from apc_calculations.R
  'table_iv_apcs_combined', #from apc_calculations.R
  'table_apcs_combined_no_other_cc', # from mpc_robustness.R
  'table_apcs_combined_debt_ach', # from mpc_robustness.R
  'matched_model_data_table', #from spend_build.R
  'spend_sumstats',
  'spend_sumstats_for_paper',
  'liquidity_distribution_stats',
  'inc_spend_avg',
  'mpc_robust_df',
  'mpc_elas_all_means'
  ) %>% 
    WriteXLS::WriteXLS(xls_spend_aggregation_tables)

