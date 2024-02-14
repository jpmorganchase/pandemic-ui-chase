# SPDX-License-Identifier: MIT
# liquidity_distribution.R
# Author: Katie Zhang
# Objective: compute some statistics to summarise the magnitude of the reversal
# of liquidity between unemployed and employed households during the pandemic.
# We want to compare Jan 2020, Apr 2020, Jul 2020 and Jan 2021.

# we want:
# Col 1: asset ($) for median unemployed household
# Col 2: take $ from col (1) and find percentile within employed group jan 2020 
# Col 3: $ from col (1) / income in 2019
# Col 4: find where col (3) falls in dist within employed group jan 2020


if(!exists("incomes_2019")) {
  # this is usually loaded on the jobfinding side and not spending, so load in
  # if it wasn't already loaded in environment
  
  if(!exists("df_demo_src_wins_2019")) {
    print("loading in df_demo_src_wins_2019 from scratch, takes about 1.5 mins")
    date_scratch_build_sp_jf <- "2023-02-23"
    df_demo_src_wins_2019 <- readRDS(str_c(data_path, "tmp/build_sp_jf/", date_scratch_build_sp_jf, "df_demo_src_wins_2019.rds"))
  }
  
  print("creating incomes from 2019 file, usually in pgm/rep_rate_prep.R, takes about 4 mins")
  incomes_2019 <- df_demo_src_wins_2019 %>% 
    mutate(inflows_ex_transfers = total_inflows - transfer_inflows) %>% 
    group_by(cust_number, periodid) %>%
    slice(1) %>%
    group_by(cust_number) %>% 
    summarise(income_2019 = sum(inflows_ex_transfers, na.rm = TRUE)) %>%
    filter(is.finite(income_2019))
}

# Col 1: asset ($) for median unemployed household ----
cust_for_liq_dist <-
  bind_rows(cust_nonui,
            # cust_thru_feb_first_week
            cust_thru_feb %>%
              #Filter to losing job right at end of March or early April, and starting benefits soon after
              #This creates closest conceptual alignment with model timing 
              dplyr::filter(job_sep_date %in% c(as.Date('2020-03-29'), as.Date('2020-04-05'))) %>%
              dplyr::filter(first_ui_week %in% c(as.Date('2020-04-05'), as.Date('2020-04-12'))) %>%
              distinct(cust_number, .keep_all = TRUE))

expect_equal(cust_for_liq_dist %>% nrow(),
             cust_for_liq_dist %>% distinct(cust_number) %>% nrow())

tmp_liq_dist_data <-
  df_demo_src_wins %>% 
  inner_join(cust_for_liq_dist, by = 'cust_number') %>% 
  distinct(cust_number, periodid, .keep_all = TRUE) %>% 
  mutate(periods_to_check = periodid %in% c(202001, 202004, 202007, 202101),
         inflows_ex_transfers = total_inflows - transfer_inflows) %>%
  rename(spend_cardcash = total_spend_narrow,
         spend_total = total_spend_expanded) %>% 
  dplyr::filter(periods_to_check) %>%
  left_join(incomes_2019) %>%
  mutate(chk_bal_over_inc_2019 = checking_acct_balance / income_2019)

unemp_hh_liq_data_stats <- # median values for unemp hh across the 4 months
  tmp_liq_dist_data %>%
  filter(group == "UI thru feb 2021") %>%
  group_by(periodid) %>%
  summarise(ct_cust = n(),
            med_chk_bal = xtile_ten(checking_acct_balance), # col 1
            med_chk_bal_inc_pct = xtile_ten(chk_bal_over_inc_2019)) # col 3

# Cols 2 to 4 ----
emp_liq_jan_2020 <- # values for emp hh as of jan 2020
  tmp_liq_dist_data %>%
  filter(group == "Employed",
         periodid == 202001) %>%
  select(cust_number, checking_acct_balance, chk_bal_over_inc_2019)



find_percentile_rank_fn <- function(mth, med_var, orig_var) {
  # this fn takes median number (either (1) chk bal or (2) chk bal / inc 2019)
  # for unemployed hhs for a certain month (jan '20, apr '20, jul '20 or jan '21)
  # and inserts it into a list of corresponding stat for employed hhs in jan '20
  # and then calculates a percentile rank 
  med_unemp_hh_bal_mth <-
    unemp_hh_liq_data_stats %>%
    filter(periodid == mth) %>%
    pull(!!sym(med_var))
    
  percentile_rank_med_unemp_mth <-
    emp_liq_jan_2020 %>%
    mutate(var = !!sym(orig_var)) %>%
    add_row(cust_number = "med_unemp",
            var = med_unemp_hh_bal_mth) %>%
    mutate(percentile_rank = rank(var) / length(var)) %>%
    filter(cust_number == "med_unemp") %>%
    pull(percentile_rank)

  return(percentile_rank_med_unemp_mth)
}

liq_data_stats <-
  unemp_hh_liq_data_stats %>%
  mutate(
    # col 2
    perc_rank_chk_bal = find_percentile_rank_fn(periodid,
                                                "med_chk_bal",
                                                "checking_acct_balance"), 
    # col 4
    perc_rank_chk_bal_over_inc = find_percentile_rank_fn(periodid, 
                                                         "med_chk_bal_inc_pct",
                                                         "chk_bal_over_inc_2019")
    )

liquidity_distribution_stats <- liq_data_stats[, c(1, 2, 3, 5, 4, 6)]

# Check: 25, 50, 75th percentile of emp hh in jan 2020 ----
perc_check_bal_emp_jan_2020 <-
  quantile(emp_liq_jan_2020$checking_acct_balance,
           probs = c(0.25, 0.5, 0.75)) %>%
  as_tibble() %>%
  pull()

test_that("25th, 50th and 75th percentiles of checking account balance for employed households in jan 2020",
          if(small_samp==TRUE){
            expect_equal(perc_check_bal_emp_jan_2020,
                         c(1029.81, 2627.15, 7108.93))
          }else{
            expect_equal(perc_check_bal_emp_jan_2020,
                       c(963.48, 2520.54, 6865.56))
            })

# where does median unemp hh in jan 2020 fall in the employed hh dist?
med_chk_bal_for_unemp_jan_2020 <-
  liq_data_stats %>%
  filter(periodid == 202001) %>%
  pull(med_chk_bal)

test_that("Median unemployed household in jan 2020 percentile of employed houshold distribution",
          expect_equal((emp_liq_jan_2020 %>%
                          filter(checking_acct_balance <= med_chk_bal_for_unemp_jan_2020) %>%
                          nrow()) /
                         (emp_liq_jan_2020 %>%
                            nrow()),
                       ifelse(small_samp == TRUE, 0.3190635, 0.355),
                       tol = 1e-02))

