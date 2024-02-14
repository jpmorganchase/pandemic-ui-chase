# SPDX-License-Identifier: MIT
# rep_rate_prep.R
# calculates the median benefits and % benefit change in two time periods: “expiration” 
# (expiration of $600 FPUC at the end of August) and “onset” (onset of $300 
# at the start of January 2021). 

# INS
dfs_used_throughout_rep_rate_prep <-
  # dataframe name + script where it was last generated in the current form
  tribble(~df_name, ~script_where_generated,
          "df_ui_cust_week", "jobfind_build_2_of_2.R",
          "tmp_for_hazard_plot_expanded", "control_prep.R",
          "df_demo_src_wins_2019", "jobfind_build_2_of_2.R")

# OUTS
# no output, generates several dataframes starting with all_states_pandemic (corresponding to 
# expiration) or all_states_december (corresponding to onset) for use in other 
# job-finding analysis scripts:
# 1. pgm/timeseries_plots.R
# 2. pgm/rep_rate_tables.R
# 3. pgm/rep_rate_figs.R
# 4. pgm/weekly_coef_figs.R
# 5. pgm/jobfind_plots.R

################################################################################

# max/min from https://oui.doleta.gov/unemploy/statelaws.asp
# withholding based on searching up on state gov websites
state_withholding_tibble <-
  tribble(~name, ~wh, ~max, ~min, ~withhold_supp,
          "NY", 0.875, 504, 104, 1,
          "OH", 0.9, 647, 135, 1,
          "GA", 0.84, 365, 55, 1,
          "IN", 0.86, 390, 37, 1,
          "WA", 0.9, 790, 188, 1,
          "FL", 0.9, 275, 32, 1,
          "MI", 0.8575, 362, 150, 1,
          "TX", 0.9, 521, 69,  1,
          "IL", 0.8505, 667, 51, 1,
          "CO", 0.8545, 649, 25, 1,
          "CT", 0.87, 824, 15, 1,
          "WI", 0.85, 370, 54, 1,
          "LA", 0.9, 247, 10, 1,
          "OR", 0.84, 673, 157, 1,
          "NJ", 0.9, 713, 120, 0,
          "CA", 0.9, 450, 40, 0)  

calculate_per_change <- function(median_df, amount_included, 
                                 amount_supplement){
  
  get_state <- function(bens, 
                        name, 
                        wh, 
                        max,
                        min,
                        withhold_supp,
                        amount_included,
                        amount_supplement){
    bens %>%
      filter(state == name) %>%
      mutate(amount_included_if_wh = ifelse(withhold_supp, wh*amount_included, amount_included),
             no_sup_bens_withholding = (1/wh)*(weekly_ben - amount_included_if_wh),
             no_sup_bens_not_withholding = weekly_ben - amount_included,
             max_ben_if_withholding = wh * max + amount_included_if_wh,
             max_ben_if_not_withholding = max + amount_included,
             payment_no_wh = (weekly_ben > max_ben_if_withholding &
                                weekly_ben <= max_ben_if_not_withholding),
             no_sup_stat_benefits = if_else(payment_no_wh,
                                            no_sup_bens_not_withholding ,
                                            no_sup_bens_withholding),
             per_change = 2 * amount_supplement /
               (2 * no_sup_stat_benefits + amount_supplement)) %>%
      filter(no_sup_bens_withholding >= min, 
             no_sup_bens_not_withholding <= max) 
  }
  
  multi_state_data <- pmap_dfr(as.list(state_withholding_tibble), 
                               get_state,
                               bens = median_df,
                               amount_included = amount_included,
                               amount_supplement = amount_supplement)
  
  multi_state_data %>%
    mutate(vin_all = ntile(per_change, 10)) %>%
    group_by(state) %>%
    mutate(above_median = ntile(abs(per_change), 2)) %>%
    ungroup() %>%
    return()
}


# construct median benefits samples ----
median_benefits_pandemic <- 
  tmp_for_hazard_plot_expanded %>%
  filter(!(cust_state == "NJ") | !(ui_inflows_mainyear %in% c(600, 1200)),
         !start_ui,
         !exit_ui) %>%
  get_median_benefits(states =  states_rep_rate_main_expire, 
                      start = "2020-04-01",
                      end = "2020-07-31",
                      quant_type = 1) %>%
  mutate(weekly_ben = ifelse(state %in% fortnight_states, reg_ben/2, reg_ben), 
         pan_ben = weekly_ben)
test_that("N recip w rep rate for expiration", expect_equal(nrow(median_benefits_pandemic), 
                                                            ifelse(small_samp==TRUE, 6403, 354195 )))

peuc_recip_to_drop <-
  df_ui_cust_week %>% 
  filter(year(week_start_date) <= 2020, 
         spell_active, 
         cust_state %in% c("NJ", "OH", "IN", "CA")) %>%
  group_by(cust_number) %>%
  filter(n() > 20) %>%
  distinct(cust_number) %>%
  ungroup()
test_that("N likely PEUC", expect_equal(nrow(peuc_recip_to_drop), ifelse(small_samp==TRUE, 1137, 64684 )))

median_benefits_december_expand <- 
  tmp_for_hazard_plot_expanded %>% 
  filter(!start_ui, !exit_ui) %>%
  get_median_benefits(states = states_rep_rate_expand, 
                      start = start_window_onset - 21,
                      end = end_window_onset_benefit,
                      quant_type = 1) %>% #Pete specified default beahvior to use
  #quantile type 3 which selects low values 
  #in some cases that I don't understand. Want to fix back to std behavior.
  mutate(weekly_ben = ifelse(state %in% fortnight_states, reg_ben/2, reg_ben),
         pan_ben = weekly_ben) %>% # not sure - copied from above for _pandemic
  ungroup()
test_that("N recip w rep rate (broad)", 
          expect_equal(nrow(median_benefits_december_expand), 
                       ifelse(small_samp==TRUE, 4958, 271640 )))
median_benefits_december <- median_benefits_december_expand %>% filter(state %in% states_rep_rate_main_onset)
test_that("N recip w rep rate for onset", 
          expect_equal(nrow(median_benefits_december), 
                       ifelse(small_samp==TRUE, 4291, 239088 )))

# construct weekly job finding with replacement rate----
all_states_pandemic_long <- 
  tmp_for_hazard_plot_expanded %>%
  mutate(week_start_date = week_start_date + 7) %>% # move exits forward one week, so it's exits by end of week
  filter(between(week_start_date, start_window_expire, end_window_expire),
         spell_active) %>%
  inner_join(
    calculate_per_change(median_benefits_pandemic, 600, 600),
    by = "cust_number"
  )  %>%
  mutate(spike = FALSE, 
         post = week_start_date >= as.Date("2020-08-01")) %>%
  ungroup()
test_that("N weeks", expect_equal(nrow(all_states_pandemic_long),
                                  ifelse(small_samp==TRUE, 82302, 4554638 )))

all_states_pandemic_full <- 
  all_states_pandemic_long %>%
  filter(week_start_date <= end_window_expire_reg)

all_states_pandemic <- 
  all_states_pandemic_full %>%
  mutate(SuppAvail = !post)

all_states_december_expand <- 
  tmp_for_hazard_plot_expanded %>%
  mutate(week_start_date = week_start_date + 7) %>%
  filter(between(week_start_date, start_window_onset, end_window_onset), 
         spell_active) %>%
  inner_join(
    calculate_per_change(median_benefits_december_expand, 0, 300),
    by = "cust_number"
  ) %>%
  mutate(spike = (week_start_date %in% as.Date(c("2021-01-03", "2021-01-10"))),
         post = week_start_date >= end_window_onset_benefit) %>%
  ungroup()
test_that("N weeks", 
          expect_equal(nrow(all_states_december_expand), 
                       ifelse(small_samp==TRUE, 88426, 4947652 )))

all_states_december_full <- 
  all_states_december_expand %>%
  filter(state %in% states_rep_rate_main_onset)

all_states_december <- 
  all_states_december_full %>%
  filter(!spike,
         week_start_date <= end_window_reg_onset) %>%
  mutate(SuppAvail = post)

# construct controls ----
# bring into all_states dfs
all_states_december_ctrls <- 
  all_states_december %>%
  filter(!spike) %>%
  mutate(SuppAvail = post) %>%
  left_join(attributes_covariates, # from pgm/control_prep.R
            by = c("cust_number", "ui_spell_number", "start_ui_date"))  %>%
  mutate(post_bin = 
           factor(
             case_when(
               week_start_date <= as.Date("2020-12-31") ~ "Pre",
               week_start_date <= as.Date("2021-03-14") ~ "Post early",
               TRUE ~ "Post late"
             ),
             levels = c("Pre", "Post early", "Post late")
           ))

test_that("Same n rows", expect_equal(nrow(all_states_december_ctrls), nrow(all_states_december) ))

write_csv(all_states_december_ctrls,
          str_c(data_path, "intrmd/", "all_states_december_ctrls.csv"))

all_states_pandemic_ctrls <- 
  all_states_pandemic  %>%
  left_join(attributes_covariates, by = c("cust_number", "ui_spell_number", "start_ui_date"))
test_that("Same n rows", expect_equal(nrow(all_states_pandemic_ctrls), nrow(all_states_pandemic) ))

write_csv(all_states_pandemic_ctrls,
          str_c(data_path, "intrmd/", "all_states_pandemic_ctrls.csv"))

# construct 2019 incomes (used in model) ----
df_demo_src_wins_2019 <- readRDS(str_c(data_path, "tmp/build_sp_jf/", date_scratch_sp_jf, "df_demo_src_wins_2019.rds"))

incomes_2019 <- df_demo_src_wins_2019 %>% 
  mutate(inflows_ex_transfers = total_inflows - transfer_inflows) %>% 
  group_by(cust_number, periodid) %>%
  slice(1) %>%
  #drop customer-periodid duplicates (possible because a customer can have multiple types, eg, '2020_ui_recipient' and '2019_nonui')
  group_by(cust_number) %>% 
  summarise(income_2019 = sum(inflows_ex_transfers, na.rm = TRUE)) %>%
  filter(is.finite(income_2019)) #1 customer has inavlid income

rm(median_benefits_december_expand)

