#spend_build.R
#Author: Peter Ganong/Max Liebeskind
#Date: Jan 2021
#Objective: build data needed for the analysis of spending around UI. (Note: this script takes 25 minutes to run.)
#Note: this script takes about 40 mins to run, including 10 mins to load input dfs (so 30 mins if run_build =TRUE)

# Spending Categories --
if(run_categories){
  # CATS
  durables = c(
    'Retail_Durables',
    'Entertainment',
    'Home_Improvement',
    'Insurance',
    'Auto_Repair',
    'Organizations_&_Institutions',
    'schools')
  non_durables = c(
    'Drug_Stores',
    'Groceries',
    'Miscellaneous_Nondurables',
    'Department_Stores',
    'Discount_Stores',
    'Telecom',
    'Hospitals',
    'Other_Healthcare',
    'Utilities',
    'Transit_&_Ground_Transportation',
    'Cash',
    'Professional_&_Personal_Services',
    'Other_Retail',
    'Flights',
    'Restaurant',
    'Clothing')
  debt_disagg_cols = c('on_us_creditcard_pmts',
                       'off_us_creditcard_pmts',
                       'mortgage_pmts',
                       'student_loan_pmts',
                       'other_loan_pmts',
                       'auto_loan_pmts')
  cats_vector <- c(durables, 
                   non_durables,
                   debt_disagg_cols)
}

#INS (dfs that must already be built)
#See load step
max_month_analysis_frame <- as.Date("2021-12-01")
sample_frame_end_periodid <- "202112"
eip1_week_list <- as.Date(c("2020-04-12", "2020-04-26", "2020-05-03", "2020-05-10", "2020-05-17", "2020-05-24"))
eip2_week_list <- as.Date(c("2020-12-27", "2021-01-03"))
eip3_week_list <- as.Date(c("2021-03-14", "2021-03-21"))

#OUTS (important dfs that this script builds)
csv_data_for_modeling_matched <- str_c(path_repo, 'src/data_for_modeling_monthly_matched_', 
                                       Sys.Date(), ifelse(run_categories == TRUE, '_disagg.csv', '.csv'))

out_dfs <- 
  c(
    "cust_ui_onset", #list of customers who experience a job separation at start of Covid
    "df_ui_onset", "df_ui_onset_collapsed", #used for analysis of UI onset in April/May
    "df_monthly_collapsed_combined", #used for analysis of overall path of income and savings during pandemic
    "df_weekly_ui_expiration", "df_weekly_ui_expiration_collapsed", #used for analysis of FPUC ($600) expiration in Aug 2020
    "state_treatment_control_lwa", "state_treatment_control_lwa_collapsed", #used for analysis of LWA ($300) onset
    "cust_nonui", #a list of nonui #customers who don’t experience job loss. use this as a control group.
    "cust_separate_start_covid", #UI groups all filter through it which makes sure that they exited a job between 03/15 and 04/05
    "cust_thru_aug", "cust_thru_feb", "cust_thru_jan", #: filtered from cust_separate_start_covid
    "cust_thru_feb_first_weeks", "cust_ui_thru_sept_2021", "cust_ui_thru_jul_2021",
    "matched_model_data_table"
  )


tm_full <- Sys.time()

#Load if not re-building from top #####
#Load in necessary files if starting from this file rather than from the top of 
#...R_analytics_driver.R. If so, note that you still need to run
#...the "SETUP" block in ui_eip_data_read_in.R
#Takes 8 mins
if (!run_build){
  tm_reload <- Sys.time()
  
  scratch_path_read_in <- "tmp/read_in/"
  scratch_path_build_sp_jf <- "tmp/build_sp_jf/"
  scratch_path_build_jf_1_of_2 <- "tmp/build_jf_1_of_2/"
  scratch_path_build_jf_2_of_2 <- "tmp/build_jf_2_of_2/"
  
  if(run_categories){
    date_scratch_read_in <- "2023-06-22"
    date_scratch_build_sp_jf <-  "2023-06-22"
  } else {
    date_scratch_read_in <- "2023-06-14"
    date_scratch_build_sp_jf <- "2023-06-15"
  }
  date_scratch_build_jf_1_of_2 <- "2023-06-16"
  date_scratch_build_jf_2_of_2 <- "2023-06-16"
  
  df_nonui_clean <- readRDS(str_c(data_path, scratch_path_read_in, date_scratch_read_in, 
                                  ifelse(run_categories==TRUE,
                                         "df_nonui_clean_disagg.rds",
                                         "df_nonui_clean.rds")))
                    #PN note: cust-by-week df with inflow and outflow categories
                    #...for current year and preceeding year. These are winsorized
                    #...per the microdata_cleaner function.
  df_ui_clean <- readRDS(str_c(data_path, scratch_path_read_in, date_scratch_read_in,
                               ifelse(run_categories==TRUE, 
                                      "df_ui_clean_disagg.rds",
                                      "df_ui_clean.rds")))
                    #PN note: same as df_nonui_clean but extra columns for first 
                    #...and last UI week and count of weeks paid. 
  df_demo_src_wins <- readRDS(str_c(data_path, scratch_path_build_sp_jf, date_scratch_build_sp_jf,
                                    ifelse(run_categories==TRUE,
                                           "df_demo_src_wins_disagg.rds",
                                           "df_demo_src_wins.rds")))
                      #PN notes to self: cust-by-month df with winsorized inflow,
                      #...outflow, and balance vars as well as customer characteristics
                      #...like 2019 income, age, tc. 
  df_lwa_cust_week_src <- readRDS(str_c(data_path, scratch_path_build_sp_jf, date_scratch_build_sp_jf,
                                        ifelse(run_categories==TRUE,
                                               "df_lwa_cust_week_src_disagg.rds",
                                               "df_lwa_cust_week_src.rds")))
                      #PN note: cust-by-week df with amount of LWA receipt in given week
  cust_eip_touse <- readRDS(str_c(data_path, scratch_path_build_sp_jf, date_scratch_build_sp_jf,
                                  ifelse(run_categories==TRUE,
                                         "cust_eip_touse_disagg.rds",
                                         "cust_eip_touse.rds")))
  cust_eip_rounds_touse <- readRDS(str_c(data_path, scratch_path_build_sp_jf, date_scratch_build_sp_jf,
                                         ifelse(run_categories==TRUE,
                                                "cust_eip_rounds_touse_disagg.rds",
                                                "cust_eip_rounds_touse.rds")))
  df_labor_cust_week <- readRDS(str_c(data_path, scratch_path_build_jf_1_of_2, date_scratch_build_jf_1_of_2, 
                                      "df_labor_cust_week.rds"))
                     #PN note: cust-by-week df with booleans for starting and stopping jobs
  df_nonui_2020_cust_week <- readRDS(str_c(data_path, scratch_path_build_jf_1_of_2, date_scratch_build_jf_1_of_2, 
                                           "df_nonui_2020_cust_week.rds"))
                     #PN note: cust-by-week df with boolean for exit_labor
  df_ui_cust_spell <- readRDS(str_c(data_path, scratch_path_build_jf_2_of_2, date_scratch_build_jf_2_of_2, 
                                    "df_ui_cust_spell.rds"))
                     #PN note: customer-by-ui spell dataframe, with start date, 
                     #...end date, n ui pmts in spell, and other vars about
                     #...exits
  df_ui_cust_week_state_only <- readRDS(str_c(data_path, scratch_path_build_jf_2_of_2, date_scratch_build_jf_2_of_2, 
                                              "df_ui_cust_week_state_only.rds"))
                     #PN note: cust-by-week df with UI info. This version only
                     #...includes cust_number and state, all we need for spend
  Sys.time() - tm_reload
  rm(tm_reload)
}
Sys.time() - tm_full 

# construct UI groups  ----
# create list of customers from OH -- we exclude them from analysis because 
# we don't pick up PUC ($600) payments from OH
oh_custs <- 
  df_ui_cust_week_state_only %>% 
  dplyr::filter(cust_state %in% c('OH')) %>% 
  distinct(cust_number)

# create list of customers who separate around the start of Covid
# (note: this code shared across weekly and monthly analyses)
cust_separate_start_covid <-
  df_labor_cust_week %>%
  #filter to customers who exit a job between 3/15 and 4/5
  dplyr::filter(between(week_start_date, as.Date("2020-03-15"), as.Date("2020-04-05")),
          exit_labor == 1) %>%
  distinct(cust_number, week_start_date) %>%
  #get each customer's job separation date (note: some customers have multiple, so we take the min)
  group_by(cust_number) %>% 
  summarise(job_sep_date = min(week_start_date)) %>% 
  anti_join(oh_custs, by = "cust_number")
test_that("Nrow cust_separate_start_covid",
          expect_equal(cust_separate_start_covid %>% nrow(), 
                       ifelse(small_samp==TRUE, 4210, 234541)))
test_that("Ncust distinct cust_separate_start_covid",
          expect_equal(cust_separate_start_covid %>% 
                         group_by(cust_number) %>% summarise(ct = n()) 
                       %>% distinct(ct) %>% pull(), 1))

cust_separate_date <-
  df_labor_cust_week %>%
  distinct(cust_number, week_start_date) %>%
  #get each customer's job separation date (note: some customers have multiple, so we take the min)
  group_by(cust_number) %>% 
  summarise(job_sep_date = min(week_start_date)) %>% 
  anti_join(oh_custs, by = "cust_number")


#UI spell data for customers who have job separation around start of Covid
cust_ui_onset <-
  df_ui_cust_spell %>%
  inner_join(cust_separate_start_covid, by = "cust_number") %>%
  rename(first_ui_week = start_ui_date) %>%
  #only include customers who exit UI 5/31 or later
  dplyr::filter(exit_ui_date >= as.Date("2020-05-31")) %>%
  mutate(group = "UI onset")


#filter: subset of cust_ui_onset who exit UI after end of Aug
cust_thru_aug <-
  cust_ui_onset %>%
  #also filter to starting UI before 6/14 -- this is on top of the [3/15, 4/5] job separation filter above
  dplyr::filter(first_ui_week <= as.Date('2020-06-14'),
                exit_ui_date >= as.Date("2020-08-30")) %>%
  mutate(group = "UI thru aug")
test_that("Nrow cust_thru_aug",
          expect_equal(cust_thru_aug %>% distinct(cust_number) %>% nrow(), 
                       ifelse(small_samp==TRUE, 1499, 79250)))

cust_thru_feb <-
  cust_ui_onset %>%
  dplyr::filter(first_ui_week <= as.Date('2020-06-14'),
                exit_ui_date >= as.Date("2021-02-28")) %>%
  mutate(group = "UI thru feb 2021")
test_that("Nrow cust_thru_feb",
          expect_equal(cust_thru_feb %>% distinct(cust_number) %>% nrow(), 
                       ifelse(small_samp==TRUE, 732, 37658)))

# list of customers for $300 onset
cust_thru_jan <-
  cust_ui_onset %>%
  #also filter to starting UI before mid-Nov -- this is on top of the [3/15, 4/5] job separation filter above
  dplyr::filter(first_ui_week <= as.Date('2020-11-01'),
                exit_ui_date >= as.Date("2021-01-31")) %>%
  mutate(group = "UI thru Jan 21")
test_that("Nrow cust_thru_jan",
          expect_equal(cust_thru_jan %>% distinct(cust_number) %>% nrow(), 
                       ifelse(small_samp==TRUE, 988, 50659 )))

# Summer expiration:
# state list:
states_puc_sep <- c("CA", "CO", "CT", "DC", "HI", "IL", "KS", "MA", "MI",
                    "MN", "NJ", "NM", "NY", "NC", "OR", "PA", "VA", "WA", "WI")

states_puc_jun <- c("AL", "AK", "FL", "GA", "ID", "IA", "MS", "MO", "MT", "NE",
                    "NH", "ND", "OH", "OK", "SC", "SD", "TX", "UT", "WV", "WY")

# cust list for summer expiration:
weeks_before <- 5
weeks_after <- 5
sep_expiration_date <- as.Date("2021-09-05")
# check with PG about this
june_expiration_date_first <- as.Date("2021-06-12") # first state expiration date (earliest)
june_expiration_date_last <- as.Date("2021-06-30") # last state expiration date (earliest)
sep_last_possible_date_to_start_ui <- sep_expiration_date - 7*weeks_before
sep_first_possible_date_to_end_ui <-  sep_expiration_date + 7*weeks_after
june_last_possible_date_to_start_ui <- june_expiration_date_first - 7*weeks_before
june_first_possible_date_to_end_ui <-  june_expiration_date_last + 7*weeks_after

cust_ui_thru_sept_2021 <-
  df_ui_cust_spell %>%
  inner_join(cust_separate_date, by = "cust_number") %>%
  rename(first_ui_week = start_ui_date) %>%
  #only include customers who exit UI 9/30/2021 or later
  filter(ui_spell_number>=1,
         exit_ui_date >= sep_first_possible_date_to_end_ui,
         first_ui_week <= sep_last_possible_date_to_start_ui,
         cust_state %in% states_puc_sep) %>% 
  mutate(group = "UI through Sept 2021")

cust_ui_thru_jul_2021 <-
  df_ui_cust_spell %>%
  inner_join(cust_separate_date, by = "cust_number") %>%
  rename(first_ui_week = start_ui_date) %>%
  #only include customers who exit UI 7/31/2021 or later
  filter(ui_spell_number>=1,
         exit_ui_date >= june_first_possible_date_to_end_ui,
         first_ui_week <= june_last_possible_date_to_start_ui,
         cust_state %in% states_puc_jun) %>% 
  mutate(group = "UI through July 2021")

##create list of non-UI (ie, employed) customers
#first, get list of non-UI 2020/21 customers who experience job separation in our analysis frame

max_month_df_nonui_2020_cust_week <- df_nonui_2020_cust_week %>%
  pull(week_start_date) %>%
  max() %>%
  floor_date(unit = "month")

df_nonui2020_w_job_sep_in_sample_frame <- 
  df_nonui_2020_cust_week %>% 
  #define job separations as exit_labor = 1, except in (a) max_month of data frame
  # (In max_month, exit_labor = 1 likely indicates that the panel ended, not that the cust lost their job)
  # or (b) months outside of analysis frame. If max_month is outside of the analysis
  #...frame, then only the analysis frame screen will bind
  dplyr::filter(floor_date(week_start_date, unit = "month") != max_month_df_nonui_2020_cust_week,
                floor_date(week_start_date - 1, unit = "month") != floor_date(max_month_df_nonui_2020_cust_week - 1, unit = "month"),
                floor_date(week_start_date, unit = "month") <= max_month_analysis_frame) %>% 
  filter(exit_labor == 1) %>%
  group_by(cust_number) %>% 
  filter(row_number() == 1) %>%
  ungroup() %>%
  transmute(cust_number, sep_date = week_start_date)
test_that("Nrow df_nonui2020_w_job_sep_in_sample_frame",
          expect_equal(df_nonui2020_w_job_sep_in_sample_frame %>% nrow(), 
                       ifelse(small_samp==TRUE, 1367, 85909 )))

count_df_nonui_2020 <- df_nonui2020_w_job_sep_in_sample_frame %>%
  count(sep_date)

##then, get list of nonui customers who don't experience job loss. This is the employed control group
cust_nonui <- 
  df_nonui_clean %>%
  distinct(cust_number) %>%
  #filter to non-UI customers who do not experience a job separation in 2020
  anti_join(df_nonui2020_w_job_sep_in_sample_frame, by = "cust_number") %>% 
  mutate(group = "Employed") %>%
  as_tibble()
test_that("Nrow cust_nonui",
          expect_equal(cust_nonui %>% nrow(), 
                       ifelse(small_samp==TRUE, 1495, 78437 )))

## get list of customers who get LWA
cust_get_lwa <- 
  df_lwa_cust_week_src %>%
  group_by(cust_number) %>%
  filter(max(lwa) > 0) %>% 
  ungroup() %>% 
  distinct(cust_number, cust_state) %>% 
  #check for customers who appear in multiple states
  group_by(cust_number) %>% 
  mutate(ct = n()) %>% 
  ungroup()
#drop small number customers who appear in multiple states
cust_get_lwa <-
  cust_get_lwa %>% 
  dplyr::filter(ct == 1) %>% 
  select(-ct)

# build monthly df to show comparison of unemployed vs employed throughout the pandemic (fact 1)----
## calculate income quintiles based on 2019 income
customer_inc_quintiles <- 
  df_demo_src_wins %>%
  dplyr::filter(between(periodid, 201901, 201912)) %>% 
  mutate(inflows_ex_transfers = total_inflows - transfer_inflows) %>% 
  #drop customer-periodid duplicates (possible because a customer can have multiple types, eg, '202021_ui_recipient' and '2019_nonui')
  distinct(cust_number, periodid, .keep_all = TRUE) %>% 
  group_by(cust_number) %>% 
  summarise(inc_2019 = sum(inflows_ex_transfers)) %>% 
  mutate(inc_2019_quintile = cut_number(inc_2019, n=5))

#join 2019 income and EIP receipt information to monthly customer-level df
test_that("Ncust df_demo_src_wins",
          expect_equal(df_demo_src_wins %>% 
                         distinct(cust_number) %>% 
                         nrow(), 
                       ifelse(small_samp==TRUE, 19647, 1170706)))
df_demo_src_wins_touse <- 
  df_demo_src_wins %>% 
  left_join(customer_inc_quintiles, by = 'cust_number') %>% 
  left_join(cust_eip_touse, by = 'cust_number') %>% 
  left_join(cust_eip_rounds_touse, by = 'cust_number') %>%
  rename(total_spend_cardcash = total_spend_narrow)
#tiny number of rows has null 2019 income -- drop these
test_that("df_demo_src_wins_touse non-NA",
          expect_equal(df_demo_src_wins_touse %>% 
                         dplyr::filter(is.na(inc_2019_quintile)) %>% 
                         nrow(), ifelse(small_samp==TRUE, 0, 754))) # KZ: no NAs here?
df_demo_src_wins_touse <- df_demo_src_wins_touse %>% dplyr::filter(!is.na(inc_2019_quintile))
test_that("Ncust df_demo_src_wins_touse",
          expect_equal(df_demo_src_wins_touse %>% 
                         distinct(cust_number) %>% 
                         nrow(), 
                       ifelse(small_samp==TRUE, 19647, 1170650)))

#Drop customers with multiple EIPs for rounds 1 or 2 (3 is outside current sample frame) 
#(code in build file drops multiple EIP1 recipients from cust_eip_touse but then
#...they just show up as NA when left join)
df_demo_src_wins_touse <-
  df_demo_src_wins_touse %>%
  dplyr::filter(is.na(eip1_txn_count) | eip1_txn_count <=1,
                is.na(eip2_txn_count) | eip2_txn_count <=1)
test_that("Ncust df_demo_src_wins_touse",
          expect_equal(df_demo_src_wins_touse %>% 
                         distinct(cust_number) %>% 
                         nrow(), 
                       ifelse(small_samp==TRUE, 19126, 1139473)))

#Drop unusual EIP weeks to ease matching and make sure these are real EIPs
tot_obs <- df_demo_src_wins_touse %>% nrow()

test_that('EIP1 filter keeps most observations',
          expect_equal(round(df_demo_src_wins_touse %>% 
                         dplyr::filter(is.na(eip_week) | eip_week %in% eip1_week_list) %>%
                                         nrow() / tot_obs, 2), 1))
test_that('EIP2 filter, when used, keeps most observations',
          expect_equal(round(df_demo_src_wins_touse %>% 
                               dplyr::filter(is.na(eip2_week) | eip2_week %in% eip2_week_list) %>%
                               nrow() / tot_obs, 2), 0.98))
rm(tot_obs)

#Version for full time-series filters just on EIP1 weeks. We later filter on EIP2 weeks too when analyzing MPC out of $300 onset.
df_demo_src_wins_touse_eipwks <-
  df_demo_src_wins_touse %>%
  dplyr::filter(is.na(eip_week) | eip_week %in% eip1_week_list )

test_that("Ncust df_demo_src_wins_touse",
          expect_equal(df_demo_src_wins_touse_eipwks %>% 
                         distinct(cust_number) %>% nrow(), 
                       ifelse(small_samp==TRUE, 19126, 1139473)))

df_demo_src_wins_touse_eipwks_summer <- df_demo_src_wins_touse %>%
  filter(is.na(eip_week) | eip_week %in% eip1_week_list ) %>%
  filter(is.na(eip_week) | eip2_week %in% eip2_week_list,
         is.na(eip_week) | eip3_week %in% eip3_week_list)

make_employed_vs_unemployed_collapsed_df_weighted <- 
  function(unemp_cust_list, 
           matching_vars = c("inc_2019_quintile", "eip_week"),
           ...){
    
    ##step 1: get list of customers to include in analysis
    combined_cust_list <-
      bind_rows(cust_nonui ,
                unemp_cust_list %>% 
                  mutate(group = "Unemployed") %>%
                  select(cust_number, group) %>%  distinct()) %>%
      mutate(group = ifelse(group == 'Employed', 'control', 'treatment'))
    
    ##step 2: compute inc quintile-by-eip date weights: we want to weight the control group
    ##        so that it matches the treatment group in terms of (inc quintile-eip date).
    ##        This function uses tidy evaluation (...) to be flexible around the matching variables
    weights_to_use <-
      df_demo_src_wins_touse_eipwks %>%
      #get a customer-by-inc quintile-by-eip date df
      distinct(cust_number, ...) %>%
      inner_join(combined_cust_list, by = "cust_number") %>%
      group_by(..., group) %>%
      summarise(ct = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = 'group', values_from = 'ct') %>%
      mutate(weight = treatment/control) %>%
      replace_na(list(control = 0,
                      treatment = 0,
                      weight = 0))
    
    weights_to_use <- weights_to_use %>% select(-treatment, -control) #deselect unnecessary colns
    
    ##step 3: get cust-by-month df, with weights
    df_to_collapse <-
      df_demo_src_wins_touse_eipwks %>%
      dplyr::filter(periodid >= 201901) %>%
      inner_join(combined_cust_list, by = 'cust_number') %>%
      #do a distinct -- there are a few duplicates because customers can have multiple cust_type
      distinct(cust_number, periodid, .keep_all = TRUE) %>%
      left_join(weights_to_use, by = matching_vars) %>%
      #weight should be 1 for treatment group
      mutate(weight = ifelse(group == 'treatment', 1, weight)) %>%
      {
        if (run_categories)
          select(.,
                 cust_number, cust_state, group, periodid, weight,
                 total_outflows,
                 total_spend_cardcash, cats_vector, uncatergorized_spend_cardcash, uncatergorized_spend,
                 total_spend_expanded,
                 total_inflows, transfer_inflows,
                 total_inflows,
                 transfer_inflows,
                 labor_inflows,
                 tax_refund_inflows,
                 outflows_ex_transfers, checking_acct_balance) %>% 
          mutate(income = total_inflows - transfer_inflows,
                 chk_bal = checking_acct_balance) %>% 
          select(-c(total_inflows, transfer_inflows, checking_acct_balance))
        else
          transmute(
            .,
            cust_number, cust_state, group, periodid, weight,
            total_outflows, total_spend_cardcash, total_spend_expanded,
            income = total_inflows - transfer_inflows,
            total_inflows, transfer_inflows, labor_inflows,
            tax_refund_inflows, outflows_ex_transfers,
            chk_bal = checking_acct_balance
          )
      } %>%
      pivot_longer(-one_of(c('cust_number', 'cust_state', 'group', 'periodid', 'weight')),
                   names_to = "category")
    
    ##step 4: collapse to group-by-periodid level
    df_collapsed <-
      df_to_collapse %>%
      group_by(group, periodid, category) %>%
      summarise(mean = weighted.mean(value, weight),
                median = xtile_ten(value, weight),
                ct_cust = n()) %>%
      ungroup() %>%
      pivot_longer(cols = c("mean", "median"),
                   names_to = "measure",
                   values_to = "value") %>%
      mutate(group = ifelse(group == 'treatment', 'unemployed', 'employed')) %>%
      select(periodid, category, group, measure, value, ct_cust) %>%
      arrange(periodid, category, group, measure)
    
    if(!run_categories){
      test_that('N rows (should be 2 groups x 32 periods x 10 names x 2 measures = 1040)',
              expect_equal(nrow(df_collapsed), 1440))} 
    
    return(df_collapsed)
  }

#this is the same as the other function but does the collapse conditional on ui_inflows>0
make_employed_vs_unemployed_collapsed_df_weighted_puc <- 
  function(unemp_cust_list,puc_group, 
           matching_vars = c("inc_2019_quintile", "eip_week"),
           ...){
    
    ##step 1: get list of customers to include in analysis
    combined_cust_list <-
      bind_rows(cust_nonui ,
                unemp_cust_list %>% 
                  mutate(group = "Unemployed") %>%
                  select(cust_number, group) %>%  distinct()) %>%
      mutate(group = ifelse(group == 'Employed', 'control', 'treatment'))
    
    ##step 2: compute inc quintile-by-eip date weights: we want to weight the control group
    ##        so that it matches the treatment group in terms of (inc quintile-eip date).
    ##        This function uses tidy evaluation (...) to be flexible around the matching variables
    weights_to_use <-
      df_demo_src_wins_touse_eipwks_summer %>%
      #get a customer-by-inc quintile-by-eip date df
      distinct(cust_number, ...) %>%
      inner_join(combined_cust_list, by = "cust_number") %>%
      group_by(..., group) %>%
      summarise(ct = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = 'group', values_from = 'ct') %>%
      mutate(weight = treatment/control) %>%
      replace_na(list(control = 0,
                      treatment = 0,
                      weight = 0))
    
    ##step 3: get cust-by-month df, with weights
    df_to_collapse <-
      df_demo_src_wins_touse_eipwks_summer %>%
      dplyr::filter(periodid >= 201901) %>%
      inner_join(combined_cust_list, by = 'cust_number') %>%
      filter(ui_inflows > 0 |
               cust_type == "nonui_2020" | periodid <= 202003) %>%
      #do a distinct -- there are a few duplicates because customers can have multiple cust_type
      distinct(cust_number, periodid, .keep_all = TRUE) %>%
      left_join(weights_to_use, by = matching_vars) %>%
      #weight should be 1 for treatment group
      #mutate(weight = ifelse(group == 'treatment', re_weight, weight_re_weight)) %>%
      mutate(weight = ifelse(group == 'treatment', 1, weight)) %>%
      {
        if (run_categories)
          select(.,
                 cust_number, cust_state, group, periodid, weight,
                 transfer_inflows, total_inflows,
                 total_spend_cardcash, cats_vector, uncatergorized_spend_cardcash, uncatergorized_spend,
                 total_spend_expanded,
                 checking_acct_balance) %>% 
          mutate(income = total_inflows - transfer_inflows,
                 chk_bal = checking_acct_balance) %>%
          select(-c(total_inflows, transfer_inflows, checking_acct_balance))
        else
          transmute(
            .,
            cust_number, cust_state, group, periodid, weight,
            total_spend_cardcash, total_spend_expanded,
            income = total_inflows - transfer_inflows,
            chk_bal = checking_acct_balance
          )
      } %>%
      pivot_longer(-one_of(c('cust_number', 'cust_state', 'group', 'periodid', 'weight')), 
                   names_to = "category")
    
    ##step 4: collapse to group-by-periodid level
    df_collapsed <-
      df_to_collapse %>%
      group_by(group, periodid, category) %>%
      summarise(mean = weighted.mean(value, weight),
                ct_cust = n()) %>%
      ungroup() %>%
      pivot_longer(cols = c("mean"),
                   names_to = "measure",
                   values_to = "value") %>%
      mutate(group = ifelse(group == 'treatment', 'unemployed', 'employed')) %>%
      select(periodid, category, group, measure, value, ct_cust) %>%
      arrange(periodid, category, group, measure)
    
    return(df_collapsed)
  }

cust_thru_feb_first_weeks <-
  cust_thru_feb %>%
  # add additional filter statement that uses only states that get FPUC by the end of august
  #filter(cust_state %in% states_puc_aug) %>%
  #Filter to losing job right at end of March or early April, and starting benefits soon after
  #This creates closest conceptual alignment with model timing 
  dplyr::filter(job_sep_date %in% c(as.Date('2020-03-29'), as.Date('2020-04-05'))) %>%
  dplyr::filter(first_ui_week %in% c(as.Date('2020-04-05'), as.Date('2020-04-12'))) %>%
  distinct(cust_number)
test_that("Nrow cust_thru_feb_first_weeks",
          expect_equal(cust_thru_feb_first_weeks %>% nrow(), 
                       ifelse(small_samp==TRUE, 138, 6567)) )

write_csv(bind_rows(cust_nonui,
                    cust_thru_feb_first_weeks %>% mutate(group = "Unemployed")),
          str_c(data_path, "tmp/spend/", "unemp_vs_emp_cust_list.csv"))

# This df makes figure 1
df_monthly_collapsed_thru_feb <-
  make_employed_vs_unemployed_collapsed_df_weighted(cust_thru_feb_first_weeks,
                                                    matching_vars = c("inc_2019_quintile",
                                                                      "eip_week"),
                                                    inc_2019_quintile,
                                                    eip_week)

df_monthly_thru_sept_2021_states_puc_sep <-
  make_employed_vs_unemployed_collapsed_df_weighted_puc(cust_ui_thru_sept_2021,
                                                        'sep_exp_state',
                                                        matching_vars = c("inc_2019_quintile",
                                                                          "eip_week",
                                                                          "eip2_week",
                                                                          "eip3_week"),
                                                        inc_2019_quintile,
                                                        eip_week,
                                                        eip2_week,
                                                        eip3_week)

df_monthly_thru_jul_2021_states_puc_jun <-
  make_employed_vs_unemployed_collapsed_df_weighted_puc(cust_ui_thru_jul_2021,
                                                        'jun_exp_state',
                                                        matching_vars = c("inc_2019_quintile",
                                                                          "eip_week",
                                                                          "eip2_week",
                                                                          "eip3_week"),
                                                        inc_2019_quintile,
                                                        eip_week,
                                                        eip2_week,
                                                        eip3_week)

## Repeat for waiting group 
cust_thru_feb_waiting <- 
  cust_thru_feb %>% 
  dplyr::filter(job_sep_date %in% c(as.Date('2020-03-29'), as.Date('2020-04-05'))) %>%
  dplyr::filter(first_ui_week %in% c(as.Date('2020-05-31'))) %>%
  distinct(cust_number)
test_that("Nrow cust_thru_feb_waiting",
          expect_equal(cust_thru_feb_waiting %>% nrow(), 
                       ifelse(small_samp==TRUE, 10, 545)) )

df_monthly_collapsed_waiting_all <-
  make_employed_vs_unemployed_collapsed_df_weighted(cust_thru_feb_waiting,
                                                    matching_vars = c("inc_2019_quintile",
                                                                      "eip_week"),
                                                    inc_2019_quintile,
                                                    eip_week)

df_monthly_collapsed_waiting <-
  df_monthly_collapsed_waiting_all %>%
  filter(group == 'unemployed' ) %>%
  mutate(group = 'waiting')

#build weekly df for waiting design (fact 2) ----
## Here, we are building a df at the week-by-UI start date level. 
## We use this df to compare spending and inflows among people who had a job separation around the start of Covid,
## but began receiving UI at different dates.
df_ui_onset <- ##takes 1.5 min
  df_ui_clean %>% 
  select(-first_ui_week) %>% 
  dplyr::filter(between(week_start_date, as.Date("2020-01-05"), as.Date("2020-05-24"))) %>% 
  arrange(cust_number, week_start_date) %>% 
  #inner join with people who get UI at covid onset 
  inner_join(cust_ui_onset %>% dplyr::filter(ui_spell_number == 1), by = "cust_number") %>%
  select(cust_number, week_start_date, first_ui_week, 
         contains('mainyear')) %>% 
  #drop duplicates -- occasionally df_ui_clean is not unique at the cust-week level (unclear why)
  distinct() %>% 
  #balance panel by cust-week, and fill missing cust-weeks with 0
  complete(week_start_date, nesting(cust_number, first_ui_week)) %>% 
  replace(is.na(.), 0)

df_ui_onset_collapsed <- 
  df_ui_onset %>% 
  ungroup() %>% 
  mutate(inflows_ex_transfers = total_inflows_mainyear - transfer_inflows_mainyear) %>% 
  #get mean value by week-by-first UI week 
  group_by(week_start_date, first_ui_week) %>% 
  summarise_at(vars(total_deposit_outflows_mainyear,
                    spend_mainyear, spend_expanded_mainyear, 
                    transfers_mainyear, ui_inflows_mainyear, tax_refund_inflows_mainyear,
                    total_inflows_mainyear,
                    inflows_ex_transfers,
                    labor_inflows_mainyear), mean) %>% 
  #also, get cust count by week-first ui week
  left_join(df_ui_onset %>% ungroup() %>% 
              group_by(week_start_date, first_ui_week) %>% 
              summarise(ct_cust = n()),
            by = c('week_start_date', 'first_ui_week')) %>% 
  ungroup() %>% 
  mutate(first_ui_week_dt = as.Date(first_ui_week),
         first_ui_week = as.factor(first_ui_week))

#build weekly df for analysis of $600 expiration (fact 3) ----
## Here, we are building a customer-by-week df to examine the impact of the $600 supplement expiring at the end of July 2020
## We compare unemployed to employed, and match on income quintile and EIP receipt date
#build weekly df of employed to compare against unemployed
df_employed_collapsed_expiration <-
  df_nonui_clean %>% 
  dplyr::filter(week_start_date >= as.Date('2020-05-31')) %>% 
  inner_join(cust_nonui, by = 'cust_number') %>% 
  left_join(customer_inc_quintiles, by = 'cust_number') %>% 
  left_join(cust_eip_touse, by = 'cust_number') %>% 
  select(cust_number, inc_2019_quintile, eip_week, week_start_date, contains('mainyear')) %>% 
  dplyr::filter(!is.na(inc_2019_quintile),
                eip_week %in% eip1_week_list) %>% 
  #drop duplicates
  distinct() %>%
  #balance panel by cust-week (we need to balance by cust week-inc quintile-eip week 
  # to preserve the inc quintile and eip week columns)
  complete(week_start_date, nesting(cust_number, inc_2019_quintile, eip_week)) %>% 
  replace_na(list(total_inflows_mainyear = 0,
                  transfer_inflows_mainyear = 0,
                  ui_inflows_mainyear = 0,
                  spend_mainyear = 0,
                  spend_expanded_mainyear = 0)) %>%
  #collapse employed at the week-by-inc quintile-by-eip week level
  group_by(week_start_date, inc_2019_quintile, eip_week) %>% 
  summarise(income_Employed = mean(total_inflows_mainyear - transfer_inflows_mainyear),
            ui_Employed = mean(ui_inflows_mainyear),
            spend_cardcash_Employed = mean(spend_mainyear),
            spend_inclusive_Employed = mean(spend_expanded_mainyear),
            n_cust = n()) %>% 
  ungroup()
test_that('Min cell size of collapsed employed data', 
          expect_equal(df_employed_collapsed_expiration %>% pull(n_cust) %>% min(),  ifelse(small_samp==TRUE, 1, 103))) 
#join employed control to unemployed
df_weekly_ui_expiration <- 
  df_ui_clean %>%
  inner_join(cust_thru_aug %>% select('cust_number') %>% distinct(), by = 'cust_number') %>%  
  #only include weeks after 5/31, since we want to focus on the period around PUC expiration
  dplyr::filter(week_start_date >= as.Date('2020-05-31')) %>% 
  #join inc quintile and eip info, and then join employed control
  left_join(customer_inc_quintiles, by = 'cust_number') %>% 
  left_join(cust_eip_touse, by = 'cust_number') %>% 
  dplyr::filter(!is.na(inc_2019_quintile),
                eip_week %in% eip1_week_list) %>% 
  select(cust_number, inc_2019_quintile, eip_week, week_start_date, contains('mainyear')) %>% 
  #drop duplicates
  distinct() %>%
  #balance panel by cust-week (we need to balance by cust week-inc quintile-eip week 
  # to preserve the inc quintile and eip week columns)
  complete(week_start_date, nesting(cust_number, inc_2019_quintile, eip_week)) %>% 
  replace_na(list(total_inflows_mainyear = 0,
                  transfer_inflows_mainyear = 0,
                  ui_inflows_mainyear = 0,
                  spend_mainyear = 0,
                  spend_expanded_mainyear = 0)) %>%
  left_join(df_employed_collapsed_expiration, by = c('week_start_date', 'inc_2019_quintile', 'eip_week')) %>% 
  mutate(income_Unemployed = total_inflows_mainyear - transfer_inflows_mainyear,
         ui_Unemployed = ui_inflows_mainyear,
         spend_cardcash_Unemployed = spend_mainyear,
         spend_inclusive_Unemployed = spend_expanded_mainyear) %>%
  #drop inc_2019_quintile and eip_week, now that we have done the employed-unemployed matching
  select(cust_number, week_start_date, contains('_Employed'), contains('_Unemployed')) %>%
  #convert to long form: cust-by-week-by-cust type-by-category
  pivot_longer(-one_of(c('cust_number', 'week_start_date')),
               names_to = c('category', 'group'),
               names_pattern = "(.*)_(Unemployed|Employed)")
#collapse to week-category-group level for plotting
df_weekly_ui_expiration_collapsed <-
  df_weekly_ui_expiration %>% 
  group_by(week_start_date, category, group) %>% 
  summarise(value = mean(value), ct_cust = n()) %>% 
  ungroup()

test_that('Median UI inflow amount',
          expect_equal(df_weekly_ui_expiration_collapsed %>%
                         filter(category == "ui",
                                group == "Unemployed") %>%
                         pull(value) %>%
                         median(),
                       ifelse(small_samp==TRUE, 416, 288),
                       tol = 0.01))

#build df for LWA analysis (fact 4) ----
## Here, we build a df to analyze the impact of LWA (Lost Wages Assistance), the federal program 
## that paid out $400/week in UI supplement in Aug-Oct. The timing of LWA payout was different 
## across states; our ID strategy uses NJ as a "control" because it paid out LWA in late Oct,
## and uses several other states that paid out LWA mostly in Sep as the "treatment".
#get LWA start and end dates by state
lwa_dates_by_state <-
  df_lwa_cust_week_src %>%
  dplyr::filter(lwa > 0) %>%
  ungroup() %>%
  count(cust_state, week_start_date) %>%
  group_by(cust_state) %>%
  mutate(share = round(n/sum(n), 2)) %>%
  #for each state's LWA dates, choose weeks with a reasonable share of total LWA in the state (> 2%)
  dplyr::filter(share > 0.02) %>%
  summarise(lwa_start_date = min(week_start_date), lwa_end_date = max(week_start_date)) %>%
  ungroup() %>%
  #manually enter IN dates based on what we found in transaction data
  dplyr::filter(cust_state != "IN") %>%
  add_row(cust_state = "IN", lwa_start_date = as.Date("2020-09-20"), lwa_end_date = as.Date("2020-09-22"))

#get customer-by-week df of LWA payments
lwa_by_week <-
  df_lwa_cust_week_src %>%
  #inner join with the set of LWA customers we want to use (no state dups)
  inner_join(cust_get_lwa) %>% 
  select(week_start_date, cust_number, lwa, ui_inflows_mainyear)
#create customer-by-week df for analysis of LWA: we use NJ as a control group b/c it gives out LWA late
state_treatment_control_lwa <- 
  df_ui_clean %>% 
  select(cust_number, week_start_date,
         spend_mainyear, spend_expanded_mainyear, total_inflows_mainyear, total_deposit_outflows_mainyear, transfer_inflows_mainyear) %>% 
  #distinct operation: there are a few duplicates
  distinct() %>% 
  inner_join(cust_get_lwa, by = 'cust_number') %>% 
  left_join(lwa_by_week, by = c('cust_number', 'week_start_date', 'cust_state')) %>% 
  mutate(group = ifelse(cust_state == 'NJ', 'Control (receive in October)', 'Treatment (receive in September)'),
         group = ordered(group, levels = c("Treatment (receive in September)", "Control (receive in October)"))) %>% 
  #null LWA weeks mean no LWA detected - fill with 0
  mutate(lwa = ifelse(is.na(lwa), 0, lwa)) %>% 
  #filter out CA, because it pays LWA gradually over 6 weeks
  dplyr::filter(cust_state != 'CA',
                #only include dates around LWA receipt
                between(week_start_date, as.Date('2020-07-26'), as.Date('2020-10-19'))) %>% 
  select(cust_number, cust_state, group, week_start_date, spend_mainyear, spend_expanded_mainyear, lwa, total_inflows_mainyear,
         total_deposit_outflows_mainyear, transfer_inflows_mainyear, ui_inflows_mainyear) %>% 
  complete(week_start_date, nesting(cust_number, group, cust_state)) %>% 
  replace(is.na(.), 0)
  
#collapse to week-by-group (treatment/control) level, for plotting
state_treatment_control_lwa_collapsed <- 
  state_treatment_control_lwa %>% 
  mutate(Income = total_inflows_mainyear - transfer_inflows_mainyear) %>% 
  group_by(group, week_start_date) %>%
  summarise_at(vars(spend_mainyear, spend_expanded_mainyear, lwa, Income), mean, na.rm = TRUE) %>% 
  rename(`Benefit supplement` = lwa, 
         `Spending (card and cash)` = spend_mainyear, 
         `Spending (total)` = spend_expanded_mainyear) %>%
  pivot_longer(c("Benefit supplement", "Spending (card and cash)", "Spending (total)", "Income")) %>%
  left_join(state_treatment_control_lwa %>% 
              group_by(group, week_start_date) %>% 
              summarise(ct_cust = n()),
            by = c('group', 'week_start_date')) %>% 
  group_by(group, name) %>%
  mutate(value_diff = value - max(value * (week_start_date == as.Date("2020-08-16"))),
         value_ratio = value / max(value * (week_start_date == as.Date("2020-08-16")))) %>% 
  ungroup() %>% 
  mutate(name = ordered(name, levels = c("Benefit supplement", "Income", "Spending (card and cash)", "Spending (total)") ))

#build weekly df for analysis of $300 onset (fact 5) ----
df_employed_collapsed_onset <-
  df_nonui_clean %>% 
  dplyr::filter(week_start_date >= as.Date('2020-11-01'),
                week_start_date < as.Date('2021-09-05')) %>% 
  inner_join(cust_nonui, by = 'cust_number') %>% 
  left_join(customer_inc_quintiles, by = 'cust_number') %>% 
  left_join(cust_eip_touse, by = 'cust_number') %>% 
  left_join(cust_eip_rounds_touse, by = 'cust_number') %>%
  select(cust_number, inc_2019_quintile, contains("_week"), week_start_date, contains('mainyear')) %>% 
  dplyr::filter(is.na(eip_week) | eip_week %in% eip1_week_list,
                is.na(eip2_week) | eip2_week %in% eip2_week_list,
                !is.na(inc_2019_quintile)) %>%
  #drop duplicates
  distinct() %>%
  complete(week_start_date, nesting(cust_number, inc_2019_quintile, eip_week, eip2_week)) %>% 
  replace_na(list(total_inflows_mainyear = 0,
                  transfer_inflows_mainyear = 0,
                  ui_inflows_mainyear = 0,
                  spend_mainyear = 0,
                  spend_expanded_mainyear = 0)) %>%
  #collapse employed at the week-by-inc quintile-by-eip week level
  group_by(week_start_date, inc_2019_quintile, eip_week, eip2_week) %>% 
  summarise(income_Employed = mean(total_inflows_mainyear - transfer_inflows_mainyear),
            ui_Employed = mean(ui_inflows_mainyear),
            spend_cardcash_Employed = mean(spend_mainyear),
            spend_inclusive_Employed = mean(spend_expanded_mainyear),
            n_cust = n()) %>% 
  ungroup()
test_that('Min cell size of collapsed employed data (onset)', 
          expect_equal(df_employed_collapsed_onset %>% pull(n_cust) %>% min(), 1))  

df_weekly_ui_onset <- 
  df_ui_clean %>%
  inner_join(cust_thru_jan %>% select('cust_number') %>% distinct(), by = 'cust_number') %>% 
  #only include weeks after 11/01, since we want to focus on the period around onset of $300 
  dplyr::filter(week_start_date >= as.Date('2020-11-01'),
                week_start_date < as.Date('2021-09-05')) %>% 
  #join inc quintile and eip info, and then join employed control
  left_join(customer_inc_quintiles, by = 'cust_number') %>%
  left_join(cust_eip_touse, by = 'cust_number') %>% 
  left_join(cust_eip_rounds_touse, by = 'cust_number') %>% 
  select(cust_number, inc_2019_quintile, contains("_week"), week_start_date, contains('mainyear')) %>% 
  dplyr::filter(is.na(eip_week) | eip_week %in% eip1_week_list,
                is.na(eip2_week) | eip2_week %in% eip2_week_list,
                !is.na(inc_2019_quintile)) %>%
  #drop duplicates
  distinct() %>%
  #balance panel by cust-week (we need to balance by cust week-inc quintile-eip week 
  # to preserve the inc quintile and eip week columns)
  complete(week_start_date, nesting(cust_number, inc_2019_quintile, eip_week, eip2_week)) %>% 
  replace_na(list(total_inflows_mainyear = 0,
                  transfer_inflows_mainyear = 0,
                  ui_inflows_mainyear = 0,
                  spend_mainyear = 0,
                  spend_expanded_mainyear = 0)) %>%
  left_join(df_employed_collapsed_onset, by = c('week_start_date', 'inc_2019_quintile', 'eip_week', 'eip2_week')) %>% 
  mutate(income_Unemployed = total_inflows_mainyear - transfer_inflows_mainyear,
         ui_Unemployed = ui_inflows_mainyear,
         spend_cardcash_Unemployed = spend_mainyear,
         spend_inclusive_Unemployed = spend_expanded_mainyear) %>%
  #drop inc_2019_quintile and eip_week, now that we have done the employed-unemployed matching
  select(cust_number, week_start_date, contains('_Employed'), contains('_Unemployed')) %>%
  #convert to long form: cust-by-week-by-cust type-by-category
  pivot_longer(-one_of(c('cust_number', 'week_start_date')),
               names_to = c('category', 'group'),
               names_pattern = "(.*)_(Unemployed|Employed)")

#collapse to week-category-group level for plotting
df_weekly_ui_onset_collapsed <-
  df_weekly_ui_onset %>% 
  group_by(week_start_date, category, group) %>% 
  summarise(value = mean(value, na.rm = TRUE), ct_cust = n()) %>% 
  ungroup()

## Matched data for model ####
df_monthly_collapsed_combined <-
  bind_rows(df_monthly_collapsed_thru_feb,
            df_monthly_collapsed_waiting) %>%
  ungroup() %>%
  filter(periodid <= sample_frame_end_periodid) %>%
  arrange(periodid, category, group, measure)

label_for_unemployed_group <- c('Unemployed (get benefits from April 2020 through February 2021)')
label_for_waiting_group <- c('Waiting (get benefits June 2020 through December 2021)')

matched_model_data_table <- 
  df_monthly_collapsed_combined %>%
  mutate(date = ymd(periodid, truncated = 1)) %>% 
  left_join(df_monthly_collapsed_combined %>% dplyr::filter(periodid == 202001) %>% 
              transmute(group, 
                        category,
                        measure,
                        value_jan2020 = value),
            by = c('group', 'category', 'measure')
  ) %>% 
  mutate(percent_change = value/value_jan2020 - 1,
         category = case_when(category == 'income' ~ 'Income',
                              category == 'total_inflows' ~ 'Total inflows',
                              category == 'transfer_inflows' ~ 'Transfer inflows',
                              category == 'labor_inflows' ~ 'Labor inflows',
                              category == 'tax_refund_inflows' ~ 'Tax refund inflows',
                              category == 'total_spend_cardcash' ~ 'Spending (card and cash)',
                              category == 'total_spend_expanded' ~ 'Spending (total)',
                              category == 'outflows_ex_transfers' ~ 'Outflows ex transfers',
                              category == 'total_outflows' ~ 'Total outflows',
                              category == 'checking_acct_balance' ~ 'Checking account balance',
                              TRUE ~ category),
         group = ifelse(group == 'unemployed', label_for_unemployed_group,
                        ifelse(group == 'waiting', label_for_waiting_group, 'Employed'))) %>%
                        {
                          if (run_categories)
                            mutate(.,
                                   name = ordered(
                                     category,
                                     levels = c('Income', 'Total inflows', 'Transfer inflows',
                                                'Labor inflows', 'Tax refund inflows', 'Spending (total)',
                                                'Spending (card and cash)', 'Outflows ex transfers',
                                                'Checking account balance', 'Total outflows', cats_vector,
                                                "uncatergorized_spend_cardcash", "uncatergorized_spend")
                                   ))
                          else
                            mutate(.,
                                   name = ordered(
                                     category,
                                     levels = c('Income', 'Total inflows', 'Transfer inflows',
                                                'Labor inflows', 'Tax refund inflows', 'Spending (total)',
                                                'Spending (card and cash)', 'Outflows ex transfers',
                                                'Checking account balance', 'Total outflows')
                                   ))
                        } %>% 
  mutate(group = ordered(group, 
                         levels = c(label_for_unemployed_group,
                                    label_for_waiting_group,'Employed')))


matched_model_data_table %>% write_csv(csv_data_for_modeling_matched)

test_that("are all dfs there", 
          expect_equal(all(out_dfs %>% map_lgl(exists)), TRUE))


Sys.time() - tm_full 


