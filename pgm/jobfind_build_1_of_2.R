# jobfind_build_1_of_2.R
# Author: Peter Ganong
# this creates two cust-week level data frames: df_labor_cust_week and
# df_ui_cust_week_add_spells

#INS
##must run ui_eip_data_build.R first

#OUTS
# df_labor_cust_week: customer-by-week level df of customers who received UI and see if in each week whether 
#                     they experienced a job separation, started a new job, or were recalled to a job 
#                     (from before they got UI), or if they experienced none of these three things.
# df_ui_cust_week_add_spells: this feeds into df_ui_cust_week (built in jobfind_build_2_of_2)
# df_ui_cust_week_alt_horizon_basic: this feeds into df_ui_cust_week_alt_horizon and compares various lengths of job separation. 

weeks_without_ui_weekly <- 3 #consecutive weeks without UI required to designate end of UI spell, in states that pay UI weekly
weeks_without_ui_fortnightly <- 4 #same, in states that pay UI fortnightly
weeks_without_labor <- 5 #consecutive weeks without labor income required to designate a job separation
fortnight_states <- c("CA", "FL", "TX", "CO", "MI", "IL") #states that pay UI fortnightly
run_build <- TRUE #whether to run the build below. Because all the relevant dfs are written to HDFS, you can set this to False 
#and simply read in the dfs in jobfind_build_2_of_2.R rather than re-running this script

tm_full <- Sys.time()

if(!exists("df_cp")) {
  # make it possible to start running from jobfind_1_of_1 without
  # first running ui_eip_data_read_in.R and ui_eip_data_build.R
  date_scratch_read_in <- "2023-06-14"
  df_cp <- readRDS(str_c(data_path, "tmp/read_in/", date_scratch_read_in, "df_cp.rds"))
}

if(run_build == TRUE){
  # build code ----

  cust_n_states_w_ui <-
    df_cp %>% 
    dplyr::filter(inflow_type %in% c("normal_ui", "PUA")) %>%
    group_by(cust_number) %>%
    summarise(n_states = n_distinct(at_counterparty_raw))
  
  #check: a small share of customers have get UI from multiple states
  test_that("Nrow cust_n_states_w_ui",
            expect_equal(nrow(cust_n_states_w_ui), 
                         ifelse(small_samp == TRUE, 14535, 875547)))
  test_that("Ncust in cust_n_states_w_ui only in 1 states",
            expect_equal(cust_n_states_w_ui %>% 
                           dplyr::filter(n_states == 1) %>% 
                           nrow(), 
                         ifelse(small_samp == TRUE, 14418, 857600))) 

  # build labor market bio ----
  # about 20 min for whole labor market block
  add_lags_complete <- function(df) {
    df %>% 
      transmute(week_start_date = as.Date(week_start_date), 
                labor_amt = inflow_amount,
                cust_number, cust_type, at_counterparty_raw) %>% 
      ungroup() %>%
      group_by(cust_number, at_counterparty_raw) %>%
      arrange(cust_number, at_counterparty_raw, week_start_date) %>%
      mutate(week_start_date_labor_lag = dplyr::lag(week_start_date),
             week_start_date_labor_lead = lead(week_start_date)) %>%
      ungroup() %>% 
      #balance panel at customer-cust_type-counterparty level
      complete(week_start_date, nesting(cust_number, cust_type, at_counterparty_raw), fill = list(labor_amt = 0)) %>% 
      arrange(cust_number, week_start_date, at_counterparty_raw) %>%
      as_tibble() 
  }
  
  #The objective of this section is to build a customer-by-week df of UI recipient customers. 
  # For each week, the df indicates whether the customer experienced a job separation, started a new job, 
  # or was recalled to a job that she had previously worked in (or experienced none of these three events).
  
  #8 minutes for this block: get customer-by-week df with leads/lags for 2020 UI recipients
  df_labor_cust_week_employer_2020 <-
    df_cp %>%
    dplyr::filter(
      inflow_type == "labor",
      cust_type %in% c("202021_ui_recipient"),
      #for 2020 UI recipients, we only care about year 2020
      week_year >= 2020) %>%
    add_lags_complete()
  
  test_that('N rows 202021 labor market bio',
            expect_equal(df_labor_cust_week_employer_2020 %>% nrow(), ifelse(small_samp == TRUE, 2652579, 179791765)))
  
  df_labor_cust_week_employer_2020 %>%
    saveRDS(str_c(data_path, "tmp/build_jf_1_of_2/", Sys.Date(), "df_labor_cust_week_employer_2020.rds"))
  
  #5 minutes for this block: get customer-by-week df with leads/lags for 2019 UI recipients
  df_labor_cust_week_employer_2019 <-
    df_cp %>%
    dplyr::filter(
      inflow_type == "labor",
      cust_type %in% c("2019_ui_recipient")
      #note: for 2019 UI recipients, we don't apply a week_year filter, because 2019 UI recipients 
      # can get UI in 2020 *as well as* 2019. Therefore, we want data in both 2019 and 2020
    ) %>%
    add_lags_complete()

  test_that('N rows 2019 labor market bio',
            expect_equal(df_labor_cust_week_employer_2019 %>% nrow(), ifelse(small_samp == TRUE, 1119042, 68239891 )))

    
  df_labor_cust_week_employer_2019 %>%
    saveRDS(str_c(data_path, "tmp/build_jf_1_of_2/", Sys.Date(), "df_labor_cust_week_employer_2019.rds"))
  
  #5 min for this block: get customer-by-week df with leads/lags for 2020 non_UI recipients
  df_labor_cust_week_employer_nonui2020 <- 
    df_cp %>%
    dplyr::filter(
      inflow_type == "labor",
      cust_type %in% c("nonui_2020"),
      week_year >= 2020) %>%
    add_lags_complete()
  
  #add job finding/job loss/recall info to UI recipient labor file
  df_labor_cust_week_employer <-
    bind_rows(df_labor_cust_week_employer_2020, df_labor_cust_week_employer_2019) %>%
    arrange(cust_number, at_counterparty_raw, week_start_date) %>% 
    group_by(cust_number, at_counterparty_raw) %>%
    mutate(start_new_job = labor_amt > 0 & is.na(week_start_date_labor_lag),
           start_recall = labor_amt > 0 &
             (!is.na(week_start_date_labor_lag) & week_start_date - week_start_date_labor_lag > weeks_without_labor*7),
           exit_labor = labor_amt > 0 &
             (is.na(week_start_date_labor_lead) | week_start_date_labor_lead - week_start_date > weeks_without_labor*7)) %>%
    ungroup()
  #summarise UI recipient labor file at cust-week level
  df_labor_cust_week <-
    df_labor_cust_week_employer %>%
    group_by(cust_number, week_start_date) %>%
    summarise_at(vars(start_recall, start_new_job, exit_labor), max) %>%
    ungroup()
  
  #For each nonUI_2020 customer, produce a summary file of labor exit
  df_nonui_2020_cust_week <-
    df_labor_cust_week_employer_nonui2020 %>% 
    arrange(cust_number, at_counterparty_raw, week_start_date) %>% 
    group_by(cust_number, at_counterparty_raw) %>%
    mutate(max_week_start_date = max(week_start_date),
           exit_labor = labor_amt > 0 &
             (is.na(week_start_date_labor_lead) | week_start_date_labor_lead - week_start_date > weeks_without_labor*7)) %>% 
    ungroup() %>% 
    group_by(cust_number, week_start_date) %>% 
    summarise_at(vars(exit_labor), max) %>% 
    ungroup()
  
  # build ui bio ----
  # about 15 minutes for UI block
  #this function is very similar to add_lags_complete above. differs in using cust_state instead of 
  #at_counterparty_raw and in the name that it gives to the UI variable. 
  add_lags_complete_ui <- function(df) {
    #this function creates a balanced customer-by-week panel of UI inflows. (It also includes a cust_state column)
    df %>% 
      group_by(cust_number, cust_state, inflow_type) %>%
      arrange(cust_number, cust_state, inflow_type, week_start_date) %>%
      mutate(week_start_date_ui_lag = dplyr::lag(week_start_date),
             week_start_date_ui_lead = lead(week_start_date)) %>%
      ungroup() %>% 
      complete(week_start_date, 
               nesting(cust_number, cust_type, cust_state, inflow_type), 
               fill = list(ui_inflows_mainyear = 0)) %>% 
      arrange(cust_number, week_start_date, cust_state, inflow_type) %>%
      as_tibble() %>%
      ungroup()
  }
  
  tm <- Sys.time()
  
  df_ui_cust_week_add_lags <-
    df_cp %>% 
    dplyr::filter(inflow_type %in% c("normal_ui", "PUA"), 
                  cust_type %in% c("2019_ui_recipient", "202021_ui_recipient"),  
                  week_year >= 2019) %>% 
    transmute(week_start_date = as.Date(week_start_date), 
              cust_state = state,
              ui_inflows_mainyear = as.numeric(inflow_amount),
              cust_number, inflow_type, cust_type, cust_state) %>%
    dplyr::filter(ui_inflows_mainyear > 0) %>%
    #drop a tiny number of duplicates
    distinct(cust_number, cust_state, week_start_date, inflow_type, .keep_all = TRUE) %>% 
    add_lags_complete_ui()
  
  #4 min
  #add columns designating whether each week is the start, end, or neither of a UI spell
  df_ui_cust_week_add_spells <- 
    df_ui_cust_week_add_lags %>%
    mutate(weeks_without_ui = ifelse(cust_state %in% fortnight_states, weeks_without_ui_fortnightly, weeks_without_ui_weekly)) %>%
    group_by(cust_number) %>%
    mutate(start_ui = ui_inflows_mainyear > 0 & 
             (is.na(week_start_date_ui_lag) | week_start_date - week_start_date_ui_lag > weeks_without_ui*7),
           exit_ui = ui_inflows_mainyear > 0 & 
             (is.na(week_start_date_ui_lead) | week_start_date_ui_lead - week_start_date > weeks_without_ui*7),
           ui_spell_number = cumsum(start_ui),
           ui_spell_number_end = cumsum(exit_ui)) %>%
    ungroup() %>% 
    mutate(spell_active = ui_spell_number == ui_spell_number_end + 1 | exit_ui,
           exit_ui = ifelse(spell_active, exit_ui, NA))
  Sys.time() - tm
  
 
  Sys.time() - tm_full
  
  #unit tests -- to confirm that data does not change massively as we load new months of data
  test_that("Nrow df_ui_cust_week_add_spells",
            expect_equal(df_ui_cust_week_add_spells %>% nrow(), 
                         ifelse(small_samp == TRUE, 1957070, 138486089 )))
  test_that("Nrow df_ui_cust_week_add_spells where exit_ui is TRUE",
            expect_equal(df_ui_cust_week_add_spells %>% 
                           dplyr::filter(exit_ui == TRUE) %>% 
                           nrow(), 
                         ifelse(small_samp == TRUE, 24066, 1520328 )))
  #save outputs ----
  #6 min to save
  df_labor_cust_week %>% saveRDS(str_c(data_path, "tmp/build_jf_1_of_2/", Sys.Date(), "df_labor_cust_week.rds")) #note: was previously saved as df_lm_w_zeros
  df_nonui_2020_cust_week %>% saveRDS(str_c(data_path, "tmp/build_jf_1_of_2/", Sys.Date(), "df_nonui_2020_cust_week.rds"))

  #4 min to save
  df_ui_cust_week_add_spells %>% saveRDS(str_c(data_path, "tmp/build_jf_1_of_2/", Sys.Date(), "df_ui_cust_week_add_spells.rds"))
  
  # compare exit spike by length of time horizon ----
  #4 min
  #The purpose of this df is to explore alternate definitions of UI spells,
  # with different rules for the number of weeks required between UI payments to create a new UI spell
  df_ui_cust_week_alt_horizon_basic <-
    df_ui_cust_week_add_spells %>%
    mutate(weeks_without_ui_32 = ifelse(cust_state %in% fortnight_states, 3, 2),
           weeks_without_ui_54 = ifelse(cust_state %in% fortnight_states, 5, 4)) %>%
    group_by(cust_number) %>%
    transmute(week_start_date, cust_state, exit_ui, spell_active,
              start_ui_32 = ui_inflows_mainyear > 0 & (is.na(week_start_date_ui_lag) | week_start_date - week_start_date_ui_lag > weeks_without_ui_32*7),
              exit_ui_32 = ui_inflows_mainyear > 0 & (is.na(week_start_date_ui_lead) | week_start_date_ui_lead - week_start_date > weeks_without_ui_32*7),
              spell_active_32 = cumsum(start_ui_32) == cumsum(exit_ui_32) + 1 | exit_ui_32,
              start_ui_54 = ui_inflows_mainyear > 0 & (is.na(week_start_date_ui_lag) | week_start_date - week_start_date_ui_lag > weeks_without_ui_54*7),
              exit_ui_54 = ui_inflows_mainyear > 0 & (is.na(week_start_date_ui_lead) | week_start_date_ui_lead - week_start_date > weeks_without_ui_54*7),
              spell_active_54 = cumsum(start_ui_54) == cumsum(exit_ui_54) + 1 | exit_ui_54) %>%
    ungroup()

  df_ui_cust_week_alt_horizon_basic %>% saveRDS(str_c(data_path, 'tmp/build_jf_1_of_2/', Sys.Date(), 'df_ui_cust_week_alt_horizon.rds'))

  rm(df_labor_cust_week_employer, df_ui_cust_week_add_lags)
}
