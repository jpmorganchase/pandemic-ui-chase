# SPDX-License-Identifier: MIT
#ui_universe_read_in_plot.R
#Author: Peter Ganong
#Date: 2021-01-22
#Analyzes all UI recipients for comparison to those who meet the primacy screen
#run after running all the analysis of the primacy screen
#build section takes about 30 minutes. plotting section takes no time at all

#INS
# must run pgm/jobfind_plots.R first
pids_lwa <- c(seq(202001, 202012), seq(202101, 202102))
weeks_without_ui_weekly <- 3
weeks_without_ui_fortnightly <- 4
fortnight_states <- c("CA", "FL", "TX", "CO", "MI", "IL")
lwa_path <- "lwa/2021-04-14lwa_weekly_" # note: last lwa file from 2021-04-14
rebuild_from_rds <- FALSE
scratch_date_univ <- "2022-06-27" # note: run on 2021-12-28, but changed to 2022-06-27 to keep consistent with other scratch files
#OUTS
rds_ui_cust_week_universe <- "df_ui_cust_week_universe.rds"

################################################################################
if (!rebuild_from_rds) {
  df_ui_cust_week_universe <- readRDS(str_c(data_path, "tmp/univ/", scratch_date_univ, rds_ui_cust_week_universe)) 
} else {
  tm <- Sys.time()
  df_ui_universe_src <- 
    pids_lwa %>%
    map_dfr(~ read_rds(str_c(data_path, lwa_path, ., ".rds"))) %>%
    transmute(week_start_date = as.Date(week_start_date),
              ui_inflows_mainyear = as.numeric(non_lwa),
              lwa = as.numeric(lwa),
              cust_state = state,
              cust_number) 
  Sys.time() - tm
  
  add_lags_complete_ui <- function(df) {
    #this function creates a balanced customer-by-week panel of UI inflows. (It also includes a cust_state column)
    # Note: function with the same name in jobfind_build_1_of_2.R but it also groups by inflow type
    df %>% 
      ungroup() %>%
      group_by(cust_number, cust_state) %>%
      arrange(cust_number, cust_state, week_start_date) %>%
      mutate(week_start_date_ui_lag = dplyr::lag(week_start_date),
             week_start_date_ui_lead = lead(week_start_date)) %>%
      ungroup() %>% 
      complete(week_start_date,
               nesting(cust_number, cust_state),
               fill = list(ui_inflows_mainyear = 0)) %>% 
      arrange(cust_number, week_start_date, cust_state) %>%
      as_tibble() 
  }
  
  #20 min
  df_ui_cust_week_universe <- 
    df_ui_universe_src %>%
    select(-lwa) %>%
    dplyr::filter(ui_inflows_mainyear > 0) %>% #filter(substr(cust_number, -2, 2) == "ab") %>%
    add_lags_complete_ui()
  
  #add columns designating whether each week is the start, end, or neither of a UI spell
  #8 min
  df_ui_cust_week_universe <- 
    df_ui_cust_week_universe %>%
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
  object.size(df_ui_cust_week_universe)
  
  df_ui_cust_week_universe %>% saveRDS(str_c(data_path, "scratch/", "2021-04-14", rds_ui_cust_week_universe)) 
}

if(!small_samp) {
  test_that("universe is about 3x larger in size", {
    expect_equal(df_ui_cust_week_universe %>% distinct(cust_number) %>% nrow(), 2913837)
    expect_equal(df_ui_cust_week %>% distinct(cust_number) %>% nrow(), 852447)
  })
}

weekly_summary_universe <- 
  df_ui_cust_week_universe  %>% 
  dplyr::filter(cust_state %in% states_rep_rate_timeseries,
                week_start_date <= last_date) %>% 
  group_by(week_start_date) %>% 
  summarise_at(vars(start_ui, spell_active, exit_ui), sum, na.rm = TRUE) %>%
  mutate(exit_ui_rate = exit_ui/spell_active,
         week_start_date = as.Date(week_start_date + 7))

max_universe_date <- max(weekly_summary_universe$week_start_date)

hero_universe_df <- weekly_summary_universe %>%
  mutate(key = "Drop Account Activity Screens") %>%
  bind_rows(weekly_summary_uncond %>% mutate(key = "Baseline")) %>%
  dplyr::filter(week_start_date >= as.Date("2020-01-19"),
                week_start_date <= as.Date(max_universe_date - 14))

write_csv(hero_universe_df,
          str_c(path_out, "hero_universe_benchmark.csv"))

rm(df_ui_cust_week_universe)

