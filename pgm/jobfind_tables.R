#jobfind_tables.R
#Author: Peter Ganong
#Date: 2021-01-20
#Purpose: make tables for job-finding analysis, 
#must be run after jobfind_plots.R

# INS
begin_ui_pandemic <- as.Date("2020-03-22")
begin_ui_pandemic_window_end_early <- as.Date("2020-04-30")
begin_ui_pandemic_window_end_late <- as.Date("2020-05-24")
begin_ui_2019 <- as.Date("2019-03-24")
end_ui_2019 <- as.Date("2019-10-27")
begin_ui_window_end_2019 <- as.Date("2019-05-26")
last_date_run_file <- "2023-02-23"

# OUTS
xls_out <- "ui_jobfind_for_export.xls"
weekly_unemployment_spells <- str_c(path_out, last_date_run_file, "tbl_ui_flows", ".csv")

################################################################################
tmp_pre <-
  df_ui_cust_spell %>%
  filter(start_ui_date < begin_ui_pandemic) %>%
  group_by(cust_number) %>%
  summarise(max_spell_n_pre = max(ui_spell_number),
            n_cust = n())

tmp_cont <- 
  df_ui_cust_week %>%
  left_join(tmp_pre, by = "cust_number") %>%
  mutate(ever_get_pre = !is.na(max_spell_n_pre)) %>%
  filter(spell_active, 
         week_start_date >= begin_ui_pandemic + 14,
         !is.na(start_ui_date)) %>% #drops 24 spells in Washington whose start_ui_date is NA b/c of earlier reallocation
  group_by(week_start_date) %>%
  summarise(`Share unemployed continuously since Apr` = 
              mean(start_ui_date <= begin_ui_pandemic_window_end_early),
            `Share unemployed continuously since May` = 
              mean(start_ui_date <= begin_ui_pandemic_window_end_late),
            n_cust = n())

tmp_prior <-
  df_ui_cust_spell %>%
  filter(start_ui_date >= begin_ui_pandemic + 14) %>%
  left_join(tmp_pre, by = "cust_number") %>%
  mutate(ever_get_pre = !is.na(max_spell_n_pre),
         first_time_spell = (ui_spell_number == 1 & !ever_get_pre) | (ui_spell_number == max_spell_n_pre + 1 & ever_get_pre)) %>%
  group_by(start_ui_date) %>%
  summarise(`Share of starts that are repeat unemp since April` = mean(!first_time_spell),
            n_cust = n()) %>%
  rename(week_start_date = start_ui_date)


# create job-finding tables to go in paper -----
tbl_ui_flows <- 
  weekly_summary_uncond %>% 
  transmute(week_start_date, `Number of active spells` = spell_active, `Number of spell starts` = start_ui, 
            `Number of spell exits` = exit_ui, 
            `Exit rate (N exits/active spells)` = exit_ui_rate) %>%
  left_join(weekly_summary_if_sep %>% transmute(week_start_date, `Exit rate if sep observed at spell start` = exit_ui_rate, 
                                                `Share of exits returning to prior emp` = start_recall_if_exit), by = "week_start_date") %>%
  left_join(weekly_summary_by_start %>% 
              filter(start_month_cohort == "Apr") %>% ungroup() %>%
              transmute(week_start_date, `Exit rate if sep in April` = exit_ui_rate), by = "week_start_date") %>%
  left_join(tmp_cont %>% select(-n_cust), by = "week_start_date") %>%
  left_join(tmp_prior %>% select(-n_cust), by = "week_start_date") %>%
  filter(week_start_date >= as.Date("2020-01-01"),
         week_start_date <= as.Date("2021-12-31"))

min_tbl_ui_flows <-
  bind_cols(
    weekly_summary_uncond %>% summarise(min(exit_ui), min(start_ui)),
    weekly_summary_if_sep %>% summarise(min(exit_ui)),
    weekly_summary_by_start %>% summarise(min(exit_ui)),
    df_ui_cust_spell %>% filter(start_ui_date >= as.Date("2020-03-22")) %>% count(start_ui_date) %>% summarise(min(n)),
    weekly_summary_uncond %>% summarise(min(spell_active))
  )


# calculate number of spells (for benchmarking) -----
n_wks_paid_state <-
  df_ui_cust_week %>%
  dplyr::filter(week_start_date <= week_after_last_reliable_payment,
                (cust_state != "IN" | week_start_date >= as.Date("2020-02-02"))) %>%
  group_by(week_start_date, cust_state) %>%
  summarise_at(vars(start_ui, spell_active, exit_ui), sum, na.rm = TRUE) %>%
  mutate(exit_ui_rate = exit_ui/spell_active) %>%
  mutate(time =
           case_when(
             week_start_date < as.Date("2020-01-01") ~ "2019",
             week_start_date <= as.Date("2020-02-28") ~ "Jan & Feb 2020",
             week_start_date <= week_after_last_reliable_payment ~ as.character(week_start_date))) %>%
  group_by(time, cust_state) %>%
  summarise_at(vars(spell_active), sum) %>%
  rename(n_weeks_w_active_spell = spell_active) %>%
  filter(n_weeks_w_active_spell >= 10)


# calculate typical benefits (for benchmarking) -----
tmp <- 
  df_ui_cust_week %>%
  filter(week_start_date > start_ui_date + 21,
         week_start_date < exit_ui_date)
state_typical_benefits_weekly_2019 <-
  tmp %>%
  filter(ui_inflows_mainyear > 0,
         week_start_date <= as.Date("2019-12-31"),
         cust_state != "NE") %>% # small cell size issue (n = 8 for NE)
  group_by(cust_state) %>%
  summarise_at(vars(ui_inflows_mainyear), list(mean_weekly_2019 = mean, pseudomedian_weekly_2019 = xtile_ten, n_weekly_2019 = ~ n())) %>%
  filter(n_weekly_2019 >= 10)
tmp_for_feb_2020 <-
  tmp %>%
  filter(start_ui_date <= as.Date("2020-01-01"),
         exit_ui_date >= as.Date("2020-03-01"),
         between(week_start_date, as.Date("2020-02-01"), as.Date("2020-02-28"))) 
state_typical_benefits_weekly <-
  tmp_for_feb_2020 %>%
  filter(ui_inflows_mainyear > 0) %>%
  group_by(cust_state) %>%
  summarise_at(vars(ui_inflows_mainyear), list(mean_weekly = mean,  n_weekly = ~ n())) %>%
  filter(n_weekly >= 10)
state_typical_benefits_monthly <-
  tmp_for_feb_2020 %>%
  group_by(cust_state, cust_number) %>%
  summarise(ui_inflows_mainyear = sum(ui_inflows_mainyear)) %>%
  summarise_at(vars(ui_inflows_mainyear), lst(mean_monthly = ~ mean(.), n_monthly = ~ n())) %>%
  filter(n_monthly >= 10)

typical_benefits_state <- 
  state_typical_benefits_weekly_2019 %>%
  left_join(state_typical_benefits_weekly) %>%
  left_join(state_typical_benefits_monthly)


rm(tmp_cont, tmp_prior, tmp_pre, tmp_for_feb_2020,
   state_typical_benefits_weekly_2019,
   state_typical_benefits_weekly, state_typical_benefits_monthly)

