# jobfind_build_2_of_2.R
# Author: Peter Ganong
# requires df_labor_cust_week and df_ui_cust_week_add_spells from step 1
# builds a much richer version of df_ui_cust_week with spell-level information on
# UI and labor market info on separations and starts

# OUTS
# df_ui_cust_week 
# df_labor_cust_spell 
#	df_ui_cust_week_alt_horizon
#	df_ui_cust_spell
#	df_ui_cust_week_state_only

#2.5 min in total
tm_full <- Sys.time()

re_run_step1 <- TRUE #boolean indicating whether you reran jobfind_build_1_of_2
#If FALSE, need to re-run through the SETUP block in ui_eip_data_read_in.R
if (!re_run_step1) {
  date_scratch_read_in <- "2023-06-14"
  date_scratch_build_sp_jf <- "2023-06-15"
  date_scratch_build_jf_1_of_2 <- "2023-06-16"
  
  df_labor_cust_week <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", date_scratch_build_jf_1_of_2, "df_labor_cust_week.rds")) #note: was previously saved as df_lm_w_zeros
  df_nonui_2020_cust_week <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", date_scratch_build_jf_1_of_2, "df_nonui_2020_cust_week.rds"))
  df_ui_cust_week_add_spells <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", date_scratch_build_jf_1_of_2, "df_ui_cust_week_add_spells.rds"))
  df_ui_cust_week_alt_horizon_basic <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", date_scratch_build_jf_1_of_2, "df_ui_cust_week_alt_horizon.rds"))
  
  df_cp <- readRDS(str_c(data_path, "tmp/read_in/", date_scratch_read_in, "df_cp.rds"))
  
  cust_n_states_w_ui <-
    df_cp %>% 
    dplyr::filter(inflow_type %in% c("normal_ui", "PUA")) %>%
    group_by(cust_number) %>%
    summarise(n_states = n_distinct(at_counterparty_raw))
  
  df_labor_cust_week_employer_2019 <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", date_scratch_build_jf_1_of_2, "df_labor_cust_week_employer_2019.rds"))
  df_labor_cust_week_employer_2020 <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", date_scratch_build_jf_1_of_2, "df_labor_cust_week_employer_2020.rds"))
  
  # for customer attributes later
  df_demo_src_wins_att <- readRDS(str_c(data_path, "tmp/build_sp_jf/", date_scratch_build_sp_jf, "df_demo_src_wins_att.rds"))
  df_demo_src_wins <- readRDS(str_c(data_path, "tmp/build_sp_jf/", date_scratch_build_sp_jf, "df_demo_src_wins.rds"))
  df_demo_src_wins_2019 <- readRDS(str_c(data_path, "tmp/build_sp_jf/", date_scratch_build_sp_jf, "df_demo_src_wins_2019.rds"))
  df_demog_touse <- readRDS(str_c(data_path, "tmp/read_in/", date_scratch_read_in, "df_demog_touse.rds"))
}

#create empty df to save stats for text. Each row has the stat description, value, and min cell size (n)
df_stats <- tribble(~stat, ~value, ~n)

#implement sample screens ----
cust_drop_nj <-
  df_ui_cust_week_add_spells %>% 
  dplyr::filter(between(week_start_date, as.Date("2020-04-12"),  as.Date("2020-07-26")),
                cust_state %in% "NJ",
                ui_inflows_mainyear > 0) %>%
  #drop customers for whom the majority of UI payments are multiple of 600 - these ppl appear to "only" receive PUC 
  mutate(likely_puc = ui_inflows_mainyear %in% c(600, 1200, 1800, 2400)) %>%
  group_by(cust_number) %>%
  dplyr::filter(mean(likely_puc) >= 0.5) %>%
  distinct(cust_number)

cust_drop_nj_jul26 <-
  df_ui_cust_week_add_spells %>% 
  #drop NJ ppl who only get PUC in jul 26 week
  dplyr::filter(week_start_date ==  as.Date("2020-07-26"),
                cust_state %in% "NJ",
                ui_inflows_mainyear == 600) %>%
  distinct(cust_number)

cust_drop_wi <-
  df_ui_cust_week_add_spells %>% 
  dplyr::filter(between(week_start_date, as.Date("2020-05-03"),  as.Date("2020-07-26")),
                cust_state %in% "WI",
                ui_inflows_mainyear > 0) %>%
  #same logic as NJ drop, but we use 573 (ie, 600 less tax withholding)
  mutate(likely_puc = ui_inflows_mainyear == 573) %>%
  group_by(cust_number) %>%
  dplyr::filter(mean(likely_puc) >= 0.5) %>%
  distinct(cust_number)

cust_drop_mult_states <- 
  bind_rows(
    #customers with multiple states (eg, based on LWA)
    df_ui_cust_week_add_spells %>% 
      group_by(cust_number) %>% 
      summarise(n_states = n_distinct(cust_state)) %>% 
      dplyr::filter(n_states > 1),
    #customers with multiple counterparties
    cust_n_states_w_ui %>% dplyr::filter(n_states > 1)
  ) %>% 
  select(cust_number) %>% distinct()

#data checks
test_that("number of customers being dropped from NJ",
          expect_equal(cust_drop_nj %>% nrow(), ifelse(small_samp == TRUE, 25, 1818)))
test_that("number of customers being dropped from NJ july 26th",
          expect_equal(cust_drop_nj_jul26 %>% nrow(), ifelse(small_samp == TRUE, 16, 1374)))
test_that("number of customers being dropped from WI",
          expect_equal(cust_drop_wi %>% nrow(), ifelse(small_samp == TRUE, 0, 166)))
test_that("number of customers being dropped from multiple states",
          expect_equal(cust_drop_mult_states %>% nrow(), ifelse(small_samp == TRUE, 117, 14711)))
df_stats <- df_stats %>% add_row(stat = 'Custs to drop in NJ',
                                 value = cust_drop_nj %>% nrow() + cust_drop_nj_jul26 %>% nrow(), 
                                 n = cust_drop_nj %>% nrow() + cust_drop_nj_jul26 %>% nrow())
df_stats <- df_stats %>% add_row(stat = 'Custs to drop for appearing in multiple states',
                                 value = cust_drop_mult_states %>% nrow(),
                                 n = cust_drop_mult_states %>% nrow())

cust_drop <- 
  bind_rows(cust_drop_nj, cust_drop_nj_jul26, cust_drop_wi, cust_drop_mult_states) %>% 
  distinct(cust_number)

ct_cust_before_drop <- df_ui_cust_week_add_spells %>% distinct(cust_number) %>% nrow()
ct_ui_spells <- df_ui_cust_week_add_spells %>% dplyr::filter(ui_spell_number != 0) %>% distinct(cust_number, ui_spell_number) %>% nrow()
ct_cust_in_pandemic <- 
  df_ui_cust_week_add_spells %>% 
  dplyr::filter(week_start_date >= as.Date('2020-04-05'), ui_inflows_mainyear > 0) %>% 
  distinct(cust_number) %>% nrow()
df_stats <- df_stats %>% add_row(stat = 'Count total customers', 
                                 value = ct_cust_before_drop,
                                 n = ct_cust_before_drop)
df_stats <- df_stats %>% add_row(stat = 'Count total UI spells',
                                 value = ct_ui_spells,
                                 n = ct_ui_spells)
df_stats <- df_stats %>% add_row(stat = 'Count of customers during pandemic (Apr 5 onward)',
                                 value = ct_cust_in_pandemic,
                                 n = ct_cust_in_pandemic)


test_that('ct customers before drop',
          {expect_equal(ct_cust_before_drop, ifelse(small_samp == TRUE, 14483, 870235))})
test_that('ct UI spells',
          {expect_equal(ct_ui_spells, ifelse(small_samp == TRUE, 24066, 1460807))})
test_that('ct customers to drop',
          {expect_equal(cust_drop %>% nrow(), ifelse(small_samp == TRUE, 158, 17788))})
test_that('ct customers get UI during pandemic',
          {expect_equal(ct_cust_in_pandemic, ifelse(small_samp == TRUE, 13355, 804924))})

df_ui_cust_week_cust_drop <-
  df_ui_cust_week_add_spells %>%
  anti_join(cust_drop)  %>%
  arrange(cust_number, week_start_date)

#create a customer-by-ui spell dataframe, with start date, end date, n ui pmts in spell
df_ui_cust_spell_basic <- 
  df_ui_cust_week_cust_drop %>% 
  dplyr::filter(start_ui) %>% 
  transmute(cust_number, inflow_type, ui_spell_number, cust_state, start_ui_date = week_start_date) %>%
  left_join(df_ui_cust_week_cust_drop %>% dplyr::filter(exit_ui) %>% transmute(cust_number, ui_spell_number, 
                                                                               inflow_type,
                                                                               exit_ui_date = week_start_date))


# add lm outcomes to UI summary ----
df_labor_cust_spell_lm <- 
  df_labor_cust_week %>%
  left_join(df_ui_cust_spell_basic %>% select(cust_number, start_ui_date, exit_ui_date, inflow_type, ui_spell_number), by = "cust_number") %>%
  mutate(weeks_since_spell_start = as.numeric(week_start_date - start_ui_date)/7,
         weeks_since_spell_end =  as.numeric(week_start_date - exit_ui_date)/7) %>% 
  group_by(cust_number, inflow_type, ui_spell_number) %>%
  #for each customer-UI spell, determine: 
  #  whether customer had a labor market exit near beginning of UI spell
  summarise(exit_labor = max(exit_labor[weeks_since_spell_start >= -8 & weeks_since_spell_start <= 2]),
            #whether customer started a new job near end of UI spell
            start_new_job_raw = max(start_new_job[weeks_since_spell_end >= -2 & weeks_since_spell_end <= 8]),
            start_new_job_cens = max(start_new_job[weeks_since_spell_end >= -2 & weeks_since_spell_end <= 1]),
            #whether customer was recalled near end of UI spell
            start_recall_raw = max(start_recall[weeks_since_spell_end >= -5 & weeks_since_spell_end <= 5]),
            start_recall_cens = max(start_recall[weeks_since_spell_end >= -5 & weeks_since_spell_end <= 1])) %>%
  ungroup() %>%
  rename(start_recall = start_recall_raw, start_new_job = start_new_job_raw)

df_labor_cust_spell <- df_labor_cust_spell_lm %>% 
  mutate_at(vars("start_new_job", "start_recall", "exit_labor", "start_new_job_cens", "start_recall_cens"), ~ ifelse(is.na(.), 0, .))

#update df_ui_cust_spell to include labor spell info
df_ui_cust_spell <- df_ui_cust_spell_basic %>%
  left_join(df_labor_cust_spell, by = c("cust_number", "ui_spell_number", "inflow_type"))

# #note that no information is merged on before first UI spell (whien ui_spell_number == 0)
df_ui_cust_week_rm_pre_spell <- df_ui_cust_week_cust_drop %>%
  left_join(df_ui_cust_spell %>% select(-cust_state),
            by = c("cust_number", "inflow_type", "ui_spell_number"))

# bring in spell + separation + recall info to alternative horizons
df_ui_cust_week_alt_horizon <- df_ui_cust_week_alt_horizon_basic %>%
  left_join(df_ui_cust_week_rm_pre_spell %>%
              select(cust_number, week_start_date, start_ui_date:exit_labor),
            by = c("cust_number", "week_start_date"))

df_ui_cust_week_alt_horizon %>% saveRDS(str_c(data_path, "tmp/build_jf_2_of_2/", Sys.Date(), "df_ui_cust_week_alt_horizon.rds"))

test_that("N rows customer-by-ui spell dataframe",
          expect_equal(df_ui_cust_spell %>% nrow(), ifelse(small_samp == TRUE, 23628, 1413566)))
test_that("N unique customers",
          expect_equal(df_ui_cust_spell %>% distinct(cust_number) %>% nrow(), ifelse(small_samp == TRUE, 14325, 852447)))
test_that("N rows customer-week dataframe", expect_equal(df_ui_cust_week_rm_pre_spell %>% nrow(), ifelse(small_samp == TRUE, 1919550, 119342580)))
test_that("N rows alt horizon", expect_equal(df_ui_cust_week_alt_horizon %>% nrow(), ifelse(small_samp == TRUE, 1957070, 122906140)))

# fix Washington state ----
#check: how many active spells and UI exits in each week? 
#  reveals very few exits the week of 9/6, a spike in exits week of 9/13
#  this is proof of what we previously discovered: WA largely did not pay out UI week of 9/6 (Labor Day week),
#  and instead paid out double UI week of 9/13
tmp_wa_exit <- 
  df_ui_cust_week_rm_pre_spell %>% 
  dplyr::filter(week_start_date >= as.Date("2020-08-23"),
                week_start_date <= as.Date("2020-10-04"),
                cust_state == "WA") %>% 
  group_by(week_start_date, cust_state) %>% 
  summarise_at(vars(spell_active, exit_ui), sum, na.rm = TRUE) %>%
  mutate(exit_ui_rate = exit_ui/spell_active)

test_that("",
          expect_equal(tmp_wa_exit$exit_ui_rate[3:4], 
                       if(small_samp == TRUE){ c(0.02898551, 0.14649682)} else {c(0.03040692, 0.13461138)}, 
                       tol = 0.0001))
test_that("",
          expect_equal(tmp_wa_exit$exit_ui[3:4], 
                       if(small_samp == TRUE){ c(4, 23)} else {c(267, 1280)}))

#define weekly exit week in each of weeks 3, 4 (9/6, 9/13) as 
# geometric average of exit rate across the two weeks
(exit_rate_per_week <- 1 - (1 - sum(tmp_wa_exit$exit_ui[3:4])/tmp_wa_exit$spell_active[4])^(1/2))
(exit_rate_change_from_observed <- exit_rate_per_week - tmp_wa_exit$exit_ui_rate[3:4])
#determine number of exits that must be reallocated from week of 9/13 to week of 9/6
(n_exits_to_reallocate_to_earlier <- -round(exit_rate_change_from_observed[2]*tmp_wa_exit$spell_active[4]))

expect_equal(n_exits_to_reallocate_to_earlier, ifelse(small_samp == TRUE, 9, 472))

#need a separate filter statement so that row_number is generated _after_ the subset as been drawn
cust_to_reallocate <- 
  df_ui_cust_week_rm_pre_spell %>%
  dplyr::filter(week_start_date == as.Date("2020-09-13"), 
                cust_state == "WA",
                spell_active,
                exit_ui) %>%
  dplyr::filter(row_number() <= n_exits_to_reallocate_to_earlier) %>%
  select(cust_number) %>%
  mutate(reallocate_me = TRUE)

df_ui_cust_week <- 
  df_ui_cust_week_rm_pre_spell %>%
  left_join(cust_to_reallocate) %>%
  mutate(exit_ui = 
           case_when(
             reallocate_me & week_start_date == as.Date("2020-09-06") ~ TRUE,
             reallocate_me & week_start_date == as.Date("2020-09-13") ~ FALSE,
             TRUE ~ exit_ui),
         spell_active =
           case_when(
             reallocate_me & week_start_date == as.Date("2020-09-06") ~ TRUE,
             reallocate_me & week_start_date == as.Date("2020-09-13") ~ FALSE,
             TRUE ~ spell_active)) %>%
  select(-reallocate_me)
rm(cust_to_reallocate, tmp_wa_exit)

df_ui_cust_week %>% saveRDS(str_c(data_path, "tmp/build_jf_2_of_2/", Sys.Date(), "df_ui_cust_week.rds"))

Sys.time() - tm_full

#unit tests and stats for text----
n_row <- df_ui_cust_week %>% nrow() 
test_that("df_ui_cust_week nrow",
          expect_equal(n_row, ifelse(small_samp == TRUE, 1919550, 133884262 )))
test_that('df_ui_cust week unique at customer-week level',
          expect_equal(df_ui_cust_week %>% 
                         distinct(cust_number, week_start_date, inflow_type) %>% 
                         nrow(), 
                       n_row)
)
rm(n_row)
ct_cust_after_cleaning <- df_ui_cust_week %>% distinct(cust_number) %>% nrow()
test_that('Number of UI customers after sample cleaning',
          expect_equal(ct_cust_after_cleaning, ifelse(small_samp == TRUE, 14325, 852766 )))
df_stats <- df_stats %>% add_row(stat = 'Count customers after all cleaning',
                                 value = ct_cust_after_cleaning,
                                 n = ct_cust_after_cleaning)

#in 3% of cases, no labor income info is availablee
#expect_equal(df_ui_cust_spell %>% filter(is.na(exit_labor)) %>% nrow(), 27045 )
#2% of UI spells do not appear in df_ui_cust_week
test_that("df_ui_cust_week with 1 or more spells",
          expect_equal(df_ui_cust_week %>% 
                         dplyr::filter(ui_spell_number > 0) %>% 
                         nrow(), 
                       ifelse(small_samp == TRUE, 1006632, 75998603 )))

test_that("df_ui_cust_week with 1 or more spells and non-NA exit labor",
          expect_equal(df_ui_cust_week %>% 
                         dplyr::filter(ui_spell_number > 0, is.na(exit_labor)) %>% 
                         nrow(), 
                       ifelse(small_samp == TRUE, 10105, 1347836 )))

#Save files needed for spend analysis----
df_ui_cust_spell %>% saveRDS(str_c(data_path, "tmp/build_jf_2_of_2/", Sys.Date(), "df_ui_cust_spell.rds"))
df_ui_cust_week_state_only <-
  df_ui_cust_week %>%
  select(cust_number, cust_state)
df_ui_cust_week_state_only %>% saveRDS(str_c(data_path, "tmp/build_jf_2_of_2/", Sys.Date(), "df_ui_cust_week_state_only.rds"))

# remove superfluous intermediate dfs
rm(df_ui_cust_week_add_spells, df_ui_cust_week_cust_drop,
   df_ui_cust_week_rm_pre_spell, df_ui_cust_week_alt_horizon_basic,
   df_ui_cust_spell_basic, df_labor_cust_spell_lm, df_labor_cust_spell,
   df_stats, cust_n_states_w_ui)
rm(list=ls(pattern = "cust_drop"))

