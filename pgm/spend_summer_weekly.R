#spend_summer_weekly.R

#inputs data frames needed: df_nonui_clean, df_ui_clean, customer_inc_quintiles, cust_eip_touse, cust_ui_sep_expire
run_spend_build <- TRUE
if(!run_spend_build){
  date_scratch_read_in <- "2023-02-23"
  date_scratch_build_sp_jf <- "2023-02-23"
  date_scratch_build_jf_1_of_2 <- "2023-02-23"
  date_scratch_build_jf_2_of_2 <- "2023-02-23"
  scratch_path_read_in <- "tmp/read_in/"
  scratch_path_build_sp_jf <- "tmp/build_sp_jf/"
  scratch_path_build_jf_1_of_2 <- "tmp/build_jf_1_of_2/"
  scratch_path_build_jf_2_of_2 <- "tmp/build_jf_2_of_2/"
  
  # Read-in rds files:
  df_nonui_clean <- readRDS(str_c(data_path, scratch_path_read_in, date_scratch_read_in, 
                                  "df_nonui_clean.rds"))
  df_ui_clean <- readRDS(str_c(data_path, scratch_path_read_in, date_scratch_read_in, 
                               "df_ui_clean.rds"))
  df_demo_src_wins <- readRDS(str_c(data_path, scratch_path_build_sp_jf, date_scratch_build_sp_jf, 
                                    "df_demo_src_wins.rds"))
  df_ui_cust_spell <- readRDS(str_c(data_path, scratch_path_build_jf_2_of_2, date_scratch_build_jf_2_of_2, 
                                    "df_ui_cust_spell.rds"))
  cust_eip_touse <- readRDS(str_c(data_path, scratch_path_build_sp_jf, date_scratch_build_sp_jf, 
                                  "cust_eip_touse.rds"))
  df_nonui_2020_cust_week <- readRDS(str_c(data_path, scratch_path_build_jf_1_of_2, date_scratch_build_jf_1_of_2, 
                                           "df_nonui_2020_cust_week.rds"))
  # Input dfs:
  customer_inc_quintiles <- 
    df_demo_src_wins %>%
    dplyr::filter(between(periodid, 201901, 201912)) %>% 
    mutate(inflows_ex_transfers = total_inflows - transfer_inflows) %>% 
    #drop customer-periodid duplicates (possible because a customer can have multiple types, eg, '202021_ui_recipient' and '2019_nonui')
    distinct(cust_number, periodid, .keep_all = TRUE) %>% 
    group_by(cust_number) %>% 
    summarise(inc_2019 = sum(inflows_ex_transfers)) %>% 
    mutate(inc_2019_quintile = cut_number(inc_2019, n=5))
  ym <- function(date){
    day(date) <- 1
    return(date)}
  max_month_df_nonui_2020_cust_week <- df_nonui_2020_cust_week %>%
    pull(week_start_date) %>%
    max() %>%
    ym()
  max_month_analysis_frame <- as.Date("2021-12-01")
  df_nonui2020_w_job_sep_in_sample_frame <- 
    df_nonui_2020_cust_week %>% 
    dplyr::filter(ym(week_start_date) != max_month_df_nonui_2020_cust_week,
                  ym(week_start_date - 1) != ym(max_month_df_nonui_2020_cust_week - 1),
                  ym(week_start_date) <= max_month_analysis_frame) %>% 
    filter(exit_labor == 1) %>%
    group_by(cust_number) %>% 
    filter(row_number() == 1) %>%
    ungroup() %>%
    transmute(cust_number, sep_date = week_start_date)
  cust_nonui <- 
    df_nonui_clean %>%
    distinct(cust_number) %>%
    #filter to non-UI customers who do not experience a job separation in 2020
    anti_join(df_nonui2020_w_job_sep_in_sample_frame, by = "cust_number") %>% 
    mutate(group = "Employed") %>%
    as_tibble()
}

# set parameters ----
states_puc_sep <- c("CA", "CO", "CT", "DC", "HI", "IL", "KS", "MA", "MI",
                    "MN", "NJ", "NM", "NY", "NC", "OR", "PA", "VA", "WA", "WI")
weeks_before <- 9
weeks_after <- 9
expiration_date <- as.Date("2021-09-05")
start_window <-                   expiration_date - 7*weeks_before
end_window <-                     expiration_date + 7*weeks_after
last_possible_date_to_start_ui <- expiration_date - 7*weeks_before
first_possible_date_to_end_ui <-  expiration_date + 7*weeks_after
eip1_week_list <- as.Date(c("2020-04-12", "2020-04-26", "2020-05-03", "2020-05-10", "2020-05-17", "2020-05-24"))

# employed weekly spending ----
df_employed_collapsed_expiration <-
  df_nonui_clean %>%
  dplyr::filter(between(week_start_date, start_window, end_window)) %>%
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

# unemployed customer list ----
cust_ui_expire_300 <-
  df_ui_cust_spell %>%
  filter(
    start_ui_date <= last_possible_date_to_start_ui,
    exit_ui_date >= first_possible_date_to_end_ui,
    cust_state %in% states_puc_sep
  ) 

# unemployed weekly spending ----
#join employed control to unemployed
df_weekly_ui_expiration <-
  df_ui_clean %>%
  inner_join(cust_ui_expire_300 %>% distinct(cust_number), by = 'cust_number') %>%
  dplyr::filter(between(week_start_date, start_window, end_window)) %>%
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

#make data frames to plot ----
#collapse to week-category-group level for plotting
df_weekly_ui_expiration_collapsed <-
  df_weekly_ui_expiration %>%
  group_by(week_start_date, category, group) %>%
  summarise(value = mean(value), ct_cust = n()) %>%
  ungroup()

colors_puc_exp <- c('Employed' = '#ffae5f',
                    'Unemployed' = '#004577',
                    'Unemployed minus employed' = '#bbd976')
shape_puc_exp <- c('Employed' = 19,
                   'Unemployed' = 17,
                   'Unemployed minus employed' = 15)
data_puc_expiration <- 
  df_weekly_ui_expiration_collapsed %>% 
  left_join(
    df_weekly_ui_expiration_collapsed %>% 
      dplyr::filter(week_start_date == expiration_date) %>% 
      transmute(category, group, value_at_expire = value),
    by = c('category', 'group')
  ) %>% 
  mutate(dollar_change = value - value_at_expire,
         category = ifelse(category == 'spend_cardcash', 'Spending (card and cash)',
                           ifelse(category == 'income', 'Income',
                                  ifelse(category == 'ui', 'Weekly unemployment insurance benefit', 'Spending (total)')
                           )),
         group = ifelse(group == 'Unemployed', 'Unemployed', 'Employed')
  ) %>% 
  mutate(name = ordered(category, levels = c('Income', 'Weekly unemployment insurance benefit', 'Spending (card and cash)', 'Spending (total)')),
         group = ordered(group, levels = c('Unemployed', 'Employed'))
  )

data_puc_expiration_norm <-
  data_puc_expiration %>%
  select(week_start_date, name, group, value) %>%
  mutate(group = if_else(grepl("Emp", group), "Employed", "Unemployed")) %>% 
  pivot_wider(names_from = "group",
              values_from = "value") %>%
  mutate(unemp_norm = Unemployed - Employed,
         unemp_ratio = Unemployed / Employed) %>%
  group_by(name) %>%
  mutate(unemp_norm = unemp_norm - unemp_norm[week_start_date == expiration_date],
         unemp_ratio = unemp_ratio - unemp_ratio[week_start_date == expiration_date])

data_puc_expiration_chg_w_norm <-
  bind_rows(data_puc_expiration %>%
              rename(val = dollar_change) %>%
              mutate(val_type = "dollar_change"),
            data_puc_expiration_norm %>%
              filter(grepl("total", name)) %>%
              ungroup() %>%
              transmute(week_start_date,
                        name = "norm_temp",
                        val = unemp_norm,
                        group = "Unemployed minus employed",
                        val_type = "normalized")) %>%
  mutate(name = case_when(name == "Income" ~ "Income: difference from end of August",
                          grepl("total", name) ~ "Spending: difference from end of August",
                          name == "norm_temp" ~ "Spending: unemployed minus employed") ) %>%
  filter(!is.na(name))


# plots ----
plt_fpuc_summer_expiration_diff_ui_cardcash <- 
  (data_puc_expiration %>%
     dplyr::filter(name %in% c('Weekly unemployment insurance benefit', 'Spending (card and cash)')) %>% 
     ggplot() +
     geom_line(aes(x = week_start_date, y = dollar_change, color = group, shape = group)) + 
     geom_point(aes(x = week_start_date, y = dollar_change, color = group, shape = group)) +
     facet_wrap(vars(name), scales = "free_y", nrow=2, ncol=1) + 
     scale_y_continuous(labels = scales::dollar) +
     scale_x_date(labels = date_format("%b '%y")) +
     scale_color_manual("", values = colors_puc_exp) +
     scale_shape_manual("", values = shape_puc_exp) +
     geom_text(label = "$300\nsupplement\nexpires", 
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               data = data.frame(date = c(expiration_date  + 7, expiration_date + 7),
                                 y = c(-400, -80),
                                 name = c('Weekly unemployment insurance benefit', 'Spending (card and cash)'))
     ) +
     geom_vline(xintercept =expiration_date, linetype="dashed", 
                color = "gray", size=.5) +
     labs(x = "", y = "", subtitle = "Difference from end of August", 
          color = "") +
     fte_theme("bottom") +
     theme(legend.text = element_text(size = 12), 
           legend.title = element_text(size=12, color = greys[7])) +
     guides(color = guide_legend(nrow=2, byrow = TRUE),
            shape = guide_legend(nrow=2, byrow = TRUE))
  ) %>% 
  gg_walk_save(
    str_c('fpuc_summer_expiration_diff_ui_cardcash'),
    height_touse = 6, 
    width_touse = 6,
    path_out_indiv = path_out
  ) 

# w normalized spend as third panel
plt_fpuc_summer_expiration_diff_inc_spendtotal_norm <- 
  (data_puc_expiration_chg_w_norm %>%
     ggplot() +
     geom_line(aes(x = week_start_date, y = val, color = group, shape = group)) + 
     geom_point(aes(x = week_start_date, y = val, color = group, shape = group)) +
     facet_wrap(vars(name), scales = "free_y", nrow=3, ncol = 1) + 
     scale_y_continuous(labels = scales::dollar) + 
     scale_x_date(labels = date_format("%b '%y")) +
     geom_text(label = "$300\nsupplement\nexpires", 
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               data = data.frame(date = c(expiration_date + 7),
                                 y = c(-200),
                                 name = c('Income: difference from end of August'))
     ) +
     geom_vline(xintercept = expiration_date, linetype="dashed", 
                color = "gray", size=.5) +
     labs(x = "", y = "", subtitle = NULL, 
          color = "") +
     scale_color_manual("", values = colors_puc_exp) +
     scale_shape_manual("", values = shape_puc_exp) +
     fte_theme("bottom") +
     theme(legend.text = element_text(size = 12), 
           legend.title = element_text(size=12, color = greys[7])) +
     guides(color = guide_legend(nrow=3, byrow = TRUE),
            shape = guide_legend(nrow=3, byrow = TRUE))
  ) %>% 
  gg_walk_save(
    str_c('fpuc_summer_expiration_diff_inc_spendtotal_norm'),
    height_touse = 6, 
    width_touse = 6,
    path_out_indiv = path_out
  ) 
