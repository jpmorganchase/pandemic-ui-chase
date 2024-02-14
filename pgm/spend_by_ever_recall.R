# SPDX-License-Identifier: MIT
# spend_by_ever_recall.R
# Author: Joe Vavra

############
# Set up----
# Needed inputs if running without running full build script or after clearing environment: 
run_from_this_script <- TRUE
if(run_from_this_script){
  print_stargazer_tables <- FALSE
  source(str_c(path_repo, 'pgm/funcs/ui_functions.R'))
  source(str_c(path_repo, 'pgm/funcs/prelim.R'))
  small_samp <- FALSE
  source(str_c(path_repo, "pgm/funcs/xtile_ten.R"))
  
  if(!exists("df_ui_cust_spell")) {
    date_scratch_build_jf_2_of_2 <- "2023-04-08"
    df_ui_cust_spell <- readRDS(str_c(data_path, "tmp/build_jf_2_of_2/", 
                                      date_scratch_build_jf_2_of_2, "df_ui_cust_spell.rds")) 
  }
  run_build <- TRUE
  if(!run_build){
    source("pgm/spend_build.R")
    source("pgm/funcs/mpc_robustness_funcs.R") 
  }
}

# Needed Functions:
summarize_df <- function(data) {
  data %>%
    mutate(date = ymd(str_c(periodid, "01"))) %>%
    group_by(category, date, start_recall) %>% 
    summarise(mean =weighted.mean(value, weight),
              unweighted_mean=mean(value),
              median = xtile_ten(value, weight),
              ct_cust = n()) %>%
    ungroup()
}

plotting_function <- function(data, date_cutoff,plot_category) {
  
  colors_fact <- c("Exit to new job" = '#ffae5f', 
                   "Exit to recall" = '#004577')
  shape_fact <- c("Exit to new job" = 17, 
                   "Exit to recall" = 15)
  
  data %>% dplyr::filter(category %in% c('total_spend_cardcash', 'income', 
                                         'total_spend_expanded', 'chk_bal','labor_inflows','ui_inflows')) %>%
    left_join(data %>% dplyr::filter(date =="2020-01-01") 
              %>% 
                transmute(start_recall, 
                          category,
                          mean_jan2020 = mean,
                          unweighted_mean_jan2020 = unweighted_mean,
                          median_jan2020 = median),
              by = c('start_recall', 'category')
    ) %>% 
    mutate(percent_change_mean = mean/mean_jan2020 - 1,
           percent_change_median = median/median_jan2020 - 1, 
           start_recall = ifelse(start_recall==0, "Exit to new job", "Exit to recall"))%>% 
    filter(category == plot_category, date>="2020-01-01" & date<date_cutoff) %>%
    ggplot() + 
    geom_line(aes(x = date, y = percent_change_mean, 
                  color = as.factor(start_recall),
                  shape = as.factor(start_recall),
                  linetype=as.factor(start_recall))) + 
    geom_point(aes(x = date, y = percent_change_mean, 
                   color = as.factor(start_recall),
                   shape = as.factor(start_recall),
                   linetype=as.factor(start_recall))) +
    scale_linetype_manual("", values = c("solid", "dashed"))+
    scale_color_manual("", values = colors_fact) +
    scale_shape_manual("", values = shape_fact) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
    scale_y_continuous(labels = scales::percent_format(1))  +
    labs(subtitle ="Total Spending", x=NULL, y=NULL) + 
    fte_theme()+
    annotate("rect", xmin = as.Date("2020-03-28"), xmax = as.Date("2020-07-10"),
             ymin = -Inf, ymax = Inf, alpha = 0.1)+
    annotate("text", x = (as.Date("2020-07-26")-3), y = -0.09, 
             hjust = "right",
             colour = greys[8],
             label = "$600 supplement\navailable") +
    theme(legend.text = element_text(size = 12), 
          legend.title = element_text(size=12, color = greys[7])) +
    guides(color = guide_legend(nrow=3, byrow = TRUE),
           shape = guide_legend(nrow=3, byrow = TRUE))
}

############
# cust list with recall status and exit_ui
cust_exit_date <- 
  df_ui_cust_spell %>%
  filter(between(start_ui_date, as.Date('2020-03-01'), as.Date('2020-05-31')), 
         exit_labor==1,
         (start_recall==1 & start_new_job==0) | (start_new_job==1 & start_recall==0),
         between(exit_ui_date, as.Date("2020-06-01"), as.Date("2021-02-28"))) %>% 
  distinct(cust_number, exit_ui_date, start_recall)

test_that(
  "spell-level df unique to cust_number 
    only *after* conditioning on UI start in March-May 2020", 
  {
    expect_gte(df_ui_cust_spell %>% nrow(), df_ui_cust_spell %>% distinct(cust_number) %>% nrow())
    expect_equal(cust_exit_date %>% nrow(), cust_exit_date %>% distinct(cust_number) %>% nrow())
  }
)

# Collapse Data For Plots----
weights_to_use <-
  df_demo_src_wins_touse_eipwks %>%
  as_tibble() %>%
  #get a customer-by-inc quintile-by-eip date df
  distinct(cust_number, inc_2019_quintile, eip_week)  %>%
  inner_join(cust_exit_date, by = "cust_number") %>%
  group_by(inc_2019_quintile, eip_week, start_recall) %>%
  summarise(ct = n()) %>%
  ungroup() %>%
  mutate(group = ifelse(start_recall==1, 'start_recall', 'start_no_recall')) %>%
  select(., -start_recall) %>%
  pivot_wider(names_from = 'group', values_from = 'ct') %>%
  mutate(weight = start_recall/start_no_recall) %>%
  replace_na(list(start_recall = 0,
                  start_no_recall = 0,
                  weight = 0)) %>% 
  select(-start_recall, -start_no_recall) 

df_to_collapse_joined_with_exit_date <-
  df_demo_src_wins_touse_eipwks %>%
  dplyr::filter(periodid >= 202001) %>%
  #customers can have multiple cust_type
  distinct(cust_number, periodid, .keep_all = TRUE) %>%
  inner_join(cust_exit_date, by = "cust_number") %>%
  left_join(weights_to_use, by = c("inc_2019_quintile", "eip_week")) %>%
  mutate(weight = ifelse(start_recall==1, 1, weight)) %>%
  transmute(cust_number, cust_state, periodid, weight,
            total_outflows,
            total_spend_cardcash, 
            total_spend_expanded,
            income = total_inflows - transfer_inflows,
            total_inflows,
            transfer_inflows,
            labor_inflows,
            tax_refund_inflows,
            outflows_ex_transfers,
            chk_bal = checking_acct_balance,
            exit_ui_date,
            start_recall) %>%
  pivot_longer(-one_of(c('cust_number', 'cust_state', 'start_recall', 'periodid','exit_ui_date','weight')), #SLOW LINE OF CODE
               names_to = "category") %>%
  mutate(date = ymd(str_c(periodid, "01")))

#Note that this code produces plots just for those exiting UI in Sept 2020
df_to_collapse_continuous_spells_through_sep <-
  df_to_collapse_joined_with_exit_date %>%
  filter(between(exit_ui_date,as.Date("2020-09-01"),as.Date("2021-09-30"))) %>%
  summarize_df(.) %>%
  plotting_function(., "2020-09-01", "total_spend_expanded") %>%
  gg_walk_save(., paste0("spending_recall_vs_not_recall_exit_ui_in_sep"),
               width_touse = 8, height_touse = 4.5)
