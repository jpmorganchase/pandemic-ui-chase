#spend_plots.R
#Author: Peter Ganong/Max Liebeskind
#Date: Jan 2021
#Objective: Create plots of spending for various event studies/ID strategies around UI.

greys <- RColorBrewer::brewer.pal(9, "Greys")
source(str_c(path_repo, 'pgm/funcs/ui_functions.R'))

#OUTS
pdf_out_spendplots <- str_c(path_out, "ui_spend_production.pdf")
csv_spend_aggregation_standards <- str_c(path_out, 'aggregation_standards_spend_plots.csv')

#FACT 1 plot: spending, inflows relative to Jan 2020 ----
colors_fact1 <- c('Employed' = '#ffae5f', 
                  'Unemployed (get benefits from April 2020 through February 2021)' = '#004577')
shapes_fact1 <- c('Employed' = 19, 
                    'Unemployed (get benefits from April 2020 through February 2021)' = 15)

###arrange data
df_in <- df_monthly_collapsed_thru_feb

tmp_monthly_prod <- df_in %>%
  dplyr::filter(category %in% c('total_spend_cardcash', 'income', 
                                'total_spend_expanded', 'chk_bal')) %>%
  mutate(date = ymd(periodid, truncated = 1)) %>% 
  left_join(df_in %>% dplyr::filter(periodid == 202001) %>% 
              transmute(group, 
                        category,
                        measure,
                        value_jan2020 = value),
            by = c('group', 'category', 'measure')
            ) %>% 
  mutate(percent_change = value/value_jan2020 - 1,
         category = ifelse(category == 'total_spend_cardcash', 'Spending (card and cash)',
                           ifelse(category == 'income', 'Income', 
                                  ifelse(category == 'total_spend_expanded', 'Spending (total)', 'Checking account balance'))),
         group = ifelse(group == 'unemployed', label_for_unemployed_group, 'Employed')
         ) %>% 
  mutate(name = ordered(category, levels = c('Income', 'Spending (total)', 'Spending (card and cash)', 'Checking account balance')),
         group = ordered(group, levels = c(label_for_unemployed_group , 'Employed'))
         )

#create rectangular shaded box indicating when PUC is available
puc_available <- 
  ##note: PUC expired on 7/31, we end the box at 7/10 because plot is monthly and this indicates that it was available thru July
  annotate("rect", xmin = as.Date("2020-03-28"), xmax = as.Date("2020-07-10"), 
           ymax = Inf, ymin = -Inf, alpha = 0.1)

puc2_available <- 
  annotate("rect", xmin = as.Date("2021-01-01"), xmax = as.Date("2021-03-01"), 
           ymax = Inf, ymin = -Inf, alpha = 0.1)

plot_u_vs_e <- function(measure_touse, subtitle_text, 
                        periodid_min = 201901,
                        periodid_max = 202102,
                        categories = c('Income', 'Spending (total)', 'Spending (card and cash)'),
                        nrow_facet = 3, ncol_facet = 1,
                        leg_text_size = 12){
  tmp_monthly_prod %>%
    dplyr::filter(measure == measure_touse, 
                  periodid >= periodid_min,
                  periodid <= periodid_max,
                  category %in% categories) %>% 
    ggplot() + 
    geom_line(aes(x = date, y = percent_change, color = group, shape = group)) + 
    geom_point(aes(x = date, y = percent_change, color = group, shape = group)) +
    facet_wrap(vars(name), scales = "free_y", nrow=nrow_facet, ncol=ncol_facet) + 
    scale_y_continuous(labels = scales::percent_format(1)) + 
    scale_x_date(labels = date_format("%b %Y")) +
    puc_available +
    labs(x = "", y = "", subtitle = subtitle_text, 
         color = "") +
    scale_color_manual("", values = colors_fact1) +
    scale_shape_manual("", values = shapes_fact1) +
    fte_theme("bottom") +
    theme(legend.text = element_text(size = 12), 
          legend.title = element_text(size=12, color = greys[7])) +
    guides(color = guide_legend(nrow=2, byrow = TRUE),
           shape = guide_legend(nrow=2, byrow = TRUE))
}

plt_spending_ts_medians <- 
  (plot_u_vs_e('median', 'Percent difference from January 2020 (median)',
               periodid_max = 202102,
               categories = c('Income', 'Spending (total)', 'Checking account balance'))  +
     geom_text(label = "$600\nsupplement\navailable",
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               data = data.frame(date = c(as.Date('2020-05-15')),
                                 y = c(-.22),
                                 name = c('Income'))
     ) +
     puc2_available +
     geom_text(label = "$300\nsupplement\navailable",
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               data = data.frame(date = c(as.Date('2021-01-15')),
                                 y = c(0.35),
                                 name = c('Income'))
     ) +
     scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date("2021-03-01"), by="3 months"),
                  labels = date_format("%b '%y"))
  ) %>% 
  gg_walk_save('spending_ts_medians', width_touse = 9, height_touse = 11) # previously fact1_plot_main


plt_spending_ts_means <- # swapped out spend card cash with checking account balance & got rid of extra text
  (plot_u_vs_e('mean', 'Percent difference from January 2020 (mean)',
               periodid_max = 202102,
               categories = c('Income', 'Spending (total)', 'Checking account balance')) +
     geom_text(label = "$600\nsupplement\navailable",
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               data = data.frame(date = c(as.Date('2020-05-15')),
                                 y = c(-.17),
                                 name = c('Income'))
     ) +
     puc2_available +
     geom_text(label = "$300\nsupplement\navailable",
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               data = data.frame(date = c(as.Date('2021-01-15')),
                                 y = c(0.27),
                                 name = c('Income'))
     ) +
     scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date("2021-03-01"), by="3 months"),
                  labels = date_format("%b '%y"))
  ) %>% 
  gg_walk_save('spending_ts_means', height_touse = 9, width_touse = 7.5) # previously fact1_plot_appendix_v2

plt_spending_ts_means_cardcash <-
  (plot_u_vs_e('mean', 'Percent difference from January 2020 (mean)',
               periodid_max = 202102,
               categories = c('Income', 'Spending (card and cash)')) +
     geom_text(label = "$600\nsupplement\navailable",
               aes(x = date, y = y),
               color = greys[6],
               size = 2.5,
               data = data.frame(date = c(as.Date('2020-05-15')),
                                 y = c(-.17),
                                 name = c('Income'))
     ) +
     puc2_available +
     geom_text(label = "$300\nsupplement\navailable",
               aes(x = date, y = y),
               color = greys[6],
               size = 2.5,
               data = data.frame(date = c(as.Date('2021-01-15')),
                                 y = c(0.27),
                                 name = c('Income'))
     ) +
     scale_x_date(labels = date_format("%b '%y"))
  ) %>% 
  gg_walk_save('spending_ts_means_cardcash', height_touse = 6, width_touse = 6) # previously fact1_plot_appendix_v3


#FACT 2 plot: UI onset (waiting design) ----
## Here, we plot the path of spending and inflows around UI onset for a group of customers who 
## have job separation at the start of Covid but start UI on different dates
colors_waiting <- c('April  5' = '#003151', 'April 19' = '#0198e1', 
                    'May 10' = '#05b8cc', 'May 31 (control)' = '#b0e0e6')
shapes_waiting <- c('April  5' = 19, 'April 19' = 18, 'May 10' = 17, 'May 31 (control)' = 15)
weeks_to_plot <- c('2020-04-05', '2020-04-19', '2020-05-10', '2020-05-31')
waiting_levels_data <- df_ui_onset_collapsed %>% 
  dplyr::filter(first_ui_week %in% weeks_to_plot) %>%
  mutate(first_ui_week = format(first_ui_week_dt, "%B %e"),
         first_ui_week = ifelse(first_ui_week == 'May 31', 'May 31 (control)', first_ui_week),
         first_ui_week = factor(first_ui_week, levels = c('April  5', 'April 19', 'May 10', 'May 31 (control)'))
  ) %>% 
  transmute(week_start_date, first_ui_week, 
            `Spending (total)` = spend_expanded_mainyear, 
            `Spending (card and cash)` = spend_mainyear, 
            `Weekly unemployment insurance benefit` = ui_inflows_mainyear, 
            `Income` = total_inflows_mainyear,
            ct_cust) %>% 
  pivot_longer(cols = c('Weekly unemployment insurance benefit', 'Income', 'Spending (total)', 'Spending (card and cash)'),
               names_to = 'category', values_to = 'amount') %>% 
  mutate(category = ordered(category, levels = c('Weekly unemployment insurance benefit', 'Income', 'Spending (total)', 'Spending (card and cash)', 'ct_cust'))) %>% 
  rename(`Week of first benefit` = first_ui_week)

waiting_diffs_data <-
  waiting_levels_data %>% 
  left_join(waiting_levels_data %>% 
              dplyr::filter(`Week of first benefit` == 'May 31 (control)') %>% 
              transmute(week_start_date, category, amount_may31 = amount),
            by = c('week_start_date', 'category')) %>% 
  mutate(amount_diff = amount - amount_may31) %>% 
  dplyr::filter(`Week of first benefit` != 'May 31 (control)')

# waiting plots, levels (replaces fact2_waiting_uionset_main)
plt_uionset_waiting_prod_levels_bothspend <- 
  (waiting_levels_data %>%
     dplyr::filter(category %in% c('Spending (card and cash)', 'Weekly unemployment insurance benefit', 
                                   'Spending (total)')) %>% 
     ggplot(aes(x = week_start_date, y = amount, color = `Week of first benefit`, shape = `Week of first benefit`)) + 
     geom_line() + geom_point() +
     labs(x = "", y = "", color = "") +
     scale_y_continuous(labels = scales::dollar) +
     scale_color_manual('Week of first benefit', values = colors_waiting) +
     scale_shape_manual('Week of first benefit', values = shapes_waiting) +
     facet_wrap(vars(category), scales = "free_y", nrow=3, ncol=1) +
     scale_x_date(labels = date_format("%b '%y")) +
     fte_theme("bottom") + 
     theme(legend.text = element_text(size = 12), 
           legend.title = element_text(size=12, color = greys[7])) +
     guides(color = guide_legend(nrow=2, byrow = TRUE),
            shape = guide_legend(nrow=2, byrow = TRUE))
  ) %>%
  gg_walk_save('waiting_onset_levels_three_panel', height_touse = 9, width_touse = 6) # fact2_waiting_uionset_main_v2

plt_uionset_waiting_prod_levels_spendtotal <- 
  (waiting_levels_data %>%
     dplyr::filter(category %in% c('Weekly unemployment insurance benefit', 
                                   'Spending (card and cash)')) %>% 
     ggplot(aes(x = week_start_date, y = amount, 
                color = `Week of first benefit`, shape = `Week of first benefit`)) + 
     geom_line() + geom_point() +
     labs(x = "", y = "", color = "") +
     scale_y_continuous(labels = scales::dollar) +
     scale_x_date(labels = date_format("%b '%y")) +
     scale_color_manual('Week of first benefit', values = colors_waiting) +
     scale_shape_manual('Week of first benefit', values = shapes_waiting) +
     facet_wrap(vars(category), scales = "free_y", nrow=3, ncol=1) + 
     fte_theme("bottom") + 
     theme(legend.text = element_text(size = 12), 
           legend.title = element_text(size=12, color = greys[7])) +
     guides(color = guide_legend(nrow=2, byrow = TRUE),
            shape = guide_legend(nrow=2, byrow = TRUE))
  ) %>%
  gg_walk_save('waiting_onset_levels_two_panel', height_touse = 6, width_touse = 6) # fact2_waiting_uionset_main_v3

# waiting plots, difference relative to May 31 group
plt_uionset_waiting_diffs_spendtotal <- 
  (waiting_diffs_data %>%
     dplyr::filter(category %in% c('Spending (total)', 'Income')) %>% 
     ggplot(aes(x = week_start_date, y = amount_diff, color = `Week of first benefit`, shape = `Week of first benefit`)) + 
     geom_line() + geom_point() +
     scale_y_continuous(labels = scales::dollar) +
     scale_color_manual('Week of first benefit', values = colors_waiting) +
     scale_shape_manual('Week of first benefit', values = shapes_waiting) +
     facet_wrap(vars(category), scales = "free_y", nrow=2, ncol=1) + 
     scale_x_date(labels = date_format("%b '%y")) +
     fte_theme("bottom") + 
     theme(legend.text = element_text(size = 12), legend.title = element_text(size=12, color = greys[7]),
           plot.subtitle = element_text(size = 12)) +
     labs(x = "", y = "", color = "",
          subtitle = 'Difference from control group (receives first benefit May 31)')
  ) %>%
  gg_walk_save('waiting_onset_diffs_two_panel_spendtotal', height_touse = 6, width_touse = 6) # previously fact2_waiting_uionset_appendix

#FACT 3 plot: PUC expiration ----
#data for PUC expiration: dollar difference relative to week of 7/5
colors_puc_exp <- c('Employed' = '#ffae5f',
                    'Unemployed (get benefits from June through the end of August)' = '#004577',
                    'Unemployed minus employed' = '#bbd976')
shape_puc_exp <- c('Employed' = 19,
                   'Unemployed (get benefits from June through the end of August)' = 17,
                   'Unemployed minus employed' = 15)
data_puc_expiration <- 
  df_weekly_ui_expiration_collapsed %>% 
  #only want to show data through 9/6
  dplyr::filter(week_start_date <= as.Date('2020-09-06')) %>% 
  #difference relative to week of 7/5
  left_join(df_weekly_ui_expiration_collapsed %>% 
              dplyr::filter(week_start_date == as.Date('2020-07-05')) %>% 
              transmute(category, group, value_july5 = value),
            by = c('category', 'group')
            ) %>% 
  mutate(dollar_change = value - value_july5,
         category = ifelse(category == 'spend_cardcash', 'Spending (card and cash)',
                           ifelse(category == 'income', 'Income',
                                  ifelse(category == 'ui', 'Weekly unemployment insurance benefit', 'Spending (total)')
                                  )),
         group = ifelse(group == 'Unemployed', 'Unemployed (get benefits from June through the end of August)', 'Employed')
  ) %>% 
  mutate(name = ordered(category, levels = c('Income', 'Weekly unemployment insurance benefit', 'Spending (card and cash)', 'Spending (total)')),
         group = ordered(group, levels = c('Unemployed (get benefits from June through the end of August)', 'Employed'))
  )

data_puc_expiration_norm <-
  data_puc_expiration %>%
  select(week_start_date, name, group, value) %>%
  mutate(group = if_else(grepl("Emp", group),
                         "Employed",
                         "Unemployed")) %>% 
  pivot_wider(names_from = "group",
              values_from = "value") %>%
  mutate(unemp_norm = Unemployed - Employed,
         unemp_ratio = Unemployed / Employed) %>%
  group_by(name) %>%
  # changes from jul 5
  mutate(unemp_norm = unemp_norm - unemp_norm[week_start_date == as.Date("2020-07-05")],
         unemp_ratio = unemp_ratio - unemp_ratio[week_start_date == as.Date("2020-07-05")])

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
  mutate(name = case_when(name == "Income" ~ "Income: difference from first week of July",
                          grepl("total", name) ~ "Spending: difference from first week of July",
                          name == "norm_temp" ~ "Spending: unemployed minus employed") ) %>%
  filter(!is.na(name))

# diffs
plt_fpuc_expiration_diff_ui_cardcash <- 
  (data_puc_expiration %>%
     dplyr::filter(name %in% c('Weekly unemployment insurance benefit', 'Spending (card and cash)')) %>% 
     ggplot() +
     geom_line(aes(x = week_start_date, y = dollar_change, color = group, shape = group)) + 
     geom_point(aes(x = week_start_date, y = dollar_change, color = group, shape = group)) +
     facet_wrap(vars(name), scales = "free_y", nrow=2, ncol=1) + 
     scale_y_continuous(labels = scales::dollar) + 
     geom_text(label = "$600\nsupplement\nexpires", 
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               data = data.frame(date = c(as.Date('2020-07-21'), as.Date('2020-07-21')),
                                 y = c(-400, -80),
                                 name = c('Weekly unemployment insurance benefit', 'Spending (card and cash)'))
     ) +
     geom_vline(xintercept = as.Date('2020-07-29'), linetype="dashed", 
                color = "gray", size=.5) +
     labs(x = "", y = "", subtitle = "Difference from first week of July", 
          color = "") +
     scale_color_manual("", values = colors_puc_exp) +
     scale_shape_manual("", values = shape_puc_exp) +
     scale_x_date(labels = date_format("%b '%y")) +
     fte_theme("bottom") +
     theme(legend.text = element_text(size = 12), 
           legend.title = element_text(size=12, color = greys[7])) +
     guides(color = guide_legend(nrow=2, byrow = TRUE),
            shape = guide_legend(nrow=2, byrow = TRUE))
   ) %>% 
  gg_walk_save('fpuc_expiration_diff_ui_cardcash', height_touse = 6, width_touse = 6) #previously fact3_fpuc_expiration_main

# w normalized spend as third panel
plt_fpuc_expiration_diff_inc_spendtotal_norm <- 
  (data_puc_expiration_chg_w_norm %>%
     ggplot() +
     geom_line(aes(x = week_start_date, y = val, color = group, shape = group)) + 
     geom_point(aes(x = week_start_date, y = val, color = group, shape = group)) +
     facet_wrap(vars(name), scales = "free_y", nrow=3, ncol=1) + 
     scale_y_continuous(labels = scales::dollar) + 
     scale_x_date(labels = date_format("%b '%y")) +
     geom_text(label = "$600\nsupplement\nexpires", 
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               data = data.frame(date = c(as.Date('2020-07-21')),
                                 y = c(-350),
                                 name = c('Income: difference from first week of July'))
     ) +
     geom_vline(xintercept = as.Date('2020-07-29'), linetype="dashed", 
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
  gg_walk_save('fpuc_expiration_diff_inc_spendtotal_norm', height_touse = 9, width_touse = 6)

# FACT 5 plot: onset of $300 ----
colors_onset <- c('Employed' = '#ffae5f',
                  'Unemployed (get benefits from Nov through end of Jan)' = '#004577',
                  'Unemployed minus employed' = '#bbd976')
shape_onset <- c('Employed' = 19,
                   'Unemployed (get benefits from Nov through end of Jan)' = 17,
                   'Unemployed minus employed' = 15)

#dollar difference relative to week of 12/06
data_300_onset <- 
  df_weekly_ui_onset_collapsed %>% 
  #difference relative to first week of December 
  left_join(df_weekly_ui_onset_collapsed %>% 
              dplyr::filter(week_start_date == as.Date('2020-12-06')) %>% 
              transmute(category, group, value_dec6 = value),
            by = c('category', 'group')
  ) %>% 
  mutate(dollar_change = value - value_dec6,
         category = ifelse(category == 'spend_cardcash', 'Spending (card and cash)',
                           ifelse(category == 'income', 'Income',
                                  ifelse(category == 'ui', 'Weekly unemployment insurance benefit', 'Spending (total)')
                           )),
         group = ifelse(group == 'Unemployed', 'Unemployed (get benefits from Nov through end of Jan)', 'Employed')
  ) %>% 
  mutate(name = ordered(category, levels = c('Income', 'Weekly unemployment insurance benefit', 'Spending (card and cash)', 'Spending (total)')),
         group = ordered(group, levels = c('Unemployed (get benefits from Nov through end of Jan)', 'Employed'))
  )

data_300_onset_norm <-
  data_300_onset %>%
  select(week_start_date, name, group, value) %>%
  mutate(group = if_else(grepl("Emp", group),
                         "Employed",
                         "Unemployed")) %>% 
  pivot_wider(names_from = "group",
              values_from = "value") %>%
  mutate(unemp_norm = Unemployed - Employed,
         unemp_ratio = Unemployed / Employed) %>%
  group_by(name) %>%
  # changes from dec 6
  mutate(unemp_norm = unemp_norm - unemp_norm[week_start_date == as.Date("2020-12-06")],
         unemp_ratio = unemp_ratio - unemp_ratio[week_start_date == as.Date("2020-12-06")])

data_300_onset_chg_w_norm <-
  bind_rows(data_300_onset %>%
              rename(val = dollar_change) %>%
              mutate(val_type = "dollar_change"),
            data_300_onset_norm %>%
              filter(grepl("total", name)) %>%
              ungroup() %>%
              transmute(week_start_date,
                        name = "norm_temp",
                        val = unemp_norm,
                        group = "Unemployed minus employed",
                        val_type = "normalized")) %>%
  mutate(name = case_when(name == "Income" ~ "Income: difference from first week of December",
                          grepl("total", name) ~ "Spending: difference from first week of December",
                          name == "norm_temp" ~ "Spending: unemployed minus employed") ) %>%
  filter(!is.na(name))

# plots
plot_onset_ui_cardcash <- 
  (data_300_onset %>%
     dplyr::filter(name %in% c('Weekly unemployment insurance benefit', 'Spending (card and cash)')) %>% 
     ggplot() +
     geom_line(aes(x = week_start_date, y = dollar_change, color = group, shape = group)) + 
     geom_point(aes(x = week_start_date, y = dollar_change, color = group, shape = group)) +
     scale_x_date(limits = as.Date(c("2020-11-01", "2021-03-01")),
                  labels = date_format("%b '%y")) +
     facet_wrap(vars(name), scales = "free_y", nrow=2, ncol=1) + 
     scale_y_continuous(labels = scales::dollar) + 
     geom_text(label = "$300\nsupplement\nbegins", 
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               hjust = "right",
               vjust = "top",
               data = data.frame(date = c(as.Date('2020-12-27'), as.Date('2020-12-27')),
                                 y = c(150, 150),
                                 name = c('Weekly unemployment insurance benefit', 'Spending (card and cash)'))
     ) +
     geom_vline(xintercept = as.Date('2020-12-29'), linetype="dashed", 
                color = "gray", size=.5) +
     labs(x = "", y = "", subtitle = "Difference from first week of December", 
          color = "") +
     scale_color_manual("", values = colors_onset) +
     scale_shape_manual("", values = shape_onset) + 
     fte_theme("bottom") +
     theme(legend.text = element_text(size = 12), 
           legend.title = element_text(size=12, color = greys[7])) +
     guides(color = guide_legend(nrow=2, byrow = TRUE),
            shape = guide_legend(nrow=2, byrow = TRUE))
  ) %>% 
  gg_walk_save('onset_300_ui_cardcash', height_touse = 6, width_touse = 6) #previously fact5_onset_300_plot_main

# normalized spend as third panel
plot_onset_inc_spendtotal_norm <- 
  (data_300_onset_chg_w_norm %>%
     filter(week_start_date >= as.Date("2020-11-01"),
            week_start_date <= as.Date("2021-03-01")) %>%
     ggplot() +
     geom_line(aes(x = week_start_date, y = val, color = group, shape = group)) + 
     geom_point(aes(x = week_start_date, y = val, color = group, shape = group)) +
     facet_wrap(vars(name), scales = "free_y", nrow=3, ncol=1) +
     scale_y_continuous(labels = scales::dollar) +
     geom_text(label = "$300\nsupplement\nbegins",
               aes(x = date, y = y),
               color = greys[6],
               size = 3,
               hjust = "left",
               vjust = "top",
               data = data.frame(date = c(as.Date('2021-01-03')),
                                 y = c(800),
                                 name = c('Income: difference from first week of December'))
     ) +
     geom_vline(xintercept = as.Date('2020-12-29'), linetype="dashed",
                color = "gray", size=.5) +
     labs(x = "", y = "", subtitle = NULL,
          color = "") +
     scale_x_date(labels = date_format("%b '%y")) +
     scale_color_manual("", values = colors_onset) +
     scale_shape_manual("", values = shape_onset) + 
     fte_theme("bottom") +
     theme(legend.text = element_text(size = 12), 
           legend.title = element_text(size=12, color = greys[7])) +
     guides(color = guide_legend(nrow=3, byrow = TRUE),
            shape = guide_legend(nrow=3, byrow = TRUE))
  ) %>% 
  gg_walk_save('onset_300_inc_spendtotal_norm', height_touse = 9, width_touse = 6)
