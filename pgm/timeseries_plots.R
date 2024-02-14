# SPDX-License-Identifier: MIT
#timeseries_plots.R
#Author: Peter Ganong / Katie Zhang
#Date: 2021-12-29
#Purpose: make timeseries plots of exit rates for jobtmp_for_hazard_plot_expanded-finding analysis

# INS
produce_hamilton_fig_csv <- FALSE # if we are outputting csvs for hamilton draft

dfs_used_throughout_timeseries_plots <-
  # dataframe name + script where it was last generated in the current form
  tribble(~df_name, ~script_where_generated,
          "df_ui_cust_week", "jobfind_build_2_of_2.R",
          "df_ui_cust_week_alt_horizon", "jobfind_build_2_of_2.R",
          "tmp_for_hazard_plot_expanded", "control_prep.R")

last_for_exits <- last_date #- 21

date_breaks <- as.Date(c("2020-02-01",
                         "2020-04-01",
                         "2020-06-01",
                         "2020-08-01",
                         "2020-10-01",
                         "2020-12-01",
                         "2021-02-01"))
date_scale <- scale_x_date(name = NULL, breaks = date_breaks, date_labels = "%b '%y")

window_dates <- as.Date(c("2021-01-03", "2021-01-10"))
l_est_date <- as.Date("2020-07-26")
r_est_date <- as.Date("2020-12-27")
cutoff_dates <- as.Date(c("2020-04-01",
                          "2020-08-01",
                          "2021-01-01"))

levels_cohort_pre_expire <- c("Pre-pandemic", "Mar", "Apr", "May", "Jun")

y_pct <- scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                            limits = c(0, NA),
                            expand = c(0.003, 0))

# OUTS
# outputted in pgm/R_analytics_driver_jan22.R
timeseries_plots <-
  c(
    "n_ui_churn_recall",
    "exit_new_job_means",
    "timeseries_placebo_test",
    "exit_no_recall_shading",
    "exit_recall",
    "active_spells_shade",
    "plot_exit_new_job_drop_peuc",
    "total_exits",
    "plot_hero_by_start"
  )

################################################################################
# 1. CONSTRUCT DATAFRAMES ----
# construct summary files, unconditional on sep ----
weekly_summary_uncond <- 
  tmp_for_hazard_plot_expanded %>%
  dplyr::filter(cust_state %in% states_rep_rate_timeseries,
                week_start_date <= week_after_last_reliable_payment) %>%
  mutate(start_recall_if_exit = ifelse(exit_ui, start_recall, NA),
         week_start_date = as.Date(week_start_date + 7)) %>%
  group_by(week_start_date) %>% 
  summarise_at(vars(start_ui, spell_active, exit_ui, start_recall_if_exit),
               sum, 
               na.rm = TRUE) %>% 
  mutate(exit_ui_rate = exit_ui/spell_active,
         `Exit rate to recall` = start_recall_if_exit/spell_active,
         `Exit rate not to recall` = (exit_ui - start_recall_if_exit)/spell_active) %>%
  filter(year(week_start_date) >= 2019) # -Inf start_recall_to_exit in last week of 2018

# construct summary files by week and state-week, conditional on sep ----
df_ui_cust_week$week_start_date %>% max()

weekly_summary_natl_full <-
  df_ui_cust_week %>% 
  filter(exit_labor == 1,
         cust_state %in% states_rep_rate_timeseries,
         week_start_date >= as.Date("2019-01-01")) %>%
  group_by(week_start_date) %>%
  weekly_summary(last_date_ = last_date_exit_timeseries)

weekly_summary_natl <- weekly_summary_natl_full %>%
  filter(week_start_date >= as.Date("2020-01-19"),
         week_start_date <= as.Date("2021-03-01"))

weekly_summary_by_state <-
  df_ui_cust_week %>% 
  filter(exit_labor == 1,
         cust_state != "NaN") %>%
  group_by(week_start_date, cust_state) %>%
  weekly_summary()

small_state <- 
  (weekly_summary_by_state %>% 
     filter(week_start_date == as.Date("2020-12-13")) %>%
     group_by(cust_state) %>% 
     filter(sum(spell_active) <= 90) %>%
     distinct(cust_state))$cust_state

weekly_summary_big_states <- 
  weekly_summary_by_state %>%
  filter(!(cust_state %in% small_state),
         cust_state != "NaN") %>%
  group_by(cust_state) %>%
  mutate(`N spells (ratio to dec 2020)` =  spell_active/max(spell_active[week_start_date == as.Date("2020-12-13")])) %>%
  ungroup()


plot_national <- function(varname, end_date = last_date, start_date = as.Date("2020-01-01")) {
  weekly_summary_natl %>%
    filter(week_start_date >= start_date,
           week_start_date <= end_date) %>%
    ggplot(aes(x = week_start_date, y = {{varname}})) +
    geom_point(colour = navy0) + geom_line(colour = navy0) +
    labs(x = "", y = "", subtitle = as_label(enquo(varname))) +
    date_scale +
    fte_theme()
}

# exit rates by week by UI start month ----
weekly_summary_by_start <- 
  tmp_for_hazard_plot_expanded %>%
  dplyr::filter(exit_labor == 1,
                cust_state %in% states_rep_rate_timeseries,
                between(start_ui_date, as.Date("2020-01-01"), as.Date("2020-06-30"))) %>%
  mutate(week_start_date = as.Date(week_start_date + 7),
         start_month_cohort =
           factor(
             case_when(
               start_ui_date <= as.Date("2020-03-15") ~ "Pre-pandemic", 
               TRUE ~ as.character(format(start_ui_date, "%b"))
             ),
             levels = levels_cohort_pre_expire),
         start_recall_if_exit = ifelse(exit_ui, start_recall, NA)) %>%
  group_by(start_month_cohort, week_start_date) %>% 
  summarise_at(vars(start_ui, spell_active, exit_ui, start_recall_if_exit), sum, na.rm = TRUE) %>% 
  mutate(exit_ui_rate = exit_ui/spell_active,
         `Exit rate to recall` = start_recall_if_exit/spell_active,
         `Exit rate not to recall` = (exit_ui - start_recall_if_exit)/spell_active) %>%
  ungroup()

#exit rates by week, with different definitions of exit (various lengths of job separation) ----
date_scratch_build_jf_2_of_2 <- "2023-06-16"
df_ui_cust_week_alt_horizon <- readRDS(str_c(data_path, "tmp/build_jf_2_of_2/", date_scratch_build_jf_2_of_2, "df_ui_cust_week_alt_horizon.rds"))

weekly_summary_compare_horizon <- 
  df_ui_cust_week_alt_horizon %>%
  filter(exit_labor == 1,
         week_start_date <= week_after_last_reliable_payment,
         cust_state %in% states_rep_rate_timeseries) %>%
  mutate(exit_ui = ifelse(is.na(exit_ui), 0, exit_ui),
         week_start_date = week_start_date + 7) %>%
  group_by(week_start_date) %>%
  summarise_at(vars(exit_ui, spell_active, exit_ui_32, spell_active_32, exit_ui_54, spell_active_54), sum) %>% 
  mutate(exit_ui_43_rate = exit_ui/spell_active,
         exit_ui_32_rate = exit_ui_32/spell_active_32,
         exit_ui_54_rate = exit_ui_54/spell_active_54)

# exit rate by week, disaggregated by whether job separation is observed ----
sep <- c("No observed job separation", "Observed job separation")
weekly_summary_by_sep <-
  tmp_for_hazard_plot_expanded %>%
  dplyr::filter(cust_state %in% states_rep_rate_timeseries) %>%
  mutate(week_start_date = week_start_date + 7) %>%
  group_by(week_start_date, exit_labor) %>%
  summarise(exit_ui_rate = sum(exit_ui)/sum(spell_active), exit_ui = sum(exit_ui)) %>% 
  ungroup() %>%
  filter(exit_labor %in% c(0, 1)) %>% # remove nonsensical values, e.g. infinity or NA
  mutate(exit_labor = ifelse(exit_labor == 0, sep[1], sep[2]))

# 2. CONSTRUCT PLOTS ----
# n ui churn recall plot ----
# construct data frames for plotting
weekly_summary_if_sep <- tmp_for_hazard_plot_expanded %>%
  dplyr::filter(cust_state %in% states_rep_rate_timeseries) %>%
  filter(exit_labor == 1,
         between(week_start_date, as.Date("2020-01-01"), as.Date("2021-03-01"))) %>%
  mutate(start_recall_if_exit = ifelse(exit_ui, start_recall, NA),
         start_new_job_if_exit = ifelse(exit_ui, start_new_job, NA),
         week_start_date = week_start_date + 7) %>%
  group_by(week_start_date) %>% 
  #note: I need na.rm = TRUE here b/c start_recall_if_exit is only coded in weeks with an exit
  summarise_at(vars(start_ui, spell_active, exit_ui,
                    start_recall_if_exit, start_new_job_if_exit),
               sum, na.rm = TRUE) %>%
  mutate(exit_ui_rate = exit_ui/spell_active,
         `Exit with recall` = start_recall_if_exit/spell_active,
         `Exit without recall` = (exit_ui - start_recall_if_exit)/spell_active,
         start_recall_if_exit = start_recall_if_exit/exit_ui,
         start_new_job_if_exit = start_new_job_if_exit/exit_ui,
         `N exit with recall` = exit_ui * start_recall_if_exit,
         `N exit without recall` = exit_ui - `N exit with recall`)

n_ui_churn_recall <- weekly_summary_if_sep %>%
  filter(as.Date(week_start_date) <= as.Date("2021-03-01")) %>%
  select(week_start_date, start_ui) %>%
  left_join(weekly_summary_if_sep %>% transmute(week_start_date = week_start_date + 7,
                                                exit_recall = `N exit with recall` * -1,
                                                exit_no_recall = `N exit without recall` * -1)) %>%
  filter(!is.na(exit_recall)) %>%
  pivot_longer(-week_start_date) %>%
  mutate(name = fct_relevel(as.factor(case_when(name == "start_ui" ~ "Start",
                                                name == "exit_recall" ~ "Exit to recall",
                                                TRUE ~ "Exit to new job")), "Start", "Exit to recall")) %>%
  ggplot(aes(x = week_start_date, y = value, fill = name)) + geom_col() +
  date_scale + scale_y_continuous(labels = scales::comma, breaks = seq(-10000, 40000, 10000)) +
  labs(x = "", y = "", subtitle = "Number of benefit recipients", fill = "") +
  scale_fill_manual(values = c(light_orange0, green0, cyan0)) +
  fte_theme() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))

# placebo plots ----
exit_new_job <- weekly_summary_natl %>%
  transmute(week_start_date, value = `Exit rate not to recall`) %>% 
  filter(week_start_date >= as.Date("2020-04-12"),
         week_start_date <= as.Date("2021-03-01"),
         !(week_start_date %in% window_dates)) %>%
  ggplot() +
  aes(x = week_start_date, y = value, group = week_start_date <= window_dates[1]) +
  geom_point(colour = navy0) +
  geom_line(colour = navy0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) + 
  fte_theme("none") +
  date_scale +
  labs(x = "", y = "", subtitle = "Exit rate to new job from unemployment benefits")

segment <- function(dates) {
  dates <- as.Date(dates)
  ymean <- weekly_summary_natl %>% 
    filter(week_start_date >= dates[1],
           week_start_date <= dates[2]) %>% 
    summarise(mean(`Exit rate not to recall`)) %>% 
    pull()
  seg <- annotate("segment", x = dates[1] - 1, xend = dates[2] + 1,
                  y = ymean,
                  yend = ymean,
                  colour = "maroon", 
                  alpha = 0.4,
                  size = 4)
}

exit_new_job_means <-
  exit_new_job + 
  segment(c("2020-07-19", "2020-07-26")) +
  segment(c("2020-08-02", "2020-08-23")) +
  segment(c("2020-12-20", "2020-12-27")) +
  segment(c("2021-01-17", "2021-02-07")) +
  geom_vline(xintercept = l_est_date + 3, linetype = "longdash") +
  geom_vline(xintercept = r_est_date + 10, linetype = "longdash") +
  annotate("text", x = l_est_date - 1, y = 0.006, hjust = "right",
           label = str_c("Expiration of\n$600 supplement\n\n", 
                         "Change:\n", round(estimate(l_est_date)*100, 2), " p.p.")) +
  annotate("text", x = r_est_date + 12, y = 0.006, hjust = "left",
           label = str_c("Onset of $300\nsupplement\n\n", 
                         "Change:\n", round(estimate(r_est_date, donut = 14)*100, 2), " p.p."))

placebo_df <- c(seq(as.Date("2020-04-12"), as.Date("2020-06-28"), 7),
                seq(as.Date("2020-08-02"), as.Date("2020-11-29"), 7)) %>% 
  map_dbl(estimate) %>%
  tibble(placebo = .)

cross_positions <- tribble(~x_coord, ~y_coord,
                           estimate(as.Date("2020-12-27"), donut = 14), 0,
                           estimate(as.Date("2020-07-26")), 0)

timeseries_placebo_test <- 
    ggplot() +
    geom_histogram(data = placebo_df,
                   aes(x = placebo),
                   bins = 15, fill = cyan0) +
    geom_point(data = cross_positions,
               aes(x = x_coord, y = y_coord),
               shape = 4, size = 5) +
    annotate("text", x = estimate(as.Date("2020-12-27"), donut = 14) * 0.9, y = 2,
             label = str_c("Onset of $300\nsupplement\n",
                           "Change:\n", round(estimate(as.Date("2020-12-27"), donut = 14)*100, 2), " p.p.")) +
    annotate("text", x = estimate(as.Date("2020-07-26")) * 0.9, y = 2,
             label = str_c("Expiration of $600\nsupplement\n",
                           "Change:\n", round(estimate(as.Date("2020-07-26"))*100, 2), " p.p.")) +
    labs(x = "Change in exit rate", 
         y = "Number of placebo estimates",
         subtitle = "Change in exit rate: supplement change vs. placebo dates with no change") +
    expand_limits(x = -0.005)  +
    fte_theme()

# exit no recall plots ----
exit_no_recall <- weekly_summary_natl %>%
  transmute(week_start_date, value = `Exit rate not to recall`) %>% 
  filter(week_start_date >= as.Date("2020-01-01"),
         week_start_date <= as.Date("2021-03-01"),
         !(week_start_date %in% window_dates)) %>%
         { max_date <<- max(.$week_start_date)
         .} %>%
  ggplot() +
  aes(x = week_start_date, y = value, group = week_start_date <= window_dates[1]) +
  geom_point(colour = navy0) +
  geom_line(colour = navy0) +
  fte_theme("none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.062),
                     expand = c(0.003, 0)) + 
  date_scale + 
  labs(x = "", y = "",
       subtitle = "Exit rate to new job from unemployment benefits")

exit_no_recall_shading <- exit_no_recall +
  annotate("rect", xmin = cutoff_dates[1], xmax = cutoff_dates[2] - 7,
           ymin = 0, ymax = 0.062, alpha = 0.16) +
  annotate("rect", xmin = cutoff_dates[3], xmax = max_date + 14,
           ymin = 0, ymax = 0.062, alpha = 0.08) +
  annotate("text", x = cutoff_dates[1] + 3, y = 0.006, hjust = "left",
           colour = greys[8], size = 3,
           label = "$600 supplement\navailable") +
  annotate("text", x = cutoff_dates[3] + 3, y = 0.006, hjust = "left",
           colour = greys[8], size = 3,
           label = "$300 supplement\navailable") 

exit_recall <- plot_national(`Exit rate to recall`, end_date = as.Date("2021-03-01")) +
  labs(subtitle = "Exit rate to recall from unemployment benefits") +
  scale_y_continuous(limits = c(0, 0.062),
                     labels = scales::percent_format(1L),
                     expand = c(0.003, 0)) +
  annotate("rect", xmin = cutoff_dates[1], xmax = cutoff_dates[2] - 7,
           ymin = 0, ymax = 0.062, alpha = 0.16) +
  annotate("rect", xmin = cutoff_dates[3], xmax = max_date + 14,
           ymin = 0, ymax = 0.062, alpha = 0.08) +
  annotate("text", x = cutoff_dates[1] + 3, y = 0.06,
           hjust = "left", vjust = "top",
           colour = greys[8], size = 3,
           label = "$600 supplement\navailable") +
  annotate("text", x = cutoff_dates[3] + 3, y = 0.06,
           hjust = "left", vjust = "top",
           colour = greys[8], size = 3,
           label = "$300 supplement\navailable")

if (produce_hamilton_fig_csv) {
  tmp_exit_no_recall <- weekly_summary_natl %>%
    transmute(week_start_date,  `Exit rate not to recall`) %>% 
    filter(week_start_date >= as.Date("2020-01-01"),
           week_start_date <= as.Date("2021-03-01"),
           !(week_start_date %in% window_dates))
  
  write_csv(tmp_exit_no_recall,
            str_c(path_out, "df_exit_no_recall.csv"))
}

# active spell plots ----
active_spells <- plot_national(spell_active, end_date = as.Date("2021-03-01")) +
  labs(subtitle = "Number of active spells") +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, NA)) +
  scale_color_manual(values = jpmci_colors)

max_n_spells <- weekly_summary_natl %>%
  filter(week_start_date >= as.Date("2020-01-01")) %>%
  summarise(max(spell_active)) %>%
  pull()

active_spells_shade <- active_spells +
  annotate("rect", xmin = cutoff_dates[1], xmax = cutoff_dates[2] - 7,
           ymin = 0, ymax = max_n_spells * 1.02, alpha = 0.16) +
  annotate("rect", xmin = cutoff_dates[3], xmax = as.Date("2021-03-01") + 7,
           ymin = 0, ymax = max_n_spells * 1.02, alpha = 0.08) +
  annotate("text", x = cutoff_dates[2] - 10, y = 20000, hjust = "right",
           colour = greys[8],
           label = "$600 supplement\navailable") +
  annotate("text", x = cutoff_dates[3] + 3, y = 20000, hjust = "left",
           colour = greys[8],
           label = "$300 supplement\navailable")


active_spells_state_breaks <- as.Date(c("2020-01-01",
                                        "2020-07-01",
                                        "2021-01-01"))

active_spells_state_labels <- c("Jan 20",
                                "Jul 20",
                                "Jan 21")

active_spells_state_scale <- scale_x_date(name = NULL,
                                          breaks = active_spells_state_breaks,
                                          labels = active_spells_state_labels)

last_date <- as.Date("2021-03-14")

# drop PEUC states ----
weekly_summary_natl_ex_peuc <-
  df_ui_cust_week %>% 
  filter(exit_labor == 1,
         cust_state %in% states_rep_rate_timeseries, 
         week_start_date <= as.Date("2021-03-01")) %>%
  anti_join(peuc_recip_to_drop) %>%
  group_by(week_start_date) %>% 
  weekly_summary() 

peuc_plot_colors <- c('All' = '#ffae5f',
                      'Drop PEUC in 4 states' = '#004577')
peuc_plot_shape <- c('All' = 19,
                      'Drop PEUC in 4 states' = 15)

plot_exit_new_job_drop_peuc <- 
  bind_rows(
    weekly_summary_natl %>% mutate(key = "All"),
    weekly_summary_natl_ex_peuc %>% mutate(key = "Drop PEUC in 4 states")
  ) %>%
  filter(week_start_date >= as.Date("2020-10-01"),
         week_start_date <= as.Date("2021-03-01")) %>%
         { max_value <<- max(.$`Exit rate not to recall`)
         .} %>%
  ggplot() +
  geom_point(aes(x = week_start_date, y = `Exit rate not to recall`, color = key, shape = key)) + 
  geom_line(aes(x = week_start_date, y = `Exit rate not to recall`, color = key, shape = key)) +
  scale_y_continuous(labels = scales::percent_format(1L),
                     limits = c(0, 1.05*max_value),
                     expand = c(0.003, 0)) +
  scale_color_manual(values = peuc_plot_colors) +
  scale_shape_manual(values = peuc_plot_shape) +
  labs(x = "", y = "", subtitle = "Exit rate not to recall") + 
  scale_x_date(name = NULL, 
               breaks = as.Date(c("2020-11-01",
                                  "2021-01-01",
                                  "2021-03-01")), 
               labels = c("Nov 20", "Jan 21", "Mar 21")) + 
  fte_theme() +
  theme(legend.key = element_rect(colour = NA, fill = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size=12, color = greys[7])) +
  guides(color = guide_legend(nrow=3, byrow = TRUE),
         shape = guide_legend(nrow=3, byrow = TRUE))

# total exits ----
total_exits <- weekly_summary_natl %>%
  filter(week_start_date <= as.Date("2021-03-01")) %>%
  transmute(week_start_date,
            total_exits = `Exit rate`) %>%
  ggplot() +
  aes(x = week_start_date, y = total_exits) +
  geom_line(colour = navy0) +
  geom_point(colour = navy0) +
  annotate("rect", xmin = cutoff_dates[1], xmax = cutoff_dates[2] - 7,
           ymin = 0, ymax = 0.08, alpha = 0.16) +
  annotate("rect", xmin = cutoff_dates[3], xmax = max_date + 14,
           ymin = 0, ymax = 0.08, alpha = 0.08) +
  annotate("text", x = cutoff_dates[1] + 3, y = 0.006, hjust = "left",
           colour = greys[8], size = 3,
           label = "$600 supplement\navailable") +
  annotate("text", x = cutoff_dates[3] + 3, y = 0.006, hjust = "left",
           colour = greys[8], size = 3,
           label = "$300 supplement\navailable") +
  fte_theme() +
  labs(x = NULL, y = NULL,
       subtitle = "All exits") +
  date_scale +
  scale_y_continuous(labels = scales::percent_format(1L),
                     limits = c(0, 0.08),
                     breaks = seq(0, 0.08, 0.02),
                     expand = c(0.003, 0))

# plots showing heterogeneity in UI exit rates by start ----
weekly_summary_by_start_2020 <-
  weekly_summary_by_start %>% 
  filter(week_start_date >= as.Date("2020-02-09"),
         week_start_date <= as.Date("2021-01-01"),
         (start_month_cohort != "Jun" | week_start_date > as.Date("2020-07-01")),
         (start_month_cohort != "May" | week_start_date > as.Date("2020-06-01")))

exit_new_job_max_y <-
  max(weekly_summary_by_start_2020$`Exit rate not to recall`) %>%
  plyr::round_any(., 0.005, f = ceiling)

plot_hero_by_start <- 
  (weekly_summary_by_start_2020 %>%
     mutate(exit_new_job = `Exit rate not to recall`) %>%
     gg_point_line("week_start_date", "exit_new_job", "start_month_cohort") +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                        limits = c(0, exit_new_job_max_y),
                        expand = c(0.003, 0)) +
     labs(subtitle = "Exit rate to new job from unemployment benefits", color = "Benefit start date") +
     date_scale + 
     scale_color_manual(values = colors_qual) +
     labs(x = "", y = "") +
     fte_theme("bottom") )
