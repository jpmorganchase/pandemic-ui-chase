# SPDX-License-Identifier: MIT
# summer_expiration.R
# Author: Rupsha Debnath
# This script produces exhibits for summer expirations.

# Set up----
source(str_c(path_repo, "pgm/funcs/summer_expiration_funcs.R"))

# IN
if (!exists("df_ui_cust_week")) {
  date_scratch_build_jf_2_of_2 <- "2023-06-16"
  df_ui_cust_week <- readRDS(str_c(
    data_path, "tmp/build_jf_2_of_2/",
    date_scratch_build_jf_2_of_2, "df_ui_cust_week.rds"
  ))
}
df_ui_cust_week_no_mi <- df_ui_cust_week %>%
  # remove Michigan from consideration as there is an unresolved bug
  filter(cust_state != "MI")
rep_rate_expand_300_expire_sep <- states_rep_rate_expand[!(states_rep_rate_expand %in% c("TX", "IN", "FL", "OH", "GA"))]
fortnight_states <- c("CA", "FL", "MI", "CO", "TX", "IL")

# Dates and windows
start_window_sepexpire <- as.Date("2021-01-01") # start date for jobfind onset of $300
end_window_sepexpire_benefit <- as.Date("2021-09-04") # end date for measuring weekly benefit amount prior to onset of $300 supplement
end_window_sepexpire <- as.Date("2021-11-08") # end date for jobfind onset of $300 plot
end_window_reg_sepexpire <- as.Date("2021-11-08") # end date for jobfind onset of $300 regression
start_window_sepexpire_reg_date <- as.Date("2021-07-08")

#OUTS
plot_exit_newjob_2022 <- "plot_sepexpire_exitrate.pdf"
binscatter_sep_expire <- "binscatter_sepexpire_4weekcutoff.pdf"
exit_rate_reprate_abovebelow <- "exit_reprate_sepexpire_4weekcutoff.pdf"
exhibit4_table_path <- "tbl_reg_est_sepexpire.csv"

# This dataframe creates durations used for PEUC cust exclusions
df_with_dur <-
  df_ui_cust_week_no_mi %>%
  filter(
    cust_state %in% rep_rate_expand_300_expire_sep,
    year(week_start_date) >= 2019,
    spell_active
  ) %>%
  select(-cust_type) %>%
  distinct() %>%
  arrange(cust_number, week_start_date, cust_state) %>%
  mutate(
    diff_dates_weeks = (week_start_date - week_start_date_ui_lag) / 7,
    reset_weeks_counter = ifelse(diff_dates_weeks > 26, 1, 0),
    reset_weeks_counter = replace_na(reset_weeks_counter, 0)
  ) %>% # This mutate creates a variable to restart the duration runnning sum
  group_by(
    cust_number, 
    spells_counter = cumsum(reset_weeks_counter == 1L)
  ) %>%
  mutate(weeks_active_run_sum = row_number()) %>% 
  ungroup() %>%
  dplyr::filter(
    cust_state %in% states_rep_rate_expand,
    (cust_state != "IN" | week_start_date >= as.Date("2020-02-02"))
  ) %>%
  mutate(
    exit_labor = ifelse(is.na(exit_labor), 0, exit_labor),
    exit_ui = ifelse(is.na(exit_ui), 0, exit_ui)
  )

# Exhibit 1: Exit Timeseries ----
weekly_summary_natl_sep <- df_with_dur %>%
  filter(exit_labor == 1, 
         cust_state %in% rep_rate_expand_300_expire_sep) %>%
  group_by(week_start_date) %>%
  weekly_summary(last_date_ = as.Date("2022-03-30"))

last_for_exits <- as.Date("2021-12-30")

exit_df <- weekly_summary_natl_sep %>%
  transmute(week_start_date, value = `Exit rate`) %>%
  filter(
    week_start_date >= start_window_sepexpire_reg_date,
    week_start_date <= last_for_exits
  )

plot_exit_newjob <- exit_df %>%
  filter(between(
    week_start_date,
    start_window_sepexpire_reg_date,
    end_window_reg_sepexpire
  )) %>%
  { max_exit_rate_df <<- max(.$value)
  .} %>%
  ggplot() +
  aes(x = week_start_date, y = (value * 100)) +
  geom_point(colour = navy0) +
  geom_line(colour = navy0) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b'%y") +
  scale_y_continuous(limits = c(0, 50)) +
  labs(
    x = "", y = "",
    subtitle = "Exit rate from unemployment benefits"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  ) +
  theme_bw() +
  annotate("rect", xmin = as.Date(start_window_sepexpire_reg_date),
    xmax = as.Date("2021-09-04"), ymax = Inf, ymin = -Inf, alpha = 0.1) +
  annotate("text", x = as.Date("2021-09-04") - 23, 
           y = 20*max_exit_rate_df, 
           hjust = "left",
           colour = greys[8],
           label = "$300 supplement\navailable") +
  fte_theme("none")
ggsave(str_c(path_out, plot_exit_newjob_2022), width = 8, height = 5)

# Exhibit 2 and 3 set up ----
median_benefits_sepexpire_expand <-
  df_with_dur %>%
  filter(!start_ui, !exit_ui) %>%
  get_median_benefits(
    states = rep_rate_expand_300_expire_sep,
    start = start_window_sepexpire,
    end = end_window_sepexpire_benefit,
    quant_type = 1
  ) %>%
  mutate(weekly_ben = ifelse(state %in% fortnight_states, reg_ben / 2, reg_ben)) %>% 
  ungroup()

sepexpire_expand <-
  df_with_dur %>%
  mutate(week_start_date = week_start_date + 7) %>%
  filter(
    between(
      week_start_date,
      start_window_sepexpire_reg_date,
      end_window_sepexpire
    ),
    spell_active
  ) %>%
  inner_join(
    calculate_per_change(median_benefits_sepexpire_expand, 300, 300),
    by = "cust_number"
  ) %>%
  group_by(cust_number) %>%
  mutate(
    spike = FALSE,
    post = week_start_date > end_window_sepexpire_benefit,
    dur_at_window_start = min(weeks_active_run_sum)
  ) %>%
  ungroup() 

# these exhibits are only for the duration cutoff of 4 weeks. So, the following line would be changed for a different cutoff
sepexpire_expand_nopeuc <- 
  sepexpire_expand %>%
  filter(dur_at_window_start < 4)

test_that(
  "93% of customers dropped by PEUC screen",
  {
    expect_equal(length(unique(sepexpire_expand$cust_number)),  130533 )
    expect_equal(length(unique(sepexpire_expand_nopeuc$cust_number)), 9277 )
  }
)

sepexpire <-
  sepexpire_expand_nopeuc %>%
  filter(week_start_date <= end_window_reg_sepexpire) %>%
  mutate(SuppAvail = !post)

# Exhibit 4 ----
# These are the slopes and the SEs for the regression estimates with different PEUC duration cutoffs,
# and whether or not they use a donut. The function uses the dataframe sepexpire_expand
# used to produce exhibits 2 and 3.
exhibit4_dfs <- list(
  donut_4weeks = exhibit_4_df(sepexpire_expand, 4, TRUE),
  nodonut_4weeks = exhibit_4_df(sepexpire_expand, 4, FALSE),
  donut_8weeks = exhibit_4_df(sepexpire_expand, 8, TRUE),
  nodonut_8weeks = exhibit_4_df(sepexpire_expand, 8, FALSE))

exhibit4_reg_list <- list(donut_4weeks = felm(exit_ui ~ SuppAvail * PctChange |0|0| cust_number, exhibit4_dfs$donut_4weeks),
                          nodonut_4weeks = felm(exit_ui ~ SuppAvail * PctChange |0|0| cust_number, exhibit4_dfs$nodonut_4weeks),
                          donut_8weeks = felm(exit_ui ~ SuppAvail * PctChange |0|0| cust_number, exhibit4_dfs$donut_8weeks),
                          nodonut_8weeks = felm(exit_ui ~ SuppAvail * PctChange |0|0| cust_number, exhibit4_dfs$nodonut_8weeks))

table_summer_expiration_latex <- exhibit4_reg_output(exhibit4_reg_list, "latex") %>%
  str_replace("cline", "cmidrule") %>%
  append("\\cmidrule(r){3-4}", 9) %>%
  append("& \\multicolumn{4}{c}{Max Observed Weeks Starting from 11 July 2021} \\\\", 10) %>%
  str_replace(" & 4 weeks & 4 weeks", "\\\\[-1.8ex] & 4 weeks & 4 weeks") %>%
  str_replace("\\[-1.8ex]\\\\hline", "\\[-1.8ex]\\\\toprule")
# remove the last hline:
table_summer_expiration_latex <- table_summer_expiration_latex[-c(21)]
# replace the different hlines to mirror the same as the tex table on outside of the firewall:
table_summer_expiration_latex[c(6, 13, 17, 21)] <- str_replace(table_summer_expiration_latex[c(6, 13, 17, 21)], 
                                                               "hline", c("toprule", "midrule", "midrule", "bottomrule"))
table_summer_expiration_latex %>%
  writeLines(str_c(path_out,  "table_summer_expiration.tex"))

table_summer_expiration_txt <- exhibit4_reg_output(exhibit4_reg_list, "text") %>%
  writeLines(str_c(path_out,  "table_summer_expiration.txt"))

# Exhibit 2 -----
# This produces figure 5 in the paper for september expirations.
expiry_new_job_data <- sepexpire_expand_nopeuc %>%
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  make_aggregate_summer()

for_intercept <- expiry_new_job_data %>%
  ungroup() %>%
  summarise(
    x = mean(-per_change),
    y = mean(estimate)
  )

grand_mean <- sepexpire_expand_nopeuc %>%
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  lm(exit_ui ~ post, .) %>%
  broom::tidy() %>%
  slice(2) %>%
  pull(estimate)

new_job_mean <- sepexpire_expand_nopeuc %>%
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  filter(post == 0) %>%
  group_by(week_start_date) %>%
  summarise(mean(exit_ui))

# slope and se from exhibit 4:
slope_4weeks_nodonut <- 
  exhibit4_reg_list$nodonut_4weeks$coefficients["SuppAvailTRUE:PctChange", "exit_ui"]

se_4weeks_nodonut <- 
  exhibit4_reg_list$nodonut_4weeks$se["SuppAvailTRUE:PctChange"]

binscatter_summer_expiry_new_job <- expiry_new_job_data %>%
  mutate(per_change = -per_change) %>%
  make_binscatter_aggregate_summer(grand_mean) +
  scale_x_continuous(
    name = "Change in benefits",
    labels = scales::percent_format(1L)
  ) +
  scale_y_continuous(name = NULL) + 
  annotate("text",
    y = 0.013, x = -1.3, hjust = "left",
    colour = "#727272",
    label = str_c(
      "Slope:", scales::number(slope_4weeks_nodonut, #Slope from the exhibit 4 table with 
                                                        #a duration cutoff of 4 weeks
                               accuracy = 0.001),
      "\nStandard error: ", round(se_4weeks_nodonut, #SE from the exhibit 4 table with 
                                                        #a duration cutoff of 4 weeks
                                  3))) +
  geom_segment(
    x = -1.1, xend = -1.17,
    y = 0.004, yend = 0.004,
    lineend = "square",
    linejoin = "bevel",
    colour = "#727272",
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  annotate("text",
    x = -1, y = 0.004,
    colour = "#727272",
    label = "Larger decrease\nin benefits"
  ) +
  geom_segment(
    x = -0.6, xend = -0.53,
    y = 0.004, yend = 0.004,
    lineend = "square",
    linejoin = "bevel",
    colour = "#727272",
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  annotate("text",
    x = -0.7, y = 0.004,
    colour = "#727272",
    label = "Smaller decrease\nin benefits"
  )
ggsave(str_c(path_out, binscatter_sep_expire), width = 8, height = 5)

# Exhibit 3 -----
# This is figure 4 in the paper for september expirations.
timeseries_sepexpire_df_new <- sepexpire_expand_nopeuc %>%
  filter(exit_labor == 1) %>%
  mutate(exit_not_to_recall = (exit_ui == 1 & start_recall == 0)) %>%
  group_by(above_median, week_start_date) %>%
  summarise(exit_rate = mean(exit_not_to_recall)) %>%
  filter(week_start_date <= as.Date("2021-12-30"))

timeseries_sepexpire_new_relative <- timeseries_sepexpire_df_new %>%
  group_by(above_median, pre = week_start_date < end_window_sepexpire_benefit) %>%
  mutate(average_search = mean(exit_rate)) %>%
  group_by(above_median) %>%
  mutate(
    exit_rate_plot = exit_rate,
    exit_rate = exit_rate / first(average_search)
  ) %>%
  { max_exit_rate_4week <<- max(.$exit_rate)
  .} 

plot_timeseries_sepexpire_new_relative <- timeseries_sepexpire_new_relative %>%
  ggplot() +
  geom_point(aes(as.Date(week_start_date), exit_rate,
                 colour = as.factor(above_median),
                 group = as.factor(above_median),
                 shape = as.factor(above_median))) +
  geom_line(aes(as.Date(week_start_date), exit_rate,
                colour = as.factor(above_median),
                group = as.factor(above_median),
                shape = as.factor(above_median))) +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%b '%y",
    limits = c(as.Date("2021-07-11"), end_window_reg_sepexpire)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = scales::percent_format(accuracy = 1L),
    expand = c(0.003, 0)
  ) +
  labs(
    subtitle = "Exit rate to new job relative to July/August group average",
    x = NULL, y = NULL
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  ) +
  theme_minimal() +
  annotate("rect",
    xmin = as.Date("2021-07-11"),
    xmax = as.Date("2021-09-04"),
    ymax = Inf, ymin = -Inf, alpha = 0.1
  ) +
  scale_colour_manual("",
                      labels = c("Lower PctChange",
                                 "Higher PctChange"),
                      values = c(`1` = "#ffae5f",
                                 `2` = "#004577"),
                      guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual("",
                      labels = c("Lower PctChange",
                                 "Higher PctChange"),
                      values = c(`1` = 17,
                                 `2` = 15),
                      guide = guide_legend(reverse = TRUE)) +
  fte_theme() +
  theme(legend.position =  c(0.95, 0),
        legend.justification = c(0.95, 0),
        legend.text = element_text(size = 12),
        legend.key = element_rect(colour = NA, fill = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA)) +
  annotate("rect", xmin = as.Date("2021-07-11"), xmax = as.Date("2021-09-04"),
           ymin = 0, ymax = 1.05*max_exit_rate_4week, alpha = 0.1) +
  annotate("text", x = as.Date("2021-09-04") - 23, 
           y = 0.06*max(timeseries_sepexpire_new_relative$exit_rate), 
           hjust = "left",
           colour = greys[8],
           label = "$300 supplement\navailable")+
  guides(color = guide_legend(nrow=3, byrow = TRUE),
         shape = guide_legend(nrow=3, byrow = TRUE))
ggsave(str_c(path_out, exit_rate_reprate_abovebelow), width = 8, height = 5)

