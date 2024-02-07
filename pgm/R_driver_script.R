#R_driver_script.R
#Author: Katie Zhang, Rupsha Debnath
#Objective: shell script to run all analysis for latest draft

#################
# sample set-up #
#################
# If small_samp set to TRUE, we would run a 1% sub-sample. FALSE would run the 100% sample.
small_samp <- FALSE
# The next script reads in the full .rds files and creates a 1% sub-sample.
# If the 1% needs to be re-built after some changes to the pyspark build, 
# then this file would be run.
create_new_1pct_sample <- FALSE
if(create_new_1pct_sample){
  source("pgm/data_readin_1pct.R")
}

####################
# function scripts #
####################
print_stargazer_tables <- FALSE # switch activates in pgm/funcs/ui_functions.R
source(str_c(path_repo, 'pgm/funcs/ui_functions.R'))
source(str_c(path_repo, 'pgm/funcs/prelim.R'))
source(str_c(path_repo, "pgm/funcs/xtile_ten.R"))
# test_thats: if you want to run the driver without breaking. If warnings is TRUE, then we would get warnings
# for test_thats instead of errors.
warnings <- FALSE
if(warnings){
  source(str_c(path_repo, "pgm/funcs/test_that_modified.R"))
}

################
# build script #
################
# the scripts in this section read in data files built with pyspark, build and
# cleans the data, builds it in a form we want for analysis, and brings it to
# the customer-week level while also adding in spell-level information
re_run_build_scripts <- FALSE
# If running category codes:
run_categories <- FALSE

if(re_run_build_scripts){
  if(run_categories){
    source("pgm/ui_eip_data_read_in.R") # 3 hours
    source("pgm/ui_eip_data_build.R") # 2 hours
  } else {
    source("pgm/ui_eip_data_read_in.R") # 3 hours
    source("pgm/ui_eip_data_build.R") # 2 hours
    source("pgm/jobfind_build_1_of_2.R") #4 hours
    source("pgm/jobfind_build_2_of_2.R") #30 min  
  }
}

####################
# jobfind analysis #
####################

if(!run_categories){
  last_date <- as.Date("2021-03-14")
  
  #for summaries by duration
  duration_summary <- grouped_exit_rates(current_duration)
  
  #for summaries by week
  exit_summary <- grouped_exit_rates(week_start_date, last_date)
  
  # these scripts create controls (industry, age, gender, etc.), then attach to
  # our dataframes with information on unemployment spells, UI received, etc.
  # for analysis
  source("pgm/control_prep.R") # 30 mins
  source("pgm/rep_rate_prep.R") # 15 mins 
}

##################
# Spend analysis #
##################

#run_build is a toggle which describes whether the first parts, especially 
# the build scripts, of the driver script was run or not. 
# If run_build is TRUE, that means the build  was run, if FALSE then it wasn't run.
run_build <- FALSE
source(str_c(path_repo, 'pgm/spend_build.R')) # 30 minutes

#############################
# jobfind plots and outputs #
#############################

if(!run_categories){
  # these scripts produces output such as timeseries plots, regression tables,
  # binscatter plots, coefficient plots. these are our main output scripts
  source("pgm/timeseries_plots.R")  # 9 mins
  source("pgm/summer_expiration.R") # 11 mins
  source("pgm/rep_rate_tables.R") # 3 mins
  source("pgm/marginal_effects_hazard_calc.R")
  source("pgm/rep_rate_figs.R") # 30 secs
  source("pgm/weekly_coef_figs.R") # 5 mins
  source("pgm/ui_universe_read_in_plot.R") # 6 mins
  source("pgm/jobfind_tables.R") # 2 mins
  
  # these scripts make output that introduce some checks on our results. for
  # example, the first checks the quality of our industry variable by comparing
  # to an external benchmark, while the latter two interact with liquidity, age
  # and presence of kids
  base_reg <- "exit_ui ~ PctChange * SuppAvail"
  source("pgm/industry_mix_change.R") # 5 mins
  source("pgm/jobfind_liquidity.R") # 8 mins
  
  # this script saves a number of dataframes for use on the outside
  source("pgm/save_time_series_for_model.R") # 1.5 min
  
  # produce minimum aggregation standards & excel workbook
  source("pgm/jobfind_stats_export.R") # 8 mins
  
  # plots for production ----
  plot_list_prod <- #Note: currently in order of google docs
    c(list(
      exit_no_recall_shading = exit_no_recall_shading,
      exit_recall = exit_recall,
      exit_new_job_means = exit_new_job_means,
      timeseries_placebo_test = timeseries_placebo_test,
      timeseries_expiry_new_relative_trunc = timeseries_expiry_new_relative_trunc,
      timeseries_onset_new_relative = timeseries_onset_new_relative,
      binscatter_expiry_new_job = binscatter_expiry_new_job,
      binscatter_onset_new_job = binscatter_onset_new_job,
      active_spells_shade = active_spells_shade,
      n_ui_churn_recall = n_ui_churn_recall,
      scatter_d_ind_mix = scatter_d_ind_mix_symmetric,
      total_exits = total_exits,
      hero_by_start = plot_hero_by_start,
      plot_exit_new_job_drop_peuc = plot_exit_new_job_drop_peuc,
      timeseries_expiry_new_trunc = timeseries_expiry_new_trunc,
      timeseries_onset_new = timeseries_onset_new,
      timeseries_expiry_all = timeseries_expiry_all,
      timeseries_onset_all = timeseries_onset_all,
      timeseries_expiry_recall = timeseries_expiry_recall,
      timeseries_onset_recall = timeseries_onset_recall,
      weekly_beta_new_job_expire = weekly_beta_new_job_expire,
      weekly_beta_new_job_onset = weekly_beta_new_job_onset,
      plot_sepexpire_exitrate = plot_exit_newjob,
      binscatter_sepexpire_4weekcutoff = binscatter_summer_expiry_new_job,
      exit_reprate_sepexpire_4weekcutoff = plot_timeseries_sepexpire_new_relative
    )
    )
  
  pdf(str_c(path_out, Sys.Date(), "_plots_for_prod.pdf"), width = 8, height = 4.5,
      onefile = TRUE)
  plot_list_prod
  dev.off()
  
  for (i in seq_along(plot_list_prod)) {
    ggsave(str_c(path_out, names(plot_list_prod)[i], ".pdf"),
           plot_list_prod[[i]],
           width = 8, height = 4.5)
  }
  
  # tables for production ----
  list_of_tables <- # this won't actually make anything yet, just for keeping track
    c(list(
      micro_macro_disincentive = regs_1_collapsed,
      tbl_ui_flows = tbl_ui_flows, # turned into monthly ui flows on outside
      tbl_ui_flows = tbl_ui_flows,
      regressions_main_coef = regressions,
      regressions_alt = regressions_alt,
      reg_control_expire = reg_control_expire,
      reg_control_onset = reg_control_onset,
      reg_ctrl_liq_exp = reg_output_liq_ctrls$sg_table[[1]],
      reg_ctrl_liq_ons = reg_output_liq_ctrls$sg_table[[2]]
    )
    )
  
  # "tablebook"
  list(
    # pgm/rep_rate_tables.R
    regs_1_collapsed,
    regressions_collapsed,
    c("\n\nTable X: Micro effect of Expanded Benefits: Alternative Measures of Exit",
      regressions_alt),
    reg_control_expire_collapsed,
    reg_control_onset_collapsed,
    # pgm/jobfind_liquidity_tables.R
    reg_output_liq_ctrls_exp_collapsed,
    reg_output_liq_ctrls_ons_collapsed) %>%
    map_chr( ~ str_c(.x, collapse = "\n")) %>%
    writeLines(str_c(path_out, Sys.Date(), "_table_all.txt"))
}

#############################
# spend plots and outputs #
#############################

if(!run_categories){
  source(str_c(path_repo, 'pgm/spend_plots.R')) #2 mins
  source(str_c(path_repo, 'pgm/spend_summer_weekly.R')) 
  
  # Save list of plots
  plot_list_spend <-  #note: sizes of a lot of plots will be messed up in this PDF
    list(spending_ts_medians = plt_spending_ts_medians,
         spending_ts_means = plt_spending_ts_means,
         spending_ts_means_cardcash = plt_spending_ts_means_cardcash,
         waiting_levels_bothspend = plt_uionset_waiting_prod_levels_bothspend,
         waiting_diffs_spendtotal = plt_uionset_waiting_diffs_spendtotal,
         fpuc_exp_diff_ui_cardcash = plt_fpuc_expiration_diff_ui_cardcash,
         fpuc_exp_diff_inc_spentotal_norm = plt_fpuc_expiration_diff_inc_spendtotal_norm,
         fpuc_summer_expiration_diff_ui_cardcash = plt_fpuc_summer_expiration_diff_ui_cardcash,
         fpuc_summer_expiration_diff_inc_spendtotal_norm = plt_fpuc_summer_expiration_diff_inc_spendtotal_norm,
         jan_uionset_ui_cardcash = plot_onset_ui_cardcash,
         jan_uionset_inc_spentotal_norm = plot_onset_inc_spendtotal_norm
    )
  # Save plots to pdf
  pdf(pdf_out_spendplots, height = 6, width = 6)
  plot_list_spend
  dev.off() 
}

if(run_categories){
  source(str_c(path_repo, 'pgm/mpc_cats.R'))
} else {
  source(str_c(path_repo, 'pgm/mpc_robustness.R'))
}

if(!run_categories){
  source(str_c(path_repo, 'pgm/mpcs_more_controls.R'))
  # If run_categories was on, then turn it off before running the next scripts:
  source(str_c(path_repo, 'pgm/spend_by_liquidity_buffer.R'))
  source(str_c(path_repo, 'pgm/mpc_by_liquidity.R'))
  source(str_c(path_repo, 'pgm/spend_by_ever_recall.R'))
  source(str_c(path_repo, 'pgm/liquidity_distribution.R'))
  source(str_c(path_repo, 'pgm/liquidity_changes.R'))
  source(str_c(path_repo, 'pgm/low_prepand_liq.R'))
  source(str_c(path_repo, 'pgm/spend_summary_stats.R')) 
}
