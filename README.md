# READme for uieip project
<img src="https://github.com/jpmorganchase/pandemic-ui-chase/assets/9026277/049e44ed-a7ed-47ec-92dd-18627cd58a8e" width="150" height="100"> <img src="https://github.com/jpmorganchase/pandemic-ui-chase/assets/9026277/23362f24-7b5d-4a8f-9e1a-9b9b21ca07ae" width="350" height="100">

#### PIs: Peter Ganong, Fiona Greig, Pascal Noel, Daniel M. Sullivan, Joseph Vavra
#### RAs: Max Liebeskind, Peter Robertson, Katie Zhang, Liam Purkey, Timotej Cejka, Rupsha Debnath
#### Date: November 21, 2023

This project reflects a collaboration between researchers who, at the time of working on this project, were at the University of Chicago (Peter Ganong, Pascal Noel, and Joseph Vavra) and the JPMorgan Chase Institute (Fiona Greig and Daniel Sullivan). The researchers worked collaboratively on this codebase and on the ideas reflected in the academic research paper ["Spending and Job-Finding Impacts of Expanded Unemployment Insurance Benefits: Evidence from Administrative Micro Data"](https://bpb-us-w2.wpmucdn.com/voices.uchicago.edu/dist/b/1275/files/2023/11/spending_job_finding_expanded_ui_November_2023.pdf), on which all five are co-authors. This repo contains the codebase that creates the figures and tables in the research paper which analyze JPMorgan Chase Institute data.




Contact information for maintainers: Pascal Noel (pascal.noel@chicagobooth.edu) and Peter Ganong (ganong@uchicago.edu)

*Note: This readme describes the entire repository used for this project. The partial replication kit submitted to the American Economics Association includes only the analysis `.R` scripts. The `.py` scripts and their driver script `ui_driver.sh` are not included.*

### Build
- `ui_driver.sh`: This driver script produces the entire build. Command-line options in `ui_driver.sh` are passed on to the main python script `pgm/daily_inflow_outflow_ui_recip.py` to specify the parts of the build and the time period for which the build should be executed.
- `pgm/daily_inflow_outflow_ui_recip.py`: This is the main python script and the only script called by `ui_driver.sh`. The output is a set of `hdfs` tables, which are also saved as `.rds` tables for analysis:
  - `demog`: tables with customer-by-month info on balances, demographics, and flows,
  - `eips`:
    - `eips_list`: tables of customer-level EIP transactions for UI customers, where EIP here refers exclusively to the April 2020 EIP round,
    - `eip_rounds_list`: tables of customer-level EIP transactions for UI customers with all 3 rounds of EIPs,
  - `weekly_cp`: customer-by-week-by-counterparty tables of labor and UI inflows,
  - `weekly_flows`: customer-by-week flows tables.
- `pgm/funcs/inflow_outflow_helper_funcs.py`: This script defines the helper functions called by `pgm/daily_inflow_outflow_ui_recip.py`. 

#### Input tables for Build
- List of customers with 2018 and 2019 JPMC activity as well as customer metadata
  - `institute_consumer.mwl_cust_covid_filters`: filtered customer list with 2018 and 2019 labor inflows
  - `institute_retail_curated.jpmci_customer_profile`: customer profile table
  - `institute_consumer.eip_cohort_info`: customer with EIP transaction details
- `institute_consumer.mwl_daily_income_rollup_for_covid_inc_updated`: daily inflows table
- `institute_consumer.outflows_rollup_by_day_granular`: daily outflows table
- `institute_retail_curated.jpmci_deposit_account`: deposit accounts table
- `institute_retail_curated.jpmci_customer_account_relationship`: customer-account relationship table
- `institute_retail_curated.jpmci_deposit_transaction`: : deposit transaction table (transaction-level)
- `institute_retail_curated.jpmci_transaction_counterparty_lookup`: firm-id crosswalk for deposit transaction table
- `institute_consumer.ui_nonui_cust_list`: list of UI and non-UI customers
- `institute_consumer.industry_classification_w4_sa`: cleaned at_counterparty values (including industries)
- `institute_consumer.mwl_ui_cp_raw_lookup_mar2021`: table with UI counterparties matched up with their respective state


### Analysis
The main driver script is:  
  
  * `pgm/R_driver_script.R`: produces a large number of plots, tables and statistics which appear in the July 2023 draft.
  
Non-Chase inputs:  

  * DOL ETA Form 203: state-month level count of unemployment insurance claims by NAICS 2-digit industry. File path: `xxx/gnlab/ui_covid/scratch/2021-08-19claimant_industry.csv`
    
  
##### Description of Script `pgm/R_driver_script.R`:

The driver script, `pgm/R_driver_script.R`, run the following scripts in the following order:    

**_Sample Set up:_**

  * To run the analysis on a 1% sample, set the vector `small_samp` to *TRUE*. Otherwise, the default is *FALSE* which runs the scripts on the full sample.
  * `pgm/data_readin_1pct.R`: If there are new builds made, and there is need to make a new 1% sample, then, set the vector `create_new_1pct_sample` to *TRUE*, which runs this script. It reads in the new full sample builds, and saves new 1% sample builds.

**_Setting up Functions:_**

  * `pgm/funcs/ui_functions.R`: a number of functions that are common across many later files. Functions include:
    * `gg_walk_save`: writes a ggplot object to PDF, and produces a CSV of the underlying data
    * `gg_point_line`: creates a line plot in ggplot, with a dot at each point on the line.
    * `diff_in_diff`: computes a difference-in-difference estimator, measured as the ratio of (change in treatment group)/(change in control group). The numerator and denominator of the ratio are themselves fractions corresponding to the year-on-year change in the treatment and control groups, respectively.
    * `yoy_change`: computes year-on-year change (or any ratio) estimator.
    * `fte_theme`: theme to construct plots with standardized aesthetic elements
    * `get_median_benefits`: Takes a customer week dataframe and returns the median benefits of the customer within a timeframe given by dates for start and end
    * `grouped_exit_rates`: produce exit rates by time or duration (including by recall status) for those who we observe a
  separation
    * `estimate`: find difference between average job-finding rate in two weeks prior to policy change to the first four weeks after the policy change.
    * `weekly_summary`: produces a weekly summary dataframe
  * `pgm/funcs/prelim.R`: makes function, `winsor`, to winsorize data
  * `pgm/funcs/xtile_ten.R`: makes a function, `xtile_ten`, that finds values at a specific percentile (but usually median) within JPMCI data while meeting data aggregation standards by taking the average of the ten values around the entered percentile.
  * `pgm/funcs/test_that_modified.R`: this is a modification to the `test_that` functions used in scripts, where instead of returning an error, as is usual, if this is run it gives a warning. To use this, set the vector `warnings` to *TRUE*. This is used extensively while running R batch submission scripts.

**_Build Script:_**

Before you run these scripts, there are two set up vectors that will determine how the driver script is run. If you would like to re-run the build scripts, then set the vector `re_run_build_scripts` to *TRUE*. Further, if you would like to run the disaggregated version of the build, which splits consumption into its constituent categories, then set the vector `run_categories` to *TRUE*. 

  * `pgm/ui_eip_data_read_in.R`: imports weekly counterparty files from `/data/jpmci/teams/gnlab/ui_covid`. This script reads in and lightly cleans RDS files from the PySpark build.
  * `pgm/ui_eip_data_build.R`: cleans up the imported data so that it is in a form useful for analysis
  * `pgm/jobfind_build_1_of_2.R` and `pgm/jobfind_build_2_of_2.R`: builds the following dataframes: 
    * `df_labor_cust_week` which is a dataframe at the customer-by-week level. Shows whether the customer has exited labor or exited UI to a new job or to recall.
    * `df_ui_cust_week_add_spells` which feeds into `df_ui_cust_week`, which is created in `jobfind_build_2_of_2`
    * `df_ui_cust_week_alt_horizon_basic` which feeds into `df_ui_cust_week_alt_horizon` (used as an end product for a plot in `timeseries_plots.R`), and compares various lengths of job seperation.
  * `pgm/jobfind_build_2_of_2.R`: uses a number of sample screens to further clean up the dfs from previous build scripts.

*NOTE: can skip the first three files and run straight from `pgm/jobfind_build_2_of_2.R` since the prior three builds and saves the relevant rds files and `pgm/jobfind_build_2_of_2.R` reads the files straight in. To run everything from `pgm/jobfind_build_2_of_2.R`, set `re_run_step1 <- FALSE` at the start.*

**_Jobfind Analysis:_**

  * Prep scripts to create controls and dataframes ready for analysis:
    * `pgm/control_prep.R`: this creates controls such as industry (based on organization that paid your last paycheck before separation), age (spell-level), gender.
    * `pgm/rep_rate_prep.R`: calculates the median benefits and % benefit change in two time periods: “expiration” (expiration of $600 FPUC at the end of August) and “onset” (onset of $300 at the start of January 2021).
  * Output scripts produce timeseries plots, DID plots, regression tables, etc.
    * `pgm/timeseries_plots.R`: make timeseries plots of exit rates for jobfind analysis using tmp_for_hazard_plot_expanded
      * Outputs: Figures 4, 5, A13ab, A14, A15, A16, A21
    * `pgm/summer_expiration.R`: makes timeseries plots for summer expirations, including exit rates and binscatters. 
      * Outputs: Figures A24ab, A25, Table A15
    * `pgm/rep_rate_tables.R`
      * Outputs: Tables 3, A2, A11b, A12, A13b, A14 
    * `pgm/marginal_effects_hazard_calc.R`: calculates inputs for hazard elasticity calculations done outside the firewall.
    * `pgm/rep_rate_figs.R`: This script produces plots for event study by above/below median rep rate as well as binscatter plots.
      * Outputs: Figures 6ab, 7ab, A17abcdef
    * `pgm/weekly_coef_figs.R`: This runs regressions with weekly coefficients to new job for binary (above vs below median) and weekly DID, then plots the coefficients.
      * Outputs: Figures A23ab
    * `pgm/ui_universe_read_in_plot.R`: Analyzes all UI recipients for comparison to those who meet the primacy screen (this is run after running all the analysis of the primacy screen)
      * Outputs: Figure A2a
    * `pgm/jobfind_tables.R`: make tables for job-finding analysis
  * Robustness checks on controls, e.g. benchmarking our industry mix and interacting our ‘main’ regression with liquidity:
    * `pgm/industry_mix_change.R`: assess the quality of the industry variable tagging in JPMCI by comparing to an external benchmark (Department of Labor ETA form 203) which gives data on UI claims by industry
      * Outputs: Figure A3
    * `pgm/jobfind_liquidity.R`: This runs regressions interacting with liquidity variable, which is measured as pre-period balance
      * Outputs: Tables A4, A5
  * `pgm/save_time_series_for_model.R`: produces model outputs that Joe Vavra uses on the outside
  * `pgm/jobfind_stats_export_jan22.R`: creates stats for text for export, minimum aggregation standards tables, other model input that is used on the outside, and a workbook (`[date]_ui_jobfind_for_export.xls`) which also includes any other data frame needed on the outside.
  
**_Spend Analysis:_**
  
  * `pgm/spend_build.R`: build data needed for the analysis of spending around UI.
  * `pgm/spend_plots.R`: create plots of spending for various event studies/ID strategies around UI.
    * Outputs: Figures 1, 2, 9ab, A4, A5, A6, A7, A8, A9, A10
  * `pgm/spend_summer_weekly.R`: produce summer expiration spend plots.
    * Outputs: Figures A11, A12
  * `pgm/mpc_robustness.R`: MPC calculations
    * Outputs: Tables 1, A10, A11a
  * `pgm/mpc_cats.R`: MPC calculations with disaggregated categories sample
    * Outputs: Tables A7, A8
  * `pgm/mpcs_more_controls.R`: MPC calculations with controls
    * Outputs: Table A9
  * `pgm/spend_by_liquidity_buffer.R`: Spending by pre-pandemic liquidity group 
    * Outputs: Figure 3, Table 2
  * `pgm/table2_V2.R`: Create another version of table 2
  * `pgm/spend_by_ever_recall.R`: Spending of recalled vs non-recalled workers
    * Outputs: Figure A22
  * `pgm/liquidity_distribution.R`: compute some statistics to summarise the magnitude of the reversal of liquidity between unemployed and employed households during the pandemic.
  * `pgm/liquidity_changes.R`: Produce liquidity change outputs for different treatment samples
    * Outputs: Table A6  
  * `pgm/low_prepand_liq.R`: Low pre-pandemic liquidity group characteristics
  * `pgm/spend_summary_stats.R`: Calculate some summary stats on spending and the spend samples

*Note: In the repo, there is a folder `r_batch_submission_scripts` with the same R scripts as in `pgm/` to run as a bash job on the edgenode, instead of on Rstudio.*

Prior to running the driver script, the pre-processing script `pgm/cust_labor_filter_table.py` creates a count of transactions at the customer-month level that is used in `pgm/daily_inflow_outflow_ui_recip.py` to filter the customer list to primary customers.`
  
**Important note on data structure of `cust_demo`**
There are 4 ‘cust_types’: `202021_ui_recipient, 2019_ui_recipient, nonui_2020, nonui_2019`. A `2019_ui_recipient` got UI in 2019, but they may also get UI in 2020. 
