# SPDX-License-Identifier: MIT
##ui_functions.R
##Max Liebeskind, July 2020
library(RColorBrewer)
library(grid)

#path_out = paste0(path_repo, 'out/pub/')
path_mil_csvs <- paste0(path_out, 'milcsvs/')
output_mil_global <- FALSE
indiv_plot_global <- TRUE

gg_walk_save <- function(gg_obj, 
                         name = "test_plot", 
                         mil_csv_path = path_mil_csvs, 
                         output_mil = output_mil_global,
                         indiv_plot = indiv_plot_global,
                         height_touse = 4.5,
                         width_touse = 8,
                         path_out_indiv = str_c(path_repo, 'out/pub/')) {
  ## Function gg_walk_save writes a ggplot object to PDF, and produces a CSV of the underlying data.
  ## INPUTS: gg_obj (ggplot object): ggplot object to write to csv.
  ##         name (string): pdf will be path_out/date<name>.pdf, csv will be mil_csv_path/name.csv
  ##         mil_csv_path (string, filepath): filepath to write csv to.
  ## OUTS: returns ggobj, and outputs a pdf and a csv
  
  if (indiv_plot) ggsave(str_c(path_out_indiv, name, ".pdf"), gg_obj, height = height_touse, width = width_touse)
  
  #write CSV of underlying plot data for MIL
  if(output_mil) ggplot_build(gg_obj)$plot$data %>% write_csv(str_c(mil_csv_path, name, '.csv'))
  
  gg_obj}

gg_point_line <- function(df, x_, y_, color_, title = waiver(), 
                          x_lab = "", y_lab = "", leg_title = "",
                          leg_position = 'bottom') {
  ## Function gg_point_line creates a line plot in ggplot, with a dot at each point on the line.
  ## INS:
  ##      df (R dataframe): dataframe of data to create plot with
  ##      x_ (string): name of x-axis column in df
  ##      y_ (string): name of y-axis column in df
  ##      color_ (string): name of color (ie, group) column in df.
  ##      title (string, default blank): title of plot
  ##      x_lab (string, default blank): x-axis title
  ##      y_lab (string, default blank): y-axis title
  ##      leg_title (string, default blank): legend title
  ##      leg_position (string, default bottom): position of legend placement.
  
  ##OUTS: returns a ggplot object
  
  #create ggplot line plot
  ggp<- 
    df %>% 
    select({{x_}}, {{y_}}, {{color_}}) %>% 
    ggplot(aes_string(x = {{x_}}, y = {{y_}}, color = {{color_}})) + geom_line() + geom_point()  +
    ggtitle(title) + xlab(x_lab) + ylab(y_lab) + theme(legend.position = leg_position) + 
    labs(color = leg_title, shape = leg_title)
  
  return(ggp)
}



diff_in_diff <- function(df, grouper_, time_var_ = 'weeks_since_start',
                         treatment_num_ = 'total_outflows_mainyear', 
                         treatment_denom_ = 'total_outflows_precyear',
                         control_num_ = 'nonui_outflows_mainyear', 
                         control_denom_ = 'nonui_outflows_precyear',
                         norm_time = NA) {
  ## Function diff_in_diff computes a difference-in-difference estimator, measured as the ratio of (change in treatment group)/(change in control group).
  ## The numerator and denominator of the ratio are themselves fractions corresponding to the year-on-year change in the treatment and control groups, respectively.
  ##
  ## INS: 
  ##      df (R dataframe): dataframe of underlying data.
  ##      grouper_ (string, column name): name of the column in df to group by.
  ##      time_var_ (string, column name): name of time variable in df.
  ##      treatment_num_, treatment_denom_ (strings, column names): names of the numerator and denominator columns for calculating year-on-year change of treatment group.
  ##      control_num_, control_denom_ (strings, column names): names of the numerator and denominator columns for calculating year-on-year change of control group.
  ##      norm_time (default NA, else a value in the time_var_ column): time_var_ value by which to normalize diff-in-diff measure. If NA, normalize by min(time_var_).
  ##
  ## OUTS: returns an R dataframe with columns with diff-in-diff and normalized diff-in-diff values, one row per grouper_-by-time_var_ value

  #compute diff-in-diff dataframe
  df_dd <- df %>% 
    rename_("time" = time_var_, "group" = grouper_, 
            'treatment_num' = treatment_num_, 'treatment_denom' = treatment_denom_,
            'control_num' = control_num_, 'control_denom' = control_denom_) %>%
    group_by_("group", "time") %>% 
    summarise(diff_in_diff = (mean(treatment_num, na.rm = TRUE)/mean(treatment_denom, na.rm = TRUE))/
                (mean(control_num, na.rm = TRUE)/mean(control_denom, na.rm = TRUE)),
              ct_rows = n()) %>% 
    ungroup() %>% 
    select(group, time, diff_in_diff, ct_rows)
    
  #normalize relative to earliest time period, or to user-specified time
  normalizer <- df_dd %>% 
    filter(if(is.na(norm_time)) time == min(time) else time==norm_time) %>% 
    transmute(group, norm_factor = diff_in_diff)
  
  df_out <- df_dd %>% 
    left_join(normalizer, by = 'group') %>% 
    transmute(group, time, diff_in_diff, diff_in_diff_norm = diff_in_diff/norm_factor, ct_rows)
    
  return(df_out)
}

yoy_change <- function(df, 
                       grouper_, 
                       num_,
                       denom_,
                       time_var_ = 'weeks_since_start',
                       norm_time = NA) {
  ## Function yoy_change computes year-on-year change (or any ratio) estimator.
  ##
  ## INS: 
  ##      df (R dataframe): dataframe of underlying data.
  ##      grouper_ (string, column name): name of the column in df to group by.
  ##      time_var_ (string, column name): name of time variable in df.
  ##      num_ (string, column name): column name of numerator in the year-on-year change or ratio calculation.
  ##      denom_ (string, column name): column name of denominator in the year-on-year change or ratio calculation.
  ##      norm_time (default NA, else a value in the time_var_ column): time_var_ value by which to normalize diff-in-diff measure. If NA, normalize by min(time_var_).
  ##
  ## OUTS: returns an R dataframe with columns with YoY change and normalized YoY change values, one row per grouper_-by-time_var_ value
  
  
    #compute yoy change dataframe
  df_yoy <- 
    df %>% 
    rename_("time" = time_var_, "group" = grouper_,
            "num" = num_, "denom" = denom_) %>% 
    group_by_("group", "time") %>% 
    summarise(yoy_change = mean(num, na.rm = TRUE)/mean(denom, na.rm = TRUE),
              ct_rows = n()) %>%
    ungroup() %>% 
    select(group, time, yoy_change, ct_rows)
  
  normalizer <- df_yoy %>% 
    filter(if(is.na(norm_time)) time == min(time) else time==norm_time) %>% 
    transmute(group, norm_factor = yoy_change)
  
  df_out <- df_yoy %>% 
    left_join(normalizer, by = 'group') %>% 
    transmute(group, time, 
              yoy_change_norm = yoy_change/norm_factor - 1, 
              yoy_change = yoy_change - 1, 
              ct_rows)
  
  return(df_out)
}

fte_theme <- function(leg_pos = c(0, 0)) {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = "white"
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=12) + #, base_family = "serif"
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.major.x=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position= leg_pos, legend.justification = leg_pos) +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(size = 14, color=color.axis.title)) +
    theme(legend.spacing.x = unit(0.3, 'cm')) +
    
    # Set title and axis labels, and format these and tick marks
    theme(axis.text=element_text(size = rel(1.2), color=color.axis.text)) +
    theme(axis.title.x=element_text(size = rel(1.2), color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size = rel(1.2), color=color.axis.title, vjust=1.25)) +
    
    
    # format facet labels
    theme(plot.subtitle = element_text(size = rel(1.2), color=color.axis.title)) +
    
    # format facet labels
    theme(strip.text = element_text(size = rel(1.2), color=color.axis.text)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) +
    theme(plot.title.position = "plot")
}


#I start with key functions then source the remainder of the code


get_median_benefits <- function(df, states = "NY", start, end, quant_type = 3){
  #Takes a customer week dataframe and returns the median benefits of the
  #customer within a timeframe given by dates for start and end
  df %>% 
    dplyr::filter(cust_state %in% states, 
           ui_inflows_mainyear > 0,
           between(week_start_date, as.Date(start), as.Date(end))) %>%
    group_by(cust_number) %>%
    mutate(#Note that can't use the median function because it will average
           # Between the central 2 weeks
           reg_ben = quantile(ui_inflows_mainyear, 
                              0.5, type = quant_type)) %>%
    ungroup() %>%
    dplyr::filter(reg_ben == ui_inflows_mainyear) %>%
    group_by(cust_number) %>%
    dplyr::filter(row_number() == n()) %>%
    ungroup() %>%
    select(meas_date = week_start_date, 
           state = cust_state,
           cust_number,
           reg_ben)
}


grouped_exit_rates <- function(time_variable, last_reliable_date = last_date - 21){
  
  # This function sets up a function to produce exit rates by
  # time or duration (including by recall status) for those who
  # we observe a separation
  
  time_variable <- enquo(time_variable)
  
  function(df, ..., 
           start = "2019-01-01",
           spell_start = "2018-01-01") {
    
    
    df <- df %>%
      filter(exit_labor == 1,
             between(week_start_date, as.Date(start), last_reliable_date),
             between(start_ui_date, as.Date(spell_start), last_reliable_date)) %>%
      mutate(start_recall_if_exit = ifelse(exit_ui & week_start_date <= last_reliable_date, start_recall, NA),
             start_new_job_if_exit = ifelse(exit_ui & week_start_date <= last_reliable_date, start_new_job, NA),
             per_change = if("per_change" %in% colnames(.)) per_change * spell_active else NA,
             income_2019 = if("income_2019" %in% colnames(.)) income_2019 * spell_active else NA,
             regular_benefits = if("regular_benefits" %in% colnames(.)) regular_benefits * spell_active else NA) 
    
    variables <- c("spell_active", "exit_ui", 
                   "start_ui", "start_recall_if_exit", 
                   "start_new_job_if_exit", "per_change",
                   "regular_benefits", "income_2019")
    
    variables <- variables[variables %in% names(df)]
    
    
    df %>%
      group_by(!!time_variable, ...) %>% 
      #note: I need na.rm = TRUE here b/c start_recall_if_exit is only coded in weeks with an exit, per change should be NA when not recieving benefits
      summarise_at(vars(variables), sum, na.rm = TRUE) %>%
      ungroup() %>%
      #Weeks with no events should be coded as 0
      complete(!!time_variable, ...) %>%
      mutate_if(is.numeric, ~if_else(is.na(.), 0, round(., 0))) %>% #was generating NAs, note means change slightly
      transmute(!!time_variable, 
                ...,
                per_change = per_change / spell_active,
                regular_benefits = regular_benefits / spell_active,
                income_2019 = income_2019 / spell_active,
                exit_ui_rate = exit_ui / spell_active,
                exit_to_recall = start_recall_if_exit / (spell_active - start_recall_if_exit),
                exit_not_to_recall = (exit_ui - start_recall_if_exit) / (spell_active - exit_ui + start_recall_if_exit),
                cumulative_recall = cumsum(start_recall_if_exit) / sum(start_ui),
                cumulative_exit = cumsum(exit_ui) / sum(start_ui))
    
  }
}


estimate <- function(date, donut = 0) {
  (weekly_summary_natl %>% 
     filter(week_start_date >= date + 7 + donut,
            week_start_date <= date + 28 + donut) %>% 
     summarise(mean(`Exit rate not to recall`)) %>% 
     pull()) - 
    (weekly_summary_natl %>% 
       filter(week_start_date >= date - 7,
              week_start_date <= date) %>% 
       summarise(mean(`Exit rate not to recall`)) %>% 
       pull())
}

if (print_stargazer_tables) {
  mod_stargazer <- stargazer
} else if (!print_stargazer_tables) {
  mod_stargazer <- function(reg_object, ...) {
    output <- capture.output(stargazer(reg_object, ...))
  }
}

weekly_summary <- function(df, last_date_ = last_date) {
  df %>%
    mutate(start_recall_if_exit = ifelse(exit_ui, start_recall, NA)) %>%
    summarise_at(vars(spell_active, exit_ui, start_recall_if_exit), 
                 sum, 
                 na.rm = TRUE) %>%
    mutate(`Exit rate` = exit_ui/spell_active,
           `Exit rate to recall` = start_recall_if_exit/spell_active,
           `Exit rate not to recall` = (exit_ui - start_recall_if_exit)/spell_active) %>%
    ungroup() %>%
    mutate(week_start_date = week_start_date + 7) %>%
    filter(week_start_date <= last_date_) 
}

reg_cluster_count <- function(x){
  #this extracts the customer counts for the regression table
  formatC(length(unique(x$clustervar$cust_number)), format="d", big.mark=",")
}
