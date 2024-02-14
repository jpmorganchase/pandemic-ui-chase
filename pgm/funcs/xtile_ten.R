# SPDX-License-Identifier: MIT
#The xtile_ten function is used to find values at a specific percentile (but usually median) within JPMCI data while meeting data aggregation standards by taking the average of the ten values around the entered percentile.
#Arguments:
# x (data; required)
# w (weights; optional)
# xtile (percentile; optional, defaults to 0.50)

#Last modified 9/28/2017 by Kerry Zhang

xtile_ten <- function (x, w, xtile = 0.50) {
  suppressMessages(require(Hmisc))  #For weighted quantile; suppressMessages used to prevent it from printing all of the red stuff
  require(dplyr)                    #For arrange
  
  #Check that inputs adhere to expected format
  if(xtile < 0 | 1 < xtile) {
    stop("xtile must be expressed as a positive decimal")
  }
  if(is.vector(x) == FALSE) {
    stop("Data must be a vector")
  }
  
  #A. Unweighted XTILE
  if(missing(w) == TRUE){
    w = rep(1, length(x)) #Set default weight
  }
  
  #B. Weighted XTILE
  else{
    if(is.vector(w) == FALSE) {
      stop("Weights must be a vector")
    }
    if(length(x) != length(w)) {
      stop("Data and Weights must be same dimension")
    }
  }
  
  df = as.data.frame(cbind(x, w))                                        #Turn vectors into a dataframe with data and weight columns
  df = arrange(df, x)                                                    #Order by x
  df = na.omit(df)                                                       #Remove rows without X or W
  df = cbind(row = c(1:dim(df)[1]), df)                                  #Add a column of row numbers
  
  row_of_median = wtd.quantile(df$row, weights = df$w, probs = c(xtile)) #Calculate row of weighted median                                
  
  if(row_of_median - 5 >= 1){
    out <- weighted.mean(x = c(df[row_of_median - 5,]$x,               #Data (X)
                               df[row_of_median - 4,]$x,
                               df[row_of_median - 3,]$x,
                               df[row_of_median - 2,]$x,
                               df[row_of_median - 1,]$x,
                               df[row_of_median    ,]$x,
                               df[row_of_median + 1,]$x,
                               df[row_of_median + 2,]$x,
                               df[row_of_median + 3,]$x,
                               df[row_of_median + 4,]$x,
                               df[row_of_median + 5,]$x),
                         
                         w = c(df[row_of_median - 5,]$w,               #Weights (W)
                               df[row_of_median - 4,]$w,
                               df[row_of_median - 3,]$w,
                               df[row_of_median - 2,]$w,
                               df[row_of_median - 1,]$w,
                               df[row_of_median    ,]$w,
                               df[row_of_median + 1,]$w,
                               df[row_of_median + 2,]$w,
                               df[row_of_median + 3,]$w,
                               df[row_of_median + 4,]$w,
                               df[row_of_median + 5,]$w),
                         na.rm = TRUE)
    
    detach("package:Hmisc", unload = TRUE)                                 #Unload Hmisc since this masks some commands from dplyr
    return(out)}
  
  else {
    stop("Not enough data - must have a minimum of eleven datapoints to meet JPMC statistical disclosure limitations")
  }
}

if (small_samp) xtile_ten <- median
