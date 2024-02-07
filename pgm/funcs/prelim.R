
winsor <- function (x, fraction_low=0.00, fraction_high=.95,
                    value_low = NA, value_high = NA,
                    verbose = TRUE, varname = "var",
                    positive_only = TRUE)
{
  #check inputs
  if(!is.na(value_low)){
    fraction_low <- ecdf(x)(value_low)
  }
  
  if(!is.na(value_high)){
    fraction_high <- ecdf(x)(value_high)
  }
  
  
  if(length(fraction_low) !=1 || fraction_low < 0 ||
     fraction_low > 0.5) {
    stop("bad value for 'fraction_low'")
  }
  if(length(fraction_high) != 1 || fraction_high < 0 ||
     fraction_high < 0.5) {
    stop("bad value for 'fraction_high'")
  }
  
  #compute limits
  if (positive_only == TRUE) {
    x_quantile_sample <- x[x>0]
  } else {
    x_quantile_sample <- x
  }
  lim <- quantile(x_quantile_sample,
                  probs=c(0, fraction_low, fraction_high, 1),
                  na.rm = TRUE)
  min_str <- min(x)
  max_str <- max(x)
  
  #winsorize
  lim[2] <- ifelse(fraction_low == 0, min_str, lim[2])
  lim[3] <- ifelse(fraction_high == 1, max_str, lim[3])
  x[ x < lim[2] ] <- lim[2]
  x[ x > lim[3] ] <- lim[3]
  if (verbose == TRUE) {
    if (fraction_low != 0) {
      print(paste0(varname," min was ",lim[1],", now winsorized to ",
                   fraction_low," percentile of ", min(x, na.rm = TRUE)))
    }
    if (fraction_high != 1) {
      print(paste0(varname," max was ",lim[4],", now winsorized to ",
                   fraction_high," percentile of ", max(x, na.rm = TRUE)))
    }
    
    print(paste0("Share of zero values in ", varname,":", round(sum(x==0, na.rm=T)/length(x)*100, 2)))
    print(paste0("Share of negative values in ", varname,":", round(sum(x<0, na.rm=T)/length(x)*100, 2)))
    
  }
  
  return(x)
}
