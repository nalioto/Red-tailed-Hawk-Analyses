################################
#  Migratory Duration Function #
#      Aughust/19/2022         #
###############################

#From: https://github.com/inbo/wmh-analysis/blob/master/src/pt2pt_fxns.R
# Don't forget to cite!


## A function to calculate the time increment between consecutive points ##
## Default output in seconds, other options are "auto", "mins", "hours","days", or "weeks" ##

duration.fun <- function(datetime, output.units = "secs") {
  duration <- c(difftime(datetime[2:length(datetime)], datetime[1:(length(datetime) - 1)], units = output.units), "NA")
  return(duration)
}


  
