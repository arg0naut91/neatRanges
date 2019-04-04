#################
#
# Utils
#
# calc_cummax calculates cummulative maximum for Date and POSIXct formats
#
# They need to be unclassed in order for cummax to function; afterwards the attributes are reset
#
#
#################

calc_cummax_Time <- function(x) (setattr(cummax(unclass(x)), "class", c("POSIXct")))

calc_cummax_Date <- function(x) (setattr(cummax(unclass(x)), "class", c("Date", "IDate")))

globalVariables(c("gap_between", 
                  "lead_max", 
                  "max_until_now")
                )