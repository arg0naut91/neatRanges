##############################
#
# Test script for chronshape
#
# Last updated on 04/04/2019
#
##############################

library(data.table)

df_collapse <- data.frame(
  id = c(rep("1111", 3), rep("2222", 3)),
  rating = c("A+", "AA", "AA", rep("B-", 3)),
  start_date = c("2014-01-01", "2015-01-01", "2016-01-01",
              "2017-01-01", "2018-01-01", "2019-01-01"),
  end_date = c("2014-12-31", "2015-12-31", "2016-03-01",
            "2017-01-31", "2018-12-31", "2020-02-01")
)

df_collapse_time <- data.frame(
  id = c(rep("1111", 3), rep("2222", 3)),
  diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
  start_time = c("2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
                 "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"),
  end_time = c("2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
               "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00")
)

collapse_range <- function(df, groups, start_var, end_var, range = "date", max_gap = 0L, fmt = "%Y-%m-%d") {
  
  calc_cummax_Time <- function(x) (setattr(cummax(unclass(x)), "class", c("POSIXct")))
  
  max_gap <- max_gap + 1L
  
  cumidx <- "cumidx4"
  group_1stlvl <- groups
  group_by_args_2lvl <- c(groups, cumidx)
  
  rangevars <- c(
    substitute(start_var),
    substitute(end_var)
    )
  
  df_collapsed <- copy(df)
  
  if (range == "date") {
    
    calc_cummax_Date <- function(x) (setattr(cummax(unclass(x)), "class", c("Date", "IDate")))
    
    df_collapsed <- setDT(df_collapsed)[
      , (rangevars) := lapply(.SD, function(x) as.Date(as.character(x), format = fmt)), .SDcols = rangevars][
        , max_until_now := shift(calc_cummax_Date(get(end_var))), by = mget(group_1stlvl)]
    
  } else if (range == "time") {
    
    if (fmt == "%Y-%m-%d") { fmt <- "%Y-%m-%d %H:%M:%OS" }
    
    calc_cummax_Time <- function(x) (setattr(cummax(unclass(x)), "class", c("POSIXct")))
    
    df_collapsed <- setDT(df_collapsed)[
      , (rangevars) := lapply(.SD, function(x) as.POSIXct(as.character(x), format = fmt)), .SDcols = rangevars][
        , max_until_now := shift(calc_cummax_Time(get(end_var))), by = mget(group_1stlvl)]
    
  } else { stop("The range argument has to be either 'date' or 'time'.") }
  
  df_collapsed <- df_collapsed[, lead_max := shift(max_until_now, type = "lead"), by = mget(group_1stlvl)][
    is.na(max_until_now), max_until_now := lead_max, by = mget(group_1stlvl)][
      (max_until_now + max_gap) < get(start_var), gap_between := 1, by = mget(group_1stlvl)][
        is.na(gap_between), gap_between := 0][
          , (cumidx) := cumsum(gap_between), by = mget(group_1stlvl)][
            , setNames(list(min(get(start_var)), max(get(end_var))), rangevars), by = mget(group_by_args_2lvl)][
              , (cumidx) := NULL]
  
  if (!any(class(df) %in% "data.table")) {
    
    return(setDF(df_collapsed))
    
  } else {
    
    return(df_collapsed)
    
  }
  
}

df <- collapse_range(df_collapse, c("id", "rating"), "start_date", "end_date")

output_date <- structure(
  list(
    id = structure(
      c(1L, 1L, 2L, 2L),
      .Label = c("1111",
                 "2222"),
      class = "factor"
    ),
    rating = structure(
      c(1L, 2L, 3L,
        3L),
      .Label = c("A+", "AA", "B-"),
      class = "factor"
    ),
    start_date = structure(c(16071,
                             16436, 17167, 17532), class = "Date"),
    end_date = structure(c(16435,
                           16861, 17197, 18293), class = "Date")
  ),
  row.names = c(NA,-4L),
  class = "data.frame"
)

output_time <-
  structure(
    list(
      id = structure(
        c(1L, 1L, 2L, 2L),
        .Label = c("1111",
                   "2222"),
        class = "factor"
      ),
      diary = structure(
        c(1L, 3L, 2L, 2L),
        .Label = c("reading", "sleeping", "watching TV"),
        class = "factor"
      ),
      start_time = structure(
        c(1388581200, 1388584800, 1420120800,
          1420128000),
        class = c("POSIXct", "POSIXt")
      ),
      end_time = structure(
        c(1388584799,
          1388599200, 1420124399, 1420142400),
        class = c("POSIXct",
                  "POSIXt")
      )
    ),
    row.names = c(NA,-4L),
    class = "data.frame"
  )
