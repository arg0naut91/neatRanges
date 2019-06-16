###############################################
#
# Test script for neatRanges - fill_ranges
#
# Last updated on 16/06/2019
#
###############################################
context("fill_ranges")

test_that("fill_ranges functions with date formats", {
  
  df <- data.frame(
    group = c("a", "a", "b", "b", "b"),
    start = c("2007-01-01", "2010-06-02", "2009-04-05", "2012-08-01", "2019-03-19"),
    end = c("2008-02-05", "2013-04-05", "2009-06-03", "2013-02-17", "2021-04-21"),
    cost = c(143, 144, 105, 153, 124)
  )
  
  output_date <- structure(
    list(
      group = structure(
        c(1L, 1L, 1L, 2L, 2L, 2L, 2L,
          2L),
        .Label = c("a", "b"),
        class = "factor"
      ),
      start = structure(c(
        13514,
        13915, 14762, 14339, 14399, 15553, 15754, 17974
      ), class = "Date"),
      end = structure(c(
        13914, 14761, 15800, 14398, 15552, 15753,
        17973, 18738
      ), class = "Date"),
      cost = c(143, NA, 144, 105,
               NA, 153, NA, 124)
    ),
    row.names = c(NA,-8L),
    class = "data.frame"
  )
  
  df <- fill_ranges(df, start_var = "start", end_var = "end", groups = c("group"))
  
  expect_equal(output_date, df)
  
}

)

test_that("fill_ranges functions with timestamp formats", {
  
  dfTime <- data.frame(
    group = c("a", "a", "b", "b", "b"),
    start = c("2007-01-01 14:30:00", "2010-06-02 18:30:31", "2009-04-05 20:23:21", "2012-08-01 04:30:04", "2019-03-19 19:30:00"),
    end = c("2008-02-05 11:12:00", "2013-04-05 23:59:00", "2009-06-03 00:00:05", "2013-02-17 13:30:00", "2021-04-21 12:00:01"),
    cost = c(143, 144, 105, 153, 124)
  )
  
  output_times <- structure(
    list(
      group = structure(
        c(1L, 1L, 1L, 2L, 2L, 2L, 2L,
          2L),
        .Label = c("a", "b"),
        class = "factor"
      ),
      start = structure(
        c(
          1167661800,
          1202209921,
          1275503431,
          1238963001,
          1243987206,
          1343795404,
          1361107801,
          1553023800
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      end = structure(
        c(
          1202209920,
          1275503430,
          1365206340,
          1243987205,
          1343795403,
          1361107800,
          1553023799,
          1619006401
        ),
        class = c("POSIXct",
                  "POSIXt"),
        tzone = "UTC"
      ),
      cost = c(143, NA, 144, 105, NA,
               153, NA, 124)
    ),
    row.names = c(NA,-8L),
    class = "data.frame"
  )
  
  df <- fill_ranges(dfTime, start_var = "start", end_var = "end", groups = c("group"), dimension = "timestamp")
  
  expect_equal(output_times, df)
  
}

)