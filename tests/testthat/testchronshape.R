##############################
#
# Test script for chronshape
#
# Last updated on 04/04/2019
#
##############################
context("collapse_ranges")

test_that("collapse_ranges functions with date formats", {
  
  df_collapse <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c("2014-01-01", "2015-01-01", "2016-01-01",
                   "2017-01-01", "2018-01-01", "2019-01-01"),
    end_date = c("2014-12-31", "2015-12-31", "2016-03-01",
                 "2017-01-31", "2018-12-31", "2020-02-01")
  )
  
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
  
  df <- collapse_ranges(df_collapse, c("id", "rating"), "start_date", "end_date")
  
  expect_equal(output_date, df)
  
}

)

test_that("collapse_ranges functions with timestamps", {
  
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c("2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
                   "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"),
    end_time = c("2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
                 "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00")
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
          c(1388584800, 1388588400, 1420124400,
            1420131600),
          class = c("POSIXct", "POSIXt"),
          tzone = "UTC"
        ),
        end_time = structure(
          c(1388588399, 1388602800, 1420127999,
            1420146000),
          class = c("POSIXct", "POSIXt"),
          tzone = "UTC"
        )
      ),
      row.names = c(NA,-4L),
      class = "data.frame"
    )
  
  df <- collapse_ranges(df_collapse_time, c("id", "diary"), "start_time", "end_time", dimension = "timestamp")
  
  expect_equal(output_time, df)
  
}

)