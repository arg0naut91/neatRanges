###############################################
#
# Test script for neatRanges - fill_ranges
#
# Last updated on 25/02/2020
#
###############################################
context("fill_ranges")

test_that("fill_ranges functions with date formats", {
  
  df <- data.frame(
    group = c("a", "a", "b", "b", "b"),
    start = c("2007-01-01", "2010-06-02", "2009-04-05", "2012-08-01", "2019-03-19"),
    end = c("2008-02-05", "2013-04-05", "2009-06-03", "2013-02-17", "2021-04-21"),
    cost = c(143, 144, 105, 153, 124), stringsAsFactors = TRUE
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

test_that("fill_ranges functions with date formats & data.tables", {
  
  df <- data.table(
    group = c("a", "a", "b", "b", "b"),
    start = c("2007-01-01", "2010-06-02", "2009-04-05", "2012-08-01", "2019-03-19"),
    end = c("2008-02-05", "2013-04-05", "2009-06-03", "2013-02-17", "2021-04-21"),
    cost = c(143, 144, 105, 153, 124)
  )
  
  output_date <- structure(
    list(
      group = c("a", "a", "a", "b", "b", "b", "b", "b"),
      start = structure(c(
        13514, 13915, 14762, 14339, 14399, 15553,
        15754, 17974
      ), class = "Date"),
      end = structure(c(
        13914, 14761,
        15800, 14398, 15552, 15753, 17973, 18738
      ), class = "Date"),
      cost = c(143,
               NA, 144, 105, NA, 153, NA, 124)
    ),
    row.names = c(NA,-8L),
    class = c("data.table",
              "data.frame")
  )
  
  df <- fill_ranges(df, start_var = "start", end_var = "end", groups = c("group"))
  
  expect_equal(output_date, df)
  
}

)

test_that("fill_ranges functions with date formats & no groups", {
  
  df <- data.frame(
    group = c("a", "c", "d", "d"),
    start = c("2007-01-01", "2010-06-02", "2014-01-01", "2015-01-01"),
    end = c("2008-02-05", "2013-04-05", "2014-12-31", "2016-12-31"),
    cost = c(143, 105, 153, 124),
    score = c(99, 33, 44, 22), stringsAsFactors = TRUE
  )
  
  output_date <- structure(
    list(
      group = structure(
        c(1L, NA, 2L, NA, 3L, 3L),
        .Label = c("a",
                   "c", "d"),
        class = "factor"
      ),
      start = structure(c(13514, 13915,
                          14762, 15801, 16071, 16436), class = "Date"),
      end = structure(c(13914,
                        14761, 15800, 16070, 16435, 17166), class = "Date"),
      cost = c(143,
               NA, 105, NA, 153, 124),
      score = c(99, NA, 33, NA, 44, 22)
    ),
    row.names = c(NA,-6L),
    class = "data.frame"
  )
  
  df <- fill_ranges(df, start_var = "start", end_var = "end")
  
  expect_equal(output_date, df)
  
}

)

test_that("fill_ranges functions with timestamp formats", {
  
  dfTime <- data.frame(
    group = c("a", "a", "b", "b", "b"),
    start = c("2007-01-01 14:30:00", "2010-06-02 18:30:31", "2009-04-05 20:23:21", "2012-08-01 04:30:04", "2019-03-19 19:30:00"),
    end = c("2008-02-05 11:12:00", "2013-04-05 23:59:00", "2009-06-03 00:00:05", "2013-02-17 13:30:00", "2021-04-21 12:00:01"),
    cost = c(143, 144, 105, 153, 124), stringsAsFactors = TRUE
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

test_that("fill_ranges functions with fill", {
  
  df <- data.frame(
    group = c("a", "a", "b", "b", "b", "c", "d", "d"),
    start = c("2007-01-01", "2010-06-02", "2009-04-05", "2012-08-01", "2019-03-19", "2020-01-05", "2014-01-01", "2015-01-01"),
    end = c("2008-02-05", "2013-04-05", "2009-06-03", "2013-02-17", "2021-04-21", "2020-01-09", "2014-12-31", "2016-12-31"),
    cost = c(143, 144, 105, 153, 124, 105, 153, 124),
    score = c(99, 33, 44, 22, 33, 105, 153, 124), stringsAsFactors = TRUE
  )
  
  output_fill <- structure(
    list(
      group = structure(
        c(1L, 1L, 1L, 2L, 2L, 2L, 2L,
          2L, 3L, 4L, 4L),
        .Label = c("a", "b", "c", "d"),
        class = "factor"
      ),
      start = structure(
        c(
          13514,
          13915,
          14762,
          14339,
          14399,
          15553,
          15754,
          17974,
          18266,
          16071,
          16436
        ),
        class = "Date"
      ),
      end = structure(
        c(
          13914,
          14761,
          15800,
          14398,
          15552,
          15753,
          17973,
          18738,
          18270,
          16435,
          17166
        ),
        class = "Date"
      ),
      cost = c(
        "143",
        "0",
        "144",
        "105",
        "0",
        "153",
        "0",
        "124",
        "105",
        "153",
        "124"
      ),
      score = c(
        "99",
        "Missing",
        "33",
        "44",
        "Missing",
        "22",
        "Missing",
        "33",
        "105",
        "153",
        "124"
      )
    ),
    row.names = c(NA,-11L),
    class = "data.frame"
  )
  
  df <- fill_ranges(df, 
                    groups = "group", 
                    start_var = "start", 
                    end_var = "end", 
                    fill = "cost = 0, score = Missing"
  )
  
  expect_equal(output_fill, df)
  
}

)