###############################################
#
# Test script for neatRanges - combine_ranges
#
# Last updated on 25/02/2020
#
###############################################
context("combine_ranges")

test_that("combine_ranges functions with date formats", {
  
  df1 <- data.frame(
    start = c("2010-01-01", "2012-06-01", "2014-10-15"),
    end = c("2010-08-05", "2013-03-03", "2015-01-01"),
    group = c("a", "a", "b"),
    infoScores = c(0, 3, 2)
  )
  
  df2 <- data.frame(
    end = c("2012-04-05", "2014-06-09", "2009-02-01"),
    group = c("b", "a", "b"),
    start = c("2009-01-15", "2012-07-08", "2008-01-01"),
    score = c(8, 2, 3)
  )
  
  df3 <- data.frame(
    end = c("2011-04-05", "2014-12-09", "2009-02-01"),
    group = c("a", "b", "c"),
    start = c("2010-02-03", "2014-07-08", "2008-01-01"),
    scoreInfo = c(1, 2, 3)
  )
  
  output_date <- structure(
    list(
      group = structure(
        c(1L, 1L, 2L, 2L, 3L),
        .Label = c("a",
                   "b", "c"),
        class = "factor"
      ),
      start = structure(c(14610, 15492,
                          13879, 16259, 13879), class = "Date"),
      end = structure(c(15069,
                        16230, 15435, 16436, 14276), class = "Date")
    ),
    row.names = c(NA,-5L),
    class = "data.frame"
  )
  
  df <- combine_ranges(list(df1, df2, df3), start_var = "start", end_var = "end", groups = "group")
  
  expect_equal(output_date, df)
  
}

)

test_that("combine_ranges functions with date formats and startendAttr", {
  
  df1 <- data.frame(
    start = c("2010-01-01", "2012-06-01", "2014-10-15"),
    end = c("2010-08-05", "2013-03-03", "2015-01-01"),
    group = c("a", "a", "b"),
    infoScores = c(0, 3, 2),
    infoStart = c(1, 2, 1),
    infoEnd = c('X10', 'X11', 'X12')
  )
  
  df2 <- data.frame(
    end = c("2012-04-05", "2014-06-09", "2009-02-01"),
    group = c("b", "a", "b"),
    start = c("2009-01-15", "2012-07-08", "2008-01-01"),
    score = c(8, 2, 3),
    infoStart = c(1, 2, 1),
    infoEnd = c('X12', 'X11', 'X10')
  )
  
  df3 <- data.frame(
    end = c("2011-04-05", "2014-12-09", "2009-02-01"),
    group = c("a", "b", "c"),
    start = c("2010-02-03", "2014-07-08", "2008-01-01"),
    scoreInfo = c(1, 2, 3),
    infoStart = c(1, 2, 1),
    infoEnd = c('X11', 'X12', 'X11')
  )
  
  output_date <- structure(
    list(
      group = structure(
        c(1L, 1L, 2L, 2L, 3L),
        .Label = c("a",
                   "b", "c"),
        class = "factor"
      ),
      start = structure(c(14610, 15492,
                          13879, 16259, 13879), class = "Date"),
      end = structure(c(15069,
                        16230, 15435, 16436, 14276), class = "Date"),
      infoStart = c("1",
                    "2", "1", "2", "1"),
      infoEnd = c("X11", "X11", "X12", "X12",
                  "X11")
    ),
    row.names = c(NA,-5L),
    class = "data.frame"
  )
  
  df <- combine_ranges(list(df1, df2, df3), start_var = "start", end_var = "end", groups = "group", startAttr = c('infoStart'), endAttr = c('infoEnd'))
  
  expect_equal(output_date, df)
  
}

)

test_that("combine_ranges functions with date formats and startendAttr and DT", {
  
  df1 <- data.table::data.table(
    start = c("2010-01-01", "2012-06-01", "2014-10-15"),
    end = c("2010-08-05", "2013-03-03", "2015-01-01"),
    group = c("a", "a", "b"),
    infoScores = c(0, 3, 2),
    infoStart = c(1, 2, 1),
    infoEnd = c('X10', 'X11', 'X12')
  )
  
  df2 <- data.table::data.table(
    end = c("2012-04-05", "2014-06-09", "2009-02-01"),
    group = c("b", "a", "b"),
    start = c("2009-01-15", "2012-07-08", "2008-01-01"),
    score = c(8, 2, 3),
    infoStart = c(1, 2, 1),
    infoEnd = c('X12', 'X11', 'X10')
  )
  
  df3 <- data.table::data.table(
    end = c("2011-04-05", "2014-12-09", "2009-02-01"),
    group = c("a", "b", "c"),
    start = c("2010-02-03", "2014-07-08", "2008-01-01"),
    scoreInfo = c(1, 2, 3),
    infoStart = c(1, 2, 1),
    infoEnd = c('X11', 'X12', 'X11')
  )
  
  output_date <- structure(
    list(
      group = c("a", "a", "b", "b", "c"),
      start = structure(c(14610,
                          15492, 13879, 16259, 13879), class = "Date"),
      end = structure(c(15069,
                        16230, 15435, 16436, 14276), class = "Date"),
      infoStart = c("1",
                    "2", "1", "2", "1"),
      infoEnd = c("X11", "X11", "X12", "X12",
                  "X11")
    ),
    row.names = c(NA,-5L),
    class = c("data.table", "data.frame")
  )
  
  df <- combine_ranges(list(df1, df2, df3), start_var = "start", end_var = "end", groups = "group", startAttr = c('infoStart'), endAttr = c('infoEnd'))
  
  expect_equal(output_date, df)
  
}

)