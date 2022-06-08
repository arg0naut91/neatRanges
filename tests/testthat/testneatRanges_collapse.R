###############################################
#
# Test script for neatRanges - collapse_ranges
#
# Last updated on 2022-04-24
#
###############################################
context("collapse_ranges")

test_that("collapse_ranges functions give error if incorrect value is passed to parameter 'dimension'", {
  
  df_collapse <- data.frame(
    id = c(rep("1111", 3L), rep("2222", 3L)),
    rating = c("A+", "AA", "AA", rep("B-", 3L)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ), stringsAsFactors = TRUE
  )
  
  expect_error(
    collapse_ranges(
      df = df_collapse,
      groups = c("id", "rating"),
      start_var = "start_date",
      end_var = "end_date",
      dimension = "f"
    )
  )
})

test_that("collapse_ranges functions give error if 'start_var' or 'end_var' are NULL", {
  df_collapse <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ), stringsAsFactors = TRUE
  )
  expect_error(
    collapse_ranges(
      df = df_collapse,
      groups = c("id", "rating")
    )
  )
})

test_that("collapse_ranges functions with date formats", {
  df_collapse <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ), stringsAsFactors = TRUE
  )
  
  output_date <- structure(
    list(
      id = structure(
        c(1L, 1L, 2L, 2L),
        .Label = c(
          "1111",
          "2222"
        ),
        class = "factor"
      ),
      rating = structure(
        c(
          1L, 2L, 3L,
          3L
        ),
        .Label = c("A+", "AA", "B-"),
        class = "factor"
      ),
      start_date = structure(c(
        16071,
        16436, 17167, 17532
      ), class = "Date"),
      end_date = structure(c(
        16435,
        16861, 17197, 18293
      ), class = "Date")
    ),
    row.names = c(NA, -4L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse, c("id", "rating"), "start_date", "end_date")
  )
  
  expect_equal(output_date, df)
})

test_that("collapse_ranges functions with date formats & no groups", {
  df <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ), stringsAsFactors = TRUE
  )
  
  output_date <- structure(
    list(
      start_date = structure(c(16071, 17167, 17532), class = "Date"),
      end_date = structure(c(16861, 17197, 18293), class = "Date")
    ),
    row.names = c(NA, -3L),
    class = "data.frame"
  )
  
  df_collapsed <- suppressWarnings(
    collapse_ranges(df,
                    start_var = "start_date",
                    end_var = "end_date",
                    max_gap = 0L,
                    fmt = "%Y-%m-%d",
                    dimension = "date")
  )
  
  expect_equal(output_date, df_collapsed)
})

test_that("collapse_ranges functions with timestamps", {
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c(
      "2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
      "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"
    ),
    end_time = c(
      "2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
      "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00"
    ), stringsAsFactors = TRUE
  )
  
  output_time <-
    structure(
      list(
        id = structure(
          c(1L, 1L, 2L, 2L),
          .Label = c(
            "1111",
            "2222"
          ),
          class = "factor"
        ),
        diary = structure(
          c(1L, 3L, 2L, 2L),
          .Label = c("reading", "sleeping", "watching TV"),
          class = "factor"
        ),
        start_time = structure(
          c(
            1388584800, 1388588400, 1420124400,
            1420131600
          ),
          class = c("POSIXct", "POSIXt"),
          tzone = "UTC"
        ),
        end_time = structure(
          c(
            1388588399, 1388602800, 1420127999,
            1420146000
          ),
          class = c("POSIXct", "POSIXt"),
          tzone = "UTC"
        )
      ),
      row.names = c(NA, -4L),
      class = "data.frame"
    )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse_time, c("id", "diary"), "start_time", "end_time", dimension = "timestamp")
  )
  
  expect_equal(output_time, df)
})

test_that("collapse_ranges functions with dates, groups and startendAttr", {
  df_collapse <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      id = structure(
        c(1L, 1L, 2L, 2L),
        .Label = c(
          "1111",
          "2222"
        ),
        class = "factor"
      ),
      rating = structure(
        c(
          1L, 2L, 3L,
          3L
        ),
        .Label = c("A+", "AA", "B-"),
        class = "factor"
      ),
      start_date = structure(c(
        16071,
        16436, 17167, 17532
      ), class = "Date"),
      end_date = structure(c(
        16435,
        16861, 17197, 18293
      ), class = "Date"),
      startName1 = c(
        "X40",
        "X50", "X70", "X80"
      ),
      startName2 = c("X40", "X50", "X70", "X80"),
      endName1 = c("X40", "X60", "X70", "X90"),
      endName2 = c(
        "X40",
        "X60", "X70", "X90"
      )
    ),
    row.names = c(NA, -4L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse, c("id", "rating"), "start_date", "end_date",
                    c("startName1", "startName2"),
                    endAttr = c("endName1", "endName2"))
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with dates, groups and start Vars only", {
  df_collapse <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      id = structure(
        c(1L, 1L, 2L, 2L),
        .Label = c(
          "1111",
          "2222"
        ),
        class = "factor"
      ),
      rating = structure(
        c(
          1L, 2L, 3L,
          3L
        ),
        .Label = c("A+", "AA", "B-"),
        class = "factor"
      ),
      start_date = structure(c(
        16071,
        16436, 17167, 17532
      ), class = "Date"),
      end_date = structure(c(
        16435,
        16861, 17197, 18293
      ), class = "Date"),
      startName1 = c(
        "X40",
        "X50", "X70", "X80"
      ),
      startName2 = c("X40", "X50", "X70", "X80")
    ),
    row.names = c(NA, -4L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(
      df_collapse, c("id", "rating"), "start_date", "end_date",
      c("startName1", "startName2"))
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with dates, groups and end Vars only", {
  df_collapse <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      id = structure(
        c(1L, 1L, 2L, 2L),
        .Label = c(
          "1111",
          "2222"
        ),
        class = "factor"
      ),
      rating = structure(
        c(
          1L, 2L, 3L,
          3L
        ),
        .Label = c("A+", "AA", "B-"),
        class = "factor"
      ),
      start_date = structure(c(
        16071,
        16436, 17167, 17532
      ), class = "Date"),
      end_date = structure(c(
        16435,
        16861, 17197, 18293
      ), class = "Date"),
      endName1 = c(
        "X40", "X60",
        "X70", "X90"
      ),
      endName2 = c("X40", "X60", "X70", "X90")
    ),
    row.names = c(NA, -4L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse, c("id", "rating"), "start_date", "end_date", 
                    endAttr = c("endName1", "endName2"))
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with dates, no groups and startendAttr", {
  df_collapse <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(list(
    start_date = structure(c(16071, 17167, 17532), class = "Date"),
    end_date = structure(c(16861, 17197, 18293), class = "Date"),
    startName1 = c("X40", "X70", "X80"), endName1 = c(
      "X60",
      "X70", "X90"
    )
  ), row.names = c(NA, -3L), class = "data.frame")
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse,
                    start_var = "start_date", end_var = "end_date",
                    startAttr = c("startName1"), endAttr = c("endName1")
    ))
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with dates, no groups and start vars only", {
  df_collapse <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(list(
    start_date = structure(c(16071, 17167, 17532), class = "Date"),
    end_date = structure(c(16861, 17197, 18293), class = "Date"),
    startName1 = c("X40", "X70", "X80")
  ), row.names = c(NA, -3L), class = "data.frame")
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse,
                    start_var = "start_date", end_var = "end_date",
                    startAttr = c("startName1"))
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with dates, no groups and end vars only", {
  df_collapse <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    rating = c("A+", "AA", "AA", rep("B-", 3)),
    start_date = c(
      "2014-01-01", "2015-01-01", "2016-01-01",
      "2017-01-01", "2018-01-01", "2019-01-01"
    ),
    end_date = c(
      "2014-12-31", "2015-12-31", "2016-03-01",
      "2017-01-31", "2018-12-31", "2020-02-01"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(list(
    start_date = structure(c(16071, 17167, 17532), class = "Date"),
    end_date = structure(c(16861, 17197, 18293), class = "Date"),
    endName2 = c("X60", "X70", "X90")
  ), row.names = c(NA, -3L), class = "data.frame")
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse,
                    start_var = "start_date", end_var = "end_date",
                    endAttr = c("endName2"))
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with times, groups and startendAttr", {
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c(
      "2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
      "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"
    ),
    end_time = c(
      "2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
      "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      id = structure(
        c(1L, 1L, 2L, 2L),
        .Label = c(
          "1111",
          "2222"
        ),
        class = "factor"
      ),
      diary = structure(
        c(1L, 3L, 2L, 2L),
        .Label = c("reading", "sleeping", "watching TV"),
        class = "factor"
      ),
      start_time = structure(
        c(
          1388584800, 1388588400, 1420124400,
          1420131600
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      end_time = structure(
        c(
          1388588399, 1388602800, 1420127999,
          1420146000
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      startName1 = c("X40", "X50", "X70", "X80"),
      startName2 = c(
        "X40",
        "X50", "X70", "X80"
      ),
      endName1 = c("X40", "X60", "X70", "X90"),
      endName2 = c("X40", "X60", "X70", "X90")
    ),
    row.names = c(NA, -4L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse_time, c("id", "diary"), "start_time", "end_time",
                    c("startName1", "startName2"),
                    endAttr = c("endName1", "endName2"), dimension = "timestamp")
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with times, groups and start Vars only", {
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c(
      "2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
      "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"
    ),
    end_time = c(
      "2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
      "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      id = structure(
        c(1L, 1L, 2L, 2L),
        .Label = c(
          "1111",
          "2222"
        ),
        class = "factor"
      ),
      diary = structure(
        c(1L, 3L, 2L, 2L),
        .Label = c("reading", "sleeping", "watching TV"),
        class = "factor"
      ),
      start_time = structure(
        c(
          1388584800, 1388588400, 1420124400,
          1420131600
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      end_time = structure(
        c(
          1388588399, 1388602800, 1420127999,
          1420146000
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      startName1 = c("X40", "X50", "X70", "X80"),
      startName2 = c(
        "X40",
        "X50", "X70", "X80"
      )
    ),
    row.names = c(NA, -4L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse_time, c("id", "diary"), "start_time", "end_time",
                    c("startName1", "startName2"),
                    dimension = "timestamp")
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with times, groups and end Vars only", {
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c(
      "2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
      "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"
    ),
    end_time = c(
      "2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
      "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      id = structure(
        c(1L, 1L, 2L, 2L),
        .Label = c(
          "1111",
          "2222"
        ),
        class = "factor"
      ),
      diary = structure(
        c(1L, 3L, 2L, 2L),
        .Label = c("reading", "sleeping", "watching TV"),
        class = "factor"
      ),
      start_time = structure(
        c(
          1388584800, 1388588400, 1420124400,
          1420131600
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      end_time = structure(
        c(
          1388588399, 1388602800, 1420127999,
          1420146000
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      endName1 = c("X40", "X60", "X70", "X90"),
      endName2 = c(
        "X40",
        "X60", "X70", "X90"
      )
    ),
    row.names = c(NA, -4L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse_time, c("id", "diary"), "start_time", 
                    "end_time", endAttr = c("endName1", "endName2"), 
                    dimension = "timestamp")
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with times, no groups and startendAttr", {
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c(
      "2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
      "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"
    ),
    end_time = c(
      "2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
      "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      start_time = structure(
        c(
          1388584800, 1420124400,
          1420131600
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      end_time = structure(
        c(1388602800, 1420127999, 1420146000),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      startName1 = c(
        "X40",
        "X70", "X80"
      ),
      startName2 = c("X40", "X70", "X80"),
      endName1 = c(
        "X60",
        "X70", "X90"
      ),
      endName2 = c("X60", "X70", "X90")
    ),
    row.names = c(NA, -3L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse_time,
                    start_var = "start_time", end_var = "end_time",
                    startAttr = c("startName1", "startName2"), 
                    endAttr = c("endName1", "endName2"), 
                    dimension = "timestamp")
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with times, no groups and start vars only", {
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c(
      "2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
      "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"
    ),
    end_time = c(
      "2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
      "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      start_time = structure(
        c(
          1388584800, 1420124400,
          1420131600
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      end_time = structure(
        c(1388602800, 1420127999, 1420146000),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      startName1 = c(
        "X40",
        "X70", "X80"
      ),
      startName2 = c("X40", "X70", "X80")
    ),
    row.names = c(NA, -3L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse_time,
                    start_var = "start_time", end_var = "end_time",
                    startAttr = c("startName1", "startName2"), dimension = "timestamp")
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with times, no groups and end vars only", {
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c(
      "2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
      "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"
    ),
    end_time = c(
      "2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
      "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      start_time = structure(
        c(
          1388584800, 1420124400,
          1420131600
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      end_time = structure(
        c(1388602800, 1420127999, 1420146000),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      endName1 = c(
        "X60",
        "X70", "X90"
      ),
      endName2 = c("X60", "X70", "X90")
    ),
    row.names = c(NA, -3L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse_time,
                    start_var = "start_time", end_var = "end_time",
                    endAttr = c("endName1", "endName2"), dimension = "timestamp")
  )
  
  expect_equal(df, output_vars)
})

test_that("collapse_ranges functions with times only", {
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c(
      "2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
      "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"
    ),
    end_time = c(
      "2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
      "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  output_vars <- structure(
    list(
      start_time = structure(
        c(
          1388584800, 1420124400,
          1420131600
        ),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      end_time = structure(
        c(1388602800, 1420127999, 1420146000),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      )
    ),
    row.names = c(NA, -3L),
    class = "data.frame"
  )
  
  df <- suppressWarnings(
    collapse_ranges(df_collapse_time, start_var = "start_time", 
                    end_var = "end_time", dimension = "timestamp")
  )
  
  expect_equal(df, output_vars)
})

test_that("error when wrong dimension", {
  
  df_collapse_time <- data.frame(
    id = c(rep("1111", 3), rep("2222", 3)),
    diary = c("reading", "watching TV", "watching TV", rep("sleeping", 3)),
    start_time = c(
      "2014-01-01 14:00:00", "2014-01-01 15:00:00", "2014-01-01 16:30:00",
      "2015-01-01 15:00:00", "2015-01-01 17:00:00", "2015-01-01 19:00:00"
    ),
    end_time = c(
      "2014-01-01 14:59:59", "2014-01-01 16:29:59", "2014-01-01 19:00:00",
      "2015-01-01 15:59:59", "2015-01-01 18:59:59", "2015-01-01 21:00:00"
    ),
    startName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    startName2 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName1 = c("X40", "X50", "X60", "X70", "X80", "X90"),
    endName2 = c("X40", "X50", "X60", "X70", "X80", "X90"), stringsAsFactors = TRUE
  )
  
  expect_error(
    collapse_ranges(df_collapse_time,
                    start_var = "start_time", end_var = "end_time",
                    endAttr = c("endName2"), dimension = "bad_time")
    # ,"The dimension argument has to be either 'date' or 'timestamp'." => depends on local in case of match.arg or can be further processed with catching the error
  )
})
