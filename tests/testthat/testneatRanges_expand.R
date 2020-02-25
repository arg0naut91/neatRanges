################################################
#
# Test script for neatRanges - expand_*
#
# Last updated on 25/02/2020
#
################################################
context("expand_dates")
context("expand_times")

test_that("expand_dates is functional", {

  df <- data.frame(
    id = c("1111", "2222", "3333"),
    gender = c("M", "F", "F"),
    start = c("2018-01-01", "2019-01-01", "2020-01-01"),
    end = c("2018-01-05", "2019-01-07", "2020-01-08")
  )

  output_date <-
    structure(
      list(
        id = structure(
          c(
            1L,
            1L,
            1L,
            1L,
            1L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L
          ),
          .Label = c("1111",
                     "2222", "3333"),
          class = "factor"
        ),
        gender = structure(
          c(
            2L,
            2L,
            2L,
            2L,
            2L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L
          ),
          .Label = c("F", "M"),
          class = "factor"
        ),
        exp_seqs = structure(
          c(
            17532,
            17533,
            17534,
            17535,
            17536,
            17897,
            17898,
            17899,
            17900,
            17901,
            17902,
            17903,
            18262,
            18263,
            18264,
            18265,
            18266,
            18267,
            18268,
            18269
          ),
          class = "Date"
        )
      ),
      row.names = c(NA,-20L),
      class = "data.frame"
    )

  df_dates <- expand_dates(df, "start", "end", "exp_seqs", vars_to_keep = c("id", "gender"))

  expect_equal(output_date, df_dates)

}

)

test_that("expand_dates is functional with data.tables", {
  
  df <- data.table(
    id = c("1111", "2222", "3333"),
    gender = c("M", "F", "F"),
    start = c("2018-01-01", "2019-01-01", "2020-01-01"),
    end = c("2018-01-05", "2019-01-07", "2020-01-08")
  )
  
  output_date <-
    structure(
      list(
        id = c(
          "1111",
          "1111",
          "1111",
          "1111",
          "1111",
          "2222",
          "2222",
          "2222",
          "2222",
          "2222",
          "2222",
          "2222",
          "3333",
          "3333",
          "3333",
          "3333",
          "3333",
          "3333",
          "3333",
          "3333"
        ),
        gender = c(
          "M",
          "M",
          "M",
          "M",
          "M",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F"
        ),
        exp_seqs = structure(
          c(
            17532,
            17533,
            17534,
            17535,
            17536,
            17897,
            17898,
            17899,
            17900,
            17901,
            17902,
            17903,
            18262,
            18263,
            18264,
            18265,
            18266,
            18267,
            18268,
            18269
          ),
          class = "Date"
        )
      ),
      row.names = c(NA,-20L),
      class = c("data.table",
                "data.frame")
    )
  
  df_dates <- expand_dates(df, "start", "end", "exp_seqs", vars_to_keep = c("id", "gender"))
  
  expect_equal(output_date, df_dates)
  
}

)

test_that("expand_times is functional", {

  df <- data.frame(
    id = c("1111", "2222", "3333"),
    gender = c("M", "F", "F"),
    start = c("2018-01-01 15:00:00", "2019-01-01 14:00:00", "2020-01-01 19:00:00"),
    end = c("2018-01-01 18:30:00", "2019-01-01 17:30:00", "2020-01-02 02:00:00")
  )

  output_time <-
    structure(
      list(
        id = structure(
          c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
            3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
          .Label = c("1111", "2222", "3333"),
          class = "factor"
        ),
        gender = structure(
          c(2L, 2L, 2L, 2L, 1L,
            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
          .Label = c("F",
                     "M"),
          class = "factor"
        ),
        Expanded = structure(
          c(
            1514818800,
            1514822400,
            1514826000,
            1514829600,
            1546351200,
            1546354800,
            1546358400,
            1546362000,
            1577905200,
            1577908800,
            1577912400,
            1577916000,
            1577919600,
            1577923200,
            1577926800,
            1577930400
          ),
          class = c("POSIXct", "POSIXt"),
          tzone = "UTC"
        )
      ),
      row.names = c(NA,-16L),
      class = "data.frame"
    )

  df_times <- expand_times(df, "start", "end", vars_to_keep = c("id", "gender"))

  expect_equal(output_time, df_times)

}

)

test_that("expand_times is functional with data.tables", {
  
  df <- data.table(
    id = c("1111", "2222", "3333"),
    gender = c("M", "F", "F"),
    start = c("2018-01-01 15:00:00", "2019-01-01 14:00:00", "2020-01-01 19:00:00"),
    end = c("2018-01-01 18:30:00", "2019-01-01 17:30:00", "2020-01-02 02:00:00")
  )
  
  output_time <-
    structure(
      list(
        id = c(
          "1111",
          "1111",
          "1111",
          "1111",
          "2222",
          "2222",
          "2222",
          "2222",
          "3333",
          "3333",
          "3333",
          "3333",
          "3333",
          "3333",
          "3333",
          "3333"
        ),
        gender = c(
          "M",
          "M",
          "M",
          "M",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F",
          "F"
        ),
        Expanded = structure(
          c(
            1514818800,
            1514822400,
            1514826000,
            1514829600,
            1546351200,
            1546354800,
            1546358400,
            1546362000,
            1577905200,
            1577908800,
            1577912400,
            1577916000,
            1577919600,
            1577923200,
            1577926800,
            1577930400
          ),
          class = c("POSIXct", "POSIXt"),
          tzone = "UTC"
        )
      ),
      row.names = c(NA,-16L),
      class = c("data.table",
                "data.frame")
    )
  
  df_times <- expand_times(df, "start", "end", vars_to_keep = c("id", "gender"))
  
  expect_equal(output_time, df_times)
  
}

)

