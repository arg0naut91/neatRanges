#' Collapses the consecutive date or timestamp ranges into one record.
#'
#' The date/time ranges where the gap between two records is equal to or less than max_gap parameter are collapsed into one record.
#'
#' @param df Your data frame (object of class 'data.frame' or 'data.table')
#' @param groups Grouping variables, character strings
#' @param start_var Start of the range, character of length 1L
#' @param end_var End of the range,  character of length 1L
#' @param startAttr Attributes linked to start of the range which should be kept (converted to character type by default)
#' @param endAttr Attributes linked to end of the range which should be kept (converted to character type by default)
#' @param max_gap Gap between date or timestamp ranges, e.g. for 0, default, it will put together all records where there is no gap in-between
#' @param dimension Indicate whether your range includes only dates ('date') or also timestamp ('timestamp'). Defaults to 'date'
#' @param fmt The format of your date or timestamp field, defaults to YMD
#' @param tz Time zone, defaults to UTC
#' @param origin Origin for timestamp conversion, defaults to '1970-01-01'
#'
#' @return 'data.frame' if initial input is a 'data.frame', 'data.table' if original object is a 'data.table' with collapsed records.
#'
#' @examples
#' df_collapse <- data.frame(
#'   id = c(rep("1111", 3), rep("2222", 3)),
#'   rating = c("A+", "AA", "AA", rep("B-", 3)),
#'   start_date = c(
#'     "2014-01-01", "2015-01-01", "2016-01-01",
#'     "2017-01-01", "2018-01-01", "2019-01-01"
#'   ),
#'   end_date = c(
#'     "2014-12-31", "2015-12-31", "2016-03-01",
#'     "2017-01-31", "2018-12-31", "2020-02-01"
#'   )
#' )
#'
#' collapse_ranges(df_collapse, c("id", "rating"), "start_date", "end_date")
#' @export
collapse_ranges <- function(df,
                            groups = NULL,
                            start_var = NULL,
                            end_var = NULL,
                            startAttr = NULL,
                            endAttr = NULL,
                            dimension = c("date", "timestamp"),
                            max_gap = 0L,
                            fmt = "%Y-%m-%d",
                            tz = "UTC",
                            origin = "1970-01-01") {
  
  if (missing(start_var) || missing(end_var) || # user did not specify those function arguments
      is.null(start_var) || is.null(end_var) || # or specified them as NULLs
      length(start_var) != 1L || length(end_var) != 1L) { # 'start_var' or 'end_var' should be of length 1L
    stop("'start_var' and 'end_var' function parameters must be given, not NULL and of length 1L!")
  }
  
  if (!start_var %chin% colnames(df) || # 'start_var' not to be found in the 'df'
      !end_var %chin% colnames(df) ) { # 'end_var' not to be found in the 'df'
    stop("'start_var' and/or 'end_var' function parameter(s) not found in 'df'")
  }
  
  if (!missing(max_gap)) { # if something else then default is given, check
    max_gap <- as.integer(max_gap) # adhering to the original idea of the variable
    stopifnot(length(max_gap) == 1L &&
                !is.na(max_gap)) # as well will throw error if there is some non-sense like string non-numeric input
  }
  
  if (!inherits(df, "data.frame")) {
    stop(
      sprintf(
        "Pass an object of class 'data.frame' to the function parameter 'df'. You gave in object of class %s",
        paste(
          class(df),
          collapse = ", "
        )
      )
    )
  }
  
  df_collapsed <- copy(df)
  
  if (!inherits(df_collapsed, "data.table")) {
    setDT(df_collapsed)
    on.exit(setDF(df_collapsed), add = TRUE) # return data.frame if input was data.frame, or data.table if input was data.table
  }
  
  if (!is.null(startAttr) || !is.null(endAttr)) {
    for (j in c(startAttr, endAttr)) {
      set(df_collapsed, j = j, value = as.character(df_collapsed[[j]]))
    }
  }
  
  dimension <- tolower(dimension)
  dimension <- match.arg(dimension) # default value (if not given) is 'date'
  
  df_collapsed <- switch(dimension, # switch encapsulates the logic between options 'date' and 'timestamp' better I think
                         date = {
                           if (any(class(df_collapsed[[start_var]]) != "Date") || any(class(df_collapsed[[end_var]]) != "Date")) {
                             check_col_mode(df_collapsed, start_var)
                             check_col_mode(df_collapsed, end_var)
                             
                             for (j in c(start_var, end_var)) {
                               set(df_collapsed, j = j, value = as.Date(as.character(df_collapsed[[j]]), format = fmt))
                             }
                           }
                           
                           setorderv(df_collapsed, c(groups, start_var))
                           
                           if (!is.null(groups)) {
                             if (!is.null(startAttr) && !is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubset(get(start_var),
                                                                              get(end_var),
                                                                              max_gap = max_gap,
                                                                              startObjects = mget(startAttr),
                                                                              endObjects = mget(endAttr)
                               ), by = mget(groups)]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr, endAttr))
                             } else if (!is.null(startAttr) && is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubset(get(start_var),
                                                                              get(end_var),
                                                                              max_gap = max_gap,
                                                                              startObjects = mget(startAttr),
                                                                              endObjects = NULL
                               ), by = mget(groups)]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr))
                             } else if (is.null(startAttr) && !is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubset(get(start_var),
                                                                              get(end_var),
                                                                              max_gap = max_gap,
                                                                              startObjects = NULL,
                                                                              endObjects = mget(endAttr)
                               ), by = mget(groups)]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, endAttr))
                             } else {
                               df_collapsed <- df_collapsed[, updateAndSubset(get(start_var),
                                                                              get(end_var),
                                                                              max_gap = max_gap,
                                                                              startObjects = NULL,
                                                                              endObjects = NULL
                               ), by = mget(groups)]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var))
                             }
                           } else {
                             if (!is.null(startAttr) && !is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubset(get(start_var),
                                                                              get(end_var),
                                                                              max_gap = max_gap,
                                                                              startObjects = mget(startAttr),
                                                                              endObjects = mget(endAttr)
                               )]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr, endAttr))
                             } else if (!is.null(startAttr) && is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubset(get(start_var),
                                                                              get(end_var),
                                                                              max_gap = max_gap,
                                                                              startObjects = mget(startAttr),
                                                                              endObjects = NULL
                               )]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr))
                             } else if (is.null(startAttr) && !is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubset(get(start_var),
                                                                              get(end_var),
                                                                              max_gap = max_gap,
                                                                              startObjects = NULL,
                                                                              endObjects = mget(endAttr)
                               )]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, endAttr))
                             } else {
                               df_collapsed <- df_collapsed[, updateAndSubset(get(start_var),
                                                                              get(end_var),
                                                                              max_gap = max_gap,
                                                                              startObjects = NULL,
                                                                              endObjects = NULL
                               )]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var))
                             }
                           }
                         },
                         timestamp = {
                           if (!any(class(df_collapsed[[start_var]]) %chin% c("POSIXct", "POSIXlt")) ||
                               !any(class(df_collapsed[[end_var]]) %chin% c("POSIXct", "POSIXlt"))) {
                             if (fmt == "%Y-%m-%d") {
                               warning("Dimension 'timestamp' selected but format unchanged. Will try to convert to '%Y-%m-%d %H:%M:%OS' ..")
                               
                               fmt <- "%Y-%m-%d %H:%M:%OS"
                             }
                             
                             for (j in c(start_var, end_var)) {
                               set(df_collapsed, j = j, value = as.POSIXct(as.character(df_collapsed[[j]]), format = fmt, tz = tz))
                             }
                           }
                           
                           setorderv(df_collapsed, c(groups, start_var))
                           
                           if (!is.null(groups)) {
                             if (!is.null(startAttr) && !is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var),
                                                                                  get(end_var),
                                                                                  max_gap = max_gap,
                                                                                  startObjects = mget(startAttr),
                                                                                  endObjects = mget(endAttr)
                               ), by = mget(groups)]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr, endAttr))
                             } else if (!is.null(startAttr) && is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var),
                                                                                  get(end_var),
                                                                                  max_gap = max_gap,
                                                                                  startObjects = mget(startAttr),
                                                                                  endObjects = NULL
                               ), by = mget(groups)]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr))
                             } else if (is.null(startAttr) && !is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var),
                                                                                  get(end_var),
                                                                                  max_gap = max_gap,
                                                                                  startObjects = NULL,
                                                                                  endObjects = mget(endAttr)
                               ), by = mget(groups)]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, endAttr))
                             } else {
                               df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var),
                                                                                  get(end_var),
                                                                                  max_gap = max_gap,
                                                                                  startObjects = NULL,
                                                                                  endObjects = NULL
                               ), by = mget(groups)]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var))
                             }
                           } else {
                             if (!is.null(startAttr) && !is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var),
                                                                                  get(end_var),
                                                                                  max_gap = max_gap,
                                                                                  startObjects = mget(startAttr),
                                                                                  endObjects = mget(endAttr)
                               )]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(start_var, end_var, startAttr, endAttr))
                             } else if (!is.null(startAttr) && is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var),
                                                                                  get(end_var),
                                                                                  max_gap = max_gap,
                                                                                  startObjects = mget(startAttr),
                                                                                  endObjects = NULL
                               )]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(start_var, end_var, startAttr))
                             } else if (is.null(startAttr) & !is.null(endAttr)) {
                               df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var),
                                                                                  get(end_var),
                                                                                  max_gap = max_gap,
                                                                                  startObjects = NULL,
                                                                                  endObjects = mget(endAttr)
                               )]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(start_var, end_var, endAttr))
                             } else {
                               df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var),
                                                                                  get(end_var),
                                                                                  max_gap = max_gap,
                                                                                  startObjects = NULL,
                                                                                  endObjects = NULL
                               )]
                               
                               setnames(df_collapsed, 1:ncol(df_collapsed), c(start_var, end_var))
                             }
                           }
                         }
  )
  
  return(df_collapsed) # will be df thanks to on.exit if the original was df, otherwise dt
}