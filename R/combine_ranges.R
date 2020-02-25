#' Combines ranges from different tables into a single table.
#'
#' @param dfs A list of your data frames, e.g. list(df1, df2)
#' @param groups Grouping variables
#' @param start_var Start of the range
#' @param end_var End of the range
#' @param startAttr Attributes linked to start of the range which should be kept (converted to character type by default)
#' @param endAttr Attributes linked to end of the range which should be kept (converted to character type by default)
#' @param max_gap Gap between date or timestamp ranges, e.g. for 0, default, it will put together all records where there is no gap in-between
#' @param dimension Indicate whether your range includes only dates ('date') or also timestamp ('timestamp'). Defaults to 'date'
#' @param fmt The format of your date or timestamp field, defaults to YMD
#' @param tz Time zone, defaults to UTC
#' @param origin Origin for timestamp conversion, defaults to 1970-01-01
#'
#' @return Returns a data frame (if first table passed is data.table, then data.table) with combined ranges.
#'
#' @examples
#' df1 <- data.frame(
#'   start = c("2010-01-01", "2012-06-01", "2014-10-15"),
#'   end = c("2010-08-05", "2013-03-03", "2015-01-01"),
#'   group = c("a", "a", "b"),
#'   infoScores = c(0, 3, 2)
#' )
#'
#' df2 <- data.frame(
#'   end = c("2012-04-05", "2014-06-09", "2009-02-01"),
#'   group = c("b", "a", "b"),
#'   start = c("2009-01-15", "2012-07-08", "2008-01-01"),
#'   score = c(8, 2, 3)
#' )
#'
#' combine_ranges(dfs = list(df1, df2), groups = "group",
#' start_var = "start", end_var = "end")
#' @export
combine_ranges <- function(dfs,
                           groups = NULL,
                           start_var = NULL,
                           end_var = NULL,
                           startAttr = NULL,
                           endAttr = NULL,
                           dimension = "date",
                           max_gap = 0L,
                           fmt = "%Y-%m-%d",
                           tz = "UTC",
                           origin = "1970-01-01") {
  
  chckClass <- any(sapply(dfs, function(x) class(x) %in% 'data.table'))
  
  dfs <- lapply(dfs, function(x) {
    
    if (!chckClass) {
      
      x <- x[, c(groups, start_var, end_var, startAttr, endAttr)]
      
    } else {
      
      colsToKeep <- c(groups, start_var, end_var, startAttr, endAttr)
      
      x <- x[, ..colsToKeep]
      
    }
    
    return(x)
    
  }
  
  )
  
  dfs <- data.table::rbindlist(dfs)

  dfs <- collapse_ranges(dfs,
                         start_var = start_var,
                         end_var = end_var,
                         groups = groups,
                         startAttr = startAttr,
                         endAttr = endAttr,
                         max_gap = max_gap,
                         dimension = dimension,
                         fmt = fmt,
                         tz = tz,
                         origin = origin
                         )

  if (!chckClass) {

    return(setDF(dfs))

  } else {

    return(dfs)

  }

}
