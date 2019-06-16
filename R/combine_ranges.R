#' Combines ranges from different tables into a single table.
#'
#' @param ... Your data frames (e.g. df1, df2, df3)
#' @param groups Grouping variables. The names should be the same across all tables
#' @param start_var Start of the range. The name should be the same across all tables
#' @param end_var End of the range. The name should be the same across all tables
#' @param dimension Indicate whether your range includes only dates ('date') or also timestamp ('timestamp'). Defaults to 'date'
#' @param fmt The format of your date or timestamp field, defaults to YMD
#' @param tz Time zone, defaults to UTC
#' @param origin Origin for timestamp conversion, defaults to 1970-01-01
#'
#' @return Returns a data frame (if initial input data.table, then data.table) with combined ranges.
#'
#' @examples
#' combine_ranges(df1, df2, df3, groups = c("id", "rating"), start_var = "start_date", end_var = "end_date")
#' @export
combine_ranges <- function(...,
                           start_var = NULL,
                           end_var = NULL,
                           groups = NULL,
                           dimension = "date",
                           fmt = "%Y-%m-%d",
                           tz = "UTC",
                           origin = "1970-01-01") {
  
  dfs <- list(...)
  dfs <- lapply(dfs, function(x) x[, colnames(x) %in% c(start_var, end_var, groups)])
  dfs <- lapply(dfs, function(x) x[, c(start_var, end_var, groups)])
  dfs <- data.table::rbindlist(dfs)
  
  return(
    collapse_ranges(dfs, start_var = start_var, end_var = end_var, groups = groups, dimension = dimension, fmt = fmt, tz = tz, origin = origin)
  )
  
}