#' Expand date ranges.
#'
#' @param df Data frame (can also be a data.table or a tibble)
#' @param start_var Start Date column
#' @param end_var End Date column
#' @param name The name of newly created column. Defaults to 'Expanded'
#' @param fmt The format of date columns, defaults to Y-M-D
#' @param vars_to_keep Which columns you would like to keep
#' @param unit By which unit of time you want to expand; the default is day
#'
#' @return Returns a full data frame with expanded sequences in a column, e.g. by day or month.
#'
#' @examples
#'
#' df <- data.frame(
#' id = c("1111", "2222", "3333"),
#' gender = c("M", "F", "F"),
#' start = c("2018-01-01", "2019-01-01", "2020-01-01"),
#' end = c("2018-01-05", "2019-01-07", "2020-01-08")
#' )
#'
#' expand_dates(df, start_var = "start", end_var = "end",
#' vars_to_keep = c("id", "gender"), unit = "day")
#'
#' @export
expand_dates <- function(df, start_var, end_var, name = "Expanded", fmt = "%Y-%m-%d", vars_to_keep = NULL, unit = "day") {

  rown <- "rn"
  grouping <- c(vars_to_keep, rown)

  expdf <- copy(df)

  if (!any(class(expdf) %in% "data.table")) setDT(expdf)

  if (class(expdf[[start_var]]) != 'Date' | class(expdf[[end_var]]) != 'Date') {

    for (j in c(start_var, end_var)) set(expdf, j = j, value = as.Date(as.character(expdf[[j]]), format = fmt))

  }

  expdf <- expdf[
    , rn := seq(.N)][
      , .(Expanded = seq.Date(get(start_var), get(end_var), by = unit)), by = mget(grouping)][
        , rn := NULL]

  setnames(expdf, "Expanded", name)

  if (!any(class(df) %in% "data.table")) {

    return(setDF(expdf))

  } else {

    return(expdf)

  }

}

#' Expand timestamp ranges.
#'
#' @param df Data frame (can also be a data.table or a tibble)
#' @param start_var Start time column
#' @param end_var End time column
#' @param name The name of newly created column. Defaults to 'Expanded'
#' @param fmt The format of date columns, defaults to Y-M-D H:M:OS
#' @param tz Desired time zone - defaults to UTC
#' @param vars_to_keep Which columns you would like to keep
#' @param unit By which unit of time you want to expand; the default is day
#'
#' @return Returns a full data frame with expanded sequences in a column, e.g. by day or month.
#'
#' @examples
#'
#' df <- data.frame(
#' id = c("1111", "2222", "3333"),
#' gender = c("M", "F", "F"),
#' start = c("2018-01-01 15:00:00", "2019-01-01 14:00:00", "2020-01-01 19:00:00"),
#' end = c("2018-01-01 18:30:00", "2019-01-01 17:30:00", "2020-01-02 02:00:00")
#' )
#'
#' expand_times(df, start_var = "start", end_var = "end",
#' vars_to_keep = c("id", "gender"), unit = "hour")
#'
#' @export
expand_times <- function(df, start_var, end_var, name = "Expanded", fmt = "%Y-%m-%d %H:%M:%OS", vars_to_keep = NULL, unit = "hour", tz = "UTC") {

  rown <- "rn"
  grouping <- c(vars_to_keep, rown)

  expdf <- copy(df)

  rangevars <- c(
    start_var, end_var
  )
  
  if (!any(class(expdf[[start_var]]) %in% c('POSIXct', 'POSIXlt')) | !any(class(expdf[[end_var]]) %in% c('POSIXct', 'POSIXlt'))) {
    
    for (j in c(start_var, end_var)) set(expdf, j = j, value = as.POSIXct(as.character(expdf[[j]]), format = fmt, tz = tz))
    
  }

  expdf <- setDT(expdf)[, rn := seq(.N)][, .(Expanded = seq.POSIXt(get(start_var), get(end_var), by = unit)), by = mget(grouping)][, rn := NULL]

  setnames(expdf, "Expanded", name)

  if (!any(class(df) %in% "data.table")) {

    return(setDF(expdf))

  } else {

    return(expdf)

  }

}
