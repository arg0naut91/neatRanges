#' Fill the gaps between ranges.
#'
#' @param df Your data frame
#' @param groups Grouping variables
#' @param start_var Start of the range
#' @param end_var End of the range
#' @param fill Fill the missing values for values coresponding to missing ranges, e.g. 'colname1 = 0, colname2 = Missing'
#' @param dimension Indicate whether your range includes only dates ('date') or also timestamp ('timestamp'). Defaults to 'date'
#' @param fmt The format of your date or timestamp field, defaults to YMD
#' @param tz Time zone, defaults to UTC
#' @param origin Origin for timestamp conversion, defaults to 1970-01-01
#'
#' @return Returns ordered data frame (if initial input data.table, then data.table) with added missing ranges.
#'
#' @examples
#' df <- data.frame(
#' group = c("a", "a", "b", "b", "b"),
#' start = c("2007-01-01", "2010-06-02", "2009-04-05", "2012-08-01", "2019-03-19"),
#' end = c("2008-02-05", "2013-04-05", "2009-06-03", "2013-02-17", "2021-04-21"),
#' cost = c(143, 144, 105, 153, 124)
#' )
#'
#' fill_ranges(df, start_var = "start", end_var = "end", groups = "group")
#' @export
fill_ranges <- function(df,
                        groups = NULL,
                        start_var = NULL,
                        end_var = NULL,
                        fill = NULL,
                        dimension = "date",
                        fmt = "%Y-%m-%d",
                        tz = "UTC",
                        origin = "1970-01-01"
) {
  
  rangevars <- c(
    start_var,
    end_var
  )
  
  groupsArrange <- c(groups, start_var)
  colsToRetain <- c(groups, rangevars)
  
  dfCopy <- copy(df)
  
  if (dimension == "date") {
    
    dfCopy <- setDT(dfCopy)[
      , (rangevars) := lapply(.SD, function(x) as.Date(as.character(x), format = fmt)), .SDcols = rangevars]
    
  } else if (dimension == "timestamp") {
    
    if (fmt == "%Y-%m-%d") {
      
      warning("Dimension 'timestamp' selected but format unchanged. Will try to convert to '%Y-%m-%d %H:%M:%OS' ..")
      
      fmt <- "%Y-%m-%d %H:%M:%OS"
      
    }
    
    dfCopy <- setDT(dfCopy)[
      , (rangevars) := lapply(.SD, function(x) as.POSIXct(as.character(x), format = fmt, tz = tz)), .SDcols = rangevars]
    
  }
  
  dfCopy <- dfCopy[with(dfCopy, do.call(order, mget(groupsArrange))),]
  
  dfFilled <- copy(dfCopy)
  
  if (!is.null(groups)) {
    
    dfFilled <- dfFilled[, gapFlag := shift(get(end_var)) < (get(start_var) - 1L), by = mget(groups)][
      , `:=` (st_tmp = get(start_var),
              shift_tmp = shift(get(end_var))), by = mget(groups)][
                is.na(gapFlag), gapFlag := FALSE][
                  gapFlag == TRUE, (start_var) := shift_tmp + 1L][
                    gapFlag == TRUE, (end_var) := st_tmp - 1L]
    
  } else {
    
    dfFilled <- dfFilled[, gapFlag := shift(get(end_var)) < (get(start_var) - 1L)][
      , `:=` (st_tmp = get(start_var),
              shift_tmp = shift(get(end_var)))][
                is.na(gapFlag), gapFlag := FALSE][
                  gapFlag == TRUE, (start_var) := shift_tmp + 1L][
                    gapFlag == TRUE, (end_var) := st_tmp - 1L]
    
  }
  
  if (!is.null(fill)) {
    
    splitFill <- unlist(lapply(strsplit(fill, split = "=|,"), trimws))
    
    nms <- splitFill[seq(1, length(splitFill), 2)]
    vals <- splitFill[seq(2, length(splitFill), 2)]
    
    dfFilled <- dfFilled[, (nms) := lapply(.SD, as.character), .SDcols = nms][
      , (nms) := as.list(vals)
      ]
    
    dfFilled <- dfFilled[!(is.na(get(start_var)) | gapFlag == FALSE), mget(c(nms, colsToRetain))]
    
  } else {
    
    dfFilled <- dfFilled[!(is.na(get(start_var)) | gapFlag == FALSE), mget(colsToRetain)]
    
  }
  
  dfFinal <- rbindlist(list(dfCopy, dfFilled), fill = T)
  
  dfFinal <- dfFinal[with(dfFinal, do.call(order, mget(groupsArrange))),]
  
  if (!any(class(df) %in% "data.table")) {
    
    return(setDF(dfFinal))
    
  } else {
    
    return(dfFinal)
    
  }
  
}