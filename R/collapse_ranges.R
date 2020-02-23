#' Collapses the consecutive date or timestamp ranges into one record.
#'
#' The date/time ranges where the gap between two records is equal to or less than max_gap parameter are collapsed into one record.
#'
#' @param df Your data frame
#' @param groups Grouping variables
#' @param start_var Start of the range
#' @param end_var End of the range
#' @param startVars Attributes linked to start of the range which should be kept (converted to character type by default)
#' @param endVars Attributes linked to end of the range which should be kept (converted to character type by default)
#' @param max_gap Gap between date or timestamp ranges, e.g. for 0, default, it will put together all records where there is no gap in-between
#' @param dimension Indicate whether your range includes only dates ('date') or also timestamp ('timestamp'). Defaults to 'date'
#' @param fmt The format of your date or timestamp field, defaults to YMD
#' @param tz Time zone, defaults to UTC
#' @param origin Origin for timestamp conversion, defaults to 1970-01-01
#'
#' @return Returns a data frame (if initial input data.table, then data.table) with collapsed records.
#'
#' @examples
#' df_collapse <- data.frame(
#' id = c(rep("1111", 3), rep("2222", 3)),
#' rating = c("A+", "AA", "AA", rep("B-", 3)),
#' start_date = c("2014-01-01", "2015-01-01", "2016-01-01",
#'               "2017-01-01", "2018-01-01", "2019-01-01"),
#' end_date = c("2014-12-31", "2015-12-31", "2016-03-01",
#'             "2017-01-31", "2018-12-31", "2020-02-01")
#'             )
#'
#' collapse_ranges(df_collapse, c("id", "rating"), "start_date", "end_date")
#' @export
collapse_ranges <- function(df,
                            groups = NULL,
                            start_var = NULL,
                            end_var = NULL,
                            startVars = NULL,
                            endVars = NULL,
                            dimension = "date",
                            max_gap = 0L,
                            fmt = "%Y-%m-%d",
                            tz = "UTC",
                            origin = "1970-01-01") {
  
  groupsArrange <- c(groups, start_var)
  
  rangevars <- c(
    start_var,
    end_var
  )
  
  df_collapsed <- copy(df)
  
  if (!any(class(df_collapsed) %in% "data.table")) setDT(df_collapsed)
  
  if (dimension == "date") {
    
    if (any(sapply(df_collapsed[[rangevars]]), is.factor) | any(sapply(df_collapsed[[rangevars]], is.character))) {
      
      df_collapsed <- df_collapsed[
        , (rangevars) := lapply(.SD, function(x) as.Date(as.character(x), format = fmt)), .SDcols = rangevars]
      
    }
    
    df_collapsed <- df_collapsed[with(df_collapsed, do.call(order, mget(groupsArrange))), ]
    
    if (!is.null(groups)) {
      
      if (!is.null(startVars) & !is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = mget(startVars),
                                                       endObjects = mget(endVars)
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startVars, endVars))
        
      } else if (!is.null(startVars) & is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = mget(startVars),
                                                       endObjects = NULL
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startVars))
        
      } else if (is.null(startVars) & !is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = NULL,
                                                       endObjects = mget(endVars)
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, endVars))
        
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
      
      if (!is.null(startVars) & !is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = mget(startVars),
                                                       endObjects = mget(endVars)
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startVars, endVars))
        
      } else if (!is.null(startVars) & is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = mget(startVars),
                                                       endObjects = NULL
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startVars))
        
      } else if (is.null(startVars) & !is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = NULL,
                                                       endObjects = mget(endVars)
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, endVars))
        
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
    
  } else if (dimension == "timestamp") {
    
    if (any(sapply(df_collapsed[[rangevars]]), is.factor) | any(sapply(df_collapsed[[rangevars]], is.character))) {
      
      if (fmt == "%Y-%m-%d") {
        
        warning("Dimension 'timestamp' selected but format unchanged. Will try to convert to '%Y-%m-%d %H:%M:%OS' ..")
        
        fmt <- "%Y-%m-%d %H:%M:%OS"
        
      }
      
      df_collapsed <- df_collapsed[
        , (rangevars) := lapply(.SD, function(x) as.POSIXct(as.character(x), format = fmt, tz = tz)), .SDcols = rangevars]
      
    }
    
    df_collapsed <- df_collapsed[with(df_collapsed, do.call(order, mget(groupsArrange))), ]
    
    if (!is.null(groups)) {
      
      if (!is.null(startVars) & !is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = mget(startVars),
                                                           endObjects = mget(endVars)
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startVars, endVars))
        
      } else if (!is.null(startVars) & is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = mget(startVars),
                                                           endObjects = NULL
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startVars))
        
      } else if (is.null(startVars) & !is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = NULL,
                                                           endObjects = mget(endVars)
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, endVars))
        
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
      
      if (!is.null(startVars) & !is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = mget(startVars),
                                                           endObjects = mget(endVars)
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startVars, endVars))
        
      } else if (!is.null(startVars) & is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = mget(startVars),
                                                           endObjects = NULL
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startVars))
        
      } else if (is.null(startVars) & !is.null(endVars)) {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = NULL,
                                                           endObjects = mget(endVars)
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, endVars))
        
      } else {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = NULL,
                                                           endObjects = NULL
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var))
        
      }
      
    }
    
  } else { stop("The dimension argument has to be either 'date' or 'timestamp'.") }
  
  if (!any(class(df) %in% "data.table")) {
    
    return(setDF(df_collapsed))
    
  } else {
    
    return(df_collapsed)
    
  }
  
}