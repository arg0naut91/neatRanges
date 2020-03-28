#' Collapses the consecutive date or timestamp ranges into one record.
#'
#' The date/time ranges where the gap between two records is equal to or less than max_gap parameter are collapsed into one record.
#'
#' @param df Your data frame
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
                            startAttr = NULL,
                            endAttr = NULL,
                            dimension = "date",
                            max_gap = 0L,
                            fmt = "%Y-%m-%d",
                            tz = "UTC",
                            origin = "1970-01-01") {
  
  df_collapsed <- copy(df)
  
  if (!any(class(df) %in% "data.table")) setDT(df_collapsed)
  
  if (!is.null(startAttr) | !is.null(endAttr)) for (j in c(startAttr, endAttr)) set(df_collapsed, j = j, value = as.character(df_collapsed[[j]])) 
  
  if (dimension == "date") {
    
    if (class(df_collapsed[[start_var]]) != 'Date' | class(df_collapsed[[end_var]]) != 'Date') {
      
      for (j in c(start_var, end_var)) set(df_collapsed, j = j, value = as.Date(as.character(df_collapsed[[j]]), format = fmt))
      
    }
    
    setorderv(df_collapsed, c(groups, start_var))
    
    if (!is.null(groups)) {
      
      if (!is.null(startAttr) & !is.null(endAttr)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = mget(startAttr),
                                                       endObjects = mget(endAttr)
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr, endAttr))
        
      } else if (!is.null(startAttr) & is.null(endAttr)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = mget(startAttr),
                                                       endObjects = NULL
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr))
        
      } else if (is.null(startAttr) & !is.null(endAttr)) {
        
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
      
      if (!is.null(startAttr) & !is.null(endAttr)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = mget(startAttr),
                                                       endObjects = mget(endAttr)
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr, endAttr))
        
      } else if (!is.null(startAttr) & is.null(endAttr)) {
        
        df_collapsed <- df_collapsed[, updateAndSubset(get(start_var), 
                                                       get(end_var), 
                                                       max_gap = max_gap,
                                                       startObjects = mget(startAttr),
                                                       endObjects = NULL
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr))
        
      } else if (is.null(startAttr) & !is.null(endAttr)) {
        
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
    
  } else if (dimension == "timestamp") {
    
    if (!any(class(df_collapsed[[start_var]]) %in% c('POSIXct', 'POSIXlt')) | !any(class(df_collapsed[[end_var]]) %in% c('POSIXct', 'POSIXlt'))) {
      
      if (fmt == "%Y-%m-%d") {
        
        warning("Dimension 'timestamp' selected but format unchanged. Will try to convert to '%Y-%m-%d %H:%M:%OS' ..")
        
        fmt <- "%Y-%m-%d %H:%M:%OS"
        
      }
      
      for (j in c(start_var, end_var)) set(df_collapsed, j = j, value = as.POSIXct(as.character(df_collapsed[[j]]), format = fmt, tz = tz))
      
    }
    
    setorderv(df_collapsed, c(groups, start_var))
    
    if (!is.null(groups)) {
      
      if (!is.null(startAttr) & !is.null(endAttr)) {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = mget(startAttr),
                                                           endObjects = mget(endAttr)
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr, endAttr))
        
      } else if (!is.null(startAttr) & is.null(endAttr)) {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = mget(startAttr),
                                                           endObjects = NULL
        ), by = mget(groups)]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(groups, start_var, end_var, startAttr))
        
      } else if (is.null(startAttr) & !is.null(endAttr)) {
        
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
      
      if (!is.null(startAttr) & !is.null(endAttr)) {
        
        df_collapsed <- df_collapsed[, updateAndSubsetTime(get(start_var), 
                                                           get(end_var), 
                                                           max_gap = max_gap,
                                                           startObjects = mget(startAttr),
                                                           endObjects = mget(endAttr)
        )]
        
        setnames(df_collapsed, 1:ncol(df_collapsed), c(start_var, end_var, startAttr, endAttr))
        
      } else if (!is.null(startAttr) & is.null(endAttr)) {
        
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
    
  } else { stop("The dimension argument has to be either 'date' or 'timestamp'.") }
  
  if (!any(class(df) %in% "data.table")) {
    
    return(setDF(df_collapsed))
    
  } else {
    
    return(df_collapsed)
    
  }
  
}