#' Split ranges into multiple records
#'
#' @param df Your data frame (can also be a data.table or a tibble)
#' @param start_var Start variable
#' @param end_var End variable
#' @param fmt Format of the date; defaults to Y-m-d
#' @param vars_to_keep Any column you'd like to retain (optional)
#' @param partition_by How should the range be partitioned ('year' or 'month'); defaults to 'year'
#'
#' @return Returns a data frame with start, end and optional grouping columns 
#'
#' @examples
#' 
#' df <- data.frame(group = c("a", "a", "b", "b", "c"),
#' start = c("2017-05-01", "2019-04-03", "2011-03-03", "2014-05-07", "2017-02-01"),
#' end = c("2018-09-01", "2020-04-03", "2012-05-03", "2016-04-02", "2017-04-05")
#' ) 
#' 
#' partition_ranges(df, "start", "end", partition_by = "month")
#' @export
partition_ranges <- function(df, start_var, end_var, fmt = "%Y-%m-%d", vars_to_keep = NULL, partition_by = "year") {
  
  rangevars <- c(
    substitute(start_var),
    substitute(end_var)
  )

  partitioned <- setDT(copy(df))[, (rangevars) := lapply(.SD, function(x) as.Date(as.character(x), format = fmt)), .SDcols = rangevars]
  
  if (partition_by == "year") {
    
    grp <- c("rl", vars_to_keep)
    
    partitioned <- partitioned[partitioned[, rep(.I, 1 + year(get(end_var)) - year(get(start_var)))]][
        , rl := rleid(get(start_var))][
          , (start_var) := pmax(get(start_var)[1], as.Date(paste0(year(get(start_var)[1]) + 0:(.N-1), '-01-01'))), by = mget(grp)][
            , (end_var) := pmin(get(end_var)[.N], as.Date(paste0(year(get(end_var)[.N]) - (.N-1):0, '-12-31'))), by = mget(grp)][
              , "rl" := NULL]
    
  }
  
  else if (partition_by == "month") {
    
    grp <- c("nrow", vars_to_keep, end_var)
    grp2ndlev <- c(vars_to_keep, start_var)
    
    partitionedIn <- partitioned[(format(get(start_var), "%Y-%m") == format(get(end_var), "%Y-%m")), ]
    partitionedOut <- partitioned[!(format(get(start_var), "%Y-%m") == format(get(end_var), "%Y-%m")), ]

    partitionedOut <- partitionedOut[, nrow := 1:.N][
      , .(seqs = seq.Date(get(start_var), get(end_var), by = "month")), by = mget(grp)][
        , seqs := c(seqs[1], as.Date(paste0(format(seqs[-1], "%Y-%m"), "-01"))), by = nrow][
      
      , seqsEnd := { 
        
        tmp <- as.POSIXlt(as.Date(paste0(format(seqs, "%Y-%m"), "-01"))); 
        tmp$mon <- tmp$mon + 1; 
        return(
          as.Date(as.character(as.POSIXct(tmp))) - 1
        ) 
        
      }
      
      ][, (start_var) := seqs][, lapply(.SD, function(x) pmin(x, seqsEnd)), by = mget(grp2ndlev), .SDcols = substitute(end_var)]
    
    nms <- names(partitionedOut)
    
    if (nrow(partitionedIn) > 0) {
      
      partitionedIn <- partitionedIn[, names(partitionedIn) %in% nms, with = FALSE]
      partitioned <- rbind(partitionedIn, partitionedOut) 
      
      } else {
        
        partitioned <- partitionedOut
        
      }
    
    partitioned <- setorderv(partitioned, nms)
    
  } else {
    
    stop("partition_by argument has to be either 'year' or 'month'.")
    
  }
  
  if (!any(class(df) %in% "data.table")) {
    
    return(setDF(partitioned))
    
  } else {
    
    return(partitioned)
    
  }
    
}