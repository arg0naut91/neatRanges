globalVariables(c("rn",
                  ".",
                  "rl",
                  "st_seq",
                  "seqsEnd",
                  "seqs",
                  "nrow",
                  "gapFlag",
                  "st_tmp",
                  "shift_tmp",
                  "startObjects",
                  "endObjects",
                  "..colsToKeep")
                )

#' check_col_mode
#'
#' Help function for checking if a data.frame/data.table column has a numeric mode.
#' Is used for warning is true, because in the next step will try to convert the colum to a Date class.
#' It is a user conscious warnings systemi in order to avoid user side mistakes.
#'
#' @param df data.frame/data.table
#' @param col df column, a charactert, that will be checked for the mode 'numeric'
#'
#' @noRd
#'
check_col_mode <- function(df, col_name) {
  if (mode(df[[col_name]]) == "numeric") {
    # warning or could be a little less scary with 'message'
    warning(sprintf(
      "Be aware: Column '%s' is not a date class variable, hence trying to convert into a date. Originally, the column is of mode 'numeric'. Was counting in ideal case with non-numeric mode of the column (e.g. 'character' or 'POSIXct')",
      col_name
    ))
  }
}
