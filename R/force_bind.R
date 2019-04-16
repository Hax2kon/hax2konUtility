#' Force rbind of two data frames, even though colnames do not match
#'
#' This is a function to force rbind of two data frames. Use with caution.
#' @keywords rbind
#' @export
#' @examples
#' force_bind()


#####   Rebind that ignores column names    #####
force_bind = function(df1, df2) {
  library(dplyr)
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)
}

