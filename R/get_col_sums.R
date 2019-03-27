#' get column sums added to a dataframe
#'
#' This function returns a new dataframe with its last row being the
#' column sums of the original dataframe. Note that the first column of the
#' input dataframe must be of character type.
#' @param a_df: a dataframe
#' @keywords dataframe; column sums
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' df <-
#'   data.frame(x = letters[1:10],
#'              y = 1,
#'              z = 2,
#'              w = rnorm(10))
#'
#' df_with_col_sums <- get_col_sums_added(df)

get_col_sums_added <- function(a_df)
{a_df[, 1] <- as.character(a_df[, 1])
 sums <- colSums(a_df[, -1])
 last_row <- c("Total", as.list(sums))
 a_df[nrow(a_df) + 1, ] <- last_row
 return(a_df)
}
