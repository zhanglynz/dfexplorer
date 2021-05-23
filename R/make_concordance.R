#' make a concordance using two variables of a dataframe
#'
#' This function creats a dataframe, which is for a concordance.
#' @param a_df A dataframe;
#' @param v1 A varaible of a_df;
#' @param v2 Another variable of a_df
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @importFrom magrittr %>%
#' @examples
#' require(dplyr)
#' a_df <-
#'   data.frame(x = sample(letters), stringAsFactor = FALSE) %>%
#'   mutate(y = toupper(x),
#'          z = rnorm(26))
#' concordance_df <- make_concordance(a_df, "x", "y")

make_concordance <- function(a_df, v1, v2)
{re_df <-
  a_df %>%
  dplyr::select_at(.vars = c(v1, v2)) %>%
  unique()
return(re_df)
}
