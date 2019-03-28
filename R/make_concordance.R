#' make a concordance using two variables of a dataframe
#'
#' This function makes a dataframe, which is for a concordance
#' @param a_df: a dataframe;
#' @param v1: a varaible of a_df;
#' @param v2: another variable of a_df
#' @keywords concordance; dataframe
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' a_df <-
#'   data.frame(x = sample(letters)) %>%
#'   mutate(y = toupper(x),
#'          z = rnorm(26))
#' concordance_df <- make_concordance(a_df, "x", "y")

#' @importFrom magrittr %>%

make_concordance <- function(a_df, v1, v2)
{re_df <-
  a_df %>%
  dplyr::select_at(.vars = c(v1, v2)) %>%
  unique()
return(re_df)
}
