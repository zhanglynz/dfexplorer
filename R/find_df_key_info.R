#' find key information about a dataframe
#'
#' This function allows you to find key information about a dataframe
#' @param a_df a dataframe
#' @keywords key info; dataframe
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' temp_df <-
#'      data.frame(a = 1:10,
#'                 b = NA,
#'                 e = c(letters[1:8], NA, NA))
#' x <- find_df_key_info(temp_df)

#' @importFrom magrittr %>%

find_df_key_info <- function(a_df)
{re_df <-
  data.frame(vari_names = names(a_df)) %>%
  dplyr::mutate(type = purrr::map_chr(a_df, typeof),
                no_of_unique_rows = purrr::map_int(a_df, function(x) length(unique(x))),
                no_of_rows = dim(a_df)[1],
                no_of_NAs = purrr::map_int(a_df, function(x) sum(is.na(x))))
 return(re_df)
}
