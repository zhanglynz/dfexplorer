#' find key information about a dataframe
#'
#' This function allows you to find key information about a dataframe.
#' @param a_df A dataframe
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' temp_df <-
#'      data.frame(a = 1:10,
#'                 b = NA,
#'                 e = c(letters[1:8], NA, NA),
#'                 stringAsFactor = FALSE)
#' x <- find_df_key_info(temp_df)


find_df_key_info <- function(a_df)
{re_df <-
  data.frame(vari_name = names(a_df), stringsAsFactors = FALSE) %>%
  dplyr::mutate(type = purrr::map_chr(a_df, typeof),
                no_of_unique_rows = purrr::map_int(a_df, function(x) length(unique(x))),
                no_of_rows = dim(a_df)[1],
                no_of_NAs = purrr::map_int(a_df, function(x) sum(is.na(x))),
                min_width = purrr::map_dbl(a_df, function(x) modified_min(stringr::str_length(x))),
                max_width = purrr::map_dbl(a_df, function(x) modified_max(stringr::str_length(x)))) %>%
  dplyr::mutate(var_values = purrr::map(a_df, unique)) %>%
  dplyr::mutate(sample_values = purrr::map(var_values, function(x) {if(length(x) <= 10) return(x);
                                                         sample(x, 10)})) %>%
  dplyr::select(-var_values) %>%

  dplyr::arrange(vari_name)
 return(re_df)
}


