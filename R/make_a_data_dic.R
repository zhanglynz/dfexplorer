#' make a simple data dictionary
#'
#' The output will be a simple data dictionary with outputs 'var_names', 'var_type', 'no_of_unique_values', 'sample_values'
#' @param a_df a dataframe
#' @param var_of_interest numeric positions of interesting variables, and the default is to choose all the variables
#' @keywords dataframe; data dictionary
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' temp_df <-
#'   data.frame(a = 1:10,
#'              b = NA,
#'              d = c(1:9, NA),
#'              e = c(letters[1:8], NA, NA),
#'              stringsAsFactors = FALSE)
#' x <- make_a_data_dic(temp_df)

#' @importFrom magrittr %>%
#' @importFrom stats quantile
#'
make_a_data_dic <- function(a_df, var_of_interest = 1:dim(a_df)[2])
{b_df <-
  a_df %>%
  as.data.frame() %>%
  dplyr::select_at(.vars = var_of_interest)

 re_df <-
   data.frame(var_name = names(b_df)) %>%
   dplyr::mutate(var_type = purrr::map_chr(b_df, typeof)) %>%
   dplyr::mutate(no_of_unique_values = purrr::map_int(b_df, function(x) length(unique(x)))) %>%
   dplyr::mutate(var_values = purrr::map(b_df, unique)) %>%
   dplyr::mutate(sample_values = purrr::map(var_values, function(x) {if(length(x) <= 10) return(x);
                                                                     sample(x, 10)})) %>%
   dplyr::mutate(min_Q1_Q2_Q3_max = purrr::map(b_df, function(x) {if(!is.numeric(x)) return(NA);
        c(min(x, na.rm = TRUE), quantile(x, 0.25, na.rm = TRUE),
        quantile(x, 0.5, na.rm = TRUE), quantile(x, 0.75, na.rm = TRUE),
        max(x, na.rm = TRUE))})) %>%
   dplyr::select(var_name, var_type, no_of_unique_values, sample_values, min_Q1_Q2_Q3_max) %>%
   dplyr::arrange(var_name)

 return(re_df)
}


