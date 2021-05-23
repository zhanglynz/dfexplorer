#' modified min function
#'
#' if an_integer_vec has only NA's then return 0, otherwise return \code{min(x, na.rm = TRUE)}.
#' @param an_integer_vec A vector
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}

modified_min <- function(an_integer_vec)
{if(sum(is.na(an_integer_vec)) == length(an_integer_vec)) return(0)
 return(min(an_integer_vec, na.rm = TRUE))
}
