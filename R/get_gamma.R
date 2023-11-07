#' Calculate Assortativity Coefficient (Gamma)
#'
#' The assortativity coefficient (Gamma) measures the degree to which individuals within a group
#' are more likely to interact with each other than with individuals from other groups.
#'
#' @param beta Numeric, the observed within-group transmission probability.
#' @param f Numeric, the fraction of the population that is in the group.
#'
#' @return Numeric, the assortativity coefficient (Gamma) computed using the provided parameters.
#'
#' @examples
#' # Calculate the assortativity coefficient for a group
#' beta <- 0.6
#' f <- 0.4
#' gamma <- get_gamma(beta, f)
#' print(gamma)

#'
#' @export
get_gamma <- function(beta, f) {
  numerator <- beta * (1 - f)
  denominator <- f * (1 - beta)
  gamma <- numerator / denominator
  return(gamma)
}
