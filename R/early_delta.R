#' Compute Delta for Multiple Columns
#'
#' Computes the delta values for multiple columns based on the binomial mixing matrix.
#'
#' @param data The input data frame containing the source_group and group columns.
#' @param min_t The minimum time for the window.
#' @param max_t The maximum time for the window.
#' @param size An integer vector representing the sizes of each group.
#' @param name A character vector containing the names of each group (respective to the `size` vector).
#'
#' @return A matrix representing the delta values for each column along with associated count data.
#'
#' @export

early_delta <- function(data, min_t, max_t, size, name) {
  Mat <- binom_mix(data, min_t = min_t, max_t = max_t)

  if(all(Mat[, c("successes", "trials")] == 0)){
    return(Mat)
  }

  delta <-
    matrix(
      nrow = nrow(Mat),
      ncol = 5,
      dimnames = list(rownames(Mat),
                      c("est", "lower_ci", "upper_ci", "successes", "trials"))
    )
  size_prop <- as.matrix(size / sum(size))
  rownames(size_prop) <- name


  for (i in rownames(Mat)) {
    numerator <- Mat[i, 1:3] * sum(size_prop[rownames(Mat) != i])

    denominator <- size_prop[i,] * (1 - Mat[i, 1:3])

    D <- numerator / denominator

    delta[i, ] <- c(D, Mat[i, 4:5])
  }

  return(delta)
}

