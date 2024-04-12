#' @title Generate Mcol Matrix
#' @description Transmission Allocation Matrix. Each row represent 'from' groups and columns represent 'to' groups.
#' Conditional on an individual in group 'i' seeding infection, the probability of transmission to group 'j' is given by Mcol[i,j]. The matrix is normalised such that the sum of each row equals 1.
#' @param group_n An integer specifying the number of groups.
#' @param size A numeric vector specifying the size of each group.
#' @param gamma A numeric vector specifying the gamma values for each group. Default is a vector of 1s.
#' @param name A character vector specifying the names of the groups. Default is NULL, in which case the groups are named using the first 'group_n' letters of the alphabet.
#' @return Transmission Allocation Matrix, rows are 'from' groups and columns are 'to' groups.
#' @keywords internal
generate_Mcol <-
  function(group_n,
           size,
           gamma = rep(1, group_n),
           name = NULL) {
    if (is.null(name)) {
      name <- LETTERS[1:group_n]
    }
    names(size) <- name
    names(gamma) <- name
    size_prop <- size / sum(size)

    # columns are from, rows are to
    Mcol <- matrix(
      rep(size_prop, each = group_n),
      nrow = group_n,
      ncol = group_n,
      dimnames = list(
        c(name),
        c(name)
      ),
      byrow = FALSE
    )

    # Multiply diagonal elements by gamma
    diag(Mcol) <- diag(Mcol) * gamma

    # Normalize rows
    Mcol <- sweep(Mcol, 1, rowSums(Mcol), `/`)

    return(Mcol)
  }


#' @title Generate M0 Matrix
#' @description Basic Reproduction Number Matrix. Each row represent 'from' groups and columns represent 'to' groups.
#' Conditional on an individual in group 'i' seeding infection, the expected number of secondary infections within group 'j' is given by M0[i,j]. The sum of each row is the basic reproduction number (R0) for each group.
#' @param group_n An integer specifying the number of groups.
#' @param size A numeric vector specifying the size of each group.
#' @param gamma A numeric vector specifying the gamma values for each group. Default is a vector of 1s.
#' @param r0 A numeric vector specifying the basic reproduction number (R0) for each group.
#' @param name A character vector specifying the names of the groups. Default is NULL, in which case the groups are named using the first 'group_n' letters of the alphabet.
#' @return Basic Reproduction Number Matrix, rows are 'from' groups and columns are 'to' groups.
#' @keywords internal

generate_M0 <-
  function(group_n,
           size,
           gamma = rep(1, group_n),
           r0,
           name = NULL) {
    names(r0) <- name
    Mcol <- generate_Mcol(
      group_n = group_n,
      name = name,
      size = size,
      gamma = gamma
    )

    # Multiply rows by r0
    M0 <- sweep(Mcol, 1, r0, `*`)

    return(M0)
  }
