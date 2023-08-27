n_groups = 3
size = c(100, 100, 100)
name = c("A", "B", "D")
r0 = c(5, 3, 3)
prop_susceptible = c(0.7,0.3, 1)
delta = c(5, 3, 2)

n_groups = 2
size = c(100, 100)
name = c("A", "B")
r0 = c(5, 6)
prop_susceptible = c(0.4, 0.6)
delta = c(5, 3)

# MCOL to delta -----------------------------------------------------------


Mcol_to_delta <- function(n_groups, size, name, Mcol) {
  homo_mat <-
    generate_Mcol(
      n_groups = n_groups,
      size = size,
      name = name,
      delta = 1
    )

  deltas <- vector("numeric", length = n_groups)

  for (i in 1:n_groups) {
    group <- name[i]
    delta_opt <- optimize(function(delta) {
      abs((delta * homo_mat[group, group]) /
            ((delta * homo_mat[group, group]) + sum(homo_mat[colnames(homo_mat) != group, group])) - Mcol[group, group])
    },
    lower = -1e10,
    upper = 1e10)$minimum

    deltas[i] <- round(delta_opt, digits = 3)
  }

  names(deltas) <- name
  return(deltas)
}

Mcol <- generate_Mcol(
  n_groups = n_groups,
  size = size,
  name = name,
  delta = delta)

Mcol_to_delta(
  n_groups = n_groups,
  size = size,
  name = name,
  Mcol = Mcol)



# M0 to delta -------------------------------------------------------------
M0_to_delta <- function(n_groups, size, name, r0, M0) {
  homo_mat <- generate_Mcol(n_groups = n_groups, size = size, name = name, delta = 1)

  deltas <- vector("numeric", length = n_groups)

  for (i in 1:n_groups) {
    group <- name[i]
    Mcol <- M0[group, group] / r0[i]  # Calculate Mcol for the current group

    delta_opt <- optimize(function(delta) {
      abs(
        (delta * homo_mat[group, group]) /
          ((delta * homo_mat[group, group]) + sum(homo_mat[colnames(homo_mat) != group, group])) - Mcol
      )
    },
    lower = -1e10,
    upper = 1e10)$minimum

    deltas[i] <- round(delta_opt, digits = 3)
  }

  names(deltas) <- name
  return(deltas)
}


M0 <- generate_M0(
  n_groups = n_groups,
  size = size,
  name = name,
  r0 = r0,
  delta = delta)

M0_to_delta(
  n_groups = n_groups,
  size = size,
  name = name,
  r0 = r0,
  M0 = M0)




# Mt_to_delta -------------------------------------------------------------


Mt_to_delta <- function(n_groups, size, name, r0, prop_susceptible, Mt) {
  homo_mat <- generate_Mcol(n_groups = n_groups, size = size, name = name, delta = 1)

  deltas <- vector("numeric", length = n_groups)

  for (i in 1:n_groups) {
    group <- name[i]
    M0 <- Mt[group, group] / prop_susceptible[i]  # Calculate M0 for the current group
    Mcol <- M0 / r0[i]

    delta_opt <- optimize(function(delta) {
      abs(
        (delta * homo_mat[group, group]) /
          ((delta * homo_mat[group, group]) + sum(homo_mat[colnames(homo_mat) != group, group])) - Mcol
      )
    },
    lower = -1e10,
    upper = 1e10)$minimum

    deltas[i] <- round(delta_opt, digits = 3)
  }

  names(deltas) <- name
  return(deltas)
}


Mt <- generate_Mt(
  n_groups = n_groups,
  size = size,
  name = name,
  r0 = r0,
  prop_susceptible = prop_susceptible,
  delta = delta
)

Mt_to_delta(
  n_groups = n_groups,
  size = size,
  name = name,
  r0 = r0,
  prop_susceptible = prop_susceptible,
  Mt = Mt
)


# Mcolt_to_delta -------------------------------------------------------------
#
#
# Mcolt_to_delta <- function(n_groups, size, name, r0, prop_susceptible, Mcolt) {
#   homo_mat <- generate_Mcol(n_groups = n_groups, size = size, name = name, delta = 1)
#
#   deltas <- vector("numeric", length = n_groups)
#
#   Rt <- r0 * prop_susceptible
#
#   Mt <- sweep(Mcolt, 2, colSums(Mt), `/`)
#
#   Mt_to_delta(n_groups, size, name, r0, prop_susceptible, Mt)
#   names(deltas) <- name
#   return(deltas)
# }
#
#
# Mcolt <- generate_Mcolt(
#   n_groups = n_groups,
#   size = size,
#   name = name,
#   r0 = r0,
#   prop_susceptible = prop_susceptible,
#   delta = delta
# )
#
# Mcolt_to_delta(
#   n_groups = n_groups,
#   size = size,
#   name = name,
#   r0 = r0,
#   prop_susceptible = prop_susceptible,
#   Mcolt = Mcolt
# )
