% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_groups_utils.R
\name{get_stats}
\alias{get_stats}
\title{Calculate statistics for each group over time}
\usage{
get_stats(data, t, group_n, name, size, groupFOI, dt)
}
\arguments{
\item{data}{A dataframe from simulate_groups().}

\item{t}{The time point.}

\item{group_n}{The number of groups.}

\item{name}{A character vector of length group_n specifying the names of the groups.}

\item{size}{A numeric vector of length group_n specifying the size of each group.}

\item{groupFOI}{A numeric vector of length group_n specifying the FOI that each group receives at time t.}

\item{dt}{A numeric value specifying the time interval between timesteps.}
}
\value{
A data frame containing the calculated statistics for each group at each time point.
}
\description{
This function calculates statistics for each group over time, including the number of susceptible and infected individuals, force of infection (FOI), infection rate, infection probability, and new cases.
}
\keyword{internal}
