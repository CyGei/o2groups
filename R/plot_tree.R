#' Plots the transmission tree over time
#'
#' @param data A dataframe from simulate_groups().
#' @param pal A named character vector of hex codes for each group.
#' @export
#' @examples
#'out <-
#'simulate_groups(
#'  duration = 100,
#'  n_groups = 2,
#'  size = c(10, 30),
#'  name = c("A", "B"),
#'  delta = c(5, 2),
#'  intro_n = c(1, 2),
#'  r0 = c(4, 3),
#'  generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2)$d(1:30)
#')
#' plot_tree(out$data, pal = c("A" = "#FA05BA", "B" = "#FFA500"))


plot_tree <- function(data, pal) {
  if (requireNamespace("plotly", quietly = TRUE)) {

    max_t <- max(data$date_infection, na.rm = TRUE)
    #generate coordinates
    clusters <-
      generate_clusters(
        n_groups = length(table(data$group)),
        name = names(table(data$group)),
        size = as.vector(table(data$group)),
        spread = 0.3,
        shape = "round"
      )
    data_list <- split(data, f = data$group)

    data_list <- lapply(names(data_list), function(group_name) {
      group_data <- data_list[[group_name]]
      cluster_data <- clusters[[group_name]][, 1:2]
      cbind(group_data, cluster_data)
    })

    data <- do.call(rbind, data_list)

    # Extract case data
    cases <-
      data[c("x", "y", "group", "id", "date_infection", "source")]

    # Extract infector's position
    infectors <- data[c("x", "y", "group", "id", "date_infection")]
    names(infectors) <- c("source_x", "source_y", "source_group", "source", "source_date_infection")


    # Match infected individuals to their infector
    segments <-
      merge(cases, infectors, by = "source", all.x = TRUE)

    # Get infector's Reproduction number
    R <- as.data.frame(table(segments$source))
    names(R) <- c("id", "R")

    # Final data
    segments <- merge(segments, R, by = "id", all.x = TRUE)
    segments$R[is.na(segments$R)] <- 0


    # Theme No axis
    Noax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )

    # Create plotly
    p <- plotly::plot_ly(
      segments,
      hoverinfo = "text",
      text = ~ paste(
        "</br> ID:",
        id,
        "</br> Group:",
        group,
        "</br> R:",
        R,
        "</br>",
        "</br> Source:",
        source,
        "</br> Source Group:",
        source_group
      )
    )

    # Add markers
    p <- plotly::add_markers(
      p,
      x = ~ date_infection,
      y = ~ y,
      symbol = ~ group,
      color = ~ group,
      colors = pal,
      showlegend = TRUE
    )

    # Add segments
    p <- plotly::add_segments(
      p,
      data = segments[!is.na(segments$source_date_infection),],
      x = ~ source_date_infection,
      y = ~ source_y,
      xend = ~ date_infection,
      yend = ~ y,
      color = ~ source_group,
      colors = pal,
      size = I(1),
      showlegend = FALSE
    )

    # Set layout
    p <- plotly::layout(
      p,
      title = "Outbreak",
      xaxis = list(
        title = "Time",
        tickmode = "linear",
        dtick = 2,
        range = c(-0.9, max_t)
      ),
      yaxis = Noax,
      showlegend = TRUE,
      plot_bgcolor = "#2d3436",
      paper_bgcolor = "#2d3436",
      font = list(color = "#ffffff")
    )

    # Set display mode bar to false
    p <- plotly::config(p, displayModeBar = FALSE)


    return(p)
  } else {
    stop("Please install plotly to use this function")
  }
}


# plot_time_tree(out.data = out$data, pal = c("A" = "purple", "B" = "orange"))
