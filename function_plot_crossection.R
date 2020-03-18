plot_crossection <- function(data,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 2,
                             colour_by = NULL) {

  colour_by <- enquo(colour_by)

  #clip pointcloud using the provided coordinates
  data_clip <- clip_transect(data,
                             p1 = p1,
                             p2 = p2,
                             width = width)

  #generate the plot
  p <- ggplot(data_clip@data, aes(X,Z)) +
    geom_point() +
    coord_equal() +
    theme_bw()

  # add colour_by if specified
  if (!is.null(colour_by)) {

    p <- p + geom_point(aes(color=!!colour_by)) +
      theme(legend.position = "bottom")

  }

  return(p)
}
