#' Create a new ggplot plot.
#'
#' @export
ggplot <- ggplot2::ggplot

#' @export
#' @rdname ggplot
#' @usage NULL
ggplot.DataFrame <- function(data, mapping = aes(), ...,
			     environment = parent.frame()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with aes or aes_string")
  }

  p <- structure(list(
    data = data,
    layers = list(),
    scales = ggplot2:::scales_list(),
    mapping = mapping,
    theme = list(),
    coordinates = coord_cartesian(),
    facet = facet_null(),
    plot_env = environment
  ), class = c("gg", "ggplot_SparkR", "ggplot"))
 
  p$labels <- ggplot2:::make_labels(mapping)

  ggplot2:::set_last_plot(p)
  p
}

#' Draw plot on current graphics device.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @keywords hplot
#' @return Invisibly returns the result of \code{\link{ggplot_build_SparkR}}, which
#'   is a list with components that contain the plot itself, the data,
#'   information about the scales, panels etc.
#' @export
#' @method print ggplot_SparkR
print.ggplot_SparkR <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  ggplot2:::set_last_plot(x)
  if(newpage) grid.newpage()

  grDevices::recordGraphics(
    requireNamespace("ggplot2.SparkR", quietly = TRUE),
    list(),
    getNamespace("ggplot2.SparkR")
  )

  data <- ggplot_build_SparkR(x)
  
  gtable <- ggplot_gtable(data)
  if(is.null(vp)) {
    grid.draw(gtable)
  } else {
    if(is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }

  invisible(data)
}

#' @rdname print.ggplot_SparkR
#' @method plot ggplot_SparkR
#' @export
plot.ggplot_SparkR <- print.ggplot_SparkR
