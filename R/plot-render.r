#' Build a plot with all the usual bits and pieces.
#'
#' This function builds all grobs necessary for displaying the plot, and
#' stores them in a special data structure called a \code{\link{gtable}}.
#' This object is amenable to programmatic manipulation, should you want
#' to (e.g.) make the legend box 2 cm wide, or combine multiple plots into
#' a single display, preserving aspect ratios across the plots.
#'
#' @seealso \code{\link{print.ggplot}} and \code{link{benchplot}} for
#'  for functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @return a \code{\link{gtable}} object
#' @keywords internal
#' @param plot plot object
#' @param data plot data generated by \code{\link{ggplot_build}}
#' @export
ggplot_gtable <- function(data) {
  print("MK_run ggplot_gtable() in plot_render.r")
  plot <- data$plot
  panel <- data$panel
  data <- data$data
  theme <- plot_theme(plot)

  build_grob <- function(layer, layer_data) {
    print("MK_run build_grob() in plot_render.r")
    if (nrow(layer_data) == 0) return()

    dlply(layer_data, "PANEL", function(df) {
      panel_i <- match(df$PANEL[1], panel$layout$PANEL)
      layer$make_grob(df, scales = panel$ranges[[panel_i]], cs = plot$coordinates)
    }, .drop = FALSE)
  }

  # helper function return the position of panels in plot_table
  find_panel <- function(table) {
    print("MK_run find_panel() in plot_render.r")
    layout <- table$layout
    panels <- layout[grepl("^panel", layout$name), , drop = FALSE]

    data.frame(
      t = min(panels$t),
      r = max(panels$r),
      b = max(panels$b),
      l = min(panels$l)
    )
  }

  # List by layer, list by panel
  geom_grobs <- Map(build_grob, plot$layers, data)

  plot_table <- facet_render(plot$facet, panel, plot$coordinates,
    plot_theme(plot), geom_grobs)

  # Axis labels
  labels <- coord_labels(plot$coordinates, list(
    x = xlabel(panel, plot$labels),
    y = ylabel(panel, plot$labels)
  ))
  xlabel <- element_render(theme, "axis.title.x", labels$x)
  ylabel <- element_render(theme, "axis.title.y", labels$y)

  panel_dim <-  find_panel(plot_table)

  xlab_height <- grobHeight(xlabel) +
    if (is.null(labels$x)) unit(0, "lines") else unit(0.5, "lines")
  plot_table <- gtable_add_rows(plot_table, xlab_height)
  plot_table <- gtable_add_grob(plot_table, xlabel, name = "xlab",
    l = panel_dim$l, r = panel_dim$r, t = -1, clip = "off")

  ylab_width <- grobWidth(ylabel) +
    if (is.null(labels$y)) unit(0, "lines") else unit(0.5, "lines")
  plot_table <- gtable_add_cols(plot_table, ylab_width, pos = 0)
  plot_table <- gtable_add_grob(plot_table, ylabel, name = "ylab",
    l = 1, b = panel_dim$b, t = panel_dim$t, clip = "off")

  # Legends
  position <- theme$legend.position
  if (length(position) == 2) {
    coords <- position
    position <- "manual"
  }

  legend_box <- if (position != "none") {
    build_guides(plot$scales, plot$layers, plot$mapping, position, theme, plot$guides, plot$labels)
  } else {
    zeroGrob()
  }

  if (is.zero(legend_box)) {
    position <- "none"
  } else {
    # these are a bad hack, since it modifies the contents fo viewpoint directly...
    legend_width  <- gtable_width(legend_box)  + theme$legend.margin
    legend_height <- gtable_height(legend_box) + theme$legend.margin

    # Set the justification of the legend box
    # First value is xjust, second value is yjust
    just <- valid.just(theme$legend.justification)
    xjust <- just[1]
    yjust <- just[2]

    if (position == "manual") {
      xpos <- theme$legend.position[1]
      ypos <- theme$legend.position[2]

      # x and y are specified via theme$legend.position (i.e., coords)
      legend_box <- editGrob(legend_box,
        vp = viewport(x = xpos, y = ypos, just = c(xjust, yjust),
          height = legend_height, width = legend_width))
    } else {
      # x and y are adjusted using justification of legend box (i.e., theme$legend.justification)
      legend_box <- editGrob(legend_box,
        vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust)))
    }
  }

  panel_dim <-  find_panel(plot_table)
  # for align-to-device, use this:
  # panel_dim <-  summarise(plot_table$layout, t = min(t), r = max(r), b = max(b), l = min(l))

  if (position == "left") {
    plot_table <- gtable_add_cols(plot_table, legend_width, pos = 0)
    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
      t = panel_dim$t, b = panel_dim$b, l = 1, r = 1, name = "guide-box")
  } else if (position == "right") {
    plot_table <- gtable_add_cols(plot_table, legend_width, pos = -1)
    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
      t = panel_dim$t, b = panel_dim$b, l = -1, r = -1, name = "guide-box")
  } else if (position == "bottom") {
    plot_table <- gtable_add_rows(plot_table, legend_height, pos = -1)
    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
      t = -1, b = -1, l = panel_dim$l, r = panel_dim$r, name = "guide-box")
  } else if (position == "top") {
    plot_table <- gtable_add_rows(plot_table, legend_height, pos = 0)
    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
      t = 1, b = 1, l = panel_dim$l, r = panel_dim$r, name = "guide-box")
  } else if (position == "manual") {
    # should guide box expand whole region or region withoug margin?
    plot_table <- gtable_add_grob(plot_table, legend_box,
        t = panel_dim$t, b = panel_dim$b, l = panel_dim$l, r = panel_dim$r,
        clip = "off", name = "guide-box")
  }

  # Title
  title <- element_render(theme, "plot.title", plot$labels$title)
  title_height <- grobHeight(title) +
    if (is.null(plot$labels$title)) unit(0, "lines") else unit(0.5, "lines")

  pans <- plot_table$layout[grepl("^panel", plot_table$layout$name), ,
    drop = FALSE]

  plot_table <- gtable_add_rows(plot_table, title_height, pos = 0)
  plot_table <- gtable_add_grob(plot_table, title, name = "title",
    t = 1, b = 1, l = min(pans$l), r = max(pans$r), clip = "off")

  # Margins
  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[1], pos = 0)
  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[2])
  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[3])
  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[4], pos = 0)

  if (inherits(theme$plot.background, "element")) {
    plot_table <- gtable_add_grob(plot_table,
      element_render(theme, "plot.background"),
      t = 1, l = 1, b = -1, r = -1, name = "background", z = -Inf)
    plot_table$layout <- plot_table$layout[c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1)),]
    plot_table$grobs <- plot_table$grobs[c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1))]
  }
  plot_table
}

#' Draw plot on current graphics device.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @keywords hplot
#' @export
#' @method print ggplot
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  print("MK_run print.ggplot() in plot_render.r")
  set_last_plot(x)
  if (newpage) grid.newpage()

  #MK_add
  #data <- ggplot_build(x)
  if(is.data.frame(x$data)) {
    data <- ggplot_build(x)
  } else {
    data <- ggplot.SparkR_build(x)
  }

  gtable <- ggplot_gtable(data)
  if (is.null(vp)) {
    grid.draw(gtable)
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }

  invisible(data)
}
#' @rdname print.ggplot
#' @method plot ggplot
#' @export
plot.ggplot <- print.ggplot


#' Generate a ggplot2 plot grob.
#'
#' @param x ggplot2 object
#' @keywords internal
#' @export
ggplotGrob <- function(x) {
  print("MK_run ggplotGrob() in plot_render.r")
  ggplot_gtable(ggplot_build(x))
}


