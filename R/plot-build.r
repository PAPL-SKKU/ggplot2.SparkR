#' Build ggplot for rendering.
#'
#' This function takes the plot object, and performs all steps necessary to
#' produce an object that can be rendered.  This function outputs two pieces:
#' a list of data frames (one for each layer), and a panel object, which
#' contain all information about axis limits, breaks etc.
#'
#' @param plot ggplot object
#' @seealso \code{\link{print.ggplot}} and \code{\link{benchplot}} for
#'  for functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @keywords internal
#' @export
ggplot_build <- function(plot) {
	print("MK_run ggplot_build() in plot-build.r");
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)

  plot <- plot_clone(plot)
  layers <- plot$layers
  print("MK_test variable layers in plot-build.r:")
  print(layers)
  layer_data <- lapply(layers, function(y) y$data)
  print("MK_test variable layer_data in plot-build.r:-1")
  print(layer_data)
  scales <- plot$scales
  # Apply function to layer and matching data
  dlapply <- function(f) {
    #MK_test
    print("MK_run dlapply() function in plot_build")
    #print("MK_test variable data in plot_build:")
    #print(data)
    #print("MK_test data in dlapply():")
    #print(data)
    #print("MK_test layers in dlapply():")
    #print(layers)
    out <- vector("list", length(data))
    for(i in seq_along(data)) {
      out[[i]] <- f(d = data[[i]], p = layers[[i]])
    }
    #MK_test
    #print("MK_test out:")
    #print(out)
    out
  }

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data

  panel <- new_panel()
  panel <- train_layout(panel, plot$facet, layer_data, plot$data)
  data <- map_layout(panel, plot$facet, layer_data, plot$data)

  #print("MK_test variable data in ggplot_build() plot_build.r-1")
  #print(data)
  # Compute aesthetics to produce data with generalised variable names
  # MK compute_aesthetics() is in layer.r
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
  #print("MK_test variable data in ggplot_build() plot_build.r-2")
  #print(data)

  data <- lapply(data, add_group)
  print("MK_test variable data in ggplot_build() plot_build.r")
  print(data)
  # Transform all scales
  data <- lapply(data, scales_transform_df, scales = scales)
  print("MK_test variable data in ggplot_build() plot_build-2.r")
  print(data)
  print(typeof(data))

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  panel <- train_position(panel, data, scale_x(), scale_y())
  print("MK_test variable panel in ggplot_build.r-2")
  print(panel)
  data <- map_position(panel, data, scale_x(), scale_y())

  # Apply and map statistics
  data <- calculate_stats(panel, data, layers)
  data <- dlapply(function(d, p) p$map_statistic(d, plot))
  data <- lapply(data, order_groups)

  # Make sure missing (but required) aesthetics are added
  scales_add_missing(plot, c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Apply position adjustments
  data <- dlapply(function(d, p) p$adjust_position(d))

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's
  # displayed, or does it include the range of underlying data
  reset_scales(panel)
  panel <- train_position(panel, data, scale_x(), scale_y())
  data <- map_position(panel, data, scale_x(), scale_y())

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }

  # Train coordinate system
  panel <- train_ranges(panel, plot$coordinates)

  list(data = data, panel = panel, plot = plot)
}

ggplot.SparkR_build <- function(plot) {
  print("MK_add ggplot.SparkR_build in plot-build.r")
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)

  plot <- plot_clone(plot)
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)

  scales <- plot$scales
 
  dlapply <- function(f) {
    #MK_test
    print("MK_run dlapply() function in plot_build")
    #print("MK_test variable data in plot_build:")
    #print(data)
    #print("MK_test data in dlapply():")
    #print(data)
    #print("MK_test layers in dlapply():")
    #print(layers)
    out <- vector("list", length(data))
    for(i in seq_along(data)) {
      out[[i]] <- f(d = data[[i]], p = layers[[i]])
    }
    #MK_test
    #print("MK_test out:")
    #print(out)
    out
  }

  map_layout.SparkR.null <- function(data) {
    data <- withColumn(data, "PANEL", cast(isNull(data[[1]]*0),"integer"))
    data
  }

  add_group.SparkR.null <- function(data) {
    data <- withColumn(data, "group", cast(isNull(data[[1]]*0),"integer"))
    data
  }

  panel <- new_panel()
  panel <- train_layout(panel, plot$facet, layer_data, plot$data)
  print("MK_test variable panel in ggplot_build.r-1")
  print(panel)

  data <- map_layout.SparkR.null(plot$data)
  data <- add_group.SparkR.null(data)
  print("as.chracter(plot$mapping$x)")
  
  #DataFrame to R List
  x1 <- as.character(plot$mapping$x)
  y1 <- as.character(plot$mapping$y)
  data <- withColumnRenamed(data, x1, "x")
  data <- withColumnRenamed(data, y1, "y")
  data <- list(collect(select(data, "x", "y", "PANEL", "group")))
  
  
  #data <- lapply(data, scales_transform_df, scales = scales)
  print("MK_test variable data in ggplot.SparkR_build")
  print(data)
  print(typeof(data))
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
    
  panel <- train_position(panel, data, scale_x(), scale_y())
  data <- map_position(panel, data, scale_x(), scale_y())
 
  # Apply and map statistics
  data <- calculate_stats(panel, data, layers)
  data <- dlapply(function(d, p) p$map_statistic(d, plot))
  data <- lapply(data, order_groups)

  # Make sure missing (but required) aesthetics are added
  scales_add_missing(plot, c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Apply position adjustments
  data <- dlapply(function(d, p) p$adjust_position(d))

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's
  # displayed, or does it include the range of underlying data
  reset_scales(panel)
  panel <- train_position(panel, data, scale_x(), scale_y())
  data <- map_position(panel, data, scale_x(), scale_y())

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }

  # Train coordinate system
  panel <- train_ranges(panel, plot$coordinates)

  list(data = data, panel = panel, plot = plot)
}
