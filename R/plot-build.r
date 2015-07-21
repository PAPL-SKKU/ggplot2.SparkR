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
  print("ggplot_build")
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)

  plot <- plot_clone(plot)
  layers <- plot$layers
  
  print("lapply")
  layer_data <- lapply(layers, function(y) y$data)
  
  print("layer_data")
  print(layer_data)
  
  scales <- plot$scales
  # Apply function to layer and matching data
  dlapply <- function(f) {
    print("dlapply")
    out <- vector("list", length(data))
    for(i in seq_along(data)) {
      out[[i]] <- f(d = data[[i]], p = layers[[i]])
    }
    out
  }

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data

  # (BJH) Add class "panel"
  panel <- new_panel()
  
  # (BJH) add panel information about layout, shrink
  panel <- train_layout(panel, plot$facet, layer_data, plot$data)
  
  #-----------------------------------------------------------------------
  # Convert complete 
  #-----------------------------------------------------------------------
  
  # (BJH) what is the meaning of PANEL?
  # basic dataframe + PANEL column
  data <- map_layout(panel, plot$facet, layer_data, plot$data)
  
  # Compute aesthetics to produce data with generalised variable names
  # (BJH) Get important data from original data (colour, x, y)
  # Get colour, x, y, PANEL columns -> make new data.frame
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
  
  # (BJH) seperate group using colour value (setosa = 1, ...)
  # add group columns
  print("lapply")
  data <- lapply(data, add_group)
  
  # Transform all scales
  print("lapply")
  data <- lapply(data, scales_transform_df, scales = scales)
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  
  panel <- train_position(panel, data, scale_x(), scale_y())
  data <- map_position(panel, data, scale_x(), scale_y())
  
  # Apply and map statistics
  data <- calculate_stats(panel, data, layers)
  data <- dlapply(function(d, p) p$map_statistic(d, plot))
  
  print("lapply")
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
    print("lapply_if1")
    lapply(data, scales_train_df, scales = npscales)
    print("lapply_if2")
    data <- lapply(data, scales_map_df, scales = npscales)
  }

  # Train coordinate system
  panel <- train_ranges(panel, plot$coordinates)
  
  list(data = data, panel = panel, plot = plot)
}

ggplot.SparkR_build <- function(plot) {
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  
  plot <- plot_clone(plot)
  
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)

  panel <- new_panel()
  panel <- train_layout(panel, plot$facet, layer_data, plot$data)

  scales <- plot$scales
  
  df <- plot$data
  mapping <- as.character(unlist(plot$mapping))
  new_df <- SparkR:::select(df, as.list(mapping))
  
  before_col <- columns(new_df)
  after_col <- names(plot$mapping)
  
  for(i in 1:length(after_col))
    new_df <- SparkR:::withColumnRenamed(new_df, before_col[i], after_col[i])
  
  # (BJH) Temporary input PANEL, group columns
  new_df <- SparkR:::withColumn(new_df, "PANEL", new_df$x*0+1L)
  new_df <- SparkR:::withColumn(new_df, "group", new_df$PANEL)

  # (BJH:TODO) change color values -> color code (columns)
  # (BJH:TODO) how to calculate "group" values? -> if-else or other function API in SparkR?
  # (BJH:TODO) make new DataFrame : 1. x.range, y,range -> min, max funtcion in x, y columns
  #                                 2. x.labels, x.major, x.major_source -> assume  # of tickle are 3
  #                                 3. x.minor, x.minor_source -> assume  # of tickle are 4
  #                                 4. y_labels, y.major, y.major_source -> assume  # of tickle are 4
  #                                 5. y.minor, y.minor_source -> assume  # of tickle are 8
  # (BJH:TODO) how about result$panel$x_scales, y_scales? what does it mean?
}