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
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)

  plot <- plot_clone(plot)
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)
  
  scales <- plot$scales
  
  # Apply function to layer and matching data
  dlapply <- function(f) {
    out <- vector("list", length(data))
    for(i in seq_along(data)) {
      out[[i]] <- f(d = data[[i]], p = layers[[i]])
    }
    out
  }

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  panel <- new_panel()
  panel <- train_layout(panel, plot$facet, layer_data, plot$data)
  data <- map_layout(panel, plot$facet, layer_data, plot$data)
  
  # Compute aesthetics to produce data with generalised variable names
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
  data <- lapply(data, add_group)
  
  # Transform all scales
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

ggplot_build.SparkR <- function(plot) {
  total_time <- 0
  if(length(plot$layers)==0) stop("No layers in plot", call.=FALSE)

  plot <- plot_clone(plot)

  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)
  
  scales <- plot$scales
  
  panel <- new_panel()
print("stage 1")
  time <- system.time(panel <- train_layout(panel, plot$facet, layer_data, plot$data))
  print(time)
  total_time <- total_time + time[3]
print("stage 2")
  time <- system.time(data <- facet_map_layout(plot$facet, plot$data, panel$layout))
  print(time)
  total_time <- total_time + time[3]

print("stage 3")
  time <- system.time(data <- compute_aesthetics.SparkR(data, plot))
  print(time)
  total_time <- total_time + time[3]
  
print("stage 4")
  time <- system.time(data <- add_group.SparkR(data))
  print(time)
  total_time <- total_time + time[3]
print("stage 5")
  time <- system.time(data <- scales_transform_df.SparkR(scales, data))
  print(time)
  total_time <- total_time + time[3]

  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

print("stage 6")
  time <- system.time(panel <- train_position.SparkR(panel, data, scale_x(), scale_y()))
  print(time)
  total_time <- total_time + time[3]
print("stage 7")
  time <- system.time(data <- map_position.SparkR(data))
  print(time)
  total_time <- total_time + time[3]

print("stage 8")
  time <- system.time(data <- calculate_stats.SparkR(data, layers))
  print(time)
  total_time <- total_time + time[3]
print("stage 9")
  time <- system.time(data <- map_statistic.SparkR(data, plot))
  print(time)
  total_time <- total_time + time[3]
  
  scales_add_missing(plot, c("x", "y"), plot$plot_env)
  
print("stage 10")
  time <- system.time(data <- reparameterise.SparkR(data, plot))
  print(time)
  total_time <- total_time + time[3]
print("stage 11")
  time <- system.time(data <- adjust_position.SparkR(data, layers))
  print(time)
  total_time <- total_time + time[3]
 
print("stage 12") 
  time <- system.time(panel <- train_position.SparkR(panel, data, scale_x(), scale_y()))
  print(time)
  total_time <- total_time + time[3]
print("stage 13")
  time <- system.time(data <- map_position.SparkR(data))
  print(time)
  total_time <- total_time + time[3]

print("stage 14")
  time <- system.time(data <- scales_transform_df.SparkR(scales, data))
  print(time)
  before_time <- total_time + time[3]
  print("before collect")
  print(total_time)
print("stage 15")
  time <- system.time(data <- collect(data))
  print(time)
  total_time <- before_time + time[3]
print("stage 16")
  time <- system.time(panel <- train_ranges.SparkR(panel, plot))
  print(time)
  total_time <- total_time + time[3]

  print("real total")
  print(total_time)
  list(data = list(data), panel = panel, plot = plot)
}
