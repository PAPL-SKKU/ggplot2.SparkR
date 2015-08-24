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
#  print("--------------------------------------")
#  print(data)
#  print("****************")
#  panel$layout <- NULL
#  panel$shrink <- NULL
#  panel$x_scales <- NULL
#  panel$y_scales <- NULL
#  print(panel)
  list(data = data, panel = panel, plot = plot)
}

ggplot.SparkR_build <- function(plot) {
  if(length(plot$layers)==0) stop("No layers in plot", call.=FALSE)
  
  plot <- plot_clone(plot)

  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)
  
  scales <- plot$scales
  
  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  # Currently, just facet_grid is possible
  panel <- new_panel()
  panel <- train_layout(panel, plot$facet, layer_data, plot$data)
  data <- facet_map_layout(plot$facet, plot$data, panel$layout)
  
  # Compute aesthetics to produce data with generalised variable names
  data <- compute_aesthetics(data, plot)
  data <- add.SparkR_group(data)
  stop("ggplot.SparkR_build")
  # Transform all scales
  data <- scales.SparkR_transform_df(scales, data)
  
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  
  # map_position() need to make
  panel <- train.SparkR_position(panel, data, scale_x(), scale_y())
  #data <- map.SparkR_position()
 
  # Apply and map statictics
  data <- calculate.SparkR_stats(panel, data, layers)
  data <- map_statistic(data, plot)
 
  data <- reparameterise(data, plot)

  # adjust_position
  # fill option -> change parameter (xmin, xmax, ymin, ymax, x, y ...)  

  showDF(data)
  stop("test")
  data
}

compute_aesthetics <- function(df, plot) {
  
  values <- as.character(unlist(plot$mapping))
  keys <- names(plot$mapping)

  cmd_str <- 'select(df, "PANEL"'
  for(name in values) 
    cmd_str <- paste(cmd_str, ', "', name, '"', sep="")
  
  cmd_str <- paste(cmd_str, ")", sep="")
  
  data <- eval(parse(text = cmd_str))

  for(index in 1:length(keys)) {
    if(keys[index] == "group") keys[index] <- "grouped"
    data <- withColumnRenamed(data, eval(values[index]), eval(keys[index]))
  }
  
  scales_add_defaults(plot$scales, data, plot$mapping, plot$plot_env)
  data
}

map_statistic <- function(data, plot) {
  # Assemble aesthetics from ayer, plot and stat mappings
  layers <- plot$layers[[1]]
  aesthetics <- layers$mapping

  if(layers$inherit.aes)  aesthetics <- defaults(aesthetics, plot$mapping)
  
  aesthetics <- defaults(aesthetics, layers$stat$default_aes())
  aesthetics <- compact(aesthetics)

  new <- strip_dots(aesthetics[is_calculated_aes(aesthetics)])
  
  if(length(new) == 0)  return(data)
  
  # Add map stat output to aesthetics
  data <- withColumn(data, names(new), data[[as.character(new)]])
  
  data
}

reparameterise <- function(data, plot) {
  objname <- plot$layers[[1]]$geom$objname
  column_data_types <- unlist(dtypes(data))
  x_data_types <- column_data_types[grep("x", column_data_types) + 1]
  
  if(x_data_types == "string") {
    distinct <- distinct(select(data, "x"))
    distinct <- SparkR::rename(distinct, x_new = distinct$x)
    distinct_value <- collect(distinct)[[1]]

    for(index in 1:length(distinct_value)) {
      temp_df <- SparkR::filter(distinct, distinct[[1]] == distinct_value[index])
      temp_df <- withColumn(temp_df, "x", cast(isNull(distinct[[1]]), "integer") + index)

      if(index > 1) unioned <- unionAll(unioned, temp_df)
      else          unioned <- temp_df
    }
 
    data <- SparkR::rename(data, x_old = data$x) 
    data <- SparkR::join(data, unioned, data$x_old == unioned$x_new, "inner")
  }

  switch(objname, 
    bar = {
      data <- SparkR::mutate(data, ymin = data$y * 0, ymax = data$y,
                                   xmin = data$x - (data$width / 2), xmax = data$x + (data$width / 2))

      if(length(grep("fill", columns(data))) == 0)
        data <- select(data, "y", "count", "x", "ndensity", "ncount", "density",
                             "PANEL", "group", "ymin", "ymax", "xmin", "xmax")
      else
        data <- select(data, "y", "count", "x", "ndensity", "ncount", "density", "fill",
                             "PANEL", "group", "ymin", "ymax", "xmin", "xmax")
    },
    boxplot = {
      params <- plot$layers[[1]]$geom_params
 
      if(is.null(params) || is.null(params$varwidth) || 
         !params$varwidth || length(grep("relvarwidth", columns(data))) == 0) {
        data <- SparkR::mutate(data, xmin = data$x - data$width / 2, xmax = data$x + data$width / 2)
      } else {
        max_relvarwidth <- collect(select(data, max(data$relvarwidth)))[[1]]
        data <- SparkR::mutate(data, xmin = data$x - (data$ralvarwidth * data$width) / (2 * max_relvarwidth),
                                     xmax = data$x + (data$relvarwidth * data$width) / (2 * max_relvarwidth))
      }

      if(length(grep("fill", columns(data))) == 0)
        data <- select(data, "ymin", "lower", "middle", "upper", "ymax",
                             "notchupper", "notchlower", "x",
                             "PANEL", "group", "weight", "xmin", "xmax")
      else
        data <- select(data, "ymin", "lower", "middle", "upper", "ymax",
                             "notchupper", "notchlower", "x", "fill",
                             "PANEL", "group", "weight", "xmin", "xmax")
    }
  )

  data
}
