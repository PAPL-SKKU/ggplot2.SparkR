
#
# A panel figures out how data is positioned within a panel of a plot,
# coordinates information from scales, facets and coords.  Eventually all
# state will move out of facets and coords, and live only in panels and
# stats, simplifying these data structures to become strategies.
#
# Information about a panel is built up progressively over time, which
# is why the initial object is empty to start with.
new_panel <- function() {
  structure(list(), class = "panel")
}

# Learn the layout of panels within a plot.
#
# This is determined by the facet, which returns a data frame, than
# when joined to the data to be plotted tells us which panel it should
# appear in, where that panel appears in the grid, and what scales it
# uses.
#
# As well as the layout info, this function also adds empty lists in which
# to house the x and y scales.
#
# @param the panel object to train
# @param the facetting specification
# @param data a list of data frames (one for each layer), and one for the plot
# @param plot_data the default data frame
# @return an updated panel object

train_layout <- function(panel, facet, data, plot_data) {
  layout <- facet_train_layout(facet, c(list(plot_data), data))
  panel$layout <- layout
  panel$shrink <- facet$shrink
  
  panel
}

# Map data to find out where it belongs in the plot.
#
# Layout map ensures that all layer data has extra copies of data for margins
# and missing facetting variables, and has a PANEL variable that tells that
# so it know what panel it belongs to. This is a change from the previous
# design which added facetting variables directly to the data frame and
# caused problems when they had names of aesthetics (like colour or group).
#
# @param panel a trained panel object
# @param the facetting specification
# @param data list of data frames (one for each layer)
# @param plot_data default plot data frame
map_layout <- function(panel, facet, data, plot_data) {
  lapply(data, function(data) {
    if (is.waive(data)) data <- plot_data
    facet_map_layout(facet, data, panel$layout)
  })
}

# Train position scales with data
#
# If panel-specific scales are not already present, will clone from
# the scales provided in the parameter
#
# @param panel the panel object to train
# @param data a list of data frames (one for each layer)
# @param x_scale x scale for the plot
# @param y_scale y scale for the plot
train_position <- function(panel, data, x_scale, y_scale) {
  # Initialise scales if needed, and possible.
  layout <- panel$layout
  
  if (is.null(panel$x_scales) && !is.null(x_scale)) {
    panel$x_scales <- rlply(max(layout$SCALE_X), scale_clone(x_scale))
  }
  if (is.null(panel$y_scales) && !is.null(y_scale)) {
    panel$y_scales <- rlply(max(layout$SCALE_Y), scale_clone(y_scale))
  }
  
  # loop over each layer, training x and y scales in turn
  for(layer_data in data) {
    match_id <- match(layer_data$PANEL, layout$PANEL)

    if (!is.null(x_scale)) {
      x_vars <- intersect(x_scale$aesthetics, names(layer_data))
      SCALE_X <- layout$SCALE_X[match_id]

      scale_apply(layer_data, x_vars, scale_train, SCALE_X, panel$x_scales)
    }

    if (!is.null(y_scale)) {
      y_vars <- intersect(y_scale$aesthetics, names(layer_data))
      SCALE_Y <- layout$SCALE_Y[match_id]

      scale_apply(layer_data, y_vars, scale_train, SCALE_Y, panel$y_scales)
    }
  }
  
  panel
}

train_position.SparkR <- function(panel, data, x_scale, y_scale) {
  layer_data <- data[[1]]

  # Initialise scales if needed, and possible.
  if (is.null(panel$x_scales) && !is.null(x_scale)) {
    panel$x_scales <- rlply(1, scale_clone(x_scale))
  }

  if (is.null(panel$y_scales) && !is.null(y_scale)) {
    panel$y_scales <- rlply(1, scale_clone(y_scale))
  }

  # Add x, y range in panel$x_scales[[1]]$range & panel$y_scales[[1]]$range
  # continuous: nothing
  # discrete: unique value of column

  if(!is.null(x_scale) && length(grep("x", columns(layer_data))) != 0 &&
      is.null(panel$x_scales[[1]]$range$range)) {
    if(panel$x_scales[[1]]$scale_name == "position_d") {
      panel$x_scales[[1]]$range$range <- distinct(select(layer_data, layer_data$x))
    }
  }

  if(!is.null(y_scale) && length(grep("y", columns(layer_data))) != 0 &&
      is.null(panel$y_scales[[1]]$range$range)) {
    if(panel$y_scales[[1]]$scale_name == "position_d") {
      panel$y_scales[[1]]$range$range <- distinct(select(layer_data, layer_data$y))
    }
  }
  
  panel
}

reset_scales <- function(panel) {
  if (!panel$shrink) return()
  l_ply(panel$x_scales, scale_reset)
  l_ply(panel$y_scales, scale_reset)
}

# Map data with scales.
#
# This operation must be idempotent because it is applied twice: both before
# and after statistical transformation.
#
# @param data a list of data frames (one for each layer)
map_position <- function(panel, data, x_scale, y_scale) {
  layout <- panel$layout

  lapply(data, function(layer_data) {
    match_id <- match(layer_data$PANEL, layout$PANEL)

    # Loop through each variable, mapping across each scale, then joining
    # back together
    x_vars <- intersect(x_scale$aesthetics, names(layer_data))
    names(x_vars) <- x_vars
    SCALE_X <- layout$SCALE_X[match_id]
    new_x <- scale_apply(layer_data, x_vars, scale_map, SCALE_X,
       panel$x_scales)
    layer_data[, x_vars] <- new_x

    y_vars <- intersect(y_scale$aesthetics, names(layer_data))
    names(y_vars) <- y_vars
    SCALE_Y <- layout$SCALE_Y[match_id]
    new_y <- scale_apply(layer_data, y_vars, scale_map, SCALE_Y,
       panel$y_scales)

    layer_data[, y_vars] <- new_y
    layer_data
  })
}

# Map data using Spark DataFrame.
#
# This operation must be idempotent because it is applied twice: both before
# and after statistical transformation.
#
# @param data a list of Spark DataFrames (one for each layer)
map_position.SparkR <- function(data) {
  # Loop through x and y variable, mapping, then joining
  # back together
  lapply(data, function(layer_data) {
    data_and_types <- dtypes(layer_data)
    column_list <- columns(layer_data)

    for(pair in data_and_types) {
      if(pair[1] == "x" && pair[2] == "string") {
        # Mapping an unique number for each unique x value
        layer_data <- withColumnRenamed(layer_data, "x", "x_old")
        
	disc_x <- bindIDs(SparkR::arrange(distinct(select(layer_data, "x_old")), "x_old"))
        disc_x <- withColumn(disc_x, "x", cast(disc_x$"_2", "int"))
        
	layer_data <- SparkR::join(layer_data, disc_x, layer_data$x_old == disc_x$"_1", "inner")
        layer_data <- select(layer_data, as.list(column_list))
      } else if(pair[1] == "x" && pair[2] == "int") {
        # Change integer-typed x value to double-typed x value
        layer_data <- SparkR::rename(layer_data, x_map = layer_data$x)
        layer_data <- SparkR::withColumn(layer_data, "x", cast(layer_data$x_map, "double"))
        layer_data <- select(layer_data, as.list(column_list))
      }

      if(pair[1] == "y" && pair[2] == "string") {
        # Mapping an unique number for each unique y value
        layer_data <- withColumnRenamed(layer_data, "y", "y_old")
        
	disc_y <- bindIDs(SparkR::arrange(distinct(select(layer_data, "y_old")), "y_old"))
        disc_y <- withColumn(disc_y, "y", cast(disc_y$"_2", "int"))
        
	layer_data <- SparkR::join(layer_data, disc_y, layer_data$y_old == disc_y$"_1", "inner")
        layer_data <- select(layer_data, as.list(column_list))
      } else if(pair[1] == "y" && pair[2] == "int") {
        # Change integer-typed y value to double-typed y value
        layer_data <- SparkR::rename(layer_data, y_map = layer_data$y)
        layer_data <- SparkR::withColumn(layer_data, "y", cast(layer_data$y_map, "double"))
        layer_data <- select(layer_data, as.list(column_list))
      }
    }

    layer_data
  })
}

# Function for applying scale function to multiple variables in a given
# data set.  Implement in such a way to minimise copying and hence maximise
# speed
scale_apply <- function(data, vars, f, scale_id, scales) {
  if (length(vars) == 0) return()
  if (nrow(data) == 0) return()

  n <- length(scales)
  if (any(is.na(scale_id))) stop()
  
  scale_index <- split_indices(scale_id, n)
  lapply(vars, function(var) {
    pieces <- lapply(seq_along(scales), function(i) {
      f(scales[[i]], data[[var]][scale_index[[i]]])
    })
    
    # Join pieces back together, if necessary
    if (!is.null(pieces)) {
      unlist(pieces)[order(unlist(scale_index))]
    }
  })
}

panel_scales <- function(panel, i) {
  this_panel <- panel$layout[panel$layout$PANEL == i, ]

  list(
    x = panel$x_scales[[this_panel$SCALE_X]],
    y = panel$y_scales[[this_panel$SCALE_Y]]
  )
}

# Compute ranges and dimensions of each panel, using the coord.
train_ranges <- function(panel, coord) {
  compute_range <- function(ix, iy) {
    # TODO: change coord_train method to take individual x and y scales
    coord_train(coord, list(x = panel$x_scales[[ix]], y = panel$y_scales[[iy]]))
  }

  panel$ranges <- Map(compute_range,
    panel$layout$SCALE_X, panel$layout$SCALE_Y)
  panel
}

train_ranges.SparkR <- function(panel, data, plot) {
  panel$layout <- collect(panel$layout)
  x_scale_name <- panel$x_scales[[1]]$scale_name
  y_scale_name <- panel$y_scales[[1]]$scale_name
  
  if(x_scale_name == "position_d" && y_scale_name == "position_d") {

    panel$x_scales[[1]]$range$range <- collect(SparkR::arrange(panel$x_scales[[1]]$range$range, "x"))[[1]]
    panel$y_scales[[1]]$range$range <- collect(SparkR::arrange(panel$y_scales[[1]]$range$range, "y"))[[1]]

  } else if(x_scale_name == "position_d" && y_scale_name == "position_c") {

    panel$x_scales[[1]]$range$range <- collect(SparkR::arrange(panel$x_scales[[1]]$range$range, "x"))[[1]]

    if(!is.null(data[[1]]$ymin)) y_min <- min(data[[1]]$ymin)
    else y_min <- min(data[[1]]$y)
    
    if(!is.null(data[[1]]$ymax)) y_max <- max(data[[1]]$ymax)
    else y_max <- max(data[[1]]$y)
    
    panel$y_scales[[1]]$range$range <- c(y_min, y_max)

  } else if(x_scale_name == "position_c" && y_scale_name == "position_d") {
    
    if(!is.null(data[[1]]$xmin)) x_min <- min(data[[1]]$xmin)
    else x_min <- min(data[[1]]$x)
    
    if(!is.null(data[[1]]$xmax)) x_max <- max(data[[1]]$xmax)
    else x_max <- max(data[[1]]$x)

    panel$x_scales[[1]]$range$range <- c(x_min, x_max)
    panel$y_scales[[1]]$range$range <- collect(SparkR::arrange(panel$y_scales[[1]]$range$range, "y"))[[1]]

  } else if(x_scale_name == "position_c" && y_scale_name == "position_c") {
    if(!is.null(data[[1]]$xmin)) x_min <- min(data[[1]]$xmin)
    else x_min <- min(data[[1]]$x)
    
    if(!is.null(data[[1]]$xmax)) x_max <- max(data[[1]]$xmax)
    else x_max <- max(data[[1]]$x)

    if(!is.null(data[[1]]$ymin)) y_min <- min(data[[1]]$ymin)
    else y_min <- min(data[[1]]$y)
    
    if(!is.null(data[[1]]$ymax)) y_max <- max(data[[1]]$ymax)
    else y_max <- max(data[[1]]$y)
 
    panel$x_scales[[1]]$range$range <- c(x_min, x_max)
    panel$y_scales[[1]]$range$range <- c(y_min, y_max)
  }

  panel <- train_ranges(panel, plot$coordinates)
  panel
}

# Calculate statistics
#
# @param layers list of layers
# @param data a list of data frames (one for each layer)
calculate_stats <- function(panel, data, layers) {
  lapply(seq_along(data), function(i) {
    d <- data[[i]]
    l <- layers[[i]]

    ddply(d, "PANEL", function(panel_data) {
      scales <- panel_scales(panel, panel_data$PANEL[1])
      l$calc_statistic(panel_data, scales)
    })
  })
}

calculate_stats.SparkR <- function(panel, data, layers) {
  lapply(seq_along(data), function(i) {
    d <- data[[i]]
    l <- layers[[i]]

    l$calc_statistic(d, NULL)
  })
}

calculate_stats.SparkR_test <- function(data, layers) {
  stat_type <- layers[[1]]$stat$objname
  sqlContext <- get("sqlContext", envir = .GlobalEnv)

  switch(stat_type, 
    boxplot = {
      qs <- c(0, 0.25, 0.5, 0.75, 1)
      coef_null <- layers[[1]]$stat_params$coef
      width_null <- layers[[1]]$stat_params$width

      coef <- if(is.null(coef_null)) 1.5 else coef_null
      width <- if(is.null(width_null)) 0.75 else width_null

      column_fill <- length(grep("fill", columns(data)))

      if(column_fill) {
        distinct_data <- collect(distinct(select(data, "x", "PANEL", "fill")))
      } else {
        distinct_data <- collect(distinct(select(data, "x", "PANEL")))
      }
     
      for(i in 1:nrow(distinct_data)) {
        if(column_fill) {
          y <- SparkR::filter(data, data$x == distinct_data$x[i] &
                                    data$PANEL == distinct_data$PANEL[i] &
                                    data$fill == distinct_data$fill[i])
        } else {
          y <- SparkR::filter(data, data$x == distinct_data$x[i] & 
                                    data$PANEL == distinct_data$PANEL[i])
        }

        y <- collect(SparkR::arrange(select(y, "y"), "y"))$y

        stats <- as.numeric(quantile(y, qs))
        names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
        iqr <- diff(stats[c(2, 4)])
        outliers <- y < (stats[2] - coef * iqr) | y > (stats[4] + coef * iqr)

        if(any(outliers)) {
          stats[c(1, 5)] <- range(c(stats[2:4], y[!outliers]), na.rm = TRUE)
        }

        df <- as.data.frame(as.list(stats))
        
        df$outliers <- I(list(y[outliers]))
        n <- sum(!is.na(y))

        df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
        df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
        df$relvarwidth <- sqrt(n)

        distinct <- cbind(distinct_data[i, ], df)

        if(i > 1) test <- rbind(test, distinct)
        else test <- distinct
      }

      outliers <- test[c("x", "outliers")]
      test$outliers <- NULL

      data <- createDataFrame(sqlContext, test)
      persist(data, "MEMORY_ONLY")

      data <- SparkR::mutate(data, width = lit(width), weight = lit(1))

      return(list(outliers, data))
    }
  )

  data
}

xlabel <- function(panel, labels) {
  panel$x_scales[[1]]$name %||% labels$x
}

ylabel <- function(panel, labels) {
  panel$y_scales[[1]]$name %||% labels$y
}
