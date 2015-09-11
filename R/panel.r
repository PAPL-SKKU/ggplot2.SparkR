# Panel object.
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
  # Initialise scales if needed, and possible.
  if (is.null(panel$x_scales) && !is.null(x_scale)) {
    panel$x_scales <- rlply(1, scale_clone(x_scale))
  }

  if (is.null(panel$y_scales) && !is.null(y_scale)) {
    panel$y_scales <- rlply(1, scale_clone(y_scale))
  }

  # Add x, y range in panel$x_scales[[1]]$range & panel$y_scales[[1]]$range
  # continuous : max, min value
  # discrete : unique value of column

  if (!is.null(x_scale) && length(grep("x", columns(data))) != 0 && is.null(panel$x_scales[[1]]$range$range)) {
    if(panel$x_scales[[1]]$scale_name == "position_c") {
      panel$x_scales[[1]]$range$range <- select(data, min(data$x), max(data$x))
    } else if(panel$x_scales[[1]]$scale_name == "position_d") {
      panel$x_scales[[1]]$range$range <- distinct(select(data, data$x))
    }
  }

  if (!is.null(y_scale) && length(grep("y", columns(data))) != 0 && is.null(panel$y_scales[[1]]$range$range)) {
    if(panel$y_scales[[1]]$scale_name == "position_c") {
      panel$y_scales[[1]]$range$range <- select(data, min(data$y), max(data$y))
    } else if(panel$y_scales[[1]]$scale_name == "position_d") {
      panel$y_scales[[1]]$range$range <- distinct(select(data, data$y))
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

map_position.SparkR <- function(data) {
  data_and_types <- dtypes(data)

  for(pair in data_and_types) {
    if(pair[1] == "x" && pair[2] == "string") {
      distinct <- distinct(select(data, "x"))
      distinct <- SparkR::rename(distinct, x_new = distinct$x)
      # overhead
      distinct_value <- SparkR::count(distinct)
      
      for(index in 1:distinct_value) {
        new_df <- limit(distinct, index)
        
        if(index == 1) old_df <- new_df
        else {
	  temp_df <- new_df
          new_df <- except(new_df, old_df)
          old_df <- temp_df
        }
 
        temp_df <- withColumn(new_df, "x", cast(isNull(new_df[[1]]), "integer") + index)

        if(index > 1) unioned <- unionAll(unioned, temp_df)
        else          unioned <- temp_df
      }
      data <- SparkR::rename(data, x_old = data$x)
      data <- SparkR::join(data, unioned, data$x_old == unioned$x_new, "inner")
    } else if(pair[1] == "y" && pair[2] == "string") {
      distinct <- distinct(select(data, "y"))
      distinct <- SparkR::rename(distinct, y_new = distinct$y)
      # overhead
      distinct_value <- SparkR::count(distinct)

      for(index in 1:distinct_value) {
        new_df <- limit(distinct, index)

        if(index == 1) old_df <- new_df
        else {
	  temp_df <- new_df
          new_df <- except(new_df, old_df)
          old_df <- temp_df
        }

        temp_df <- withColumn(new_df, "y", cast(isNull(new_df[[1]]), "integer") + index)
        if(index > 1) unioned <- unionAll(unioned, temp_df)
        else          unioned <- temp_df
      }
      
      data <- SparkR::rename(data, y_old = data$y)
      data <- SparkR::join(data, unioned, data$y_old == unioned$y_new, "inner")
    }
  }
  
  data
}

map_position.SparkR_test <- function(data) {
  data_and_types <- dtypes(data)

  for(pair in data_and_types) {
    if(pair[1] == "x" && pair[2] == "string") {
      distinct <- distinct(select(data, "x"))
      distinct <- SparkR::rename(distinct, x_new = distinct$x)
      # overhead
      distinct_value <- collect(distinct)[[1]]

      for(index in 1:length(distinct_value)) {
        temp_df <- SparkR::filter(distinct, distinct[[1]] == distinct_value[index])
        temp_df <- withColumn(temp_df, "x", cast(isNull(distinct[[1]]), "integer") + index)

        if(index > 1) unioned <- unionAll(unioned, temp_df)
        else          unioned <- temp_df
      }

      data <- SparkR::rename(data, x_old = data$x)
      data <- SparkR::join(data, unioned, data$x_old == unioned$x_new, "inner")
    } else if(pair[1] == "y" && pair[2] == "string") {
      distinct <- distinct(select(data, "y"))
      distinct <- SparkR::rename(distinct, y_new = distinct$y)
      # overhead
      distinct_value <- collect(distinct)[[1]]

      for(index in 1:length(distinct_value)) {
        temp_df <- SparkR::filter(distinct, distinct[[1]] == distinct_value[index])
        temp_df <- withColumn(temp_df, "y", cast(isNull(distinct[[1]]), "integer") + index)

        if(index > 1) unioned <- unionAll(unioned, temp_df)
        else          unioned <- temp_df
      }

      data <- SparkR::rename(data, y_old = data$y)
      data <- SparkR::join(data, unioned, data$y_old == unioned$y_new, "inner")
    }
  }
 
  data
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

calculate_stats.SparkR <- function(data, layers) {
  stat_type <- layers[[1]]$stat$objname
  switch(stat_type, 
    bin = {
      width <- if(is.null(layers[[1]]$stat_params$width)) 0.9 else layers[[1]]$stat_params$width
      if(length(grep("fill", columns(data))))
        data <- SparkR::count(groupBy(data, "x", "PANEL", "group", "fill"))
      else
        data <- SparkR::count(groupBy(data, "x", "PANEL", "group"))
     
      data <- SparkR::mutate(data, density = data$count / abs(data$count) / width,
                                   ncount = data$count / abs(data$count),
                                   width = data$count * 0 + width)
      
      max_density <- select(data, max(abs(data$density)))
      max_density <- SparkR::rename(max_density, max_density = max_density[[1]])
      temp_df <- SparkR::join(data, max_density)
      data <- withColumn(temp_df, "ndensity", temp_df$density / temp_df$max_density)
    },
    bin2d = {
      bins_null <- layers[[1]]$stat_params$bins
      bins <- if(is.null(bins_null)) 30 else bins_null

      binwidth <- collect(select(data, (max(data$x) - min(data$x)) / bins, (max(data$y) - min(data$y)) / bins))
      range <- collect(select(data, min(data$x), max(data$x), min(data$y), max(data$y)))
      origin <- unlist(range[c(-2, -4)])
      breaks <- list(
        x = seq(origin[1], range[[2]] + binwidth[[1]], binwidth[[1]]),
        y = seq(origin[2], range[[4]] + binwidth[[2]], binwidth[[2]])
      )
      
      for(xy in 1:length(breaks)) {
        col <- names(breaks)[xy] 
        for(index in 1:(length(breaks[[xy]])-1)) {
          lower <- breaks[[xy]][index]
          upper <- breaks[[xy]][index + 1]
         
          if(index > 1) filter_df <- filter(data, data[[eval(col)]] > lower & data[[eval(col)]] <= upper)
          else          filter_df <- filter(data, data[[eval(col)]] >= lower & data[[eval(col)]] <= upper)

          filter_col <- cast(isNull(filter_df[[eval(col)]]), "integer")
          temp_df <- SparkR::mutate(filter_df, xy_min = filter_col + lower, xy_max = filter_col + upper )

          if(index > 1) unioned <- unionAll(unioned, temp_df)
          else          unioned <- temp_df
        }
        
        if(xy == 1) data <- SparkR::rename(unioned, xmin = unioned$xy_min, xmax = unioned$xy_max)
        else        data <- SparkR::rename(unioned, ymin = unioned$xy_min, ymax = unioned$xy_max)
      }

      if(length(grep("fill", columns(data))))
        data <- SparkR::count(groupBy(data, "PANEL", "group", "fill", "xmin", "xmax", "ymin", "ymax"))
      else
        data <- SparkR::count(groupBy(data, "PANEL", "group", "xmin", "xmax", "ymin", "ymax"))

      sum_count <- select(data, sum(data$count))
      sum_count <- SparkR::rename(sum_count, sum_count = sum_count[[1]])
      temp_df <- SparkR::join(data, sum_count)
      data <- withColumn(temp_df, "density", temp_df$count / temp_df$sum_count)
    },
    boxplot = {
      qs <- c(0, 0.25, 0.5, 0.75, 1)
      coef_null <- layers[[1]]$stat_params$coef
      width_null <- layers[[1]]$stat_params$width
      weight_null <- layers[[1]]$geom_params$weight

      coef <- if(is.null(coef_null)) 1.5 else coef_null
      width <- if(is.null(width_null)) 0.75 else width_null
      weight <- if(is.null(weight_null)) 1 else weight_null

      if(length(grep("fill", columns(data))))
        stats <- agg(groupBy(data, "x", "fill", "PANEL", "group"), ymin = min(data$y),
                     lower = min(data$y)+(max(data$y)-min(data$y))*qs[2],
                     middle = min(data$y)+(max(data$y)-min(data$y))*qs[3],
                     upper = min(data$y)+(max(data$y)-min(data$y))*qs[4],
                     ymax = max(data$y), iqr = (max(data$y)-min(data$y))*(qs[4]-qs[2]),
                     relvarwidth = sqrt(sum(cast(isNotNull(data$y), "integer"))))
      else
        stats <- agg(groupBy(data, "x", "PANEL", "group"), ymin = min(data$y),
                     lower = min(data$y)+(max(data$y)-min(data$y))*qs[2],
                     middle = min(data$y)+(max(data$y)-min(data$y))*qs[3],
                     upper = min(data$y)+(max(data$y)-min(data$y))*qs[4],
                     ymax = max(data$y), iqr = (max(data$y)-min(data$y))*(qs[4]-qs[2]),
                     relvarwidth = sqrt(sum(cast(isNotNull(data$y), "integer"))))

      # How to calculate "outliers" column?
      #outliers <- withColumn(stats, "outliers", stats$y < (stats$lower - coef * stats$iqr) | stats$y > (stats$upper + coef * stats$iqr))

      stats <- SparkR::mutate(stats, width = stats$ymin * 0 + width,
                                     weight = stats$ymin * 0 + weight,
                                     notchupper = stats$middle + ((stats$iqr / stats$relvarwidth) * 1.58), 
                                     notchlower = stats$middle - ((stats$iqr / stats$relvarwidth) * 1.58))
      data <- stats
    },
    sum = {
      if(length(grep("fill", columns(data))))
        data <- SparkR::count(groupBy(data, "PANEL", "x", "y", "fill", "group"))
      else
        data <- SparkR::count(groupBy(data, "PANEL", "x", "y", "group"))

      count_group <- SparkR::count(groupBy(data, "group"))

      data <- SparkR::rename(data, n = data$count, group_old = data$group)
      data <- SparkR::join(data, count_group, data$group_old == count_group$group, "inner")
      data <- withColumn(data, "prop", data$count^(-1))
      data <- select(data, "PANEL", "x", "y", "group", "n", "prop")
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
