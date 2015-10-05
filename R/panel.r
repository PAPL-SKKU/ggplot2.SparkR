
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
    if(panel$x_scales[[1]]$scale_name == "position_d") {
      panel$x_scales[[1]]$range$range <- distinct(select(data, data$x))
    }
  }

  if (!is.null(y_scale) && length(grep("y", columns(data))) != 0 && is.null(panel$y_scales[[1]]$range$range)) {
    if(panel$y_scales[[1]]$scale_name == "position_d") {
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
      distinct <- SparkR::arrange(distinct(select(data, "x")), "x")
      distinct <- SparkR::rename(distinct, x_new = distinct$x)
      
      distinct_id <- bindIDs(distinct)
      distinct_id <- SparkR::rename(distinct_id, x_new = distinct_id$"_1")
      distinct_id <- select(withColumn(distinct_id, "x", cast(distinct_id$"_2", "integer")), "x_new", "x")
      
      col_list <- as.list(columns(data))
      data <- SparkR::rename(data, x_old = data$x)
      data <- SparkR::join(data, distinct_id, data$x_old == distinct_id$x_new, "inner")
      data <- select(data, col_list)  
    } else if(pair[1] == "x" && pair[2] == "int") {
      data <- SparkR::rename(data, x_map = data$x)
      data <- SparkR::withColumn(data, "x", cast(data$x_map, "double"))
    }

    if(pair[1] == "y" && pair[2] == "string") {
      distinct <- SparkR::arrange(distinct(select(data, "y")), "y")
      distinct <- SparkR::rename(distinct, y_new = distinct$y)

      distinct_id <- bindIDs(distinct)
      distinct_id <- SparkR::rename(distinct_id, y_new = distinct_id$"_1")
      distinct_id <- select(withColumn(distinct_id, "y", cast(distinct_id$"_2", "integer")), "y_new", "y")
      
      col_list <- as.list(columns(data))
      data <- SparkR::rename(data, y_old = data$y)
      data <- SparkR::join(data, distinct_id, data$y_old == distinct_id$y_new, "inner")
      data <- select(data, col_list)    
    } else if(pair[1] == "y" && pair[2] == "int") {
      data <- SparkR::rename(data, y_map = data$y)
      data <- SparkR::withColumn(data, "y", cast(data$y_map, "double"))
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

train_ranges.SparkR <- function(panel, data, plot) {
  panel$layout <- collect(panel$layout)
  x_scale_name <- panel$x_scales[[1]]$scale_name
  y_scale_name <- panel$y_scales[[1]]$scale_name

  if(x_scale_name == "position_d" && y_scale_name == "position_d") {
    panel$x_scales[[1]]$range$range <- collect(panel$x_scales[[1]]$range$range)[[1]]
    panel$y_scales[[1]]$range$range <- collect(panel$y_scales[[1]]$range$range)[[1]]
  } else if(x_scale_name == "position_d" && y_scale_name == "position_c") {
    panel$x_scales[[1]]$range$range <- collect(panel$x_scales[[1]]$range$range)[[1]]

    if(!is.null(data[[1]]$ymin)) y_min <- min(data[[1]]$ymin)
    else y_min <- if(min(data[[1]]$y) >= 1) 0 else min(data[[1]]$y)
    
    if(!is.null(data[[1]]$ymax)) y_max <- max(data[[1]]$ymax)
    else y_max <- if(max(data[[1]]$y) >= 1) 0 else max(data[[1]]$y)
    
    panel$y_scales[[1]]$range$range <- c(y_min, y_max)
  } else if(x_scale_name == "position_c" && y_scale_name == "position_d") {
    if(!is.null(data[[1]]$xmin)) x_min <- min(data[[1]]$xmin)
    else x_min <- if(min(data[[1]]$x) >= 1) 0 else min(data[[1]]$x)
    
    if(!is.null(data[[1]]$xmax)) x_max <- max(data[[1]]$xmax)
    else x_max <- if(max(data[[1]]$x) >= 1) 0 else max(data[[1]]$x)

    panel$x_scales[[1]]$range$range <- c(x_min, x_max)
    panel$y_scales[[1]]$range$range <- collect(panel$y_scales[[1]]$range$range)[[1]]
  } else if(x_scale_name == "position_c" && y_scale_name == "position_c") {
    if(!is.null(data[[1]]$xmin)) x_min <- min(data[[1]]$xmin)
    else x_min <- if(min(data[[1]]$x) >= 1) 0 else min(data[[1]]$x)
    
    if(!is.null(data[[1]]$xmax)) x_max <- max(data[[1]]$xmax)
    else x_max <- if(max(data[[1]]$x) >= 1) 0 else max(data[[1]]$x)

    if(!is.null(data[[1]]$ymin)) y_min <- min(data[[1]]$ymin)
    else y_min <- if(min(data[[1]]$y) >= 1) 0 else min(data[[1]]$y)
    
    if(!is.null(data[[1]]$ymax)) y_max <- max(data[[1]]$ymax)
    else y_max <- if(max(data[[1]]$y) >= 1) 0 else max(data[[1]]$y)
    
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

calculate_stats.SparkR <- function(data, layers) {
  stat_type <- layers[[1]]$stat$objname
  switch(stat_type, 
    bin = {
      width <- if(is.null(layers[[1]]$stat_params$width)) 0.9 else layers[[1]]$stat_params$width
      x_test <- select(data, "x")
 
      if(dtypes(x_test)[[1]][2] == "int") {
        if(length(grep("fill", columns(data))))
          data <- SparkR::count(groupBy(data, "x", "PANEL", "fill"))
        else
          data <- SparkR::count(groupBy(data, "x", "PANEL"))
     
      } else if(dtypes(x_test)[[1]][2] == "double") {
        range <- as.numeric(collect(select(data, min(data$x), max(data$x))))
        binwidth <- if(is.null(layers[[1]]$stat_params$binwidth)) diff(range) / 30
                    else layers[[1]]$stat_params$binwidth
        breaks <- layers[[1]]$stat_params$breaks
        origin <- layers[[1]]$stat_params$origin
        right <- if(is.null(layers[[1]]$stat_params$right)) TRUE else layers[[1]]$stat_params$right

        if(is.null(breaks)) {
	  if(is.null(origin)) {
	    breaks <- fullseq(range, binwidth, pad = TRUE)
          } else {
            breaks <- seq(origin, max(range) + binwidth, binwidth)
          }
        }

        diddle <- 1e-07 * stats::median(diff(breaks))
        if(right) {
          fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
        } else {
          fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
        }

        fuzzybreaks <- sort(breaks) + fuzz
        width <- diff(breaks)
        left <- breaks[-length(breaks)]
        right <- breaks[-1]

        for(index in 1:length(left)) {
	  filter_df <- filter(data, data$x >= left[index] & data$x < right[index])
          filter_df <- SparkR::rename(filter_df, x_bin = filter_df$x)
          filter_df <- withColumn(filter_df, "x", 
                                  cast(isNull(filter_df$x_bin), "integer") + (left[index] + right[index]) / 2)
  
          if(index == 1) unioned <- filter_df
          else unioned <- unionAll(unioned, filter_df)
        }
 
        if(length(grep("fill", columns(data))))
          data <- SparkR::count(groupBy(unioned, "PANEL", "fill", "x"))
        else
          data <- SparkR::count(groupBy(unioned, "PANEL", "x"))
      }
      
      data <- SparkR::mutate(data, density = data$count / abs(data$count) / width,
                                   ncount = data$count / abs(data$count),
                                   width = data$count * 0 + width)
      
      max_density <- select(data, max(abs(data$density)))
      max_density <- SparkR::rename(max_density, max_density = max_density[[1]])
      temp_df <- SparkR::join(data, max_density)
      data <- SparkR::arrange(withColumn(temp_df, "ndensity", temp_df$density / temp_df$max_density), "x")
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
 
      for(index in 1:(length(breaks$y)-1)) {
        lower <- breaks$y[index]
        upper <- breaks$y[index + 1]
          
        if(index > 1) y_df <- filter(data, data$y > lower & data$y <= upper)
        else          y_df <- filter(data, data$y >= lower & data$y <= upper)

        y_df <- SparkR::mutate(y_df, ymin = cast(isNull(y_df$y), "integer") + lower,
                                     ymax = cast(isNull(y_df$y), "integer") + upper )
 
        if(index > 1) {unioned_y <- unionAll(unioned_y, y_df)}
        else          {unioned_y <- y_df}
      } 

      for(index in 1:(length(breaks$x)-1)) {
        lower <- breaks$x[index]
        upper <- breaks$x[index + 1]
          
        if(index > 1) x_df <- filter(data, data$x > lower & data$x <= upper)
        else          x_df <- filter(data, data$x >= lower & data$x <= upper)

        x_df <- SparkR::mutate(x_df, xmin = cast(isNull(x_df$x), "integer") + lower,
                                     xmax = cast(isNull(x_df$x), "integer") + upper )
 
        if(index > 1) {unioned_x <- unionAll(unioned_x, x_df)}
        else          {unioned_x <- x_df}
      }        

      joined_list <- columns(data)
      joined_list <- joined_list[joined_list != "x_map"]

      complete_list <- "ID"
      for(name in joined_list) {
        joined_df <- select(data, name)
        types <- dtypes(joined_df)[[1]][2]
        
        joined_df <- bindIDs(joined_df)
        joined_df <- withColumn(joined_df, name, cast(joined_df$"_1", types))
        complete_list <- append(name, complete_list)

        if(name != joined_list[1]) {
          joined <- SparkR::join(joined, joined_df, joined$ID == joined_df$"_2", "inner")
        }
        else {
          joined <- withColumnRenamed(joined_df, "_2", "ID")
        }
#        joined <- select(joined, as.list(complete_list)) 
      }

      xmin <- bindIDs(select(unioned_x, "xmin"))
      xmin <- withColumn(xmin, "xmin", cast(xmin$"_1", "double"))
      complete_list <- append(complete_list, "xmin")
      joined <- SparkR::join(joined, xmin, joined$ID == xmin$"_2", "inner")
#      joined <- select(joined, as.list(complete_list)) 
      
      xmax <- bindIDs(select(unioned_x, "xmax"))
      xmax <- withColumn(xmax, "xmax", cast(xmax$"_1", "double"))
      complete_list <- append(complete_list, "xmax")
      joined <- SparkR::join(joined, xmax, joined$ID == xmax$"_2", "inner")
#      joined <- select(joined, as.list(complete_list)) 
      
      ymin <- bindIDs(select(unioned_y, "ymin"))
      ymin <- withColumn(ymin, "ymin", cast(ymin$"_1", "double"))
      complete_list <- append(complete_list, "ymin")
      joined <- SparkR::join(joined, ymin, joined$ID == ymin$"_2", "inner")
#      joined <- select(joined, as.list(complete_list)) 

      ymax <- bindIDs(select(unioned_y, "ymax"))
      ymax <- withColumn(ymax, "ymax", cast(ymax$"_1", "double"))
      complete_list <- append(complete_list, "ymax")
      joined <- SparkR::join(joined, ymax, joined$ID == ymax$"_2", "inner")

      data <- select(joined, as.list(complete_list[complete_list != "ID"])) 
      
      if(length(grep("fill", columns(data))))
        data <- SparkR::count(groupBy(data, "PANEL", "fill", "xmin", "xmax", "ymin", "ymax"))
      else {
        data <- SparkR::count(groupBy(data, "PANEL", "xmin", "xmax", "ymin", "ymax"))
      }

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
        stats <- agg(groupBy(data, "x", "fill", "PANEL") , ymin = min(data$y),
                     lower = min(data$y)+(max(data$y)-min(data$y))*qs[2],
                     middle = min(data$y)+(max(data$y)-min(data$y))*qs[3],
                     upper = min(data$y)+(max(data$y)-min(data$y))*qs[4],
                     ymax = max(data$y), iqr = (max(data$y)-min(data$y))*(qs[4]-qs[2]),
                     relvarwidth = sqrt(sum(cast(isNotNull(data$y), "integer"))))
      else
        stats <- agg(groupBy(data, "x", "PANEL"), ymin = min(data$y),
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
        data <- SparkR::count(groupBy(data, "PANEL", "x", "y", "fill"))
      else
        data <- SparkR::count(groupBy(data, "PANEL", "x", "y"))

      count_group <- SparkR::count(groupBy(data, "group"))

      data <- SparkR::rename(data, n = data$count, group_old = data$group)
      data <- SparkR::join(data, count_group, data$group_old == count_group$group, "inner")
      data <- withColumn(data, "prop", data$count^(-1))
      data <- select(data, "PANEL", "x", "y", "n", "prop")
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
