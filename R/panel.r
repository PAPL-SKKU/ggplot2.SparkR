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
  print("new_panel")
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
  print("train_layout")
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
  print("map_layout")
  print("lapply")
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
  print("train_position")
  # Initialise scales if needed, and possible.
  layout <- panel$layout
  if (is.null(panel$x_scales) && !is.null(x_scale)) {
    print("rlply")
    # (BJH) rlply {plyr}
    # Evaluate expression n times then combine results into a list
    panel$x_scales <- rlply(max(layout$SCALE_X), scale_clone(x_scale))
  }
  if (is.null(panel$y_scales) && !is.null(y_scale)) {
    # (BJH) rlply {plyr}
    # Evaluate expression n times then combine results into a list
    panel$y_scales <- rlply(max(layout$SCALE_Y), scale_clone(y_scale))
  }

  # loop over each layer, training x and y scales in turn
  i <- 0
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
    i <- i+1
  }
  
  panel
}


reset_scales <- function(panel) {
  print("reset_scales")
  if (!panel$shrink) return()
  
  print("l_ply")
  # (BJH) l_ply {plyr}
  # For each element of a list, apply function and discard results
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
  print("map_position")
  layout <- panel$layout

  print("lapply")
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

# Function for applying scale function to multiple variables in a given
# data set.  Implement in such a way to minimise copying and hence maximise
# speed
scale_apply <- function(data, vars, f, scale_id, scales) {
  print("scale_apply")
  if (length(vars) == 0) return()
  if (nrow(data) == 0) return()

  n <- length(scales)
  if (any(is.na(scale_id))) stop()

  # (BJH) split_indices {plyr}
  # An optimised version of split for the special case of splitting row indices into groups, as used by splitter_d
  scale_index <- split_indices(scale_id, n)

  print("lapply")
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
  print("train_ranges")
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
  print("calculate_stats")
  print("lapply")
  lapply(seq_along(data), function(i) {
    d <- data[[i]]
    l <- layers[[i]]

    print("ddply")
    ddply(d, "PANEL", function(panel_data) {
      scales <- panel_scales(panel, panel_data$PANEL[1])
      l$calc_statistic(panel_data, scales)
    })
  })
}


xlabel <- function(panel, labels) {
  print("xlabel")
  panel$x_scales[[1]]$name %||% labels$x
}

ylabel <- function(panel, labels) {
  print("ylabel")
  panel$y_scales[[1]]$name %||% labels$y
}
