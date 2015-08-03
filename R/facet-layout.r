# Layout panels in a 2d grid.
#
# @params data list of data frames, one for each layer
# @params rows variables that form the rows
# @params cols variables that form the columns
# @return a data frame with columns \code{PANEL}, \code{ROW} and \code{COL},
#   that match the facetting variable values up with their position in the
#   grid
layout_grid <- function(data, rows = NULL, cols = NULL, margins = NULL, drop = TRUE, as.table = TRUE) {
  if (length(rows) == 0 && length(cols) == 0) return(layout_null())
  rows <- as.quoted(rows)
  cols <- as.quoted(cols)

  base_rows <- layout_base(data, rows, drop = drop)
  if (!as.table) {
    rev_order <- function(x) factor(x, levels = rev(ulevels(x)))
    base_rows[] <- lapply(base_rows, rev_order)
  }
  base_cols <- layout_base(data, cols, drop = drop)
  base <- df.grid(base_rows, base_cols)

  # Add margins
  base <- add_margins(base, list(names(rows), names(cols)), margins)
  # Work around bug in reshape2
  base <- unique(base)

  # Create panel info dataset
  panel <- id(base, drop = TRUE)
  panel <- factor(panel, levels = seq_len(attr(panel, "n")))

  rows <- if (is.null(names(rows))) 1L else id(base[names(rows)], drop = TRUE)
  cols <- if (is.null(names(cols))) 1L else id(base[names(cols)], drop = TRUE)

  panels <- data.frame(PANEL = panel, ROW = rows, COL = cols, base,
    check.names = FALSE)
  panels <- panels[order(panels$PANEL), , drop = FALSE]
  rownames(panels) <- NULL
  
  panels
}

layout.SparkR_grid <- function(data, rows = NULL, cols = NULL, margins = NULL, drop = TRUE, as.table = TRUE) {
  if(length(rows) == 0 && length(cols) == 0) return(layout_null())

  rows_char <- as.character(rows)
  cols_char <- as.character(cols)
  
  rows_is_null <- length(rows_char) == 0
  cols_is_null <- length(cols_char) == 0

  data <- data[1][[1]]
  
  # Set unique number for ROW grid
  if(!rows_is_null) rows <- distinct(select(data, eval(rows_char)))

  # Set unique number for COL grid
  if(!cols_is_null) cols <- distinct(select(data, eval(cols_char)))
 
  # Create PANEL info dataset
  if(!rows_is_null && !cols_is_null) {
    panels <- SparkR::join(rows, cols)
    panels <- withColumn(panels, "init", cast(isNull(panels[[1]]), "integer"))
   
    value_row <- collect(select(panels, eval(rows_char)))[[1]]
    value_col <- collect(select(panels, eval(cols_char)))[[1]]

    for(index in 1:length(value_row)) {
      temp_df <- filter(panels, panels[[eval(rows_char)]] == value_row[index] &
                                panels[[eval(cols_char)]] == value_col[index])
      temp_df <- withColumn(temp_df, "PANEL", temp_df$init + index)
      
      if(index > 1) unioned <- unionAll(unioned, temp_df)
      else          unioned <- temp_df
    }

    panels <- select(unioned, eval(rows_char), eval(cols_char), "PANEL")
  } else if(!rows_is_null) {
    panels <- withColumn(rows, "init", cast(isNull(rows[[1]]), "integer"))
    value_row <- collect(select(panels, eval(rows_char)))[[1]]
    
    for(index in 1:length(value_row)) {
      temp_df <- filter(panels, panels[[eval(rows_char)]] == value_row[index])
      temp_df <- withColumn(temp_df, "PANEL", temp_df$init + index)
      
      if(index > 1) unioned <- unionAll(unioned, temp_df)
      else          unioned <- temp_df
    }

    panels <- select(unioned, eval(rows_char), "PANEL")
  } else if(!cols_is_null) {
    panels <- withColumn(cols, "init", cast(isNull(cols[[1]]), "integer"))
    value_col <- collect(select(panels, eval(cols_char)))[[1]]
    
    for(index in 1:length(value_col)) {
      temp_df <- filter(panels, panels[[eval(cols_char)]] == value_col[index])
      temp_df <- withColumn(temp_df, "PANEL", temp_df$init + index)
      
      if(index > 1) unioned <- unionAll(unioned, temp_df)
      else          unioned <- temp_df
    }

    panels <- select(unioned, eval(cols_char), "PANEL")
  }

  panels
}

# Layout out panels in a 1d ribbon.
#
# @params drop should missing combinations be excluded from the plot?
# @keywords internal
layout_wrap <- function(data, vars = NULL, nrow = NULL, ncol = NULL, as.table = TRUE, drop = TRUE) {
  vars <- as.quoted(vars)
  if (length(vars) == 0) return(layout_null())

  base <- unrowname(layout_base(data, vars, drop = drop))
  id <- id(base, drop = TRUE)
  n <- attr(id, "n")

  dims <- wrap_dims(n, nrow, ncol)
  layout <- data.frame(PANEL = factor(id, levels = seq_len(n)))

  if (as.table) {
    layout$ROW <- as.integer((id - 1L) %/% dims[2] + 1L)
  } else {
    layout$ROW <- as.integer(dims[1] - (id - 1L) %/% dims[2])
  }
  layout$COL <- as.integer((id - 1L) %% dims[2] + 1L)
  
  panels <- cbind(layout, unrowname(base))
  panels <- panels[order(panels$PANEL), , drop = FALSE]
  rownames(panels) <- NULL
  
  panels
}
  
layout.SparkR_wrap <- function(data, vars = NULL, nrow = NULL, ncol = NULL, as.table = TRUE, drop = TRUE) {
  vars <- as.character(unlist(vars))
  if(length(vars) == 0) stop("Error: No variable for calculate")

  data <- data[1][[1]]
  base <- select(data, eval(vars))
  base <- distinct(base, eval(vars))

  # Get total number of layer
  n <- collect(select(base, countDistinct(base[[eval(vars)]])))[[1]]
  dims <- wrap_dims(n, nrow, ncol)
  
  base <- withColumn(base, "init", cast(isNull(base[[eval(vars)]]), "integer"))
  value <- collect(select(base, eval(vars)))[[1]]
  
  # Set row, column number that is fitting in total number of layer
  nrow <- if(dims[1] >= n) n else dims[1]
  ncol <- if(dims[2] >= n) n else dims[2]
  
  for(index in 1:n) {
    temp_df <- withColumn(filter(base, base[[eval(vars)]] == value[index]), "PANEL", base$init + index)
    if(index > 1) unioned <- unionAll(unioned, temp_df)
    else          unioned <- temp_df
  }
  panels <- select(unioned, eval(vars), "PANEL")  
 
  # Calculate row number (y-axis number of layer) 
  for(index in 1:nrow) {
    temp_df <- withColumn(filter(base, base[[eval(vars)]] == value[1]), "ROW", base$init + index)
    if(index > 1) unioned <- unionAll(unioned, temp_df)
    else          unioned <- temp_df
  }
  rows <- select(unioned, "ROW")

  # Calculate column number (x-axis number of layer)
  for(index in 1:ncol) {
    temp_df <- withColumn(filter(base, base[[eval(vars)]] == value[1]), "COL", base$init + index)
    if(index > 1) unioned <- unionAll(unioned, temp_df)
    else          unioned <- temp_df
  }
  cols <- select(unioned, "COL")

  layout <- SparkR::join(rows, cols)
  layout <- withColumn(layout, "PANEL_init", (layout$ROW - 1) * ncol + layout$COL)
 
  # Create PANEL info dataset
  panels <- SparkR::join(layout, panels, layout$PANEL_init == panels$PANEL, "inner")
  panels <- select(panels, "ROW", "COL", "PANEL", eval(vars))
 
  panels
}

layout_null <- function(data) {
   data.frame(PANEL = 1, ROW = 1, COL = 1)
}

# Base layout function that generates all combinations of data needed for
# facetting
# The first data frame in the list should be the default data for the plot.
# Other data frames in the list are ones that are added to layers.
#
# @params data list of data frames (one for each layer)
# @keywords internal
layout_base <- function(data, vars = NULL, drop = TRUE) {
  if (length(vars) == 0) return(data.frame())

  # For each layer, compute the facet values
  values <- compact(llply(data, quoted_df, vars = vars))

  # Form the base data frame which contains all combinations of facetting
  # variables that appear in the data
  has_all <- unlist(llply(values, length)) == length(vars)
  if (!any(has_all)) {
    stop("At least one layer must contain all variables used for facetting")
  }

  base <- unique(ldply(values[has_all]))
  if (!drop) {
    base <- unique_combs(base)
  }

  # Systematically add on missing combinations
  for (value in values[!has_all]) {
    if (empty(value)) next;

    old <- base[setdiff(names(base), names(value))]
    new <- unique(value[intersect(names(base), names(value))])
    if (drop) {
      new <- unique_combs(new)
    }

    base <- rbind(base, df.grid(old, new))
  }

  if (is.null(base)) {
    stop("Faceting variables must have at least one value")
  }

  base
}

ulevels <- function(x) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else {
    sort(unique(x))
  }
}

unique_combs <- function(df) {
  if (length(df) == 0) return()

  unique_values <- llply(df, ulevels)
  rev(expand.grid(rev(unique_values), stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = TRUE))
}

df.grid <- function(a, b) {
  if (nrow(a) == 0) return(b)
  if (nrow(b) == 0) return(a)

  indexes <- expand.grid(
    i_a = seq_len(nrow(a)),
    i_b = seq_len(nrow(b))
  )
  unrowname(cbind(
    a[indexes$i_a, , drop = FALSE],
    b[indexes$i_b, , drop = FALSE]
  ))
}

quoted_df <- function(data, vars) {
  values <- eval.quoted(vars, data, emptyenv(), try = TRUE)
  as.data.frame(compact(values), optional = TRUE)
}

# Arrange 1d structure into a grid
wrap_dims <- function(n, nrow = NULL, ncol = NULL) {
    if (is.null(ncol) && is.null(nrow)) {
      rc <- grDevices::n2mfrow(n)
      nrow <- rc[2]
      ncol <- rc[1]
    } else if (is.null(ncol)) {
      ncol <- ceiling(n / nrow)
    } else if (is.null(nrow)) {
      nrow <- ceiling(n / ncol)
    }
    stopifnot(nrow * ncol >= n)

    c(nrow, ncol)
}
