# Take single layer of data and combine it with panel information to split
# data into different panels. Adds in extra data for missing facetting
# levels and for margins.
#
# @params data a data frame
locate_grid <- function(data, panels, rows = NULL, cols = NULL, margins = FALSE) {
  if (empty(data)) {
    return(cbind(data, PANEL = integer(0)))
  }
  
  rows <- as.quoted(rows)
  cols <- as.quoted(cols)
  vars <- c(names(rows), names(cols))

  # Compute facetting values and add margins
  margin_vars <- list(intersect(names(rows), names(data)),
    intersect(names(cols), names(data)))
  data <- add_margins(data, margin_vars, margins)
  facet_vals <- quoted_df(data, c(rows, cols))
  
  # If any facetting variables are missing, add them in by
  # duplicating the data
  missing_facets <- setdiff(vars, names(facet_vals))
  if (length(missing_facets) > 0) {
    to_add <- unique(panels[missing_facets])

    data_rep <- rep.int(1:nrow(data), nrow(to_add))
    facet_rep <- rep(1:nrow(to_add), each = nrow(data))

    data <- unrowname(data[data_rep, , drop = FALSE])
    facet_vals <- unrowname(cbind(
      facet_vals[data_rep, ,  drop = FALSE],
      to_add[facet_rep, , drop = FALSE]))
  }

  # Add PANEL variable
  if (nrow(facet_vals) == 0) {
    # Special case of no facetting
    data$PANEL <- 1
  } else {
    facet_vals[] <- lapply(facet_vals[], as.factor)
    facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)

    keys <- join.keys(facet_vals, panels, by = vars)

    data$PANEL <- panels$PANEL[match(keys$x, keys$y)]
  }
  
  data[order(data$PANEL), , drop = FALSE]
}

locate.SparkR_grid <- function(data, panels, rows = NULL, cols = NULL, margins = FALSE) {
  rows_char <- as.character(rows)
  cols_char <- as.character(cols)
  
  rows_is_null <- length(rows_char) == 0
  cols_is_null <- length(cols_char) == 0
 
  # Add PANEL variable 
  if(!rows_is_null && !cols_is_null) {
    panels_rename <- withColumnRenamed(panels, eval(rows_char), "row_old")
    panels_rename <- withColumnRenamed(panels_rename, eval(cols_char), "col_old")

    keys <- SparkR::join(panels_rename, data, panels_rename$row_old == data[[eval(rows_char)]] &
                                              panels_rename$col_old == data[[eval(cols_char)]], "inner")
  } else if(!rows_is_null) {
    panels_rename <- withColumnRenamed(panels, eval(rows_char), "row_old")

    keys <- SparkR::join(panels_rename, data, panels_rename$row_old == data[[eval(rows_char)]], "inner")
  } else if(!cols_is_null) {
    panels_rename <- withColumnRenamed(panels, eval(cols_char), "col_old")

    keys <- SparkR::join(panels_rename, data, panels_rename$col_old == data[[eval(cols_char)]], "inner")
  }

  # Return with unnessary columns (col_old, row_old, COL, ROW)
  keys
}

locate_wrap <- function(data, panels, vars) {
  if (empty(data)) {
    return(cbind(data, PANEL = integer(0)))
  }
  vars <- as.quoted(vars)

  facet_vals <- quoted_df(data, vars)
  facet_vals[] <- lapply(facet_vals[], as.factor)

  missing_facets <- setdiff(names(vars), names(facet_vals))
  if (length(missing_facets) > 0) {

    to_add <- unique(panels[missing_facets])

    data_rep <- rep.int(1:nrow(data), nrow(to_add))
    facet_rep <- rep(1:nrow(to_add), each = nrow(data))

    data <- unrowname(data[data_rep, , drop = FALSE])
    facet_vals <- unrowname(cbind(
      facet_vals[data_rep, ,  drop = FALSE],
      to_add[facet_rep, , drop = FALSE]))
  }

  keys <- join.keys(facet_vals, panels, by = names(vars))

  data$PANEL <- panels$PANEL[match(keys$x, keys$y)]
  data[order(data$PANEL), ]
}

locate.SparkR_wrap <- function(data, panels, vars) {
  vars <- as.character(unlist(vars))
  panels <- withColumnRenamed(panels, eval(vars), "init")

  keys <- SparkR::join(data, panels, data[[eval(vars)]] == panels$init, "inner")

  keys
}
