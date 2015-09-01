#' Facet specification: a single panel.
#'
#' @inheritParams facet_grid
#' @export
#' @examples
#' # facet_null is the default facetting specification if you
#' # don't override it with facet_grid or facet_wrap
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
facet_null <- function(shrink = TRUE) {
  facet(shrink = shrink, subclass = "null")
}

#' @export
facet_train_layout.null <- function(facet, data) {
  data_class <- class(data[1][[1]])

  if(length(grep("DataFrame", data_class)) == 0) {
    data.frame(
      PANEL = 1L, ROW = 1L, COL = 1L,
      SCALE_X = 1L, SCALE_Y = 1L)
  } 
  else {
    temp_df <- SparkR::limit(data[1][[1]], 1)
    temp_df <- SparkR::mutate(temp_df, PANEL = cast(isNotNull(temp_df[[1]]), "integer"),
                    ROW = cast(isNotNull(temp_df[[1]]), "integer"),
                    COL = cast(isNotNull(temp_df[[1]]), "integer"),
                    SCALE_X = cast(isNotNull(temp_df[[1]]), "integer"),
                    SCALE_Y = cast(isNotNull(temp_df[[1]]), "integer"))

    temp_df <- select(temp_df, "PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y")
    temp_df
  }
}

#' @export
facet_map_layout.null <- function(facet, data, layout) {
  # Need the is.waive check for special case where no data, but aesthetics
  # are mapped to vectors
  data_class <- class(data)

  if(length(grep("DataFrame", data_class)) == 0) {
    if (is.waive(data) || empty(data))
      return(cbind(data, PANEL = integer(0)))
    data$PANEL <- 1L
  } else {
    data <- withColumn(data, "PANEL", cast(isNull(data[[1]]), "integer") + 1)
  }
  
  data
}

#' @export
facet_render.null <- function(facet, panel, coord, theme, geom_grobs) {
  range <- panel$ranges[[1]]

  # Figure out aspect ratio
  aspect_ratio <- theme$aspect.ratio %||% coord_aspect(coord, range)
  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }

  fg <- coord_render_fg(coord, range, theme)
  bg <- coord_render_bg(coord, range, theme)

  # Flatten layers - we know there's only one panel
  geom_grobs <- lapply(geom_grobs, "[[", 1)
  panel_grobs <- c(list(bg), geom_grobs, list(fg))

  panel_grob <- gTree(children = do.call("gList", panel_grobs))
  axis_h <- coord_render_axis_h(coord, range, theme)
  axis_v <- coord_render_axis_v(coord, range, theme)

  all <- matrix(list(
    axis_v,     panel_grob,
    zeroGrob(), axis_h
  ), ncol = 2, byrow = TRUE)

  layout <- gtable_matrix("layout", all,
    widths = unit.c(grobWidth(axis_v), unit(1, "null")),
    heights = unit.c(unit(aspect_ratio, "null"), grobHeight(axis_h)),
    respect = respect, clip = c("off", "off", "on", "off"),
    z = matrix(c(3, 2, 1, 4), ncol = 2, byrow = TRUE)
  )
  layout$layout$name <- c("axis-l", "spacer", "panel", "axis-b")

  layout
}

#' @export
facet_vars.null <- function(facet) ""
