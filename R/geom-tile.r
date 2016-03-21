# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
#' @export
GeomTile_SparkR <- ggplot2::ggproto("GeomTile_SparkR", ggplot2::GeomTile,
  setup_data = function(data, params) {
    data
  }
)
