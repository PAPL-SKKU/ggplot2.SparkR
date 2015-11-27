#' This function is only for Spark DataFrame
#'
#' Return a new DataFrame containing the binding of ID with original DataFrame.
#' This method only used by one-column DataFrame now.
#' @param data A Spark DataFrame
#' @return A DataFrame containing the result of the binding with IDs
#' @rdname bindIDs
#' @export
bindIDs <- function(data) {
  rdd1 <- SparkR:::coalesce(SparkR:::map(SparkR:::toRDD(data),
      function(rdd) { toString(rdd) }), 1)
  rdd2 <- SparkR:::coalesce(SparkR:::map(SparkR:::parallelize(sc, 1:nrow(data)),
      function(rdd) { toString(rdd) }), 1)

  rdd <- SparkR:::zipRDD(rdd1, rdd2)
  df <- createDataFrame(sqlContext, rdd)

  df
}
