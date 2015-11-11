#' Count number of observation in rectangular bins.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "bin2d")}
#'
#' @inheritParams stat_identity
#' @param bins numeric vector giving number of bins in both vertical and
#'   horizontal directions. Set to 30 by default.
#' @param drop if \code{TRUE} removes all cells with 0 counts.
#' @seealso \code{\link{stat_binhex}} for hexagonal binning
#' @export
#' @examples
#' \donttest{
#' d <- ggplot(diamonds, aes(carat, price))
#' d + stat_bin2d()
#' d + geom_bin2d()
#'
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + stat_bin2d(bins = 10)
#' d + stat_bin2d(bins = 30)
#'
#' # Or by specifying the width of the bins
#' d + stat_bin2d(binwidth = c(1, 1000))
#' d + stat_bin2d(binwidth = c(.1, 500))
#'
#' # Or with a list of breaks
#' x <- seq(min(diamonds$carat), max(diamonds$carat), by = 0.1)
#' y <- seq(min(diamonds$price), max(diamonds$price), length = 50)
#' d + stat_bin2d(breaks = list(x = x, y = y))
#' }
stat_bin2d <- function (mapping = NULL, data = NULL, geom = NULL, position = "identity",
bins = 30, drop = TRUE, ...) {
  StatBin2d$new(mapping = mapping, data = data, geom = geom, position = position,
  bins = bins, drop = drop, ...)
}

StatBin2d <- proto(Stat, {
  objname <- "bin2d"

  default_aes <- function(.) aes(fill = ..count..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomRect

  calculate <- function(., data, scales, binwidth = NULL, bins = 30, breaks = NULL,
      origin = NULL, drop = TRUE, ...) {
    range <- list(
      x = scale_dimension(scales$x, c(0, 0)),
      y = scale_dimension(scales$y, c(0, 0))
    )

    # Determine origin, if omitted
    if (is.null(origin)) {
      origin <- c(NA, NA)
    } else {
      stopifnot(is.numeric(origin))
      stopifnot(length(origin) == 2)
    }

    originf <- function(x) if (is.integer(x)) -0.5 else min(x, na.rm = TRUE)
    if (is.na(origin[1])) origin[1] <- originf(data$x)
    if (is.na(origin[2])) origin[2] <- originf(data$y)

    # Determine binwidth, if omitted
    if (is.null(binwidth)) {
      binwidth <- c(NA, NA)
      if (is.integer(data$x)) {
        binwidth[1] <- 1
      } else {
        binwidth[1] <- diff(range$x) / bins
      }
      if (is.integer(data$y)) {
        binwidth[2] <- 1
      } else {
        binwidth[2] <- diff(range$y) / bins
      }
    }
 
    stopifnot(is.numeric(binwidth))
    stopifnot(length(binwidth) == 2)

    # Determine breaks, if omitted
    if (is.null(breaks)) {
      breaks <- list(
        seq(origin[1], max(range$x) + binwidth[1], binwidth[1]),
        seq(origin[2], max(range$y) + binwidth[2], binwidth[2])
      )
    } else {
      stopifnot(is.list(breaks))
      stopifnot(length(breaks) == 2)
      stopifnot(all(sapply(breaks, is.numeric)))
    }
    names(breaks) <- c("x", "y")

    xbin <- cut(data$x, sort(breaks$x), include.lowest = TRUE)
    ybin <- cut(data$y, sort(breaks$y), include.lowest = TRUE)

    if (is.null(data$weight)) data$weight <- 1

    counts <- as.data.frame(
      xtabs(weight ~ xbin + ybin, data), responseName = "count")
    if (drop) counts <- subset(counts, count > 0)

    within(counts,{
      xint <- as.numeric(xbin)
      xmin <- breaks$x[xint]
      xmax <- breaks$x[xint + 1]

      yint <- as.numeric(ybin)
      ymin <- breaks$y[yint]
      ymax <- breaks$y[yint + 1]

      density <- count / sum(count, na.rm = TRUE)
    })
  }

  calculate.SparkR <- function(., data, scales, binwidth = NULL, bins = 30, breaks = NULL,
      origin = NULL, drop = TRUE, ...) {
    x_test <- select(data, "x")
    y_test <- select(data, "y")

    x_types <- unlist(dtypes(x_test))[2] == "int"
    y_types <- unlist(dtypes(y_test))[2] == "int"

    range <- as.numeric(collect(select(data, min(data$x), max(data$x), min(data$y), max(data$y))))

    # Determine origin, if omitted
    if (is.null(origin)) {
      origin <- c(NA, NA)
    } else {
      stopifnot(is.numeric(origin))
      stopifnot(length(origin) == 2)
    }

    originf <- function(x, value) if (x) -0.5 else value
    if (is.na(origin[1])) origin[1] <- originf(x_types, range[1])
    if (is.na(origin[2])) origin[2] <- originf(y_types, range[3])

    # Determine binwidth, if omitted
    if (is.null(binwidth)) {
      binwidth <- c(NA, NA)
      if(x_types) {
        binwidth[1] <- 1
      } else {
        binwidth[1] <- diff(range[1:2]) / bins
      }
      if(y_types) {
        binwidth[2] <- 1 
      } else {
        binwidth[2] <- diff(range[3:4]) / bins
      }
    }

    stopifnot(is.numeric(binwidth))
    stopifnot(length(binwidth) == 2)

    # Determine breaks, if omitted
    if (is.null(breaks)) {
      breaks <- list(
        x = seq(origin[1], range[2] + binwidth[1], binwidth[1]),
        y = seq(origin[2], range[4] + binwidth[2], binwidth[2])
      )
    } else {
      stopifnot(is.list(breaks))
      stopifnot(length(breaks) == 2)
      stopifnot(all(sapply(breaks, is.numeric)))
    }

    x_test <- withColumnRenamed(x_test, "x", "x_OLD")
    y_test <- withColumnRenamed(y_test, "y", "y_OLD")

    if(x_types == TRUE & y_types == TRUE) {
      data <- SparkR::mutate(data, xmin = data$x - 0.5, xmax = data$x + 0.5,
      			     ymin = data$y - 0.5, ymax = data$y + 0.5)
    } else if(x_types == TRUE) {
      unioned <- distinct(bin2d.SparkR(y_test, breaks$y, "y"))

      data <- SparkR::join(data, unioned, data$y == unioned$y_OLD, "inner")
      data <- SparkR::mutate(data, xmin = data$x - 0.5, xmax = data$x + 0.5)
    } else if(y_types == TRUE) {
      unioned <- distinct(bin2d.SparkR(x_test, breaks$x, "x"))

      data <- SparkR::join(data, unioned, data$x == unioned$x_OLD, "inner")
      data <- SparkR::mutate(data, ymin = data$y - 0.5, ymax = data$y + 0.5)
    } else {
      unioned_x <- distinct(bin2d.SparkR(x_test, breaks$x, "x"))
      unioned_y <- distinct(bin2d.SparkR(y_test, breaks$y, "y"))

      data <- SparkR::join(data, unioned_x, data$x == unioned_x$x_OLD, "inner")
      data <- SparkR::join(data, unioned_y, data$y == unioned_y$y_OLD, "inner")
    }

    basic <- c("x", "y", "PANEL", "xmin", "xmax", "ymin", "ymax")
    if(length(grep("fill", columns(data)))) {
      data <- select(data, as.list(append(basic, "fill")))
      data <- SparkR::count(groupBy(data, "PANEL", "xmin", "xmax", "ymin", "ymax", "fill"))
    } else if(length(grep("colour", columns(data)))) {
      data <- select(data, as.list(append(basic, "colour")))
      data <- SparkR::count(groupBy(data, "PANEL", "xmin", "xmax", "ymin", "ymax", "colour"))
    } else {
      data <- select(data, as.list(basic))
      data <- SparkR::count(groupBy(data, "PANEL", "xmin", "xmax", "ymin", "ymax"))
    }

    sum_count <- select(data, sum(data$count))
    sum_count <- SparkR::rename(sum_count, sum_count = sum_count[[1]])

    data <- SparkR::join(data, sum_count)
    data <- withColumn(data, "density", data$count / data$sum_count)
  }

  bin2d.SparkR <- function(df, breaks, name) {
    col <- paste0(name, "_OLD")

    for(index in 1:(length(breaks)-1)) {
      lower <- breaks[index]
      upper <- breaks[index + 1]

      if(index > 1) {
        counts <- filter(df, df[[col]] > lower & df[[col]] <= upper)
      } else {
        counts <- filter(df, df[[col]] >= lower & df[[col]] <= upper)
      }

      counts <- withColumn(counts, paste0(name, "min"), lit(lower))
      counts <- withColumn(counts, paste0(name, "max"), lit(upper))

      if(index > 1) unioned <- unionAll(unioned, counts) else unioned <- counts
    } 
    unioned
  }
})
