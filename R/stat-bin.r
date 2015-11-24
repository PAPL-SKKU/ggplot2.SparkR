#' Bin data.
#'
#' Missing values are currently silently dropped.
#'
#' @inheritParams stat_identity
#' @param binwidth Bin width to use. Defaults to 1/30 of the range of the
#'   data
#' @param breaks Actual breaks to use.  Overrides bin width and origin
#' @param origin Origin of first bin
#' @param width Width of bars when used with categorical data
#' @param right If \code{TRUE}, right-closed, left-open, if \code{FALSE},
#'   the default, right-open, left-closed.
#' @param drop If TRUE, remove all bins with zero counts
#' @return New data frame with additional columns:
#'   \item{count}{number of points in bin}
#'   \item{density}{density of points in bin, scaled to integrate to 1}
#'   \item{ncount}{count, scaled to maximum of 1}
#'   \item{ndensity}{density, scaled to maximum of 1}
#' @export
#' @examples
#' \donttest{
#' simple <- data.frame(x = rep(1:10, each = 2))
#' base <- ggplot(simple, aes(x))
#' # By default, right = FALSE intervals are of the form [a, b)
#' base + stat_bin(binwidth = 1, drop = FALSE, right = FALSE, col = "black")
#' # If right = TRUE, and intervals are of the form (a, b]
#' base + stat_bin(binwidth = 1, drop = FALSE, right = TRUE, col = "black")
#'
#' m <- ggplot(movies, aes(x=rating))
#' m + stat_bin()
#' m + stat_bin(binwidth=0.1)
#' m + stat_bin(breaks=seq(4,6, by=0.1))
#' # See geom_histogram for more histogram examples
#'
#' # To create a unit area histogram, use aes(y = ..density..)
#' (linehist <- m + stat_bin(aes(y = ..density..), binwidth=0.1,
#'   geom="line", position="identity"))
#' linehist + stat_density(colour="blue", fill=NA)
#'
#' # Also works with categorical variables
#' ggplot(movies, aes(x=mpaa)) + stat_bin()
#' }
stat_bin <- function (mapping = NULL, data = NULL, geom = "bar", position = "stack",
width = 0.9, drop = FALSE, right = FALSE, binwidth = NULL, origin = NULL, breaks = NULL, ...) {
  StatBin$new(mapping = mapping, data = data, geom = geom, position = position,
  width = width, drop = drop, right = right, binwidth = binwidth, origin = origin, breaks = breaks, ...)
}

StatBin <- proto(Stat, {
  objname <- "bin"
  informed <- FALSE

  calculate_groups <- function(., data, ...) {
    if (!is.null(data$y) || !is.null(match.call()$y)) {
      stop("May not have y aesthetic when binning", call. = FALSE)
    }

    .$informed <- FALSE
    .super$calculate_groups(., data, ...)
  }

  calculate_groups.SparkR <- function(., data, ...) {
    if(length(grep("y", columns(data))) != 0) {
      stop("May not have y aesthetic when binning", call. = FALSE)
    }

    .$informed <- FALSE
    .super$calculate_groups(., data, ...)
  }

  calculate <- function(., data, scales, binwidth=NULL, origin=NULL, breaks=NULL, width=0.9,
      drop = FALSE, right = FALSE, ...) {
    range <- scale_dimension(scales$x, c(0, 0))

    if (is.null(breaks) && is.null(binwidth) && !is.integer(data$x) && !.$informed) {
      message("stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.")
      .$informed <- TRUE
    }

    bin(data$x, data$weight, binwidth=binwidth, origin=origin, breaks=breaks, range=range,
      width=width, drop = drop, right = right)
  }

  calculate.SparkR <- function(., data, binwidth=NULL, origin=NULL, breaks=NULL, width=0.9, 
      drop=FALSE, right=FALSE, ...) {
    x_types <- dtypes(select(data, "x"))[[1]]

    if (is.null(breaks) && is.null(binwidth) && !(x_types[2] == "int") && !.$informed) {
      message("stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.")
      .$informed <- TRUE
    }

    bin.SparkR(data, binwidth=binwidth, origin=origin, breaks=breaks, range=NULL, width=width,
      drop = drop, right = right)
  }

  default_aes <- function(.) aes(y = ..count..)
  required_aes <- c("x")
  default_geom <- function(.) GeomBar
})

bin.SparkR <- function(data, binwidth=NULL, origin=NULL, breaks=NULL, range=NULL, width=0.9,
    drop = FALSE, right = TRUE) {
  x_test <- select(data, "x")

  if(dtypes(x_test)[[1]][2] == "int") {
      if(length(grep("fill", columns(data)))) {
        data <- SparkR::count(groupBy(data, "x", "PANEL", "fill"))
      } else if(length(grep("colour", columns(data)))) {
        data <- SparkR::count(groupBy(data, "x", "PANEL", "colour"))
      } else {
        data <- SparkR::count(groupBy(data, "x", "PANEL"))
      }

      data <- SparkR::mutate(data, density = lit(1 / width), ndensity = lit(1),
    			     ncount = lit(1), width = lit(width), y = data$count)
  } else if(dtypes(x_test)[[1]][2] == "double") {
    if(is.null(range)) range <- as.numeric(collect(select(data, min(data$x), max(data$x))))
    if(is.null(binwidth)) binwidth <- diff(range) / 30
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
    zero_filter <- c()

    for(index in 1:length(left)) {
      filter_df <- filter(data, data$x >= left[index] & data$x < right[index])
      if(nrow(filter_df) == 0) {
        zero_filter <- append(zero_filter, index)
      }

      filter_df <- SparkR::rename(filter_df, x_bin = filter_df$x)
      filter_df <- withColumn(filter_df, "x", lit((left[index] + right[index]) / 2))	

      if(index == 1) unioned <- filter_df
      else unioned <- unionAll(unioned, filter_df)
    }

    if(length(grep("fill", columns(data)))) {
      data <- SparkR::count(groupBy(unioned, "PANEL", "fill", "x"))
    } else if(length(grep("colour", columns(data)))){
      data <- SparkR::count(groupBy(unioned, "PANEL", "colour", "x"))
    } else {
      data <- SparkR::count(groupBy(unioned, "PANEL", "x"))
    }

    max_sum_count <- select(data, sum(data$count), max(data$count))
    max_sum_count <- SparkR::rename(max_sum_count, sum_count = max_sum_count[[1]],
  				    max_count = max_sum_count[[2]])

    data <- SparkR::join(data, max_sum_count)
    data <- SparkR::mutate(data, density = data$count / width[1] / data$sum_count,
  			   ncount = data$count / data$max_count,
  			   width = lit(width[1]), y = data$count)

    max_density <- select(data, max(abs(data$density)))
    max_density <- SparkR::rename(max_density, max_density = max_density[[1]])

    data <- SparkR::join(data, max_density)
    data <- withColumn(data, "ndensity", data$density / data$max_density)
  }

  basic <- c("PANEL", "x", "count", "density", "ncount", "width", "ndensity")
  if(length(grep("fill", columns(data)))) {
    data <- select(data, as.list(append(basic, "fill")))
  } else if(length(grep("colour", columns(data)))) {
    data <- select(data, as.list(append(basic, "colour")))
  } else {
    data <- select(data, as.list(basic))
  }

  if(dtypes(x_test)[[1]][2] == "double") {
    remained <- data.frame(PANEL = 1, x = (left[zero_filter] + right[zero_filter]) / 2,
    			   count = 0, density = 0, ncount = 0, width = width[1],
			   ndensity = 0)
    remained <- createDataFrame(sqlContext, remained)

    data <- unionAll(data, remained)
  }

  data <- SparkR::arrange(data, "x")
  persist(data, "MEMORY_ONLY")

  if (drop) data <- filter(data, data$count > 0)

  data
}

bin <- function(x, weight=NULL, binwidth=NULL, origin=NULL, breaks=NULL, range=NULL, width=0.9,
    drop = FALSE, right = TRUE) {
  if (length(na.omit(x)) == 0) return(data.frame())
  if (is.null(weight))  weight <- rep(1, length(x))
  weight[is.na(weight)] <- 0

  if (is.null(range))    range <- range(x, na.rm = TRUE, finite=TRUE)
  if (is.null(binwidth)) binwidth <- diff(range) / 30
  if (is.integer(x)) {
    bins <- x
    x <- sort(unique(bins))
    width <- width
  } else if (diff(range) == 0) {
    width <- width
    bins <- x
  } else { # if (is.numeric(x))
    if (is.null(breaks)) {
      if (is.null(origin)) {
        breaks <- fullseq(range, binwidth, pad = TRUE)
      } else {
        breaks <- seq(origin, max(range) + binwidth, binwidth)
      }
    }

    # Adapt break fuzziness from base::hist - this protects from floating
    # point rounding errors
    diddle <- 1e-07 * stats::median(diff(breaks))
    if (right) {
      fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
    } else {
      fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
    }
    fuzzybreaks <- sort(breaks) + fuzz
    bins <- cut(x, fuzzybreaks, include.lowest=TRUE, right = right)
    left <- breaks[-length(breaks)]
    right <- breaks[-1]
    x <- (left + right)/2
    width <- diff(breaks)
  }

  results <- data.frame(
    count = as.numeric(tapply(weight, bins, sum, na.rm=TRUE)),
    x = x,
    width = width
  )
 
  if (sum(results$count, na.rm = TRUE) == 0) {
    return(results)
  }

  res <- within(results, {
    count[is.na(count)] <- 0
    density <- count / width / sum(abs(count), na.rm=TRUE)
    ncount <- count / max(abs(count), na.rm=TRUE)
    ndensity <- density / max(abs(density), na.rm=TRUE)
  })
 
  if (drop) res <- subset(res, count > 0)
 
  res
}
