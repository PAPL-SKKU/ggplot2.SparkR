# Ensure that the data frame contains a grouping variable.
#
# If the \code{group} variable is not present, then a new group
# variable is generated from the interaction of all discrete (factor or
# character) vectors, excluding \code{label}.
#
# @param data.frame
# @value data.frame with group variable
# @keyword internal
add_group <- function(data) {
  if (empty(data)) return(data)

  if (is.null(data$group)) {
    disc <- vapply(data, is.discrete, logical(1))
    disc[names(disc) == "label"] <- FALSE
    
    if (any(disc)) {
      data$group <- id(data[disc], drop = TRUE)
    } else {
      data$group <- 1L
    }
  } else {
    data$group <- id(data["group"], drop = TRUE)
  }
  
  data
}

isDiscrete <- function(data) {
  data_types <- unlist(dtypes(data))
  isDiscrete <- FALSE
  isDiscreteY <- FALSE

  if(length(grep("grouped", data_types)) == 0) {
    x_data_types <- data_types[grep("x", data_types) + 1]
    if(x_data_types == "string" || x_data_types == "boolean")
      isDiscrete <- TRUE
    else if(collect(select(data, countDistinct(data$x)))[[1]] < 7)
      isDiscrete <- TRUE

    if(length(grep("y", data_types)) != 0) {
      y_data_types <- data_types[grep("y", data_types) + 1]
      if(y_data_types == "string" || y_data_types == "boolean") {
        isDiscrete <- TRUE
        isDiscreteY <- TRUE
      } else if(collect(select(data, countDistinct(data$y)))[[1]] < 7) {
        isDiscrete <- TRUE
        isDiscreteY <- TRUE
      }
    }

    if(isDiscrete && isDiscreteY) column_arr <- c("x", "y")
    else if(isDiscrete)           column_arr <- c("x")
    else                          column_arr <- c() 

  } else {
    column_arr <- c("grouped")  
  }

  if(length(grep("fill", data_types)) != 0)    column_arr <- append(column_arr, "fill")
  if(length(grep("colour", data_types)) != 0)  column_arr <- append(column_arr, "colour")

  column_arr
}

add.SparkR_group <- function(data) {
  discrete_col <- isDiscrete(data)
  filter_cmd <- 'filter(data, data[["PANEL"]] == disc[["PANEL"]][index]'
  
  for(col in discrete_col) {
    filter_cmd <- paste(filter_cmd, ' & data[["', col, '"]] == disc[["', col, '"]][index]', sep="")
  }

  filter_cmd <- paste(filter_cmd, ")")
  disc <- collect(distinct(select(data, append(as.list(discrete_col), "PANEL"))))
 
  for(index in 1:nrow(disc)) {
    temp_df <- eval(parse(text = filter_cmd))
    temp_df <- withColumn(temp_df, "group", cast(isNull(temp_df[[1]]), "integer") + index)
  
    if(index > 1)  group <- unionAll(group, temp_df)
    else           group <- temp_df
  } 

  group
}

order_groups <- function(data) {
  if (is.null(data$order)) return(data)

  data[order(data$order), ]
}
