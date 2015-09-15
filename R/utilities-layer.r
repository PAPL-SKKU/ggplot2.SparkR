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
    print(disc)
    if (any(disc)) {
      data$group <- id(data[disc], drop = TRUE)
    } else {
      data$group <- 1L
    }
  } else {
    data$group <- id(data["group"], drop = TRUE)
  }
  print(data)
  stop("add_group")
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
#    else if(collect(select(data, countDistinct(data$x)))[[1]] < 7)
#      isDiscrete <- TRUE

    if(length(grep("y", data_types)) != 0) {
      y_data_types <- data_types[grep("y", data_types) + 1]
      if(y_data_types == "string" || y_data_types == "boolean") {
        isDiscrete <- TRUE
        isDiscreteY <- TRUE
      }# else if(collect(select(data, countDistinct(data$y)))[[1]] < 7) {
       # isDiscrete <- TRUE
       # isDiscreteY <- TRUE
      #}
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

add_group.SparkR <- function(data) {
  discrete_col <- append(isDiscrete(data), "PANEL")
  disc <- distinct(select(data, as.list(discrete_col)))
  complete_col <- "group"

  for(index in 1:length(discrete_col)) {
    temp_df <- select(disc, eval(discrete_col[index]))
    type_disc <- unlist(dtypes(temp_df))[2]
    temp_df <- bindIDs(temp_df)

    temp_df <- withColumn(temp_df, paste0(eval(discrete_col[index]), "_OLD"), cast(temp_df$"_1", eval(type_disc)))
    temp_df <- withColumn(temp_df, "group_id", cast(temp_df$"_2", "integer"))
    temp_df <- select(temp_df, paste0(eval(discrete_col[index]), "_OLD"), "group_id")
    complete_col <- append(complete_col, paste0(discrete_col[index], "_OLD"))

    if(index == 1) {
      joined <- temp_df
      joined <- withColumnRenamed(joined, "group_id", "group")
    }
    else {
      joined <- SparkR::join(joined, temp_df, joined$group == temp_df$group_id)
    }
    joined <- select(joined, as.list(complete_col))
  }

  joined_cmd <- 'SparkR::join(data, joined, data$PANEL == joined$PANEL_OLD'
  
  for(col in discrete_col[discrete_col != "PANEL"]) {
    joined_cmd <- paste0(joined_cmd, ' & data$', col, ' == joined$', col, "_OLD", sep="")
  }

  joined_cmd <- paste0(joined_cmd, ")")
  columns_data <- columns(data)
  joined <- eval(parse(text = joined_cmd))
  
  group <- select(joined, append(as.list(columns_data), "group"))
  group
}

order_groups <- function(data) {
  if (is.null(data$order)) return(data)

  data[order(data$order), ]
}
