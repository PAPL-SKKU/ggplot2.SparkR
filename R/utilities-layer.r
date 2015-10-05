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

make_group.SparkR <- function(data) {
  discrete_col <- append(isDiscrete(data), "PANEL")
  disc <- distinct(select(data, as.list(discrete_col)))
  complete_col <- "group"

  for(index in 1:length(discrete_col)) {
    temp_df <- select(disc, eval(discrete_col[index]))
    type_disc <- unlist(dtypes(temp_df))[2]
    temp_df <- bindIDs(temp_df)

    temp_df <- withColumn(temp_df, eval(discrete_col[index]), cast(temp_df$"_1", eval(type_disc)))
    temp_df <- withColumn(temp_df, "group_id", cast(temp_df$"_2", "integer"))
    temp_df <- select(temp_df, eval(discrete_col[index]), "group_id")
    complete_col <- append(complete_col, discrete_col[index])

    if(index == 1) {
      joined <- temp_df
      joined <- withColumnRenamed(joined, "group_id", "group")
    }
    else {
      joined <- SparkR::join(joined, temp_df, joined$group == temp_df$group_id, "inner")
    }
    joined <- select(joined, as.list(complete_col))
  }

  joined
}

add_group.SparkR <- function(data, group) {
  column_list <- as.list(append(columns(data), "group"))
  group <- SparkR::rename(group, PANEL_OLD = group$PANEL)

  if(length(grep("x", columns(group)))) {
    group <- SparkR::rename(group, x_OLD = group$x)
    if(length(grep("fill", columns(group)))) {
      group <- SparkR::rename(group, fill_OLD = group$fill)
      joined <- SparkR::join(data, group, data$x == group$x_OLD & data$PANEL == group$PANEL_OLD &
                                          data$fill == group$fill_OLD, "inner")
    } else {
      joined <- SparkR::join(data, group, data$x == group$x_OLD & data$PANEL == group$PANEL_OLD, "inner") 
    }
  } else {
    if(length(grep("fill", columns(group)))) {
      group <- SparkR::rename(group, fill_OLD = group$fill)
      joined <- SparkR::join(data, group, data$PANEL == group$PANEL_OLD & data$fill == group$fill_OLD, "inner")
    } else {
      joined <- SparkR::join(data, group, data$PANEL == group$PANEL_OLD, "inner")
    }
  }

  data <- select(joined, column_list)
  data
}

order_groups <- function(data) {
  if (is.null(data$order)) return(data)

  data[order(data$order), ]
}
