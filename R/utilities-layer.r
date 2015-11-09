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

    if(isDiscrete && isDiscreteY) {
      column_arr <- c("x", "y")
    } else if(isDiscrete) {
      column_arr <- c("x")
    } else {
      column_arr <- c()
    }
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
  disc <- map_position.SparkR(list(disc))[[1]]
  complete_col <- "group"

  for(index in 1:length(discrete_col)) {
    temp_df <- select(disc, discrete_col[index])
    type_disc <- unlist(dtypes(temp_df))[2]
    temp_df <- bindIDs(temp_df)

    temp_df <- withColumn(temp_df, discrete_col[index], cast(temp_df$"_1", eval(type_disc)))

    temp_df <- withColumn(temp_df, "group_id", cast(temp_df$"_2", "integer"))
    temp_df <- select(temp_df, discrete_col[index], "group_id")
    complete_col <- append(complete_col, discrete_col[index])
    
    if(index == 1) {
      joined <- withColumnRenamed(temp_df, "group_id", "group")
    } else {
      joined <- SparkR::join(joined, temp_df, joined$group == temp_df$group_id, "inner")
    }
    
    joined <- select(joined, as.list(complete_col))
  }
  
  joined
}

add_group.SparkR <- function(data, group, plot) {
  if(plot$layers[[1]]$geom$objname != "bin2d") {
    column_list <- as.list(append(columns(data), "group"))
    group <- SparkR::rename(group, PANEL_OLD = group$PANEL)
    joined_cmd <- "data$PANEL == group$PANEL_OLD"

    if(length(grep("x", columns(group)))) {
      group <- SparkR::rename(group, x_OLD = group$x)
      joined_cmd <- paste0(joined_cmd, " & data$x == group$x_OLD")
    }

    if(length(grep("y", columns(group)))) {
      group <- SparkR::rename(group, y_OLD = group$y)
      joined_cmd <- paste0(joined_cmd, " & data$y == group$y_OLD")
    }

    if(length(grep("fill", columns(group)))) {
      group <- SparkR::rename(group, fill_OLD = group$fill)
      joined_cmd <- paste0(joined_cmd, " & data$fill == group$fill_OLD")
    }

    joined <- SparkR::join(data, group, eval(parse(text = joined_cmd)), "inner")
    data <- select(joined, column_list)
 
    if(length(grep("fill", columns(group)))) {
      data <- SparkR::arrange(data, "x", "fill")
    }
  } 
  
  data
}

order_groups <- function(data) {
  if (is.null(data$order)) return(data)

  data[order(data$order), ]
}
