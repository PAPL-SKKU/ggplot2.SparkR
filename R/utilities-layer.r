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

add.SparkR_group <- function(data) {
  select_cmd <- 'select(data, "PANEL"'
  filter_cmd <- 'filter(data, data[["PANEL"]] == disc[["PANEL"]][index]'

  for(data_types in dtypes(data)) {
    if(data_types[2] == "string") {
      select_cmd <- paste(select_cmd, ', "', data_types[1], '"', sep="")
      filter_cmd <- paste(filter_cmd, ' & data[["', data_types[1], '"]] == disc[["', data_types[1], '"]][index]', sep="")
    }
  }
  select_cmd <- paste(select_cmd, ")")
  filter_cmd <- paste(filter_cmd, ")")

  disc <- collect(distinct(eval(parse(text = select_cmd))))
 
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
