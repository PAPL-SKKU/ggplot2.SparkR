# ggplot2.SparkR

ggplot2.SparkR is an R package for scalable visualization of big data represented in Spark DataFrame. ggplot2.sparkR is an etension to the original ggplot2 package and can seamlessly handle both R data.frame and Spark DataFrame with no modifications to the original API.

ggplot2.SparkR requires no additional training for existing R users who are already familiar with ggplot2 and allows them to benefit from powerful distributed processing capabilities of Spark for efficient visualization of big data. 

Until now, 5 graph types (bar, bin2d, boxplot, histogram, stat-sum graphs) and 8 options (facet-grid, facet-wrap, coord-flip, scale-x-log10, scale-y-log10, position-fill, position-stack, position-dodge) are supported. We plans to further extend it in the future.

## Installation

Get the released version from CRAN: //after release

```R
install.packages("ggplot2.sparkR")
```

Or the development version from github: //after release

```R
# install.packages("devtools")
devtools::install_github("PAPL-SKKU/ggplot2.sparkR")
```
