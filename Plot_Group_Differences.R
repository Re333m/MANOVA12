#' Plot Group Differences
#'
#' This function creates boxplots or scatter plots to visualize group differences
#' for the response variables in a MANOVA analysis.
#'
#' @param data A data frame containing the dataset.
#' @param group_col A character string specifying the name of the grouping variable.
#' @param response_cols A character vector of response variable column names.
#' @param plot_type A character string specifying the type of plot ("boxplot" or "scatter").
#' @return A ggplot object visualizing group differences.
#' @examples
#' plot_group_differences(iris, group_col = "Species", response_cols = c("Sepal.Length"))
plot_group_differences <- function(data, group_col, response_cols, plot_type = "boxplot") {
  library(ggplot2)

  if (plot_type == "boxplot") {
    p <- ggplot(data, aes_string(x = group_col)) +
      geom_boxplot(aes_string(y = response_cols[1]), fill = "skyblue", alpha = 0.7) +
      labs(title = paste("Boxplot of", response_cols[1], "by", group_col),
           x = group_col, y = response_cols[1]) +
      theme_minimal()
  } else if (plot_type == "scatter" && length(response_cols) > 1) {
    p <- ggplot(data, aes_string(x = response_cols[1], y = response_cols[2], color = group_col)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(title = paste("Scatter Plot of", response_cols[1], "vs", response_cols[2]),
           x = response_cols[1], y = response_cols[2]) +
      theme_minimal()
  } else {
    stop("Invalid plot type or insufficient response variables for scatter plot.")
  }

  return(p)
}
