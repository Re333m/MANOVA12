#' Compute Within-Group and Between-Group Matrices
#'
#' This function calculates the within-group (W) and between-group (B) matrices for a dataset.
#'
#' @param data A data frame containing the dataset.
#' @param group_col A character string specifying the name of the grouping variable.
#' @param response_cols A character vector of response variable column names.
#' @return A list with the W and B matrices.
#' @examples
#' compute_wb_matrices(iris, group_col = "Species", response_cols = c("Sepal.Length", "Sepal.Width"))
compute_wb_matrices <- function(data, group_col, response_cols) {
  groups <- unique(data[[group_col]])
  response_data <- data[response_cols]

  overall_mean <- colMeans(response_data)
  W <- matrix(0, nrow = length(response_cols), ncol = length(response_cols))
  B <- matrix(0, nrow = length(response_cols), ncol = length(response_cols))

  for (group in groups) {
    group_data <- response_data[data[[group_col]] == group, ]
    group_mean <- colMeans(group_data)
    W <- W + t(group_data - group_mean) %*% (group_data - group_mean)
    B <- B + nrow(group_data) * (group_mean - overall_mean) %*% t(group_mean - overall_mean)
  }

  return(list(W = W, B = B))
}
