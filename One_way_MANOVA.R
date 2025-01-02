#' Perform One-Way MANOVA
#'
#' This function performs a one-way MANOVA for a specified grouping variable and response variables.
#'
#' @param data A data frame containing the dataset.
#' @param group_col A character string specifying the name of the grouping variable.
#' @param response_cols A character vector of response variable column names.
#' @return An object of class "manova" containing the MANOVA results.
#' @examples
#' one_way_manova(iris, group_col = "Species", response_cols = c("Sepal.Length", "Sepal.Width"))
one_way_manova <- function(data, group_col, response_cols) {
  formula <- as.formula(paste0("cbind(", paste(response_cols, collapse = ","), ") ~ ", group_col))
  manova_result <- manova(formula, data = data)
  return(manova_result)
}
