#' Perform Two-Way MANOVA
#'
#' This function performs a two-way MANOVA for specified grouping variables and response variables.
#'
#' @param data A data frame containing the dataset.
#' @param group_cols A character vector specifying the names of the two grouping variables.
#' @param response_cols A character vector of response variable column names.
#' @return An object of class "manova" containing the MANOVA results.
#' @examples
#' two_way_manova(mock_data, group_cols = c("Factor1", "Factor2"), response_cols = c("Response1", "Response2"))
two_way_manova <- function(data, group_cols, response_cols) {
  if (length(group_cols) != 2) {
    stop("Two grouping variables must be specified for a two-way MANOVA.")
  }
  formula <- as.formula(paste0("cbind(", paste(response_cols, collapse = ","), ") ~ ", paste(group_cols, collapse = "*")))
  manova_result <- manova(formula, data = data)
  return(manova_result)
}
