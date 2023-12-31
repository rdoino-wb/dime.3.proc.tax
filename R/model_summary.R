#' Model Summary in LaTeX Format
#'
#' This function creates a summary of a regression model and outputs it in LaTeX format.
#' The summary includes various statistics like \( R^2 \), adjusted \( R^2 \), and others.
#'
#' @param reg Regression object for which summary is generated.
#' @param output Output format, although the function currently only supports LaTeX.
#' @param notes Notes to include at the bottom of the table.
#' @param coef_map List of coefficients to omit from the summary.
#'
#' @return A LaTeX-formatted summary of the regression model.
#' @export
#'
#' @importFrom tibble tribble
#' @importFrom modelsummary modelsummary
#' @importFrom format_bigmark dime.3.proc.tax
#'
#' @examples
#' \dontrun{
#' # Assuming 'model' is a regression object
#' model_summary(model, "latex", list_names = c("Intercept" = "Constant"),
#'               notes = "Standard errors in parentheses.",
#'               rows = NULL, coef_omit_list = NULL)
#' }
model_summary <- function (reg, output, notes, coef_map)
{

  gm <- tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N", 0,
    "Mean of DV", "Mean of Dep Var", 2,
    "r.squared", "R^{2}", 2,
    "adj.r.squared", "Adj R^{2}", 2
  )

  gm$fmt <- list(
    function(x) dime.3.proc.tax::format_bigmark(x, 0),
    function(x) dime.3.proc.tax::format_bigmark(x, 2),
    function(x) dime.3.proc.tax::format_bigmark(x, 2),
    function(x) dime.3.proc.tax::format_bigmark(x, 2)
  )

  if (output == "gt") {
    modelsummary::modelsummary(
      reg, coef_map = coef_map,
      fmt = NULL, estimate = "{dime.3.proc.tax::format_bigmark(estimate)}{stars}",
      statistic = "({(dime.3.proc.tax::format_bigmark(std.error))})",
      output = "gt", stars = TRUE, gof_map = gm, escape = FALSE,
      notes = notes
    )
  } else {
    modelsummary::modelsummary(
      reg, coef_map = coef_map,
      fmt = NULL, estimate = "{dime.3.proc.tax::format_bigmark(estimate)}{stars}",
      statistic = "({(dime.3.proc.tax::format_bigmark(std.error))})",
      output = file.path(table_output, output),
      stars = TRUE, gof_map = gm, escape = FALSE, notes = notes
    )
  }
}

