#' Model Summary in LaTeX Format
#'
#' This function creates a summary of a regression model and outputs it in LaTeX format.
#' The summary includes various statistics like \( R^2 \), adjusted \( R^2 \), and others.
#'
#' @param reg Regression object for which summary is generated.
#' @param output Output format, although the function currently only supports LaTeX.
#' @param list_names List of coefficient names to rename in the summary.
#' @param notes Notes to include at the bottom of the table.
#' @param rows Additional rows to include in the summary table.
#' @param coef_omit_list List of coefficients to omit from the summary.
#'
#' @return A LaTeX-formatted summary of the regression model.
#' @export
#'
#' @importFrom tibble tribble
#' @importFrom modelsummary modelsummary
#'
#' @examples
#' \dontrun{
#' # Assuming 'model' is a regression object
#' model_summary(model, "latex", list_names = c("Intercept" = "Constant"),
#'               notes = "Standard errors in parentheses.",
#'               rows = NULL, coef_omit_list = NULL)
#' }
model_summary <- function(reg, output, list_names, notes, rows, coef_omit_list) {

  # we save the output in latex
  gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "Mean of DV","Mean of Dep Var", 2,
    "r.squared", "R^{2}", 2,
    "adj.r.squared", "Adj R^{2}", 2
  )

  f <- function(x) formatC(x, digits = 3, big.mark = ",", format = "f")

  model = modelsummary::modelsummary(
    reg,
    coef_rename = list_names,
    omit = coef_omit_list,
    fmt = f,
    estimate = "{round(estimate, 3)}{stars}",
    statistic = "({(round(std.error, 3))})",
    stars = TRUE,
    gof_map = gm,
    add_rows = rows,
    escape = FALSE,
    notes = notes)

}
