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
#'
#' @examples
#' \dontrun{
#' # Assuming 'model' is a regression object
#' model_summary(model, "latex", list_names = c("Intercept" = "Constant"),
#'               notes = "Standard errors in parentheses.",
#'               rows = NULL, coef_omit_list = NULL)
#' }
model_summary <- function(reg, output, notes, coef_map) {

  if (output == "gt") {

    # we save the output in latex
    gm <- tibble::tribble(
      ~raw,        ~clean,          ~fmt,
      "nobs",      "N",             0,
      "Mean of DV","Mean of Dep Var", 2,
      "r.squared", "R^{2}", 2,
      "adj.r.squared", "Adj R^{2}", 2
    )

    format_bigmark <- function(x, digits = 3) {
      format(round(x, digits), big.mark = ",", scientific = FALSE)
    }

    gm$fmt <- list(
      function(x) format_bigmark(x, 0),
      function(x) format_bigmark(x, 2),
      function(x) format_bigmark(x, 2),
      function(x) format_bigmark(x, 2)
    )

    modelsummary::modelsummary(
      reg,
      coef_map = coef_map,
      fmt = NULL,
      estimate = "{format_bigmark(estimate)}{stars}",
      statistic = "({(format_bigmark(std.error))})",
      output = "gt",
      stars = TRUE,
      gof_map = gm,
      escape = FALSE,
      notes = notes)

  } else {

    # we save the output in latex
    gm <- tibble::tribble(
      ~raw,        ~clean,          ~fmt,
      "nobs",      "N",             0,
      "Mean of DV","Mean of Dep Var", 2,
      "r.squared", "R^{2}", 2,
      "adj.r.squared", "Adj R^{2}", 2
    )

    format_bigmark <- function(x, digits = 3) {
      format(round(x, digits), big.mark = ",", scientific = FALSE)
    }

    gm$fmt <- list(
      function(x) format_bigmark(x, 0),
      function(x) format_bigmark(x, 2),
      function(x) format_bigmark(x, 2),
      function(x) format_bigmark(x, 2)
    )

    modelsummary::modelsummary(
      reg,
      coef_map = coef_map,
      fmt = NULL,
      estimate = "{format_bigmark(estimate)}{stars}",
      statistic = "({(format_bigmark(std.error))})",
      output = file.path(table_output, output),
      stars = TRUE,
      gof_map = gm,
      escape = FALSE,
      notes = notes)

  }

}
