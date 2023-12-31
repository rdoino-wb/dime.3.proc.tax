% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_theme_ggplots.R
\name{set_theme}
\alias{set_theme}
\title{Customize ggplot2 Theme}
\usage{
set_theme(
  theme = theme_classic(),
  size = 16,
  title_hjust = 0.5,
  y_title_size = size,
  x_title_size = size,
  y_title_margin = NULL,
  x_title_margin = NULL,
  y_text_size = size,
  x_text_size = size,
  y_text_color = "black",
  x_text_color = "black",
  y_title_color = "black",
  x_title_color = "black",
  legend_text_size = size,
  legend_position = "none",
  legend_key_fill = NA,
  legend_key_color = NA,
  plot_title_position = NULL,
  plot_margin = ggplot2::margin(25, 25, 10, 25),
  axis_title_y_blank = FALSE,
  aspect_ratio = NULL,
  plot_caption_position = "plot",
  axis_line_x = element_blank(),
  axis_line_y = element_blank(),
  axis_text_x = ggtext::element_markdown(size = x_text_size, color = y_text_color, family
    = "LM Roman 10"),
  axis_text_y = ggtext::element_markdown(size = y_text_size, color = y_text_color, family
    = "LM Roman 10"),
  legend_box_background = element_rect(fill = "white", color = "black")
)
}
\arguments{
\item{theme}{ggplot2 theme object, default is theme_classic().}

\item{size}{Numeric, base size for texts, default is 16.}

\item{title_hjust}{Numeric, horizontal justification for title, default is 0.5.}

\item{y_title_size}{Numeric, size for y-axis title, default inherits from 'size'.}

\item{x_title_size}{Numeric, size for x-axis title, default inherits from 'size'.}

\item{y_title_margin}{Margin for y-axis title, default is NULL.}

\item{x_title_margin}{Margin for x-axis title, default is NULL.}

\item{y_text_size}{Numeric, size for y-axis text, default inherits from 'size'.}

\item{x_text_size}{Numeric, size for x-axis text, default inherits from 'size'.}

\item{y_text_color}{Character, color for y-axis text, default is "black".}

\item{x_text_color}{Character, color for x-axis text, default is "black".}

\item{y_title_color}{Character, color for y-axis title, default is "black".}

\item{x_title_color}{Character, color for x-axis title, default is "black".}

\item{legend_text_size}{Numeric, size for legend text, default inherits from 'size'.}

\item{legend_position}{Character, position for legend, default is "none".}

\item{legend_key_fill}{Fill color for legend key, default is NA.}

\item{legend_key_color}{Color for legend key, default is NA.}

\item{plot_title_position}{Position for plot title, default is NULL.}

\item{plot_margin}{Margin for plot, default is margin(25, 25, 10, 25).}

\item{axis_title_y_blank}{Logical, whether to blank y-axis title for full left-alignment, default is FALSE.}

\item{aspect_ratio}{Numeric, aspect ratio for plot, default is NULL.}

\item{plot_caption_position}{Position for plot caption, default is "plot".}

\item{axis_line_x}{ggplot2 element for x-axis line, default is element_blank().}

\item{axis_line_y}{ggplot2 element for y-axis line, default is element_blank().}

\item{axis_text_x}{ggplot2 element for x-axis text, default is element_markdown().}

\item{axis_text_y}{ggplot2 element for y-axis text, default is element_markdown().}

\item{legend_box_background}{ggplot2 element for legend box background, default is element_rect().}
}
\value{
Modified ggplot2 theme object.
}
\description{
This function allows for extensive customization of ggplot2 themes.
You can specify multiple parameters like text size, color, axis titles,
and much more to tailor the plot appearance.
}
\examples{
\dontrun{
library(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p + set_theme(size = 14)
}
}
