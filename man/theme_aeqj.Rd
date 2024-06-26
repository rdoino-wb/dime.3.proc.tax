% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_theme_ggplots.R
\name{theme_aeqj}
\alias{theme_aeqj}
\title{Customize ggplot2 Theme}
\usage{
theme_aeqj()
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
