#' Customize ggplot2 Theme
#'
#' This function allows for extensive customization of ggplot2 themes.
#' You can specify multiple parameters like text size, color, axis titles,
#' and much more to tailor the plot appearance.
#'
#' @return Modified ggplot2 theme object.
#' @export
#'
#' @importFrom ggplot2 theme_classic element_text element_blank element_rect theme
#' @importFrom ggtext element_markdown
#' @importFrom stringr str_c
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + set_theme(size = 14)
#' }

theme_aeqj <- function() {

    theme_minimal(base_family = "LM Roman 10", base_size = 20) +

      theme(

        # Text Size
        text = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 22, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "italic"),
        plot.title.position = 'plot',

        # Grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),

        # Axis lines
        axis.line = element_line(color = "black", size = .6),
        axis.ticks = element_line(color = "black", size = .6),

        # Legend
        legend.position = "none",

        # Background
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),

        # Margins
        plot.margin = margin(2, 2, 1, 2)

      )

  }
