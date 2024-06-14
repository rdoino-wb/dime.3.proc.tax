#' Customize ggplot2 Theme
#'
#' This function allows for extensive customization of ggplot2 themes.
#' You can specify multiple parameters like text size, color, axis titles,
#' and much more to tailor the plot appearance.
#'
#' @param theme ggplot2 theme object, default is theme_classic().
#' @param size Numeric, base size for texts, default is 16.
#' @param title_hjust Numeric, horizontal justification for title, default is 0.5.
#' @param y_title_size Numeric, size for y-axis title, default inherits from 'size'.
#' @param x_title_size Numeric, size for x-axis title, default inherits from 'size'.
#' @param y_title_margin Margin for y-axis title, default is NULL.
#' @param x_title_margin Margin for x-axis title, default is NULL.
#' @param y_text_size Numeric, size for y-axis text, default inherits from 'size'.
#' @param x_text_size Numeric, size for x-axis text, default inherits from 'size'.
#' @param y_text_color Character, color for y-axis text, default is "black".
#' @param x_text_color Character, color for x-axis text, default is "black".
#' @param y_title_color Character, color for y-axis title, default is "black".
#' @param x_title_color Character, color for x-axis title, default is "black".
#' @param legend_text_size Numeric, size for legend text, default inherits from 'size'.
#' @param legend_position Character, position for legend, default is "none".
#' @param legend_key_fill Fill color for legend key, default is NA.
#' @param legend_key_color Color for legend key, default is NA.
#' @param plot_title_position Position for plot title, default is NULL.
#' @param plot_margin Margin for plot, default is margin(25, 25, 10, 25).
#' @param axis_title_y_blank Logical, whether to blank y-axis title for full left-alignment, default is FALSE.
#' @param aspect_ratio Numeric, aspect ratio for plot, default is NULL.
#' @param plot_caption_position Position for plot caption, default is "plot".
#' @param axis_line_x ggplot2 element for x-axis line, default is element_blank().
#' @param axis_line_y ggplot2 element for y-axis line, default is element_blank().
#' @param axis_text_x ggplot2 element for x-axis text, default is element_markdown().
#' @param axis_text_y ggplot2 element for y-axis text, default is element_markdown().
#' @param legend_box_background ggplot2 element for legend box background, default is element_rect().
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

}
