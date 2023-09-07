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
set_theme <- function(

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
    axis_title_y_blank = FALSE, # to fully left-align
    aspect_ratio = NULL,
    plot_caption_position = "plot",
    axis_line_x  = element_blank(),
    axis_line_y = element_blank(),
    axis_text_x = element_markdown(size = x_text_size, color = y_text_color, family="LM Roman 10"),
    axis_text_y = element_markdown(size = y_text_size, color = y_text_color, family="LM Roman 10"),
    legend_box_background = element_rect(fill = "white", color = "black")

) {

  # Size
  size_ <- str_c("size = ", size) # this argument always included

  if (is.na(y_title_size)) {
    y_title <- "element_blank()"
  } else {

    # y-title margin
    if (!is.null(y_title_margin)) y_title_margin_ <- str_c("margin = margin(", y_title_margin, ")")
    else y_title_margin_ <- ""


    # y-title color
    if (!is.null(y_title_color)) y_title_color_ <- str_c("color = '", y_title_color, "'")
    else y_title_color_ <- ""

    # create y_title
    y_title <- str_c("element_text(", size_, ",", y_title_margin_, ",", y_title_color_, ")")

  }
  if (is.na(x_title_size)) {
    x_title <- "element_blank()"
  }
  else {
    # x-title margin
    if (!is.null(x_title_margin)) x_title_margin_ <- str_c("margin = margin(", x_title_margin, ")")
    else x_title_margin_ <- ""

    # x-title color
    if (!is.null(x_title_color)) x_title_color_ <- str_c("color = '", x_title_color, "'")
    else x_title_color_ <- ""

    # create x_title
    x_title <- str_c("element_text(", size_, ",", x_title_margin_, ",", x_title_color_, ")")
  }

  if (axis_title_y_blank) {
    y_title <- "element_blank()" # overwrite what it was written as above
  }

  # Legend key
  if (is.na(legend_key_fill) & is.na(legend_key_color)) {
    legend_key <- "element_blank()"
  } else {
    if (is.na(legend_key_fill) & !(is.na(legend_key_color))) {
      legend_key <- str_c("element_rect(",
                          "fill = NA", ",",
                          "color = '", legend_key_color, "'", ", ",
                          ")"
      )
    } else if (!(is.na(legend_key_fill)) & is.na(legend_key_color)) {
      legend_key <- str_c("element_rect(",
                          "fill = '", legend_key_fill, "'", ", ",
                          "color = NA",
                          ")"
      )
    } else { # neither missing
      legend_key <- str_c("element_rect(",
                          "fill = '", legend_key_fill, "'", ", ",
                          "color = '", legend_key_color, "'",
                          ")"
      )
    }
  }

  theme + theme(
    plot.title =  element_text(hjust = -0.05, size = size, color = y_text_color, family="LM Roman 10"),
    axis.title.y = eval(parse(text = y_title)),
    axis.title.x = eval(parse(text = x_title)),
    axis.text.y = axis_text_y,
    axis.text.x = axis_text_x,
    axis.line.x = axis_line_x , # manual axes
    axis.line.y = axis_line_y,
    legend.key = eval(parse(text = legend_key)),
    legend.text = element_text(size = legend_text_size, family="LM Roman 10"),
    legend.title = element_text(size = legend_text_size, family="LM Roman 10"),
    aspect.ratio = aspect_ratio,
    plot.margin = plot_margin,
    legend.position = legend_position,
    plot.caption.position = plot_caption_position,
    plot.caption = element_text(size = 12, color = "gray50", family="LM Roman 10"),
    text         = element_text(family="LM Roman 10", color = "black"),
    axis.ticks.length = ggplot2::unit(.25, "cm"),
    legend.box.background = legend_box_background
  )

}
