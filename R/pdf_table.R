#' pdf_table
#'
#' @description
#' This function produces a .tex file for tables and print a pdf of the table (usually regression or summary tables)
#'
#' @param table This is the table we want to save
#' @param file_name This is the name of the file that we want to create
#'
#' @importFrom tinytex pdflatex
#' @importFrom magick image_read_pdf
#' @importFrom magick image_convert
#'
#' @export
pdf_table <- function(table, file_name) {

  if (substr(file_name, nchar(file_name) - 3, nchar(file_name)) != ".tex") {

    stop("file_name needs to be in a .tex extension!")

  }

  if (file.exists(file_name)) {

    unlink(file_name)

  }

  for (j in 1:length(table)) { # loop over the length of each table

    if (length(grep("caption", (table)[j]) == 1) |
        length(grep("Note", (table)[j]) == 1)) {

      print("Caption deleted")

    } else {

      if (j == 1) {

        cat("\\documentclass[border=10pt]{standalone}", "\n", file = file_name, append = "TRUE")
        cat("\\usepackage{varwidth}", "\n", file = file_name, append = "TRUE")
        cat("\\usepackage{amssymb}", "\n", file = file_name, append = "TRUE")
        cat("\\newcommand{\\cmark}{\\ding{51}}", "\n", file = file_name, append = "TRUE")
        cat("\\newcommand{\\xmark}{\\ding{55}}", "\n", file = file_name, append = "TRUE")
        cat("\\usepackage{pifont}", "\n", file = file_name, append = "TRUE")
        cat("\\usepackage{booktabs}", "\n", file = file_name, append = "TRUE")

        cat("\\begin{document}", "\n", file = file_name, append = "TRUE")
        cat("\\begin{varwidth}{2000pt}", "\n", file = file_name, append = "TRUE")

      }

      cat((table)[j], "\n", file = file_name, append = "TRUE")

      if (j == length(table)) {

        cat("\\end{varwidth}", "\n", file = file_name, append = "TRUE")
        cat("\\end{document}", "\n", file = file_name, append = "TRUE")

      }
    }

  }

  tinytex::pdflatex(
    file_name
  )

  if (file.exists(file_name)) {

    unlink(file_name)

  }

  for (j in 3:(length(table) - 1)) { # loop over the length of each table

      cat((table)[j], "\n", file = file_name, append = "TRUE")

  }

  # Convert PDF to PNG
  image <- image_read_pdf(file_name, density = 300)
  image <- image_convert(image, format = "png")

  # Save the PNG image
  image_write(image, paste0(substr("table.tex", 0, nchar("table.tex") - 3)), ".png")

}
