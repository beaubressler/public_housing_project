####
# Table Utilities
# Helper functions for formatting and processing tables
####

#' Remove table wrappers from saved LaTeX files
#'
#' This function processes saved .tex files to remove table environment wrappers
#' (table, centering), making them compatible with manual threeparttable wrappers
#' in LaTeX documents. Use this after save_tt() when you want to manually control
#' the table environment in your .tex file.
#'
#' @param filepath Path to the .tex file to process
#'
#' @examples
#' # After saving with tinytable:
#' save_tt(my_table, "output/my_table.tex")
#' remove_table_wrappers("output/my_table.tex")
#'
remove_table_wrappers <- function(filepath) {
  tex_content <- readLines(filepath)

  # Remove table environment wrappers
  tex_content <- tex_content[!grepl("\\\\begin\\{table\\}", tex_content)]
  tex_content <- tex_content[!grepl("\\\\end\\{table\\}", tex_content)]
  tex_content <- tex_content[!grepl("\\\\centering", tex_content)]

  writeLines(tex_content, filepath)

  invisible(filepath)
}
