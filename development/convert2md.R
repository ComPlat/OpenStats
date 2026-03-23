files <- list.files("./OpenStats/inst/www", full.names = TRUE, pattern = "*.html")
files
output_dir <- "./development/docu/"

convert_html_to_md <- function(input_file, output_dir) {
  output_file <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(basename(input_file)), ".md")
  )

  cmd <- sprintf(
    'pandoc -f html -t markdown "%s" -o "%s"',
    input_file, output_file
  )

  system(cmd)
  message("Converted: ", input_file, " -> ", output_file)
}

invisible(lapply(files, convert_html_to_md, output_dir = output_dir))

quarto::quarto_preview(file = "./development/docu/assumptions.md")
