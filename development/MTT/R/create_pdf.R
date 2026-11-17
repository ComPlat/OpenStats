report_plots <- function(ic50List) {
  p3 <- ggdraw() +
    draw_line(x = c(0, 1), y = c(0.5, 0.5), color = "black", size = 1) +
    theme_void()
  for(i in seq_along(ic50List)) {
    if(is(ic50List[[i]], "errorClass")) {
      p <- ic50List[[i]]$object
      p <- p + 
        annotate("text", x = -Inf, y = -Inf,
                 hjust = -0.2, vjust = -1, label = ic50List[[i]]$error_message)
      print(p)
      print(p3)
      next
    }
    p1 <- ic50List[[i]][[2]]
    a <- ic50List[[i]][[1]]
    a <- a |> t() |> as.data.frame()
    a <- data.frame(names = row.names(a), Predicition = a)
    a[a$names == "Response_lowestdose_predicted", 1] <- "Response_lowestdose"
    a[a$names == "Response_highestdose_predicted", 1] <- "Response_highestdose"
    problem <- a[a$names == "Problems", 2]
    a <- a[(a$names != "Problems") & (a$names != "name"), ]

    # Generate horizontal line positions between rows
    line_df <- data.frame(y = seq_len(nrow(a)) - 0.5)

    p2 <- ggplot(a, aes(x = 0, y = factor(names), label = Prediction)) +
      # geom_hline(data = line_df, aes(yintercept = y), color = "grey80", inherit.aes = FALSE) +
      geom_line(size = 0) +
      geom_text(position = position_nudge(x = -1.1), hjust = 0, size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(hjust = 0, face = "bold"),
        axis.line.y = element_line(),
        plot.caption = element_text(hjust = 1, face = "italic", colour = "darkred", 
          size = 7))


    # p2 <- ggplot(a, aes(x = 0, y = factor(names), label = Prediction)) +
    #   geom_line(size = 0) +
    #   geom_text(position = position_nudge(x = -1.1), hjust = 0, size = 3) +
    #   theme_minimal() +
    #   theme(axis.text.x = element_blank(),
    #         axis.ticks.x = element_blank(),
    #         panel.grid.major.x = element_blank(),
    #         panel.grid.minor.x = element_blank(),
    #         panel.grid.major.y = element_blank(),
    #         panel.grid.minor.y = element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.title.y = element_blank(),
    #         axis.text.y = element_text(hjust = 0, face = "bold"),
    #         axis.line.y = element_line(),
    #         plot.caption = element_text(hjust = 1, face = "italic", colour = "darkred", 
    #                                     size = 7) ) 
    if(problem != "") {
      p2 <- p2 + labs(caption = paste("Note:", as.character(problem)) )
    }

    bar <- ggplot() +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
        fill = "black") +
      theme_void()

    p <- plot_grid(p2, p1, bar, rel_heights = c(0.75, 1, 0.0025), ncol = 1)
    print(p)

  }
}

#' Creates report file
#'
#' @export
#' @import openxlsx
#' @import ggplot2
#' @param ic_50List the list result of a call to ic50.
#' @param text the variable text which has to be inserted in the document
#' @param file_name the name of the excel file where the results should be saved.
#' @examples
#' path <- system.file("data", package = "MTT")
#' df <- read.csv(paste0(path, "/ExampleData.txt"))
#' res <- ic50(df, "abs", "conc", "names", "neg", "pos")
#' report_pdf(res, "blabla", "report.pdf")
report_pdf <- function(ic_50List, text, file_name) {
  library(quarto)
  file <- tempfile()
  saveRDS(list(ic_50List, text),
          file =  file)
  path <- system.file("examples", package = "MTT")
  quarto::quarto_render(paste0(path, "/template_pdf.qmd"),
                        execute_params = list(file = file), output_file = file_name)
}

