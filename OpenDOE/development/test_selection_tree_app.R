options(shiny.launch.browser = TRUE)

tree <- list(
  type = "question", label = "Anzahl Levels?",
  children = list(
    list(
      type = "answer", label = "1–2",
      children = list(
        list(
          type = "question", label = "Normalverteilt?",
          children = list(
            list(
              type = "answer", label = "ja",
              children = list(
                list(
                  type = "question", label = "Varianzen homogen?",
                  children = list(
                    list(type = "answer", label = "ja", children = list(
                      list(type = "leaf", label = "t-Test\n(Student)", status = "missing",
                        doc = "<b>t-Test (Student)</b><br>Fehlt aktuell. estimate_sample_size() hat var.equal fest auf FALSE (= Welch) verdrahtet - kein Pfad für gleiche Varianzen.")
                    )),
                    list(type = "answer", label = "nein", children = list(
                      list(type = "leaf", label = "Welch\nt-Test", status = "implemented",
                        doc = "<b>Welch t-Test</b><br>estimate_sample_size() - aktuell der einzige t-Test-Pfad (var.equal=FALSE hardcoded).")
                    ))
                  )
                )
              )
            ),
            list(
              type = "answer", label = "nein",
              children = list(
                list(type = "leaf", label = "Wilcoxon\nRangsummentest", status = "implemented",
                  doc = "<b>Wilcoxon Rangsummentest</b><br>estimate_sample_size_wilcoxon()")
              )
            )
          )
        )
      )
    ),
    list(
      type = "answer", label = ">2",
      children = list(
        list(
          type = "question", label = "Ziel?",
          children = list(
            list(
              type = "answer", label = "Omnibus-Test",
              children = list(
                list(
                  type = "question", label = "Normalverteilt?",
                  children = list(
                    list(
                      type = "answer", label = "ja",
                      children = list(
                        list(
                          type = "question", label = "Varianzen homogen?",
                          children = list(
                            list(type = "answer", label = "ja", children = list(
                              list(type = "leaf", label = "ANOVA\n(Student)", status = "implemented",
                                doc = "<b>ANOVA (Student)</b><br>determine_sample_size() - nutzt aov(), setzt gleiche Varianzen voraus.")
                            )),
                            list(type = "answer", label = "nein", children = list(
                              list(type = "leaf", label = "Welch-\nANOVA", status = "missing",
                                doc = "<b>Welch-ANOVA</b><br>Fehlt komplett. Kein oneway.test(var.equal=FALSE)-Äquivalent vorhanden.")
                            ))
                          )
                        )
                      )
                    ),
                    list(type = "answer", label = "nein", children = list(
                      list(type = "leaf", label = "Kruskal-\nWallis", status = "implemented",
                        doc = "<b>Kruskal-Wallis-Test</b><br>determine_sample_size_kruskal() - nur einfaktoriell.")
                    ))
                  )
                )
              )
            ),
            list(
              type = "answer", label = "Paarweise\nVergleiche (MCC)",
              children = list(
                list(
                  type = "question", label = "Normalverteilt?",
                  children = list(
                    list(
                      type = "answer", label = "ja",
                      children = list(
                        list(
                          type = "question", label = "Varianzen homogen?",
                          children = list(
                            list(type = "answer", label = "ja", children = list(
                              list(type = "leaf", label = "paarw. t-Test\n+ MCC", status = "missing",
                                doc = "<b>paarweiser t-Test (Student) + Holm/Hochberg/Bonferroni</b><br>Fehlt - gleicher Grund wie oben, estimate_sample_size() deckt nur den Welch-Fall ab.")
                            )),
                            list(type = "answer", label = "nein", children = list(
                              list(type = "leaf", label = "paarw. Welch-t\n+ MCC", status = "implemented",
                                doc = "<b>paarweiser Welch-t + Holm/Hochberg/Bonferroni</b><br>estimate_sample_size(mcc=..., family=...)")
                            ))
                          )
                        )
                      )
                    ),
                    list(type = "answer", label = "nein", children = list(
                      list(type = "leaf", label = "paarw. Wilcoxon\n+ MCC", status = "implemented",
                        doc = "<b>paarweiser Wilcoxon-Test + Holm/Hochberg/Bonferroni</b><br>estimate_sample_size_wilcoxon(mcc=..., family=...)")
                    ))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

assign_paths <- function(node, path) {
  node$path <- path
  if (!is.null(node$children)) {
    if (node$type == "question") {
      node$children <- lapply(seq_along(node$children), function(i) {
        assign_paths(node$children[[i]], c(path, i))
      })
    } else {
      node$children <- lapply(node$children, assign_paths, path = path)
    }
  }
  node
}
tree <- assign_paths(tree, integer(0))

collect_answer_nodes <- function(node, acc = list()) {
  if (!is.null(node$children)) {
    for (ch in node$children) {
      if (ch$type == "answer") acc <- c(acc, list(ch))
      acc <- collect_answer_nodes(ch, acc)
    }
  }
  acc
}
answer_nodes <- collect_answer_nodes(tree)

path_id <- function(path) paste0("ans_", paste(path, collapse = "_"))

node_at_path <- function(root, path) {
  node <- root
  for (i in path) {
    node <- node$children[[i]]$children[[1]]
  }
  node
}

breadcrumb <- function(root, path) {
  node <- root
  crumbs <- character(0)
  for (i in path) {
    q <- gsub("\n", " ", node$label, fixed = TRUE)
    ans <- node$children[[i]]
    a <- gsub("\n", " ", ans$label, fixed = TRUE)
    crumbs <- c(crumbs, paste0(q, " → ", a))
    node <- ans$children[[1]]
  }
  crumbs
}

wizard_css <- "
.breadcrumb { font-size: 13px; color: #56686e; margin: 0 auto 16px; max-width: 640px; text-align: center; }
.breadcrumb .crumb { display: inline-block; background: #eafaff; border-radius: 5px; padding: 3px 8px; margin: 2px 4px; }
.step-card { max-width: 560px; margin: 0 auto 16px; border: 2px solid #0aa9d6; background: #eafaff; border-radius: 10px; padding: 26px 30px; text-align: center; }
.step-card h3 { margin-top: 0; }
.step-answers { display: flex; gap: 14px; justify-content: center; flex-wrap: wrap; margin-top: 18px; }
.step-answers button { border: 2px solid #7d8b90 !important; background: #ffffff !important; color: #384245 !important; font-weight: bold; border-radius: 8px; padding: 10px 22px; }
.step-answers button:hover { border-color: #00c0ef !important; color: #00c0ef !important; }
.result-box { max-width: 560px; margin: 0 auto 16px; border-radius: 10px; padding: 18px 24px; border: 2px solid; text-align: center; }
.result-box.implemented { border-color: #1f9d76; background: #e7f7f1; }
.result-box.missing { border-color: #c4522a; background: #fbece5; }
.result-box .badge { display: inline-block; border-radius: 5px; padding: 2px 10px; font-size: 12px; font-weight: bold; color: #fff; margin-left: 8px; vertical-align: middle; }
.result-box.implemented .badge { background: #1f9d76; }
.result-box.missing .badge { background: #c4522a; }
.controls-row { max-width: 560px; margin: 0 auto 24px; text-align: center; }
"

ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(shiny::HTML(wizard_css))),
  shiny::titlePanel("Test-Auswahl-Baum"),
  shiny::tags$p(
    style = "text-align: center; color: #56686e;"
  ),
  shiny::uiOutput("breadcrumb_ui"),
  shiny::uiOutput("step_ui"),
  shiny::uiOutput("controls_ui")
)

server <- function(input, output, session) {
  active_path <- shiny::reactiveVal(integer(0))

  lapply(answer_nodes, function(node) {
    path <- node$path
    shiny::observeEvent(input[[path_id(path)]], {
      active_path(path)
    }, ignoreInit = TRUE)
  })

  shiny::observeEvent(input$back_step, {
    p <- active_path()
    if (length(p) > 0L) active_path(p[-length(p)])
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$reset_tree, {
    active_path(integer(0))
  }, ignoreInit = TRUE)

  output$breadcrumb_ui <- shiny::renderUI({
    path <- active_path()
    if (length(path) == 0L) return(NULL)
    crumbs <- breadcrumb(tree, path)
    htmltools::div(class = "breadcrumb", lapply(crumbs, function(c) htmltools::span(class = "crumb", c)))
  })

  output$step_ui <- shiny::renderUI({
    node <- node_at_path(tree, active_path())

    if (node$type == "leaf") {
      status_label <- if (node$status == "implemented") "implementiert" else "fehlt"
      return(htmltools::div(
        class = paste("result-box", node$status),
        htmltools::h3(
          gsub("\n", " ", node$label, fixed = TRUE),
          htmltools::tags$span(class = "badge", status_label)
        ),
        htmltools::HTML(node$doc)
      ))
    }

    htmltools::div(
      class = "step-card",
      htmltools::h3(gsub("\n", " ", node$label, fixed = TRUE)),
      htmltools::div(
        class = "step-answers",
        lapply(node$children, function(ans) {
          shiny::actionButton(path_id(ans$path), label = shiny::HTML(gsub("\n", "<br>", ans$label, fixed = TRUE)))
        })
      )
    )
  })

  output$controls_ui <- shiny::renderUI({
    path <- active_path()
    if (length(path) == 0L) return(NULL)
    htmltools::div(
      class = "controls-row",
      shiny::actionButton("back_step", "← Zurück"),
      shiny::actionLink("reset_tree", "↺ neu starten", style = "margin-left: 16px;")
    )
  })
}

shiny::shinyApp(ui, server)
