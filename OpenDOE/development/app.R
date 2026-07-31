ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      #main {
      display: flex;
      flex-direction: column;
      align-items: center;
      }
      #main > div {
      text-align: center;
      }
      .doe-arrow { font-size: 28px; color: #00c0ef; }
      .doe-diagram-box {
      border: 2px solid #00c0ef;
      border-radius: 8px;
      padding: 20px;
      width: 350px;
      white-space: normal;
      text-align: justify;
      margin: 20px auto;
      }
      "))
  ),
  shiny::uiOutput("main")
)

server <- function(input, output, session) {
  state <- shiny::reactiveValues(what_to_show = "overview")
  steps <- list(
    "Define groups", "Sample Size calculation", "Design of Experiments", "Random assignment", "Finite assignment"
  )
  explanations <- list(
    "Define groups" =
    "You have to define the parameters of interest.
     For example you want to investigate the effect of three different drugs compared to a control.
     Then you can define here the group 'drug' with four different group-levels: drug-A, drug-B, drug-C and Control.
    ",
    "Sample Size calculation" =
    "Within this section you can calculate how often a group has to be repeated (= sample size).
     Considering the example with the drugs you retrieve the number of repetitions for drug-A, drug-B, drug-C and Control.
    ",
    "Design of Experiments" =
    "Here you create a list of experiments which is called design-matrix.
     Each row in the matrix is one experiment for one of your group-levels (drug-A, drug-B, drug-C, and Control).",
    "Random assignment" =
    "Randomly assigns an infinite resource (e.g. 3 experimental days) to each row of your design-matrix.
     This spreads every group-level (drug-A, drug-B, drug-C, Control) evenly across days.",
    "Finite assignment" =
    "Assigns a finite resource (e.g. 50 animals with known weights) to each row of your design-matrix,
     so weights are evenly balanced across group-levels (drug-A, drug-B, drug-C, Control)."
  )

  output[["main"]] <- shiny::renderUI({
    if (state$what_to_show == "overview") {
      l <- lapply(steps, function(s) {
        b <- shiny::actionButton(
          paste0("open_", s),
          label = htmltools::tagList(
            htmltools::div(s, style = "font-weight: bold;"),
            htmltools::div(shiny::HTML(explanations[[s]]), style = "font-size: 14px; color: #666;")
          ),
          class = "doe-diagram-box"
        )
        if (s == "Finite assignment") {
          return(b)
        }
        htmltools::div(
          b,
          htmltools::br(),
          htmltools::span(shiny::HTML("&dArr;"), class = "doe-arrow")
        )
      })
      do.call(htmltools::tagList, l)
    }
  })
}

shiny::shinyApp(ui, server)
