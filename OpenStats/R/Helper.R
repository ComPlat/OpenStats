# nocov start helper
# divs
info_div <- function(message) {
  div(
    class = "info-box",
    h3(strong(message))
  )
}

render_df <- function(df, n_fixed_cols = 1) {
  small <- nrow(df) <= 20 && ncol(df) <= 8
  if (small) {
    renderDT(df)
  } else {
    renderDT(
      df,
      extensions = c("Buttons", "FixedColumns", "Scroller"),
      options = list(
        dom = "Bfrtip",
        scrollX = TRUE,
        # scroller = TRUE,
        paging = TRUE,
        fixedColumns = list(leftColumns = n_fixed_cols)
      ),
      class = "compact stripe nowrap"
    )
  }

}

# check and print warnings
print_warn <- function(message) {
  showNotification(message, type = "warning")
}

# check and print error
print_err <- function(message) {
  showNotification(message, type = "error")
}

# check and print notifications
print_req <- function(expr, message) {
  if (!expr) {
    showNotification(message, type = "message")
  }
  req(expr)
}

# print notification without check
print_noti <- function(message) {
  showNotification(message, type = "message")
}

# print success
print_success <- function(message) {
  showNotification(message)
}

# check formula and open modal window if no formula is set
print_form <- function(formula) {
  if (is.null(formula)) {
    showNotification("You have to set a formula",
      action = tags$div(
        showModal(modalDialog(
          title = "FormulaEditor",
          FormulaEditorUI("FO"),
          easyClose = TRUE,
          size = "l",
          footer = tagList(
            modalButton("Close")
          )
        ))
      ),
      type = "message"
    )
  }
  req(!is.null(formula))
}

# check that result is only of allowed type
check_type_res <- function(res) {
  allowed <- c("numeric", "factor", "integer", "logical", "character", "data.frame")
  if (!(class(res) %in% allowed)) {
    stop(paste0("Found result with unallowed type: ", class(res)))
  }
}

# Check length of input code
check_length_code <- function(code) {
  if (nchar(code) > 4000) {
    stop("The code is too long to be evaluated")
  }
}

# Check that formula is of type response ~ predictor
check_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("Input must be a formula of the type response ~ predictor")
  }
  terms <- all.vars(formula)
  if (length(terms) != 2) {
    stop("Formula must have exactly two terms: response ~ predictor")
  }
  return(TRUE)
}
# nocov end helper
