Test <- R6::R6Class(
  "Test",
  public = list(
    name = NULL,
    tab = NULL,
    run_type = NULL,
    description = NULL,
    closed_form_alt = FALSE,
    parent_answer = NULL,
    initialize = function(name, tab, run_type, description, closed_form_alt = FALSE) {
      self$name <- name
      self$tab <- tab
      self$run_type <- run_type
      self$description <- description
      self$closed_form_alt <- closed_form_alt
    }
  )
)

Question <- R6::R6Class(
  "Question",
  public = list(
    question = NULL,
    parent_answer = NULL,
    answers = list(),
    initialize = function(question, answers) {
      self$question <- question
      self$answers <- test_selection_tree_build_answers(answers, self)
    }
  )
)

Answer <- R6::R6Class(
  "Answer",
  public = list(
    label = NULL,
    parent_node = NULL,
    next_node = NULL,
    initialize = function(label, parent_node, next_node) {
      self$label <- label
      self$parent_node <- parent_node
      self$next_node <- next_node
      next_node$parent_answer <- self
    }
  )
)

test_selection_tree_build_answers <- function(answers, parent_node) {
  Map(function(label, node) Answer$new(label, parent_node, node), names(answers), answers)
}

test_selection_tree <- Question$new("Number of levels?", answers = list(
  "1-2" = Question$new("Normally distributed?", answers = list(
    "yes" = Question$new("Variances homogeneous?", answers = list(
      "yes" = Test$new("t-test (Student)", tab = "Pairwise comparisons", run_type = "mc_student_t",
        description = "Two groups, normally distributed, equal variances assumed.",
        closed_form_alt = TRUE),
      "no" = Test$new("Welch t-test", tab = "Pairwise comparisons", run_type = "mc",
        description = "Two groups, normally distributed, unequal variances assumed.")
    )),
    "no" = Test$new("Wilcoxon rank-sum test", tab = "Pairwise comparisons", run_type = "mc_wilcoxon",
      description = "Two groups, no normal distribution assumed.")
  )),
  ">2" = Question$new("Goal?", answers = list(
    "Omnibus test" = Question$new("Normally distributed?", answers = list(
      "yes" = Question$new("Variances homogeneous?", answers = list(
        "yes" = Test$new("ANOVA (Student)", tab = "Monte Carlo: anova", run_type = "mc_anova",
          description = ">2 levels, normally distributed, equal variances assumed.",
          closed_form_alt = TRUE),
        "no" = Test$new("Welch-ANOVA", tab = "Monte Carlo: one-way test", run_type = "mc_welch_anova",
          description = ">2 levels, normally distributed, unequal variances assumed. One-way (single predictor).")
      )),
      "no" = Test$new("Kruskal-Wallis test", tab = "Monte Carlo: one-way test", run_type = "mc_kruskal",
        description = ">2 levels, no normal distribution assumed. One-way (single predictor).")
    )),
    "Pairwise comparisons (MCC)" = Question$new("Normally distributed?", answers = list(
      "yes" = Question$new("Variances homogeneous?", answers = list(
        "yes" = Test$new("pairwise t-test (Student)", tab = "Pairwise comparisons", run_type = "mc_student_t",
          description = ">2 levels, pairwise comparisons with Holm/Hochberg/Bonferroni, equal variances assumed."),
        "no" = Test$new("pairwise Welch t-test", tab = "Pairwise comparisons", run_type = "mc",
          description = ">2 levels, pairwise comparisons with Holm/Hochberg/Bonferroni, unequal variances assumed.")
      )),
      "no" = Test$new("pairwise Wilcoxon test", tab = "Pairwise comparisons", run_type = "mc_wilcoxon",
        description = ">2 levels, pairwise comparisons with Holm/Hochberg/Bonferroni, no normal distribution assumed.")
    ))
  ))
))

test_selection_tree_start_node <- function(root, group_count) {
  label <- if (group_count <= 2L) "1-2" else ">2"
  Find(function(a) a$label == label, root$answers)$next_node
}

test_selection_tree_branch_of <- function(node, root) {
  while (!identical(node$parent_answer$parent_node, root)) {
    node <- node$parent_answer$parent_node
  }
  node
}

test_selection_tree_breadcrumb <- function(node, root) {
  crumbs <- character(0)
  while (!is.null(node$parent_answer)) {
    ans <- node$parent_answer
    text <- if (identical(ans$parent_node, root)) {
      paste0(ans$label, " (automatic)")
    } else {
      paste0(ans$parent_node$question, " → ", ans$label)
    }
    crumbs <- c(text, crumbs)
    node <- ans$parent_node
  }
  crumbs
}

test_selection_tree_max_answers <- function(node) {
  if (inherits(node, "Test")) return(0L)
  child_max <- vapply(node$answers, function(a) test_selection_tree_max_answers(a$next_node), integer(1))
  max(length(node$answers), child_max)
}
test_selection_tree_max_answer_slots <- test_selection_tree_max_answers(test_selection_tree)

test_selection_tree_answer_input_id <- function(i) paste0("wizard_answer_", i)
test_selection_tree_answer_button_id <- function(i) paste0("SAMPLESIZE-", test_selection_tree_answer_input_id(i))

test_selection_tree_find <- function(node, ...) {
  for (label in c(...)) {
    node <- Find(function(a) a$label == label, node$answers)$next_node
  }
  node
}
