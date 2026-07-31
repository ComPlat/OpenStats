traverse <- function(tree, parent_node) {
  if (is.list(tree)) {
    names <- names(tree)
    Map(function(name, node) {
      Answer$new(name, parent_node, node)
    }, names(tree), tree)
  }
}

Test <- R6::R6Class(
  "Test",
  public = list(
    name = NULL,
    parent_answer = NULL,
    initialize = function(name) {
      self$name <- name
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
      self$answers <- traverse(answers, self)
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

create_tree <- function() {
  Question$new("Number of levels?", answers = list(
    "1-2" = Question$new("Normally distributed?", answers = list(
      "yes" = Question$new("Variances homogeneous?", answers = list(
        "yes" = Test$new("t-test (Student)"),
        "no"  = Test$new("Welch t-test")
      )),
      "no" = Test$new("Wilcoxon rank-sum test")
    )),
    ">2" = Question$new("Goal?", answers = list(
      "Omnibus test" = Question$new("Normally distributed?", answers = list(
        "yes" = Question$new("Variances homogeneous?", answers = list(
          "yes" = Test$new("ANOVA (Student)"),
          "no"  = Test$new("Welch-ANOVA")
        )),
        "no" = Test$new("Kruskal-Wallis test")
      )),
      "Pairwise comparisons (MCC)" = Question$new("Normally distributed?", answers = list(
        "yes" = Question$new("Variances homogeneous?", answers = list(
          "yes" = Test$new("pairwise t-test (Student)"),
          "no"  = Test$new("pairwise Welch t-test")
        )),
        "no" = Test$new("pairwise Wilcoxon test")
      ))
    ))
  ))
}

tree <- create_tree()
