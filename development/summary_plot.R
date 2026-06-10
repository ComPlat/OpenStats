#     X Plant   Type  Treatment conc uptake
# "1" 1   Qn1 Quebec nonchilled   95   16.0
# "2" 2   Qn1 Quebec nonchilled  175   30.4
# "3" 3   Qn1 Quebec nonchilled  250   34.8
# "4" 4   Qn1 Quebec nonchilled  350   37.2
# "5" 5   Qn1 Quebec nonchilled  500   35.3
# "6" 6   Qn1 Quebec nonchilled  675   39.2
# [1] "Treatment" "Type"
# [1] "uptake" "conc"
df <- CO2
column_by <- c("Treatment", "Type")
for_which <- "uptake"

library(ggplot2)

summary_plot <- function(df, for_which, column_by) {
  ggplot(data = df, aes(y = .data[[for_which]])) +
    geom_boxplot() +
    facet_wrap(facets = column_by)
}

summary_plot(df, for_which, column_by)
