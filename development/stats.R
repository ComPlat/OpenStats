# ------------------------------------------------------------------
# Post Hoc tests
# ------------------------------------------------------------------
# stats
summary(fm1 <- aov(breaks ~ wool + tension, data = warpbreaks))
TukeyHSD(fm1, "tension", ordered = TRUE)
plot(TukeyHSD(fm1, "tension"))
# agricolae
res <- agricolae::HSD.test(fm1, trt = "tension")
res
plot(res)
# stats: parametric
pairwise.t.test(warpbreaks$breaks, warpbreaks$tension, p.adjust.method = "holm")
# stats: non parametric
pairwise.wilcox.test(warpbreaks$breaks, warpbreaks$tension, p.adjust.method = "holm")

# ------------------------------------------------------------------
# ave
# ------------------------------------------------------------------
CO2$mean_uptake_by_treatment <- ave(CO2$uptake, CO2$Treatment)
head(CO2, n = 50)

# ------------------------------------------------------------------
# mahalanobis distance
# ------------------------------------------------------------------
ma <- cbind(1:6, 1:3)
S <-  var(ma)
mahalanobis(c(0, 0), 1:2, S)

# ------------------------------------------------------------------
# reshape
# ------------------------------------------------------------------
head(CO2)
wide <- reshape(CO2,
                v.names = "uptake", idvar = "Plant",
                timevar = "conc", direction = "wide")
wide

# ------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------
# ansari.test
# * non parameteric test for equality of scales (dispersion/variance)
# * the test assumes that the distributions have the same value for the location
set.seed(1234)
x <- rnorm(20, sd = 1)
y <- rnorm(20, sd = 4)
ansari.test(x, y)
set.seed(1234)
y <- rnorm(20, sd = 1.1)
ansari.test(x, y)

# wilcox.test
# * non parameteric test for equality of location
# * non parameteric alternative to t.test
set.seed(1234)
x <- c(4,5,6,7,8)
y <- c(1,2,3,4,5)
wilcox.test(x, y)
wilcox.test(x, x + runif(5L, 0, 0.1))

# ks.test
# * Tests whether two samples come from the same distribution.
set.seed(1234)
x <- rnorm(100000, mean = 1, sd = 0.2)
y <- rnorm(100000, mean = 1, sd = 0.2)
ks.test(x, y)
set.seed(1234)
x <- rgamma(100000, shape = 0.02)
y <- rnorm(100000, mean = 1)
ks.test(x, y)


# Rank based anova alternatives
# ------------------------------------------------------------------
# kruskal.test
# * non parameteric one way anova
kruskal.test(uptake ~ Treatment, data = CO2)
# friedman.test
# * non parameteric repeated measurement anova
friedman.test(uptake ~ conc | Plant, data = CO2)
# quade.test
# * similar to friedman but more powerful when block effects differ strongly
quade.test(uptake ~ conc | Plant, data = CO2)

# variance tests
# ------------------------------------------------------------------
# var.test
# * Tests whether two populations have equal variances.
set.seed(1234)
x <- rnorm(20, sd = 1)
y <- rnorm(20, sd = 2)
var.test(x, y)
# fligner.test
# * nonparametric
# * based on ranked absolute deviations
# * most robust test in package stats
fligner.test(uptake ~ Treatment, data = CO2)
# bartlett.test
# * Tests equal variances across multiple groups
# * very sensitive to non-normality.
bartlett.test(uptake ~ Treatment, data = CO2)
# ansari.test see above

# proportion tests
# ------------------------------------------------------------------
# binom.test
# * exact test for a single proportion
# * tests whether the probability of success equals a reference value
binom.test(60, 100, p = 0.5)
binom.test(70, 100, p = 0.5)
# prop.test
# * approximate chi-squared test for one or multiple proportions
# * can test equality of proportions across groups
prop.test(60, 100, p = 0.5)
prop.test(x = c(60, 45), n = c(100, 100))
prop.test(x = c(60, 45, 30), n = c(100, 100, 100))
# pairwise.prop.test
# * pairwise comparisons between group proportions
# * p-values are adjusted for multiple testing
pairwise.prop.test(
  x = c(60, 45, 30),
  n = c(100, 100, 100),
  p.adjust.method = "holm"
)
# prop.trend.test
# * chi-squared test for trend in proportions across ordered groups
# * useful if groups have a natural ordering (e.g. dose levels)
prop.trend.test(
  x = c(10, 20, 35, 50),
  n = c(100, 100, 100, 100)
)

# contingency table tests
# ------------------------------------------------------------------
# chisq.test
# * chi-squared test for independence in contingency tables
# * tests whether two categorical variables are independent
tbl <- matrix(c(20, 15, 30, 35), nrow = 2)
chisq.test(tbl)
# fisher.test
# * exact test for independence in contingency tables
# * preferred when expected counts are small
fisher.test(tbl)
# mcnemar.test
# * test for paired categorical data
# * tests marginal homogeneity in 2x2 tables
tbl_paired <- matrix(c(30, 10, 5,  40), nrow = 2)
mcnemar.test(tbl_paired)
# mantelhaen.test
# * Mantel–Haenszel test for association controlling for strata
# * used for several 2x2 tables across strata
tbl3d <- array(c(
  12, 5, 7, 9,
  10, 8, 6, 11
), dim = c(2,2,2))
mantelhaen.test(tbl3d)
