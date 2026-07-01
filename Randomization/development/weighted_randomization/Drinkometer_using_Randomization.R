# ----------------------------------------------------------
# Drinking per bodyweight of drinkometer rats
# ----------------------------------------------------------

# Randomization requires the R package ast2ast which can be installed using
# devtools::install_github("Konrad1991/ast2ast", build_vignettes = TRUE)
# --> to run this installation command the R package devtools is required
library(Randomization)
library(readxl)
library(ggplot2)

# Read and clean data
# ----------------------------------------------------------
read_and_clean <- function(base_path, file_name) {
  df <- read_excel(paste0(base_path, file_name))
  df <- df[-c(1, 19, 27, 28), ]
  df <- as.data.frame(df)
  cols <- c("Wasser", "5%ETOH", "10%ETOH", "20%ETOH")
  df[cols] <- lapply(df[cols], as.numeric)
  df
}
base_path <- "Randomization/development/weighted_randomization/"
day1_path <- "trinkenkeller9126.xlsx"
day2_path <- "trinkenkeller12126.xlsx"
day1 <- read_and_clean(base_path, day1_path)
day2 <- read_and_clean(base_path, day2_path)

# calc drink volume
# ----------------------------------------------------------
# volume = bottleweight friday - monday
drinkvolume <- as.matrix(day1) -  as.matrix(day2)
vol <- data.frame(alcohol =
            drinkvolume[,3L] / 20 +
            drinkvolume[,4L] / 10 +
            drinkvolume[,5L] / 5
)
volpd <- vol / 3.0
# auf Körpergewicht berechnen: Vol/Körpergewicht * 1000
volpdbdw <- read_excel(paste0(base_path, "bodyweightdrinkingUG.xlsx"))
volpdbdw$`Vol/d` <- volpd
volpdbdw$`Vol/d/kg` <- volpdbdw$`Vol/d`*volpdbdw$Bodyweight / 1000

# Normalisation
# ----------------------------------------------------------
volpdbdw[, 4L] <- unlist(volpdbdw[, 4L])
volpdbdw[, 5L] <- unlist(volpdbdw[, 5L])
volpdbdw <- as.data.frame(volpdbdw)
volpdbdw$bw_z <- scale(volpdbdw$Bodyweight)
volpdbdw$bl_z <- scale(volpdbdw$`Vol/d/kg`)

m <- volpdbdw[volpdbdw$Sex == "m", ]
groups <- m[, c("bw_z", "bl_z")]
n_blocks <- data.frame(groups = c(rep("A", 11L), rep("B", 5L), rep("C", 6L)))
seed <- 42L
variation_weight <- 0.7
res_m <- random_finite_assign(seed, groups, n_blocks, lambda_m2 = variation_weight, lambda_cov = 0.1)
m <- m[res_m$perm, ]
m$group <- res_m$blocks

f <- volpdbdw[volpdbdw$Sex == "f", ]
groups <- f[, c("bw_z", "bl_z")]
n_blocks <- data.frame(groups = c(rep("A", 8L), rep("B", 4L), rep("C", 4L)))
seed <- 42L
res_f <- random_finite_assign(seed, groups, n_blocks, lambda_m2 = variation_weight, lambda_cov = 0.1)
f <- f[res_f$perm, ]
f$group <- res_f$blocks

df <- rbind(m, f)
ggplot(data = df, aes(x = group, y = `Vol/d`)) +
  geom_boxplot() +
  geom_point(data = df, aes(colour = group)) +
  facet_wrap(~ Sex)
ggplot(data = df, aes(x = group, y = `Vol/d/kg`)) +
  geom_boxplot() +
  geom_point(data = df, aes(colour = group)) +
  facet_wrap(~ Sex)

aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, m, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, m, sd)

aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, f, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, f, sd)
