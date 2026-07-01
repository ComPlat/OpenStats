# ----------------------------------------------------------
# Drinking per bodyweight of drinkometer rats
# ----------------------------------------------------------
library("readxl")
library("writexl")
library("combinat")

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
write_xlsx(volpdbdw, paste0(base_path, "volpdbw.xlsx"))
volpdbdw$bw_z <- scale(volpdbdw$Bodyweight)
volpdbdw$bl_z <- scale(volpdbdw$`Vol/d/kg`)
m_volpdbw <- volpdbdw[volpdbdw$Sex == "m",]
f_volpdbw <- volpdbdw[volpdbdw$Sex == "f",]

# Find best grouping
# ----------------------------------------------------------
# create all possible combinations of animals divided into two groups
# Generate all combinations of the elements of x taken m at a time.
comb <- combn(nrow(m_volpdbw), nrow(m_volpdbw)/2)
var_weight <- 0.7

objective <- function(df, idx) {
  gA <- df[idx, ]
  gB <- df[-idx, ]
  diff_bw_mean <- mean(gA$bw_z) - mean(gB$bw_z)
  diff_bl_mean <- mean(gA$bl_z) - mean(gB$bl_z)
  diff_bw_sd <- sd(gA$bw_z) - sd(gB$bw_z)
  diff_bl_sd <- sd(gA$bl_z) - sd(gB$bl_z)
  sqrt(diff_bw_mean^2 + diff_bl_mean^2 + var_weight*(diff_bw_sd^2 + diff_bl_sd^2))
}
run_all <- function(comb, df) {
  min_idx <- 1L
  min_val <- objective(df, comb[, min_idx])
  for (i in seq_len(ncol(comb))) {
    temp_val <- objective(df, comb[, i])
    if (temp_val < min_val) {
      min_idx <- i
      min_val <- temp_val
    }
  }
  comb[, min_idx]
}
best_comb <- run_all(comb, m_volpdbw)
m_volpdbw$group <- "B"
m_volpdbw$group[best_comb] <- "A"
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, m_volpdbw, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, m_volpdbw, sd)

comb <- combn(nrow(f_volpdbw), nrow(f_volpdbw)/2)
best_comb <- run_all(comb, f_volpdbw)
f_volpdbw$group <- "B"
f_volpdbw$group[best_comb] <- "A"
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, f_volpdbw, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, f_volpdbw, sd)

# Visualization
# ----------------------------------------------------------
par(mfrow = c(2, 2))
boxplot(`Vol/d/kg` ~ group, data = m_volpdbw)
stripchart(`Vol/d/kg` ~ group,  data = m_volpdbw,  vertical = TRUE,  method = "jitter",  pch = 16,  add = TRUE)

boxplot(Bodyweight ~ group, data = m_volpdbw)
stripchart(Bodyweight ~ group, data = m_volpdbw, vertical = TRUE,  method = "jitter", pch = 16,  add = TRUE)

boxplot(`Vol/d/kg` ~ group, data = f_volpdbw)
stripchart(`Vol/d/kg` ~ group,  data = f_volpdbw,  vertical = TRUE,  method = "jitter",  pch = 16,  add = TRUE)

boxplot(Bodyweight ~ group, data = f_volpdbw)
stripchart(Bodyweight ~ group, data = f_volpdbw, vertical = TRUE,  method = "jitter", pch = 16,  add = TRUE)
dev.off()

grouplist <- rbind(f_volpdbw, m_volpdbw)
write_xlsx(grouplist, paste0(base_path, "group.xlsx"))

# Sort subgroups: Male B into 2 (DN1 vs Sal)
# ----------------------------------------------------------
sub_f <- grouplist[grouplist$group == "B" & grouplist$Sex == "f", ]
sub_m <- grouplist[grouplist$group == "B" & grouplist$Sex == "m", ]

comb <- combn(nrow(sub_f), nrow(sub_f)/2)
best_comb <- run_all(comb, sub_f)
sub_f$group <- "B"
sub_f$group[best_comb] <- "C"

# Without floor error. Also in the original script
comb <- combn(nrow(sub_m), floor(nrow(sub_m)/2))
best_comb <- run_all(comb, sub_m)
sub_m$group <- "B"
sub_m$group[best_comb] <- "C"

# Visualization
# ----------------------------------------------------------
boxplot(`Vol/d/kg` ~ group, data = sub_f)
stripchart(`Vol/d/kg` ~ group,  data = sub_f,  vertical = TRUE,  method = "jitter",  pch = 16,  add = TRUE)
boxplot(Bodyweight ~ group, data = sub_f)
stripchart(Bodyweight ~ group, data = sub_f, vertical = TRUE,  method = "jitter", pch = 16,  add = TRUE)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, sub_f, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, sub_f, sd)

# Visualization
# ----------------------------------------------------------
boxplot(`Vol/d/kg` ~ group, data = sub_m)
stripchart(`Vol/d/kg` ~ group,  data = sub_m,  vertical = TRUE,  method = "jitter",  pch = 16,  add = TRUE)
boxplot(Bodyweight ~ group, data = sub_m)
stripchart(Bodyweight ~ group, data = sub_m, vertical = TRUE,  method = "jitter", pch = 16,  add = TRUE)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, sub_m, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, sub_m, sd)

grouplist <- rbind(subset(grouplist, group == "A"), sub_f, sub_m)
grouplist$bw_z <- NULL
grouplist$bl_z <- NULL
write_xlsx(grouplist, paste0(base_path, "group.xlsx"))
