base_path <- "Randomization/development/weighted_randomization/"
setwd(base_path)
#Drinking per bodyweight of drinkometer rats
#Libraries

library("readxl")
library("writexl")
library("combinat")

#get data
day1 = read_excel("trinkenkeller9126.xlsx")
day2 = read_excel("trinkenkeller12126.xlsx")

#clean data
day1 = day1[-c(1, 19, 27, 28),]
day2 = day2[-c(1, 19, 27, 28),]


#format into df & numerization
day1 = as.data.frame(day1)
day2 = as.data.frame(day2)
cols <- c("Wasser", "5%ETOH", "10%ETOH", "20%ETOH")
day1[cols] <- lapply(day1[cols], as.numeric)
day2[cols] <- lapply(day2[cols], as.numeric)


#calc drink volume 
drinkvolume = as.matrix(day1) -  as.matrix(day2) #volume = bottleweight friday - monday
drinkvolume


#summarize into concrete alcohol amount
vol = data.frame("alcohol" = 
            drinkvolume[,3] /20 +
            drinkvolume[,4] /10 +
            drinkvolume[,5] /5
)
vol
#per day
volpd = vol/3
volpd

#auf Körpergewicht berechnen: Vol/Körpergewicht * 1000
volpdbdw = read_excel("bodyweightdrinkingUG.xlsx")
volpdbdw
volpdbdw$`Vol/d` = volpd
volpdbdw$`Vol/d/kg` = volpdbdw$`Vol/d`*volpdbdw$Bodyweight / 1000
volpdbdw$`Vol/d/kg`


###unlist zum abspeichern
volpdbdw$`Vol/d` = unlist(volpdbdw$`Vol/d`)
volpdbdw$`Vol/d/kg` = unlist(volpdbdw$`Vol/d/kg`)
volpdbdw$`Vol/d/kg`= unname(volpdbdw$`Vol/d/kg`)
volpdbdw$`Vol/d`= unname(volpdbdw$`Vol/d`)



#speichern
write_xlsx(volpdbdw, "C:/Users/BDM140308/Documents/Nanobodies/Drinkometer baseline/volpdbw.xlsx")



#######Balanciertes Gruppieren
#Mittelwert und SD standartisieren damit vergleichabr

volpdbdw$bw_z = scale(volpdbdw$Bodyweight)
volpdbdw$bl_z = scale(volpdbdw$`Vol/d/kg`)

m_volpdbw = volpdbdw[17:38,] #männliche Tiere
m_volpdbw$`Vol/d/kg`

f_volpdbw = volpdbdw[1:16,]  #weibliche tiere
f_volpdbw$`Vol/d/kg`


###male
comb <- combn(nrow(m_volpdbw), nrow(m_volpdbw)/2) #erzeugt alle möglichen Kombinationen, die Tiere in 2 gleich große gruppen aufzuteilen
var_weight = 0.7 #Gewichtung der Varianz, kann beliebig geändert werden; hoher Wert: SD wichtiger, niedriger Wert: Mittelwert wichtiger

# 3) Zielfunktion
objective <- function(idx) {
  gA <- m_volpdbw[idx, ]     #Gruppe x
  gB <- m_volpdbw[-idx, ]   #Gruppe y
  
  diff_bw_mean <- mean(gA$bw_z) - mean(gB$bw_z)   #Mittelwertunterschiede berechnen; näher 0: Unterschied geringer
  diff_bl_mean <- mean(gA$bl_z) - mean(gB$bl_z)
  
  diff_bw_sd   <- sd(gA$bw_z) - sd(gB$bw_z)       # Unterschiede Streuung
  diff_bl_sd   <- sd(gA$bl_z) - sd(gB$bl_z)
  
  sqrt(diff_bw_mean^2 + diff_bl_mean^2 + var_weight*(diff_bw_sd^2 + diff_bl_sd^2))     #Abstand zwischen Gruppen; kleiner Score: kleinerer Unterschied
}

# 4) Beste Kombination finden
scores <- apply(comb, 2, objective)   # Anwenden der Funktion auf jede mögliche Kombination
best   <- comb[, which.min(scores)]   #"Filter": kleinster score

# 5) Gruppenzuweisung
m_volpdbw$group <- "B"
m_volpdbw$group[best] <- "A"

aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, m_volpdbw, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, m_volpdbw, sd)




###Balancing females
comb <- combn(nrow(f_volpdbw), nrow(f_volpdbw)/2)
objective <- function(idx) {
  gA <- f_volpdbw[idx, ]
  gB <- f_volpdbw[-idx, ]
  
  diff_bw_mean <- mean(gA$bw_z) - mean(gB$bw_z)
  diff_bl_mean <- mean(gA$bl_z) - mean(gB$bl_z)
  
  diff_bw_sd   <- sd(gA$bw_z) - sd(gB$bw_z)
  diff_bl_sd   <- sd(gA$bl_z) - sd(gB$bl_z)
  
  sqrt(diff_bw_mean^2 + diff_bl_mean^2 + var_weight*(diff_bw_sd^2 + diff_bl_sd^2))
}
scores <- apply(comb, 2, objective)
best   <- comb[, which.min(scores)]

f_volpdbw$group <- "B"
f_volpdbw$group[best] <- "A"

aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, f_volpdbw, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, f_volpdbw, sd)


###Plot
###Plot
boxplot(`Vol/d/kg` ~ group, data = m_volpdbw)
stripchart(`Vol/d/kg` ~ group,  data = m_volpdbw,  vertical = TRUE,  method = "jitter",  pch = 16,  add = TRUE)

boxplot(Bodyweight ~ group, data = m_volpdbw)
stripchart(Bodyweight ~ group, data = m_volpdbw, vertical = TRUE,  method = "jitter", pch = 16,  add = TRUE)

boxplot(`Vol/d/kg` ~ group, data = f_volpdbw)
stripchart(`Vol/d/kg` ~ group,  data = f_volpdbw,  vertical = TRUE,  method = "jitter",  pch = 16,  add = TRUE)

boxplot(Bodyweight ~ group, data = f_volpdbw)
stripchart(Bodyweight ~ group, data = f_volpdbw, vertical = TRUE,  method = "jitter", pch = 16,  add = TRUE)



##Kombinieren und säubern
grouplist = rbind(f_volpdbw, m_volpdbw)
#grouplist$bw_z = NULL
#grouplist$bl_z = NULL

write_xlsx(grouplist, "group.xlsx")


## Sort subgroups:
#- Male B into 2 (DN1 vs Sal)
#df mit nur Tieren "B"
sub_f = subset(grouplist, group == "B")
sub_f = subset(sub_f, Sex == "f")

#male
sub_m = subset(grouplist, group == "B")
sub_m = subset(sub_m, Sex == "m")



#balance female

comb <- combn(nrow(sub_f), nrow(sub_f)/2)
objective <- function(idx) {
  gA <- sub_f[idx, ]
  gB <- sub_f[-idx, ]
  diff_bw_mean <- mean(gA$bw_z) - mean(gB$bw_z)
  diff_bl_mean <- mean(gA$bl_z) - mean(gB$bl_z)
  diff_bw_sd   <- sd(gA$bw_z) - sd(gB$bw_z)
  diff_bl_sd   <- sd(gA$bl_z) - sd(gB$bl_z)
  sqrt(diff_bw_mean^2 + diff_bl_mean^2 + var_weight*(diff_bw_sd^2 + diff_bl_sd^2))
}
scores <- apply(comb, 2, objective)
best   <- comb[, which.min(scores)]
sub_f$group <- "B"
sub_f$group[best] <- "C"

#look at data
boxplot(`Vol/d/kg` ~ group, data = sub_f)
stripchart(`Vol/d/kg` ~ group,  data = sub_f,  vertical = TRUE,  method = "jitter",  pch = 16,  add = TRUE)
boxplot(Bodyweight ~ group, data = sub_f)
stripchart(Bodyweight ~ group, data = sub_f, vertical = TRUE,  method = "jitter", pch = 16,  add = TRUE)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, sub_f, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, sub_f, sd)




#balance male
comb <- combn(nrow(sub_m), nrow(sub_m)/2)
objective <- function(idx) {
  gA <- sub_m[idx, ]
  gB <- sub_m[-idx, ]
  diff_bw_mean <- mean(gA$bw_z) - mean(gB$bw_z)
  diff_bl_mean <- mean(gA$bl_z) - mean(gB$bl_z)
  diff_bw_sd   <- sd(gA$bw_z) - sd(gB$bw_z)
  diff_bl_sd   <- sd(gA$bl_z) - sd(gB$bl_z)
  sqrt(diff_bw_mean^2 + diff_bl_mean^2 + var_weight*(diff_bw_sd^2 + diff_bl_sd^2))
}
scores <- apply(comb, 2, objective)
best   <- comb[, which.min(scores)]
sub_m$group <- "B"
sub_m$group[best] <- "C"

sub_m

#look at data
boxplot(`Vol/d/kg` ~ group, data = sub_m)
stripchart(`Vol/d/kg` ~ group,  data = sub_m,  vertical = TRUE,  method = "jitter",  pch = 16,  add = TRUE)
boxplot(Bodyweight ~ group, data = sub_m)
stripchart(Bodyweight ~ group, data = sub_m, vertical = TRUE,  method = "jitter", pch = 16,  add = TRUE)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, sub_m, mean)
aggregate(cbind(Bodyweight, `Vol/d/kg`) ~ group, sub_m, sd)

##clean and save
grouplist = rbind(subset(grouplist, group == "A"),
                  sub_f, sub_m)
grouplist$bw_z = NULL
grouplist$bl_z = NULL

grouplist
write_xlsx(grouplist, "C:/Users/BDM140308/Documents/Nanobodies/Drinkometer baseline/group.xlsx")
