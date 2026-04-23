df <- read.table("~/Downloads/Masterarbeit_Jerome.txt", sep = "\t", header = TRUE)
names(df)[6L] <- "values"
head(df)
sub <- df[df$Product == "H2", ]
sub
