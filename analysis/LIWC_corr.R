library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)

# Load LIWC
LIWC <- read_csv("~/Workspace/CDC/data/LIWC2015 Results (12 files).csv")

# Load Scores
Scores <- read_csv("~/Workspace/CDC/data/ES_2-5_79cases_newVnames.csv")
Scores <- Scores[,c(1,15)]
names(Scores) <- c("fam", "hop2")
qplot(Scores$hop2, bins = 30)
summary(Scores$hop2)

# Make HIGH - LOW
Scores$hopC <- "LOW"
Scores[Scores$hop2 >= 6, "hopC"] <- "HIGH"
qplot(Scores$hopC, bins = 30)

filename_funs <- LIWC %>% group_by(Filename) %>% summarise_each(funs(mean))
filename_funs <- filename_funs[,-c(2)] #Drop segment col
filename_funs$Filename <- c("ESC805A2", "ESC837A2", "ESE009", "ESE343", 
                            "ESE371", "ESE395", "ESE939", "ESP470",
                            "ESP636", "ESP654", "ESP656", "ESP841")

filename_funs <- merge(Scores, filename_funs, by.x = "fam", by.y = "Filename")

# Calculate spearman corr w/ p-vals corrected using Benjamin-Hochberg
correlations <- sapply(filename_funs[, 2:83], function(x) cor(filename_funs$hop2, x, method = "spearman"))
correlations.p <- apply(filename_funs[,2:83], 2, function(x) cor.test(filename_funs$hop2, x, method = "spearman", exact = T)$p.value)
p.adjust(correlations.p, "BH")


