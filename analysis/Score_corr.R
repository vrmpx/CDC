setwd("~/Workspace/CDC")
library(readr)
scores <- read_csv("~/Workspace/CDC/data/ES_2-5_79cases_newVnames.csv")

#Replace 999 with nan
scores[scores == 999] <- NA

# (auto-?)correlation
autocor <- function(name){
  cols <- sapply(2:5, function(x){paste0(name, x)})
  cor(scores[,cols], use="pairwise.complete.obs")
}


autocor("Eng")
autocor("FB")
autocor("Act")
autocor("Obs")
autocor("Con")
autocor("Car")
autocor("Hop")
