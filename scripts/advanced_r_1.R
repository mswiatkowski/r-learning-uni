#install.packages('psych')
library(psych)
library(tidyverse)
library(rio)

gcbs <- readRDS("~/personal_stuff/r-learning-uni/gcbs.rds")

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

means <- colMeans(gcbs)
maxes <- colMax(gcbs)
mins <- colMin(gcbs)

summary_df <- data.frame(means, maxes, mins)

EFA_model <- fa(gcbs)
