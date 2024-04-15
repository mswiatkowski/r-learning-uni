#install.packages('psych')
library(psych)
library(tidyverse)
library(rio)

gcbs <- readRDS("~/personal_stuff/r-learning-uni/gcbs.rds")

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

summary_custom <- function(data){
  means <- colMeans(data)
  maxes <- colMax(data)
  mins <- colMin(data)
  summary_df <- data.frame(means, maxes, mins)
  return(summary_df)
}

EFA_model <- fa(gcbs)

EFA_model
EFA_model$loadings


head(gcbs)

rowSums(head(gcbs))

head(EFA_model$scores)
summary(EFA_model$scores)

describe(gcbs)

error.dots(gcbs)


N <- nrow(gcbs)
indices <- seq(1, N)

indices_EFA <- sample(indices, floor((.5*N)))

indices_CFA <- indices[!(indices %in% indices_EFA)]

head(indices_EFA)

gcbs_EFA <- gcbs[indices_EFA, ]
gcbs_CFA <- gcbs[indices_CFA, ]



gcbs_EFA_summary_df <- summary_custom(gcbs_EFA)
gcbs_CFA_summary_df <- summary_custom(gcbs_CFA)

describe(gcbs_EFA)
describe(gcbs_CFA)






