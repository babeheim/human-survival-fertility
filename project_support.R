
library(rethinking)
library(lme4)
library(mgcv)
library(RColorBrewer)
library(knitr)
library(rmarkdown)
library(gdata)
library(sp)
library(RCurl)
library(readxl)
library(dplyr)

set.seed(1854)

save_temp <- TRUE

options(warnPartialMatchDollar=TRUE)

files <- list.files("R", full.names = TRUE)
for (i in 1:length(files)) {
  source(files[i])
}