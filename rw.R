library(tidyverse)
library(plyr)
library(psych)
library(skimr)
library(corrplot)
library(Hmisc)
#library(psy)
library(afex)
#install.packages("devtools")
#devtools::install_github("cillianmiltown/R_desnum")
library(desnum)
#devtools::install_github("crsh/papaja")
library(papaja)
library(purrr)
library(foreign)


getwd()
df <- read.csv("../data/ICSMP_cleaned_data.csv")

skimr::skim(as.data.frame(df))


class(df$moralid4)
df$Moralid4R <- reverse.code(c(-1), df$Moralid4)
df$Moralid7R <- reverse.code(-1, df$Moralid7)


moral_identity_internal <- subset(df,select=c(
    Moralid1
  , Moralid2
  , Moralid4R
  , Moralid7R
  , Moralid10
))
