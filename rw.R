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
df$moralid4
df %>%
  mutate(moralid4=
  dplyr::recode(moralid4,
              "1"="9"))



#### moral identity internal alpha and combine ####

df$moralid4R <- reverse.code(c(-1), df$moralid4)
df$moralid7R <- reverse.code(-1, df$moralid7)


moral_identity_internal <- subset(df,select=c(
    moralid1
  , moralid2
  , moralid4R
  , moralid7R
  , moralid10
))



alpha(moral_identity_internal)

df$moral_identity_internal <- rowMeans(moral_identity_internal)




#### moral identity symbolic alpha and combine ####

moral_identity_symbolic <- subset(df,select=c(
    moralid3
  , moralid5
  , moralid6
  , moralid8
  , moralid9
))

alpha(moral_identity_symbolic)

df$moral_identity_symbolic <- rowMeans(moral_identity_symbolic, na.rm = TRUE)


#### physical contact alpha and combine ####

df$contact2R <- reverse.code(-1, df$contact2)

physical_contact_tot <- subset(df,select=c(
    contact1
  , contact2R
  , contact3
  , contact4
  , contact5))

alpha(physical_contact_tot)

df$physical_contact_tot <- rowMeans(physical_contact_tot, na.rm = TRUE)


#### physical hygiene alpha and combine ####

physical_hygiene_tot <- subset(df,select=c(
    hygiene1
  , hygiene2
  , hygiene3
  , hygiene4
  , hygiene5
))

alpha(physical_hygiene_tot)

df$physical_hygiene_tot <- rowMeans(physical_hygiene_tot, na.rm = TRUE)

#### policy support alpha and combine ####

policy_support_tot <- subset(df,select=c(
    psupport1
  , psupport2
  , psupport3
  , psupport4
  , psupport5
))

alpha(policy_support_tot)

df$policy_support_tot <- rowMeans(policy_support_tot, na.rm = TRUE)



#### narcissism admiration alpha and combine ####

narcissism_admiration_tot <- subset(df,select=c(
    narc2
  , narc4
  , narc5
))

alpha(narcissism_admiration_tot)

df$narcissism_admiration_tot <- rowMeans(narcissism_admiration_tot, na.rm = TRUE)

#### narcissism rivalry alpha and combine ####

narcissism_rivalry_tot <- subset(df,select=c(
    narc1
  , narc3
  , narc6
))

alpha(narcissism_rivalry_tot)

df$narcissism_rivalry_tot <- rowMeans(narcissism_rivalry_tot, na.rm = TRUE)

#### collective narcissism alpha and combine ####

collective_narcissism_tot <- subset(df,select=c(
    cnarc1
  , cnarc2
  , cnarc3
))

alpha(collective_narcissism_tot)

df$collective_narcissism_tot <- rowMeans(collective_narcissism_tot, na.rm = TRUE)


#### trait self control alpha and combine ####

df$slfcont3R <- reverse.code(-1, df$slfcont3)
df$slfcont4R <- reverse.code(-1, df$slfcont4)

self_control_tot <- subset(df,select=c(
    slfcont1
  , slfcont2
  , slfcont3R
  , slfcont4R
))

alpha(self_control_tot)

df$self_control_tot <- rowMeans(self_control_tot, na.rm = TRUE)

#### open minded alpha and combine ####

df$omind1R <- reverse.code(-1, df$omind1)
df$omind5R <- reverse.code(-1, df$omind5)
df$omind6R <- reverse.code(-1, df$omind6)

open_minded_tot <- subset(df,select=c(
    omind1R
  , omind2
  , omind3
  , omind4
  , omind5R
  , omind6R
))

alpha(open_minded_tot)

df$open_minded_tot <- rowMeans(open_minded_tot, na.rm = TRUE)

#### trait optimism alpha and combine ####

trait_optimism_tot <- subset(df,select=c(
    optim1
  , optim2
))

alpha(trait_optimism_tot)

df$trait_optimism_tot <- rowMeans(trait_optimism_tot, na.rm = TRUE)

#### National Identification ####

national_ID_tot <- subset(df,select=c(
    nidentity1
  , nidentity2
))

alpha(national_ID_tot)

df$national_ID_tot <- rowMeans(national_ID_tot, na.rm = TRUE)

#### Conspiracy Theories ####

conspiracy_tot <- subset(df,select=c(
    ctheory1
  , ctheory2
  , ctheory3
  , ctheory4
))

alpha(conspiracy_tot)

df$conspiracy_tot <- rowMeans(conspiracy_tot, na.rm = TRUE)

#### Social Belonging ####

social_belonging_tot <- subset(df,select=c(
    sbelong1
  , sbelong2
  , sbelong3
  , sbelong4
))

alpha(social_belonging_tot)

df$social_belonging_tot <- rowMeans(social_belonging_tot, na.rm = TRUE)


#### Risk Perception ####

risk_perception_tot <- subset(df,select=c(
  risk_perception__1
  , risk_perception__2
))

alpha(risk_perception_tot)

df$risk_perception_tot <- rowMeans(risk_perception_tot, na.rm = TRUE)


####
as.data.frame(table(df$country_name,df$Power.Distance))

length(levels(as.factor(df$country_name[which(is.na(df$Power.Distance==0)==FALSE)])))
length(levels(as.factor(df$country_name[which(is.na(df$Individualism==0)==FALSE)])))
length(levels(as.factor(df$country_name[which(is.na(df$Uncertainty.Avoidance==0)==FALSE)])))
length(levels(as.factor(df$country_name[which(is.na(df$Masculinity==0)==FALSE)])))

length(levels(as.factor(df$country_name[which(is.na(df$Indulgence==0)==FALSE)])))
length(levels(as.factor(df$country_name[which(is.na(df$Long.Term.Orientation==0)==FALSE)])))


length(levels(as.factor(df$country_name[which(is.na(df$CTL_DG==0)==FALSE)])))
length(levels(as.factor(df$country_name[which(is.na(df$CTL_DS==0)==FALSE)])))
length(levels(as.factor(df$country_name[which(is.na(df$CTL_C==0)==FALSE)])))

x <- x %>% group_by(country_name) %>% do(data = (.)) %>% with( set_names(data, country_name) )

