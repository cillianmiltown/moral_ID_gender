
rm(list = ls())
#source("set_up_data_calculate_means.R")
load("data/data_with_means_raw.RData")

df$sex3 <- as.factor(df$sex1)
df$sex3 <- dplyr::recode(df$sex3,
                         "1"="male"
                         ,"2"="female"
                         ,"3"="other")

df$country_lower <- tolower(df$country)
df$country_name <- countrycode(df$ISO3, origin = "iso3c", destination = "country.name")


table(df$sex3)
sum(is.na(df$sex3))

desnum::descriptives(df$age)
df_full <- df
rm(df)
rm(list = ls())
#source("set_up_data_calculate_means.R")
load("data/data_with_means.RData")

df$sex3 <- as.factor(df$sex1)
df$sex3 <- dplyr::recode(df$sex3,
                         "1"="male"
                         ,"2"="female"
                         ,"3"="other")

df$country_lower <- tolower(df$country)
df$country_name <- countrycode::countrycode(df$ISO3, origin = "iso3c", destination = "country.name")


table(df$sex3)
sum(is.na(df$sex3))

desnum::descriptives(df$age)


x <- df_full

table(x$country_name)
country_Ns_full <- table(x$country_name)
country_Ns <- table(x$country_name)


cbind(country_Ns,country_Ns_full)

cbind(
  rownames(country_Ns)
  ,rownames(country_Ns_full)
)

country_full_names <- rownames(country_Ns_full)
country_names <- rownames(country_Ns)

sum(country_full_names %in% country_names)

country_full_names[which(!(country_full_names %in% country_names))]
Ghana <- df_full[which(df_full$country_name=="Ghana"),]

df$country_name

table(x$country_name)
country_Ns <- table(x$country_name)
sum(as.vector(country_Ns)<50)

sum(as.vector(country_Ns)<20)
sum(as.vector(country_Ns)<100)

sum(as.vector(country_Ns)<500)-sum(as.vector(country_Ns)<100)

##### cultural variables #####
levels(as.factor(df$country_name[which(is.na(df$Power.Distance))]))
levels(as.factor(df$country_name[which(is.na(df$Individualism))]))
levels(as.factor(df$country_name[which(is.na(df$Masculinity))]))
levels(as.factor(df$country_name[which(is.na(df$Uncertainty.Avoidance))]))
levels(as.factor(df$country_name[which(is.na(df$Long.Term.Orientation))]))
levels(as.factor(df$country_name[which(is.na(df$Indulgence))]))
levels(as.factor(df$country_name[which(is.na(df$CTL_DG))]))
levels(as.factor(df$country_name[which(is.na(df$CTL_DS))]))
levels(as.factor(df$country_name[which(is.na(df$CTL_C))]))


#### Countries with Cultural variables ####

levels(as.factor(df$country_name[which(is.na(df$Power.Distance)==FALSE)]))
levels(as.factor(df$country_name[which(is.na(df$Individualism)==FALSE)]))
levels(as.factor(df$country_name[which(is.na(df$Masculinity)==FALSE)]))
levels(as.factor(df$country_name[which(is.na(df$Uncertainty.Avoidance)==FALSE)]))
levels(as.factor(df$country_name[which(is.na(df$Long.Term.Orientation)==FALSE)]))
levels(as.factor(df$country_name[which(is.na(df$Indulgence)==FALSE)]))
levels(as.factor(df$country_name[which(is.na(df$CTL_DG)==FALSE)]))
levels(as.factor(df$country_name[which(is.na(df$CTL_DS)==FALSE)]))
levels(as.factor(df$country_name[which(is.na(df$CTL_C)==FALSE)]))



