rm(list = ls())

load("data/data_with_means_raw.RData")
df_nobots <- read.csv("../data/ICSMP_cleaned_data_nobots.csv")

head(df)

no_bots <- df_nobots$participantID

df_raw <- df

x <- df
x <- subset(x, (participantID %in% no_bots))

df <- x

write.csv(df, "data/data_with_means.csv", row.names = FALSE)
save(df, file = "data/data_with_means.RData")

x <- df_raw

sum(x$att_check_nobots, na.rm = T)
variable.names(x)

46745-46490

x <- x[which(x$att_check_nobots==1),]
df <- x

write.csv(df, "data/data_with_means_semi_clean.csv", row.names = FALSE)
save(df, file = "data/data_with_means_semi_clean.RData")
