rm(list = ls())

load("data/data_with_means_raw.RData")
df_nobots <- read.csv("../data/ICSMP_cleaned_data_nobots.csv")

head(df)

no_bots <- df_nobots$participantID


x <- df
x <- subset(x, (participantID %in% no_bots))

df <- x

write.csv(df, "data/data_with_means.csv", row.names = FALSE)
save(df, file = "data/data_with_means.RData")
