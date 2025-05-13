#install.packages("dplyr")
library(dplyr)
df=read.csv("Données/Ensemble-com-2021_csv/donnees_communes.csv",sep=";")


df = df %>%
  filter(PMUN >= 10000 & PMUN < 20000)
  
write.csv(df, "communes_10k_20k.csv", row.names = FALSE, fileEncoding = "UTF-8")
