#install.packages("dplyr")
library(dplyr)
library(stringr)

df=read.csv("communes_10k_20k.csv",sep=",")
df2=read.csv("SAE_rolland_BDD_regroupée.csv",sep=";")



df2 <- df2 %>%
  mutate(CODGEO = str_pad(CODGEO, width = 5, pad = "0"))

df <- df %>%
  left_join(df2, by = c("COM" = "CODGEO"))

write.table(resultat,file="Communes10-20K-Maslow.csv",sep=";",row.names=F,fileEncoding = "utf-8")

head(df,n=1)
