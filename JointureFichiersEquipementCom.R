library(dplyr)

listecom=read.csv("communes_10k_20k.csv")
equip=read.csv("fusion_equipements_2021.csv",sep=";")

# Harmoniser les formats de codes
listecom <- listecom %>%
  mutate(COM = sprintf("%05s", COM))

# Jointure gauche : garder seulement les communes de listecom
jointure <- listecom %>%
  left_join(equip, by = c("COM" = "CODGEO"))

# Sauvegarder si besoin
write_delim(jointure, "jointure_communes_equipements.csv", delim = ";")


# Affiche quelques valeurs pour vérifier visuellement
head(listecom$COM)
head(equip$CODGEO)

# Vérifie les longueurs exactes
unique(nchar(listecom$COM))
unique(nchar(equip$CODGEO))
