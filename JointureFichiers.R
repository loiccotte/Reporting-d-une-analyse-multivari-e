library(dplyr)
library(readr)
library(purrr)

# Chemin du dossier contenant les fichiers CSV
chemin_fichiers <- "Données/Ensemble-com-2021_csv"  # à adapter

# Lister tous les fichiers CSV commençant par "equip"
fichiers <- list.files(path = chemin_fichiers, pattern = "^equip.*\\.csv$", full.names = TRUE)

# Lire tous les fichiers CSV avec séparateur ; et encodage UTF-8
liste_df <- lapply(fichiers, function(f) {
  read_delim(f, delim = ";", locale = locale(encoding = "UTF-8"))
})

# Identifier la colonne commune à tous les fichiers (ex: "CODGEO" ou "Commune")
colonnes_communes <- Reduce(intersect, lapply(liste_df, names))
colonne_jointure <- if ("CODGEO" %in% colonnes_communes) "CODGEO" else "Commune"

# Fusionner tous les fichiers avec un full_join sur la colonne identifiée
df_final <- reduce(liste_df, full_join, by = colonne_jointure)

# Exporter le résultat en CSV séparé par ; et encodé en UTF-8
write_delim(df_final, "fusion_equipements_2021.csv", delim = ";", na = "", append = FALSE)
