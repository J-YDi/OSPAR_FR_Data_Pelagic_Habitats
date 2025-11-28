#_______________________________________________________________________________
# Nom               : Gravelines_chla_submission.r
# Date de modif     : 24/11/2025
# Objet             : Mise en forme des donnees IGA Gravelines
# Auteurs           : J-Y. Dias
# Version R         : 4.5.0
#_______________________________________________________________________________

#_______________________________ Packages_______________________________________

# Function to install packages if not present and/or load them
# in state does not work for packages that install outside CRAN
loadpackages <- function(packages){
  for (pkg in packages){
    if (!requireNamespace(pkg,quietly = T)){
      install.packages(pkg)
      library(pkg,character.only = T)
    }else{
      library(pkg,character.only = T)
    }
  }
}
packages_needed <- c("readr","dplyr","ggplot2","cowplot","tidyr","lubridate","readxl")

loadpackages(packages_needed)

#__________________________Loading Original Data________________________________


IGA <-read_delim("data/SEANOE_Gravelines_Hydro_Plancton_2024.csv",delim = ";",col_types = cols(`Passage : Date` = col_date(format = "%d/%m/%Y")),
                 escape_double = FALSE,  trim_ws = TRUE)

# Change columns names
{
  colnames(IGA)[which(names(IGA) == "Passage : Année")] <- "ANNEE"
  colnames(IGA)[which(names(IGA) == "Lieu : Libellé")] <- "SITE"
  colnames(IGA)[which(names(IGA) == "Passage : Date")] <- "DATE"
  colnames(IGA)[which(names(IGA) == "Passage : Heure")] <- "HEURE"
  colnames(IGA)[which(names(IGA) == "Passage : Format UT de l'heure")] <- "FORMAT_HEURE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Niveau de prélèvement : Libellé")] <- "NIVEAU_PROFONDEUR"
  colnames(IGA)[which(names(IGA) == "Résultat : Paramètre : Code")] <- "PARAM"
  colnames(IGA)[which(names(IGA) == "Résultat : Taxon référent : Libellé")] <- "TAXON"
  colnames(IGA)[which(names(IGA) == "Résultat : Taxon saisi : Libellé")] <- "TAXON_USER"
  colnames(IGA)[which(names(IGA) == "Résultat : Numéro d'individu")] <- "N_INDIVIDU"
  colnames(IGA)[which(names(IGA) == "Résultat : Paramètre : Libellé")] <- "PARAM_TITRE"
  colnames(IGA)[which(names(IGA) == "Résultat : Valeur quantitative")] <- "VALEUR"
  colnames(IGA)[which(names(IGA) == "Résultat : Valeur qualitative : Libellé")] <- "VALEUR_QUANTI"
  colnames(IGA)[which(names(IGA) == "Résultat : Précision : Libellé")] <- "PRECISION"
  colnames(IGA)[which(names(IGA) == "Résultat : Incertitude : Valeur")] <- "INCERTITUDE"
  colnames(IGA)[which(names(IGA) == "Résultat : Incertitude : Type : Libellé")] <- "INCERTITUDE_UNIT"
  colnames(IGA)[which(names(IGA) == "Résultat : Support : Libellé")] <- "SUPPORT"
  colnames(IGA)[which(names(IGA) == "Résultat : Fraction : Libellé")] <- "FRACTION"
  colnames(IGA)[which(names(IGA) == "Résultat : Méthode : Libellé")] <- "METHODE"
  colnames(IGA)[which(names(IGA) == "Résultat : Unité : Symbole")] <- "UNITE"
  colnames(IGA)[which(names(IGA) == "Résultat : Unité : Libellé")] <- "UNITE_TITRE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Engin de prélèvement : Libellé")] <- "ENGIN"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Engin de prélèvement : Taille")] <- "ENGIN_TAILLE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Engin de prélèvement : Taille : Unité : Symbole")] <- "TAILLE_UNITE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Service préleveur : Libellé")] <- "LABO_PRELEVEMENT"
  colnames(IGA)[which(names(IGA) == "Résultat : Service analyste : Libellé")] <- "LABO_ANALYSTE"
  colnames(IGA)[which(names(IGA) == "Résultat : Service saisisseur : Libellé")] <- "LABO_SAISISSEUR"
  colnames(IGA)[which(names(IGA) == "Résultat : Niveau de qualité : Libellé")] <- "QUALITE"
  colnames(IGA)[which(names(IGA) == "Résultat : Commentaire de qualification")] <- "QUALITE_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Passage : Latitude (Min)")] <- "LON"
  colnames(IGA)[which(names(IGA) == "Passage : Longitude (Min)")] <- "LAT"
  colnames(IGA)[which(names(IGA) == "Passage : Commentaire")] <- "PASSAGE_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Commentaire")] <- "PRELEVEMENT_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Echantillon : Commentaire")] <- "ECHANTILLON_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Résultat : Commentaires")] <- "RESULTAT_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Sonde")] <- "SONDE"
  colnames(IGA)[which(names(IGA) == "Symbole unité de la sonde")] <- "UNITE_SONDE"
  colnames(IGA)[which(names(IGA) == "Lieu : Mnémonique")] <- "ID_SITE"
  colnames(IGA)[which(names(IGA) == "Passage : Identifiant")] <- "ID_PASSAGE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Identifiant")] <- "ID_PRELEVEMENT"
  colnames(IGA)[which(names(IGA) == "Echantillon : Identifiant")] <- "ID_ECHANTILLON"
  colnames(IGA)[which(names(IGA) == "Résultat : Identifiant")] <- "ID_RESULTAT"
  colnames(IGA)[which(names(IGA) == "Echantillon : Libellé du support")] <- "SUPPORT_ECHANTILLON"
  colnames(IGA)[which(names(IGA) == "Passage : Mois")] <- "MOIS"
}

# Unuseful columns
IGA <- IGA |>
  select(-c(`Passage : Mnémonique`,`Prélèvement : Mnémonique`,`Echantillon : Mnémonique`,`Résultat : Date de contrôle`:`Résultat : Date de qualification`,
            `Prélèvement : Service préleveur : Code`,
            INCERTITUDE,INCERTITUDE_UNIT,PRECISION,ENGIN_TAILLE,TAILLE_UNITE,QUALITE_COMMENTAIRE,PASSAGE_COMMENTAIRE,
            MOIS,ANNEE,`Passage : Delta UT`,`Résultat : Groupe de taxons : Libellé`,`Passage : Latitude (Max)`,`Passage : Coordonnées : Source`,`Résultat : Commentaire`,
            `Passage : Sonde`,`Passage : Sonde : Unité : Symbole`,`Lieu : Identifiant`,`Echantillon : Support : Libellé`:`Date d'extraction de la donnée`))

# Quality filter
IGA <- IGA |>
  filter(QUALITE != "Douteux") |>
  select(-QUALITE)

# Datetime as it should be
IGA$HEURE[is.na(IGA$HEURE)] <- as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "UTC")
IGA$DATETIME <- as.POSIXct(paste(IGA$DATE,IGA$HEURE),format = "%Y-%m-%d %H:%M:%S",tz="UTC")

# Keep only chla
IGA_HYDRO <- IGA |>
  filter(PARAM =="CHLOROA") |>
  select(-c(SUPPORT,TAXON,TAXON_USER,N_INDIVIDU,VALEUR_QUANTI,FRACTION,UNITE_TITRE))

# Quality flag by HPEL
IGA_HYDRO$Q_HPEL <- NA

# All variables have the same unit so we can delete this column
# There are different "ENGIN" but useless so we delete also
IGA_HYDRO <- select(IGA_HYDRO,-c(UNITE,ENGIN))

# 0 are flags as doubtful
IGA_HYDRO$Q_HPEL <- ifelse(IGA_HYDRO$VALEUR == 0,4,0)

IGA_HYDRO <- IGA_HYDRO %>%
  arrange(SITE, DATE)

# Preparation to confront raw and processed data
remove_accents <- function(df) {
  # Vérifie que stringi est dispo
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Le package 'stringi' est requis. Installez-le avec install.packages('stringi').")
  }
  
  df_clean <- as.data.frame(
    lapply(df, function(x) {
      if (is.character(x)) {
        stringi::stri_trans_general(x, "Latin-ASCII")
      } else {
        x
      }
    }),
    stringsAsFactors = FALSE
  )
  
  return(df_clean)
}

IGA_HYDRO_clean <- remove_accents(IGA_HYDRO) 


HYDRO_Pro <-read_delim("data/SEANOE_Gravelines_Process_Hydro.csv",delim = ";",
                       escape_double = FALSE,  trim_ws = TRUE,locale = locale(encoding = "Windows-1252"))

{
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Passage...Annee")] <- "ANNEE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Lieu.de.surveillance...Libelle")] <- "SITE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Passage...Date")] <- "DATE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Passage...Heure")] <- "HEURE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Passage...Format.UT.de.l'heure")] <- "FORMAT_HEURE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Prelevement...Niveau")] <- "NIVEAU_PROFONDEUR"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Code.parametre")] <- "PARAM"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Nom.du.taxon.referent")] <- "TAXON"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Nom.du.taxon.saisi")] <- "TAXON_USER"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Numero.d'individu")] <- "N_INDIVIDU"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Libelle.parametre")] <- "PARAM_TITRE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Valeur.de.la.mesure")] <- "VALEUR"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Valeur.qualitative")] <- "VALEUR_QUANTI"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Libelle.precision")] <- "PRECISION"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Incertitude")] <- "INCERTITUDE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Symbole.de.l'unite.incertitude")] <- "INCERTITUDE_UNIT"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Libelle.support")] <- "SUPPORT"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Libelle.fraction")] <- "FRACTION"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Libelle.methode")] <- "METHODE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Symbole.unite.de.mesure.associe.au.quintuplet")] <- "UNITE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Libelle.unite.de.mesure.associe.au.quintuplet")] <- "UNITE_TITRE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Libelle.de.l'engin.de.prelevement")] <- "ENGIN"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Taille.de.l'engin.de.prelevement")] <- "ENGIN_TAILLE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Symbole.unite.de.la.taille.de.l'engin.de.prelev.")] <- "TAILLE_UNITE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Prelevement...Service.preleveur...Libelle")] <- "LABO_PRELEVEMENT"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Service.analyste...Libelle")] <- "LABO_ANALYSTE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Service.saisisseur...Libelle")] <- "LABO_SAISISSEUR"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Niveau.de.qualite")] <- "QUALITE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Commentaire.de.qualification")] <- "QUALITE_COMMENTAIRE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Coordonnees.passage...Coordonnees minx")] <- "LON"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Coordonnees.passage...Coordonnees miny")] <- "LAT"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Passage...Commentaire")] <- "PASSAGE_COMMENTAIRE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Prelevement...Commentaire")] <- "PRELEVEMENT_COMMENTAIRE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Echantillon...Commentaire")] <- "ECHANTILLON_COMMENTAIRE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Commentaires")] <- "RESULTAT_COMMENTAIRE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Sonde")] <- "SONDE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Symbole.unite.de.la.sonde")] <- "UNITE_SONDE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Lieu.de.surveillance...Mnemonique")] <- "ID_SITE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Passage...Identifiant.interne")] <- "ID_PASSAGE"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Prelevement...Identifiant.interne")] <- "ID_PRELEVEMENT"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Echantillon...Identifiant.interne")] <- "ID_ECHANTILLON"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Resultat...Identifiant")] <- "ID_RESULTAT"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Echantillon...Libelle.du.support")] <- "SUPPORT_ECHANTILLON"
  colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "Passage...Mois")] <- "MOIS"
}

# Unuseful columns
HYDRO_Pro <- HYDRO_Pro|>
  select(-c(`Coordonnees.passage...Coordonnees.maxx`,`Coordonnees.passage...Coordonnees.maxy`,MOIS,ANNEE,FORMAT_HEURE,Passage...Semaine,FORMAT_HEURE,UNITE_TITRE))

# Compare raw and processed values
colnames(HYDRO_Pro)[which(names(HYDRO_Pro) == "VALEUR")] <- "VALEUR_PRO"

IGA_HYDRO <- left_join(IGA_HYDRO_clean,select(HYDRO_Pro,DATE,HEURE,METHODE,PARAM,VALEUR_PRO),by = join_by(DATE, HEURE, METHODE,PARAM))
IGA_HYDRO <- select(IGA_HYDRO,SITE:VALEUR,VALEUR_PRO,METHODE,ID_SITE:Q_HPEL)

# Associate methods with BODC code
BODC_QUADRIGE <- remove_accents(read_excel("data/BODC_QUADRIGE_DIAS_JY.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "skip", "skip")))

IGA_HYDRO <- left_join(IGA_HYDRO,BODC_QUADRIGE)

# Keep only surface data
IGA_HYDRO <- filter(IGA_HYDRO,NIVEAU_PROFONDEUR == "Surface (0-1m)")

# New columns
IGA_HYDRO <- pivot_wider(IGA_HYDRO,names_from = "BODC",values_from = VALEUR)

# OCEAN formating

OCEAN <- IGA_HYDRO
OCEAN$Cruise <- as.numeric(format(IGA_HYDRO$DATETIME, "%Y"))
OCEAN$Type <- "*"
OCEAN$`yyyy-mm-ddThh:mm:ss.sss` <- format(IGA_HYDRO$DATETIME, "%Y-%m-%dT%H:%MZ")
OCEAN$`Longitude [degrees_east]` <- 2.15003
OCEAN$`Latitude [degrees_north]` <- 51.02219
OCEAN$`Bot. Depth [m]` <- NA
OCEAN$`Platform Code` <- "ZZ99"
OCEAN$`Device Category Code` <- "30" # Bottle and CTD
OCEAN$`Distributor Code` <- "6088"
OCEAN$`Custodian Code` <- "6088"
OCEAN$`Originator Code` <- "6088"
OCEAN$`Project Code` <- NA
OCEAN$`Depth [m]` <- 1
OCEAN$`Chlorophyll a trichroma CPHLSSP1` <- IGA_HYDRO$CPHLSSP1
OCEAN$`QV:ODV:Chlorophyll a trichroma CPHLSSP1` <- with(OCEAN,ifelse( is.na(IGA_HYDRO$CPHLSSP1),1,
                                                                       ifelse(IGA_HYDRO$CPHLSSP1 == 0,4,0)))
OCEAN$`Chlorophyll a monochroma CPHLSXP1` <- IGA_HYDRO$CPHLSXP1
OCEAN$`QV:ODV:Chlorophyll a monochroma CPHLSXP1` <- with(OCEAN,ifelse( is.na(IGA_HYDRO$CPHLSXP1),1,
                                                                       ifelse(IGA_HYDRO$CPHLSXP1 == 0,4,0)))


IGA_OCEAN <- select(OCEAN,Cruise:`QV:ODV:Chlorophyll a monochroma CPHLSXP1`)

IGA_OCEAN <- unique(IGA_OCEAN)
IGA_OCEAN <- IGA_OCEAN %>%
  filter(!(is.na(`Chlorophyll a trichroma CPHLSSP1`) & is.na(`Chlorophyll a monochroma CPHLSXP1`)))

# As we cannot submit true replicates we compute the mean by date
IGA_OCEAN <- IGA_OCEAN |>
  group_by(`yyyy-mm-ddThh:mm:ss.sss`, `Longitude [degrees_east]`, `Latitude [degrees_north]`) |>
  mutate(
    `Chlorophyll a trichroma CPHLSSP1` = mean(`Chlorophyll a trichroma CPHLSSP1`, na.rm = TRUE),
    `Chlorophyll a monochroma CPHLSXP1` = mean(`Chlorophyll a monochroma CPHLSXP1`, na.rm = TRUE),
    
    `QV:ODV:Chlorophyll a trichroma CPHLSSP1` = min(`QV:ODV:Chlorophyll a trichroma CPHLSSP1`, na.rm = TRUE),
    `QV:ODV:Chlorophyll a monochroma CPHLSXP1` = min(`QV:ODV:Chlorophyll a monochroma CPHLSXP1`, na.rm = TRUE),
  ) |>
  ungroup() |>
  unique()

IGA_OCEAN <- unique(IGA_OCEAN)

IGA_OCEAN$Station <- paste0("GRAV - Canal Amenee",rownames(IGA_OCEAN))

IGA_OCEAN <- select(IGA_OCEAN,Cruise,Station,Type:`QV:ODV:Chlorophyll a monochroma CPHLSXP1`)

pre_header <- c(
  "//ICES_parameter_mapping",
  "//<subject>ICES:LOCAL:Depth [m]</subject><object>ICES:P01::DPSAZZ01</object><units>ICES:P06::ULAA</units>",
  "//<subject>ICES:LOCAL:Chlorophyll a trichroma CPHLSSP1</subject><object>ICES:P01::CPHLSSP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:Chlorophyll a monochroma CPHLSXP1</subject><object>ICES:P01::CPHLSXP1</object><units>ICES:P06::UMMC</units>",
  "//",
  paste(colnames(IGA_OCEAN), collapse = ",")
)

# Final dataset
write_lines(pre_header, "output/OCEAN_PH2_IGA_Ready_version.csv",na = "") # metadata lines
write_excel_csv(IGA_OCEAN,file = "output/OCEAN_PH2_IGA_Ready_version.csv", append = TRUE,na = "") # complete dataset


