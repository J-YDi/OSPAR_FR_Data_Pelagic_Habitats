#_______________________________________________________________________________
# Nom               : REPHY_submission_withNUT.r
# Date de modif     : 05/12/2025
# Objet             : Mise en forme des donnees REPHY
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

REPHY <- read_delim("data/QUADRIGE_20250618_60457410_OSPAR_DIAS.csv",
                    delim = ";", col_types = cols(`Passage : Date` = col_date(format = "%d/%m/%Y")), escape_double = FALSE, trim_ws = TRUE)

REPHY <- select(REPHY,-`Date d'extraction de la donnée`)

options(scipen = 999)

REPHY <- select(REPHY, -c(`Résultat : Service analyste : Libellé`,`Regroupement géo : Libellé : ZONESMARINES`,`Lieu : Identifiant`))

# Not keeping the doubtful data
REPHY <- REPHY |> 
  filter(`Passage : Niveau de qualité : Libellé` != "Douteux" ) |>
  select(-`Passage : Niveau de qualité : Libellé`)

REPHY <- REPHY |> 
  filter(`Prélèvement : Niveau de qualité : Libellé` != "Douteux" ) |>
  select(-`Prélèvement : Niveau de qualité : Libellé`)

REPHY <- select(REPHY, -`Echantillon : Niveau de qualité : Libellé`)

REPHY <- REPHY |> 
  filter(`Résultat : Niveau de qualité : Libellé` != "Douteux" ) |>
  select(-`Résultat : Niveau de qualité : Libellé`)

# New columns names
colnames(REPHY)[which(names(REPHY) == "Regroupement géo : Code : ZONESMARINES")] <- "ZM" 
colnames(REPHY)[which(names(REPHY) == "Lieu : Mnémonique")] <- "ID_SITE"
colnames(REPHY)[which(names(REPHY) == "Lieu : Libellé")] <- "SITE"
colnames(REPHY)[which(names(REPHY) == "Lieu : Latitude (Min)")] <- "LAT"
colnames(REPHY)[which(names(REPHY) == "Lieu : Longitude (Min)")] <- "LON"
colnames(REPHY)[which(names(REPHY) == "Passage : Date")] <- "DATE"
colnames(REPHY)[which(names(REPHY) == "Prélèvement : Heure")] <- "HEURE"
colnames(REPHY)[which(names(REPHY) == "Prélèvement : Niveau de prélèvement : Libellé")] <- "NIVEAU_PROFONDEUR"
colnames(REPHY)[which(names(REPHY) == "Prélèvement : Immersion : Valeur")] <- "PROFONDEUR"
colnames(REPHY)[which(names(REPHY) == "Résultat : Programme : Code : Liste")] <- "PROGRAM"
colnames(REPHY)[which(names(REPHY) == "Résultat : Paramètre : Code")] <- "PARAM"
colnames(REPHY)[which(names(REPHY) == "Résultat : Méthode : Libellé")] <- "METHODE"
colnames(REPHY)[which(names(REPHY) == "Résultat : Unité : Symbole")] <- "UNITE"
colnames(REPHY)[which(names(REPHY) == "Résultat : Valeur quantitative")] <- "VALEUR"
colnames(REPHY)[which(names(REPHY) == "Résultat : Service analyste : Code")] <- "LABO"
colnames(REPHY)[which(names(REPHY) == "Passage : Identifiant")] <- "ID_PASSAGE"
colnames(REPHY)[which(names(REPHY) == "Prélèvement : Identifiant")] <- "ID_PRELEVEMENT"
colnames(REPHY)[which(names(REPHY) == "Echantillon : Identifiant")] <- "ID_ECHANTILLON"
colnames(REPHY)[which(names(REPHY) == "Résultat : Identifiant")] <- "ID_RESULTAT"

colnames(REPHY)[which(names(REPHY) == "Résultat : Taxon référent : Identifiant (TAXON_NAME_ID)")] <- "TAXON_REF_IREMER"
colnames(REPHY)[which(names(REPHY) == "Résultat : Taxon référent : Libellé")] <- "TAXON"
colnames(REPHY)[which(names(REPHY) == "Résultat : Taxon référent : Libellé")] <- "TAXON"
colnames(REPHY)[which(names(REPHY) == "Résultat : Taxon référent : Niveau taxinomique")] <- "NIVEAU_TAX"
colnames(REPHY)[which(names(REPHY) == "Résultat : Taxon référent : Taxon parent : Libellé")] <- "TAXON_SUP"
colnames(REPHY)[which(names(REPHY) == "Résultat : Taxon référent : WoRMS : AphiaID")] <- "aphiaID"

REPHY$HEURE[is.na(REPHY$HEURE)] <- as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "UTC")
REPHY$DATETIME <- as.POSIXct(paste(REPHY$DATE,REPHY$HEURE),format = "%Y-%m-%d %H:%M:%S",tz="UTC")

REPHY <- select(REPHY,"PROGRAM","ZM","ID_SITE","SITE","LAT","LON","DATETIME","DATE","HEURE","NIVEAU_PROFONDEUR","PROFONDEUR","ID_PASSAGE","ID_PRELEVEMENT",
                "PARAM","METHODE","UNITE","ID_ECHANTILLON","LABO","TAXON_REF_IREMER","TAXON","NIVEAU_TAX","TAXON_SUP","aphiaID","VALEUR","ID_RESULTAT",
                "Passage : Mnémonique","Passage : Commentaire","Prélèvement : Mnémonique","Prélèvement : Commentaire","Echantillon : Mnémonique","Echantillon : Commentaire",
                "Résultat : Paramètre : Libellé","Résultat : Commentaire")


# Filter marine areas

Zones_marines <- read_csv("data/Additional_data/Zones_marines_OK.csv")
Zones_marines<- select(Zones_marines,c(ZM,`Code Facade`))
REPHY$ZM <- as.double(REPHY$ZM)
REPHY <- left_join(REPHY,Zones_marines)

# Keep only atlantic ocean and English Channel
REPHY <- REPHY |>
  filter(as.numeric(ZM) %in% filter(Zones_marines,`Code Facade` %in% c(1,2))$ZM) |>
  select(-c(ZM,`Code Facade`))


REPHY <- REPHY |>
  select(-c(`Passage : Mnémonique`)) |>
  filter(is.na(REPHY$`Echantillon : Mnémonique`)) |> # nothing to note
  select(-c(`Echantillon : Mnémonique`)) |>
  select(-c(`Prélèvement : Mnémonique`)) # nothing to note

# Not useful commentary as it is difficult to check them all
REPHY <- REPHY |>
  select(-c(`Passage : Commentaire`,`Prélèvement : Commentaire`,`Echantillon : Commentaire`,`Résultat : Commentaire`))

# Sampling lab is not useful 
REPHY <- select(REPHY,-LABO)

# In the NIVEAU_TAX column, there is no need for redundancy/translation.
REPHY$NIVEAU_TAX <- sub(" -.*", "", REPHY$NIVEAU_TAX)

# Make it chronological
REPHY <- REPHY %>%
  arrange(SITE, DATE)

REPHY <- select(REPHY, -`Résultat : Paramètre : Libellé`)

# Correct depth levels according to the nomenclature in the ‘Manual for data entry into the Quadrige database for programmes: REPHY-REPHYTOX Version 5 of 2024’.
# Based on FLORTOT sampling issues, for HYDRO there are other levels that require correction: not relevant for OSPAR.

REPHY$NIVEAU_PROFONDEUR[REPHY$NIVEAU_PROFONDEUR == "2 mètres"] <- "Mi-profondeur"
REPHY$NIVEAU_PROFONDEUR[REPHY$NIVEAU_PROFONDEUR == "de 3 à 5 mètres"] <- "Mi-profondeur"

REPHY <- REPHY %>%
  mutate(NIVEAU_PROFONDEUR = case_when(
    NIVEAU_PROFONDEUR == "Mi-profondeur" & PROFONDEUR <= 1 ~ "Surface (0-1m)",
    NIVEAU_PROFONDEUR == "Surface (0-1m)" & PROFONDEUR > 1 ~ "Mi-profondeur",
    NIVEAU_PROFONDEUR == "Colonne d'eau" & PROFONDEUR <= 1 ~ "Surface (0-1m)",
    NIVEAU_PROFONDEUR == "Fond/sonde-1m" & PROFONDEUR <= 1 ~ "Surface (0-1m)",
    TRUE ~ NIVEAU_PROFONDEUR
  ))

### Working on FLORTOT data ######
FLORTOT <- filter(REPHY,PARAM == "FLORTOT")

# Replace 0 by NA
FLORTOT[FLORTOT$VALEUR == 0,"VALEUR"] <- NA

# Associate taxonomy and aphiaID
FLORTOT <- select(FLORTOT,-aphiaID)

Taxonomy_correspondance_REPHY <- read_xlsx("data/Additional_data/Taxonomy_correspondance_REPHY_rank_complete.xlsx")
FLORTOT <- left_join(FLORTOT,Taxonomy_correspondance_REPHY)

# Delete some unuseful columns
FLORTOT <- FLORTOT |>
  select(-c(PARAM,METHODE,UNITE,TAXON_REF_IREMER,TAXON_SUP))

# Duplicate counts: I follow Maud Lemoine's recommendations for REPHY: we take the maximum value.
# We preserve duplicates if samples are different.
FLORTOT <- FLORTOT |>
  select(-c(ID_SITE,ID_PRELEVEMENT,ID_PASSAGE,NIVEAU_TAX,ID_RESULTAT,Kingdom:rank)) |>
  pivot_wider(names_from = "TAXON",values_from = VALEUR,values_fn = max) |>
  pivot_longer(cols = `Scrippsiella + Ensiculifera + Pentapharsodinium`:Coccolithus,names_to = "TAXON",values_to = "VALEUR") |>
  filter(!is.na(VALEUR))

# Correcting depth : if Surface 0-1m without numeric depth, depth is 1m
FLORTOT$PROFONDEUR <- ifelse(FLORTOT$NIVEAU_PROFONDEUR == "Surface (0-1m)" & is.na(FLORTOT$PROFONDEUR),1,FLORTOT$PROFONDEUR)

### Working on physico-chim data ###
HYDRO_param <- c("OXYGENE","SALI","TEMP","PO4","NO3+NO2","SIOH","TURB","TURB-FNU","CHLOROA","NH4")
HYDRO <- filter(REPHY, PARAM %in% HYDRO_param)

# Only keep stations that are in the FLORTOT list
HYDRO <- filter(HYDRO,SITE %in% FLORTOT$SITE)

# 0 becomes NA as it doesnt have sense
HYDRO[HYDRO$VALEUR <= 0,"VALEUR"] <- NA

# Associate methods to BODC codes
BODC_QUADRIGE <- read_excel("data/BODC_QUADRIGE_DIAS_JY.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "skip", "skip"))

HYDRO <- left_join(HYDRO,BODC_QUADRIGE)

# Keep only surface data (only 3 is not at the surface)
REPHY_HYDRO <- filter(HYDRO,NIVEAU_PROFONDEUR == "Surface (0-1m)")

# New columns 
REPHY_HYDRO <- REPHY_HYDRO |>
  select(-c(PARAM,METHODE,ID_RESULTAT,UNITE,ID_ECHANTILLON,ID_PRELEVEMENT,ID_PASSAGE)) |>
  filter(!is.na(VALEUR)) |>
  pivot_wider(names_from = "BODC",values_from = VALEUR, values_fn = mean)

#OCEAN formating

OCEAN <- REPHY_HYDRO
OCEAN$Cruise <- as.numeric(format(REPHY_HYDRO$DATETIME, "%Y"))
OCEAN$Station <- REPHY_HYDRO$SITE
OCEAN$Type <- "*"
OCEAN$`yyyy-mm-ddThh:mm:ss.sss` <- ifelse(
  format(REPHY_HYDRO$DATETIME, "%H:%M") == "00:00",
  format(REPHY_HYDRO$DATETIME, "%Y-%m-%d"),
  format(REPHY_HYDRO$DATETIME, "%Y-%m-%dT%H:%MZ")
)
OCEAN$`Longitude [degrees_east]` <- REPHY_HYDRO$LON
OCEAN$`Latitude [degrees_north]` <- REPHY_HYDRO$LAT
OCEAN$`Bot. Depth [m]` <- NA
OCEAN$`Platform Code` <- "ZZ99"
OCEAN$`Device Category Code` <- "30" # Bottle and CTD
OCEAN$`Distributor Code` <- "6088"
OCEAN$`Custodian Code` <- "6088"
OCEAN$`Originator Code` <- "6088"
OCEAN$`Project Code` <- NA
OCEAN$`Depth [m]` <- 1
OCEAN$CPHLSSP1 <- REPHY_HYDRO$CPHLSSP1
OCEAN$`QV:ODV:CPHLSSP1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$CPHLSSP1),1,
                                                                      ifelse(REPHY_HYDRO$CPHLSSP1 == 0,4,0)))
OCEAN$CPHLSXP1 <- REPHY_HYDRO$CPHLSXP1
OCEAN$`QV:ODV:CPHLSXP1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$CPHLSXP1),1,
                                                                       ifelse(REPHY_HYDRO$CPHLSXP1 == 0,4,0)))

OCEAN$CPHLHPP1 <- REPHY_HYDRO$CPHLHPP1
OCEAN$`QV:ODV:CPHLHPP1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$CPHLHPP1),1,
                                                                 ifelse(REPHY_HYDRO$CPHLHPP1 == 0,4,0)))
OCEAN$CPHLFLP1 <- REPHY_HYDRO$CPHLFLP1
OCEAN$`QV:ODV:CPHLFLP1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$CPHLFLP1),1,
                                                                 ifelse(REPHY_HYDRO$CPHLFLP1 == 0,4,0)))

OCEAN$TEMPPR01 <- REPHY_HYDRO$TEMPPR01
OCEAN$`QV:ODV:TEMPPR01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$TEMPPR01),1,
                                                                 ifelse(REPHY_HYDRO$TEMPPR01 == 0,4,0)))

OCEAN$NEPHIF01 <- REPHY_HYDRO$NEPHIF01
OCEAN$`QV:ODV:NEPHIF01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$NEPHIF01),1,
                                              ifelse(REPHY_HYDRO$NEPHIF01 == 0,4,0)))

OCEAN$PSALKT01 <- REPHY_HYDRO$PSALKT01
OCEAN$`QV:ODV:PSALKT01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$PSALKT01),1,
                                              ifelse(REPHY_HYDRO$PSALKT01 == 0,4,0)))

OCEAN$SLCAAATX <- REPHY_HYDRO$SLCAAATX
OCEAN$`QV:ODV:SLCAAATX` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$SLCAAATX),1,
                                              ifelse(REPHY_HYDRO$SLCAAATX == 0,4,0)))

OCEAN$PHOSAAD1 <- REPHY_HYDRO$PHOSAAD1
OCEAN$`QV:ODV:PHOSAAD1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$PHOSAAD1),1,
                                              ifelse(REPHY_HYDRO$PHOSAAD1 == 0,4,0)))

OCEAN$DOXYPE01 <- REPHY_HYDRO$DOXYPE01
OCEAN$`QV:ODV:DOXYPE01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$DOXYPE01),1,
                                              ifelse(REPHY_HYDRO$DOXYPE01 == 0,4,0)))

OCEAN$PSALTC01 <- REPHY_HYDRO$PSALTC01
OCEAN$`QV:ODV:PSALTC01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$PSALTC01),1,
                                              ifelse(REPHY_HYDRO$PSALTC01 == 0,4,0)))

OCEAN$AMONMATX <- REPHY_HYDRO$AMONMATX
OCEAN$`QV:ODV:AMONMATX` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$AMONMATX),1,
                                              ifelse(REPHY_HYDRO$AMONMATX == 0,4,0)))

OCEAN$NTRZAATX <- REPHY_HYDRO$NTRZAATX
OCEAN$`QV:ODV:NTRZAATX` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$NTRZAATX),1,
                                              ifelse(REPHY_HYDRO$NTRZAATX == 0,4,0)))

OCEAN$PSSTTS01 <- REPHY_HYDRO$PSSTTS01
OCEAN$`QV:ODV:PSSTTS01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$PSSTTS01),1,
                                              ifelse(REPHY_HYDRO$PSSTTS01 == 0,4,0)))

OCEAN$AMONDSRX <- REPHY_HYDRO$AMONDSRX
OCEAN$`QV:ODV:AMONDSRX` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$AMONDSRX),1,
                                              ifelse(REPHY_HYDRO$AMONDSRX == 0,4,0)))

OCEAN$DOXYAAOP <- REPHY_HYDRO$DOXYAAOP
OCEAN$`QV:ODV:DOXYAAOP` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$DOXYAAOP),1,
                                              ifelse(REPHY_HYDRO$DOXYAAOP == 0,4,0)))

OCEAN$TURBPR01 <- REPHY_HYDRO$TURBPR01
OCEAN$`QV:ODV:TURBPR01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$TURBPR01),1,
                                              ifelse(REPHY_HYDRO$TURBPR01 == 0,4,0)))

OCEAN$PSALPR01 <- REPHY_HYDRO$PSALPR01
OCEAN$`QV:ODV:PSALPR01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$PSALPR01),1,
                                              ifelse(REPHY_HYDRO$PSALPR01 == 0,4,0)))

OCEAN$PSALBSTX <- REPHY_HYDRO$PSALBSTX
OCEAN$`QV:ODV:PSALBSTX` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$PSALBSTX),1,
                                              ifelse(REPHY_HYDRO$PSALBSTX == 0,4,0)))

OCEAN$TEMPRTNX <- REPHY_HYDRO$TEMPRTNX
OCEAN$`QV:ODV:TEMPRTNX` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$TEMPRTNX),1,
                                              ifelse(REPHY_HYDRO$TEMPRTNX == 0,4,0)))

OCEAN$SLCAMATX <- REPHY_HYDRO$SLCAMATX
OCEAN$`QV:ODV:SLCAMATX` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$SLCAMATX),1,
                                              ifelse(REPHY_HYDRO$SLCAMATX == 0,4,0)))

OCEAN$PHOSMAD1 <- REPHY_HYDRO$PHOSMAD1
OCEAN$`QV:ODV:PHOSMAD1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$PHOSMAD1),1,
                                              ifelse(REPHY_HYDRO$PHOSMAD1 == 0,4,0)))
OCEAN$AMONFID2 <- REPHY_HYDRO$AMONFID2
OCEAN$`QV:ODV:AMONFID2` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$AMONFID2),1,
                                              ifelse(REPHY_HYDRO$AMONFID2 == 0,4,0)))
OCEAN$PSALHD01 <- REPHY_HYDRO$PSALHD01
OCEAN$`QV:ODV:PSALHD01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$PSALHD01),1,
                                              ifelse(REPHY_HYDRO$PSALHD01 == 0,4,0)))
OCEAN$DOXYWITX <- REPHY_HYDRO$DOXYWITX
OCEAN$`QV:ODV:DOXYWITX` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$DOXYWITX),1,
                                              ifelse(REPHY_HYDRO$DOXYWITX == 0,4,0)))
OCEAN$PSALRF01 <- REPHY_HYDRO$PSALRF01
OCEAN$`QV:ODV:PSALRF01` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$PSALRF01),1,
                                              ifelse(REPHY_HYDRO$PSALRF01 == 0,4,0)))
OCEAN$NTRZMATX <- REPHY_HYDRO$NTRZMATX
OCEAN$`QV:ODV:NTRZMATX` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$NTRZMATX),1,
                                              ifelse(REPHY_HYDRO$NTRZMATX == 0,4,0)))

OCEAN$Station <- paste0(REPHY_HYDRO$SITE,"_",REPHY_HYDRO$DATETIME)


REPHY_OCEAN <- select(OCEAN,Cruise:`QV:ODV:NTRZMATX`,TEMPPR01:NTRZMATX)

REPHY_OCEAN <- unique(REPHY_OCEAN)
#REPHY_OCEAN <- REPHY_OCEAN %>%
#  filter(!(is.na(`Chlorophyll a trichroma CPHLSSP1`) & is.na(`Chlorophyll a monochroma CPHLSXP1`) & is.na(`Chlorophyll a HPLC CPHLHPP1`) & is.na(`Chlorophyll a fluo CPHLFLP1`)))


# Compute the mean by date and station as we cannot provide true replicates
REPHY_OCEAN <- REPHY_OCEAN |>
  group_by(`yyyy-mm-ddThh:mm:ss.sss`, Station, `Depth [m]`) |>
  mutate(
    `CPHLSSP1` = mean(`CPHLSSP1`, na.rm = TRUE),
    `CPHLSXP1` = mean(`CPHLSXP1`, na.rm = TRUE),
    `CPHLHPP1` = mean(`CPHLHPP1`, na.rm = TRUE),
    `CPHLFLP1` = mean(`CPHLFLP1`, na.rm = TRUE),
    
    TEMPPR01 = mean(TEMPPR01, na.rm = TRUE),
    NEPHIF01 = mean(NEPHIF01, na.rm = TRUE),
    PSALKT01 = mean(PSALKT01, na.rm = TRUE),
    SLCAAATX = mean(SLCAAATX, na.rm = TRUE),
    PHOSAAD1 = mean(PHOSAAD1, na.rm = TRUE),
    DOXYPE01 = mean(DOXYPE01, na.rm = TRUE),
    PSALTC01 = mean(PSALTC01, na.rm = TRUE),
    AMONMATX = mean(AMONMATX, na.rm = TRUE),
    NTRZAATX = mean(NTRZAATX, na.rm = TRUE),
    PSSTTS01 = mean(PSSTTS01, na.rm = TRUE),
    AMONDSRX = mean(AMONDSRX, na.rm = TRUE),
    DOXYAAOP = mean(DOXYAAOP, na.rm = TRUE),
    TURBPR01 = mean(TURBPR01, na.rm = TRUE),
    PSALPR01 = mean(PSALPR01, na.rm = TRUE),
    PSALBSTX = mean(PSALBSTX, na.rm = TRUE),
    TEMPRTNX = mean(TEMPRTNX, na.rm = TRUE),
    SLCAMATX = mean(SLCAMATX, na.rm = TRUE),
    PHOSMAD1 = mean(PHOSMAD1, na.rm = TRUE),
    AMONFID2 = mean(AMONFID2, na.rm = TRUE),
    PSALHD01 = mean(PSALHD01, na.rm = TRUE),
    DOXYWITX = mean(DOXYWITX, na.rm = TRUE),
    PSALRF01 = mean(PSALRF01, na.rm = TRUE),
    NTRZMATX = mean(NTRZMATX, na.rm = TRUE),
    
    
    `QV:ODV:CPHLSSP1` = min(`QV:ODV:CPHLSSP1`, na.rm = TRUE),
    `QV:ODV:CPHLSXP1` = min(`QV:ODV:CPHLSXP1`, na.rm = TRUE),
    `QV:ODV:CPHLHPP1` = min(`QV:ODV:CPHLHPP1`, na.rm = TRUE),
    `QV:ODV:CPHLFLP1` = min(`QV:ODV:CPHLFLP1`, na.rm = TRUE),
    
    `QV:ODV:TEMPPR01` = min(`QV:ODV:TEMPPR01`, na.rm = TRUE),
    `QV:ODV:NEPHIF01` = min(`QV:ODV:NEPHIF01`, na.rm = TRUE),
    `QV:ODV:PSALKT01` = min(`QV:ODV:PSALKT01`, na.rm = TRUE),
    `QV:ODV:SLCAAATX` = min(`QV:ODV:SLCAAATX`, na.rm = TRUE),
    `QV:ODV:PHOSAAD1` = min(`QV:ODV:PHOSAAD1`, na.rm = TRUE),
    `QV:ODV:DOXYPE01` = min(`QV:ODV:DOXYPE01`, na.rm = TRUE),
    `QV:ODV:PSALTC01` = min(`QV:ODV:PSALTC01`, na.rm = TRUE),
    `QV:ODV:AMONMATX` = min(`QV:ODV:AMONMATX`, na.rm = TRUE),
    `QV:ODV:NTRZAATX` = min(`QV:ODV:NTRZAATX`, na.rm = TRUE),
    `QV:ODV:PSSTTS01` = min(`QV:ODV:PSSTTS01`, na.rm = TRUE),
    `QV:ODV:AMONDSRX` = min(`QV:ODV:AMONDSRX`, na.rm = TRUE),
    `QV:ODV:DOXYAAOP` = min(`QV:ODV:DOXYAAOP`, na.rm = TRUE),
    `QV:ODV:TURBPR01` = min(`QV:ODV:TURBPR01`, na.rm = TRUE),
    `QV:ODV:PSALPR01` = min(`QV:ODV:PSALPR01`, na.rm = TRUE),
    `QV:ODV:PSALBSTX` = min(`QV:ODV:PSALBSTX`, na.rm = TRUE),
    `QV:ODV:TEMPRTNX` = min(`QV:ODV:TEMPRTNX`, na.rm = TRUE),
    `QV:ODV:SLCAMATX` = min(`QV:ODV:SLCAMATX`, na.rm = TRUE),
    `QV:ODV:PHOSMAD1` = min(`QV:ODV:PHOSMAD1`, na.rm = TRUE),
    `QV:ODV:AMONFID2` = min(`QV:ODV:AMONFID2`, na.rm = TRUE),
    `QV:ODV:PSALHD01` = min(`QV:ODV:PSALHD01`, na.rm = TRUE),
    `QV:ODV:DOXYWITX` = min(`QV:ODV:DOXYWITX`, na.rm = TRUE),
    `QV:ODV:PSALRF01` = min(`QV:ODV:PSALRF01`, na.rm = TRUE),
    `QV:ODV:NTRZMATX` = min(`QV:ODV:NTRZMATX`, na.rm = TRUE),
    
    
  ) |>
  unique()



## 1) Colonnes de regroupement
#group_cols <- c("Station", "yyyy-mm-ddThh:mm:ss.sss")
#
## 2) Plage d'indices et noms correspondants
#range_idx   <- 15:72
#range_names <- names(REPHY_OCEAN)[range_idx]
#
## 3) Séparer : colonnes QV vs non-QV
#cols_qv     <- grep("^QV:ODV:", range_names, value = TRUE)
#cols_nqv    <- setdiff(range_names, cols_qv)
#
## 4) Autres colonnes à conserver par first()
#other_cols  <- setdiff(names(REPHY_OCEAN), c(group_cols, range_names))
#
## 5) Fonction utilitaire : transformer en numérique sans casser les types
##    (utile si des colonnes sont caractères mais numériques "dans le fond")
#to_numeric_safe <- function(x) {
#  if (is.numeric(x)) return(x)
#  suppressWarnings(as.numeric(x))
#}

# 6) Agrégation
# REPHY_OCEAN <- REPHY_OCEAN %>%
#   group_by(across(all_of(group_cols))) %>%
#   summarise(
#     # Moyenne pour colonnes non-QV (en numérique, avec na.rm)
#     across(
#       all_of(cols_nqv),
#       ~ mean(to_numeric_safe(.x), na.rm = TRUE),
#       .names = "{.col}"
#     ),
#     # Minimum pour colonnes QV (en numérique, avec na.rm)
#     across(
#       all_of(cols_qv),
#       ~ min(to_numeric_safe(.x), na.rm = TRUE),
#       .names = "{.col}"
#     ),
#     # Conserver les autres colonnes (première valeur du groupe)
#     across(
#       all_of(other_cols),
#       ~ first(.x),
#       .names = "{.col}"
#     ),
#     .groups = "drop"
#   )


#test <- unique(select(REPHY_OCEAN,Station, `Latitude [degrees_north]`, `Longitude [degrees_east]`))

pre_header <- c(
  "//ICES_parameter_mapping",
  "//<subject>ICES:LOCAL:Depth [m]</subject><object>ICES:P01::DPSAZZ01</object><units>ICES:P06::ULAA</units>",
  "//<subject>ICES:LOCAL:CPHLSSP1</subject><object>ICES:P01::CPHLSSP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:CPHLSXP1</subject><object>ICES:P01::CPHLSXP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:CPHLHPP1</subject><object>ICES:P01::CPHLHPP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:CPHLFLP1</subject><object>ICES:P01::CPHLFLP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:TEMPPR01</subject><object>ICES:P01::TEMPPR01</object><units>ICES:P06::UPAA</units>",
  "//<subject>ICES:LOCAL:NEPHIF01</subject><object>ICES:P01::NEPHIF01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:PSALKT01</subject><object>ICES:P01::PSALKT01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:SLCAAATX</subject><object>ICES:P01::SLCAAATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PHOSAAD1</subject><object>ICES:P01::PHOSAAD1</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:DOXYPE01</subject><object>ICES:P01::DOXYPE01</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSALTC01</subject><object>ICES:P01::PSALTC01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:AMONMATX</subject><object>ICES:P01::AMONMATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:NTRZAATX</subject><object>ICES:P01::NTRZAATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSSTTS01</subject><object>ICES:P01::PSSTTS01</object><units>ICES:P06::UPAA</units>",
  "//<subject>ICES:LOCAL:AMONDSRX</subject><object>ICES:P01::AMONDSRX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:DOXYAAOP</subject><object>ICES:P01::DOXYAAOP</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:TURBPR01</subject><object>ICES:P01::TURBPR01</object><units>ICES:P06::USTU</units>",
  "//<subject>ICES:LOCAL:PSALPR01</subject><object>ICES:P01::PSALPR01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:PSALBSTX</subject><object>ICES:P01::PSALBSTX</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:TEMPRTNX</subject><object>ICES:P01::TEMPRTNX</object><units>ICES:P06::UPAA</units>",
  "//<subject>ICES:LOCAL:SLCAMATX</subject><object>ICES:P01::SLCAMATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PHOSMAD1</subject><object>ICES:P01::PHOSMAD1</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:AMONFID2</subject><object>ICES:P01::AMONFID2</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSALHD01</subject><object>ICES:P01::PSALHD01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:DOXYWITX</subject><object>ICES:P01::DOXYWITX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSALRF01</subject><object>ICES:P01::PSALRF01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:NTRZMATX</subject><object>ICES:P01::NTRZMATX</object><units>ICES:P06::UPOX</units>",
  "//",
  paste(colnames(REPHY_OCEAN), collapse = ",")
)

# Final dataset
write_lines(pre_header, "output/OCEAN_PH2_REPHY_withNUT.csv",na = "") # metadata lines
write_excel_csv(REPHY_OCEAN,file = "output/OCEAN_PH2_REPHY_withNUT.csv", append = TRUE,na = "") # complete dataset

REPHY_OCEAN_89_2014 <- filter(REPHY_OCEAN, Cruise <= 2014)

pre_header <- c(
  "//ICES_parameter_mapping",
  "//<subject>ICES:LOCAL:Depth [m]</subject><object>ICES:P01::DPSAZZ01</object><units>ICES:P06::ULAA</units>",
  "//<subject>ICES:LOCAL:CPHLSSP1</subject><object>ICES:P01::CPHLSSP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:CPHLSXP1</subject><object>ICES:P01::CPHLSXP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:CPHLHPP1</subject><object>ICES:P01::CPHLHPP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:CPHLFLP1</subject><object>ICES:P01::CPHLFLP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:TEMPPR01</subject><object>ICES:P01::TEMPPR01</object><units>ICES:P06::UPAA</units>",
  "//<subject>ICES:LOCAL:NEPHIF01</subject><object>ICES:P01::NEPHIF01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:PSALKT01</subject><object>ICES:P01::PSALKT01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:SLCAAATX</subject><object>ICES:P01::SLCAAATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PHOSAAD1</subject><object>ICES:P01::PHOSAAD1</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:DOXYPE01</subject><object>ICES:P01::DOXYPE01</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSALTC01</subject><object>ICES:P01::PSALTC01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:AMONMATX</subject><object>ICES:P01::AMONMATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:NTRZAATX</subject><object>ICES:P01::NTRZAATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSSTTS01</subject><object>ICES:P01::PSSTTS01</object><units>ICES:P06::UPAA</units>",
  "//<subject>ICES:LOCAL:AMONDSRX</subject><object>ICES:P01::AMONDSRX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:DOXYAAOP</subject><object>ICES:P01::DOXYAAOP</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:TURBPR01</subject><object>ICES:P01::TURBPR01</object><units>ICES:P06::USTU</units>",
  "//<subject>ICES:LOCAL:PSALPR01</subject><object>ICES:P01::PSALPR01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:PSALBSTX</subject><object>ICES:P01::PSALBSTX</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:TEMPRTNX</subject><object>ICES:P01::TEMPRTNX</object><units>ICES:P06::UPAA</units>",
  "//<subject>ICES:LOCAL:SLCAMATX</subject><object>ICES:P01::SLCAMATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PHOSMAD1</subject><object>ICES:P01::PHOSMAD1</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:AMONFID2</subject><object>ICES:P01::AMONFID2</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSALHD01</subject><object>ICES:P01::PSALHD01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:DOXYWITX</subject><object>ICES:P01::DOXYWITX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSALRF01</subject><object>ICES:P01::PSALRF01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:NTRZMATX</subject><object>ICES:P01::NTRZMATX</object><units>ICES:P06::UPOX</units>",
  "//",
  paste(colnames(REPHY_OCEAN), collapse = ",")
)

# Final dataset
write_lines(pre_header, "output/OCEAN_PH2_REPHY_withNUT_89_14.csv",na = "") # metadata lines
write_excel_csv(REPHY_OCEAN_89_2014,file = "output/OCEAN_PH2_REPHY_withNUT_89_14.csv", append = TRUE,na = "") # complete dataset

library(stringr)
stations_autorisees <- c(
  "Antifer ponton pétrolier", "Bréhat", "Cancale nord \\(b\\)", "Houat",
  "Ile d'Aix", "Ile Dumet \\(a\\)", "Ile d'Yeu est", "Kerist",
  "Kervel", "Lanvéoc", "Le Croisic \\(a\\)", "les Hébihens",
  "Locquemeau", "Loguivy", "Merquel", "Mont St Michel",
  "Ouessant - Youc'h korz", "Paluden", "Pen al Lann \\(a\\)",
  "Pointe du But", "Pont de Lézardieux - 152E08",
  "Port Saint Hubert", "St Cast"
)

pattern <- paste0("^(", paste(stations_autorisees, collapse = "|"), ")")

REPHY_OCEAN_2015_25 <- REPHY_OCEAN |>
  ungroup() |>
  filter(str_detect(Station, pattern)) |>
  filter(Cruise >=2015)

pre_header <- c(
  "//ICES_parameter_mapping",
  "//<subject>ICES:LOCAL:Depth [m]</subject><object>ICES:P01::DPSAZZ01</object><units>ICES:P06::ULAA</units>",
  "//<subject>ICES:LOCAL:CPHLSSP1</subject><object>ICES:P01::CPHLSSP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:CPHLSXP1</subject><object>ICES:P01::CPHLSXP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:CPHLHPP1</subject><object>ICES:P01::CPHLHPP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:CPHLFLP1</subject><object>ICES:P01::CPHLFLP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:TEMPPR01</subject><object>ICES:P01::TEMPPR01</object><units>ICES:P06::UPAA</units>",
  "//<subject>ICES:LOCAL:NEPHIF01</subject><object>ICES:P01::NEPHIF01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:PSALKT01</subject><object>ICES:P01::PSALKT01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:SLCAAATX</subject><object>ICES:P01::SLCAAATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PHOSAAD1</subject><object>ICES:P01::PHOSAAD1</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:DOXYPE01</subject><object>ICES:P01::DOXYPE01</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSALTC01</subject><object>ICES:P01::PSALTC01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:AMONMATX</subject><object>ICES:P01::AMONMATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:NTRZAATX</subject><object>ICES:P01::NTRZAATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSSTTS01</subject><object>ICES:P01::PSSTTS01</object><units>ICES:P06::UPAA</units>",
  "//<subject>ICES:LOCAL:AMONDSRX</subject><object>ICES:P01::AMONDSRX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:DOXYAAOP</subject><object>ICES:P01::DOXYAAOP</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:TURBPR01</subject><object>ICES:P01::TURBPR01</object><units>ICES:P06::USTU</units>",
  "//<subject>ICES:LOCAL:PSALPR01</subject><object>ICES:P01::PSALPR01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:PSALBSTX</subject><object>ICES:P01::PSALBSTX</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:TEMPRTNX</subject><object>ICES:P01::TEMPRTNX</object><units>ICES:P06::UPAA</units>",
  "//<subject>ICES:LOCAL:SLCAMATX</subject><object>ICES:P01::SLCAMATX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PHOSMAD1</subject><object>ICES:P01::PHOSMAD1</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:AMONFID2</subject><object>ICES:P01::AMONFID2</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSALHD01</subject><object>ICES:P01::PSALHD01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:DOXYWITX</subject><object>ICES:P01::DOXYWITX</object><units>ICES:P06::UPOX</units>",
  "//<subject>ICES:LOCAL:PSALRF01</subject><object>ICES:P01::PSALRF01</object><units>ICES:P06::UUUU</units>",
  "//<subject>ICES:LOCAL:NTRZMATX</subject><object>ICES:P01::NTRZMATX</object><units>ICES:P06::UPOX</units>",
  "//",
  paste(colnames(REPHY_OCEAN), collapse = ",")
)

# Final dataset
write_lines(pre_header, "output/OCEAN_PH2_REPHY_withNUT_stationsmanquesIFREMER.csv",na = "") # metadata lines
write_excel_csv(REPHY_OCEAN_2015_25,file = "output/OCEAN_PH2_REPHY_withNUT_stationsmanquesIFREMER.csv", append = TRUE,na = "") # complete dataset


