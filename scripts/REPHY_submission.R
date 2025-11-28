#_______________________________________________________________________________
# Nom               : REPHY_submission.r
# Date de modif     : 28/11/2025
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

FLORTOT <- filter(FLORTOT,!is.na(PROFONDEUR))

DOME <- FLORTOT

# Reporting laboratory
DOME$RLABO <- "BAMN" # BOREA

# Ship or platform code 
DOME$SHIPC <- "AA31" # Unknown

# Cruise identifier (series of sampling occasions) 
# "Make it up if you don't go on cruises - one name to be used for a year is fine."
DOME$CRUIS <- format(as.Date(FLORTOT$DATE),"%Y")

# Station identification /Sampling event ID
DOME$STNNO <- FLORTOT$ID_ECHANTILLON

# Latitude
DOME$LATIT <- round(FLORTOT$LAT,digits = 4)

# Longitude
DOME$LONGI <- round(FLORTOT$LON,digits = 4)

# Position system "WGS84 assumed if field is blank"
DOME$POSYS <- NA

# Station name
DOME$STATN <- FLORTOT$SITE

# Sample date
DOME$SDATE <- format(as.Date(FLORTOT$DATE),"%Y%m%d")

# Sample time
DOME$STIME <- FLORTOT$HEURE

# Sounding depth in metres
DOME$WADEP <- NA

# Sample number / Sample identification 
DOME$SMPNO <- FLORTOT$ID_ECHANTILLON

# Factors potentially influencing guideline compliance and interpretation of data
DOME$FINFL <- NA

# Total sampled volume in litres
DOME$SMVOL <- 0.250

# Minimum depth - surface = 0 metres
DOME$MNDEP <- FLORTOT$PROFONDEUR

# Maximum depth in metres
DOME$MXDEP <- FLORTOT$PROFONDEUR

# Species from WoRMS
# manually indicate taxa without aphia ID if needed
DOME$SPECI <- FLORTOT$aphiaID

# Species trophic status
DOME$TRPHY <- NA

# Stage of development
DOME$STAGE <- NA

# Parameter
DOME$PARAM <- "ABUNDNR" # Abundance number (number counted)

# Value measured
DOME$VALUE <- FLORTOT$VALEUR

# Measurement unit
DOME$MUNIT <- "nrcells/l"

# Qualifier flag (non mandatory)
DOME$QFLAG <- NA

# Analytical laboratory
DOME$ALABO <- NA

# Method of analysis
DOME$METOA <- NA

# Method of fixation/preservation
DOME$METFP <- NA

# Sampling laboratry
DOME$SLABO <- NA

# Sampler type
DOME$SMTYP <- NA

# Monitoring Programme
DOME$MPROG <- "CEMP~NATL" # OSPAR and National reporting

# Purpose of Monitoring
DOME$PURPM <- "T~S" # Temporal trend monitoring and Spatial (geographical) distribution monitoring

# Monitoring Year
DOME$MYEAR <- format(as.Date(FLORTOT$DATE),"%Y")

# Data Type
DOME$DTYPE <- "PP"

# Species codelist
DOME$RLIST <- "ERID" # WORMS

# Size Class
DOME$SIZCL <- NA

# Size Class Ref. List
DOME$SIZRF <- NA

# Species flag
DOME$SFLAG <- NA

# Keep only the DOME format columns
DOME_PP_FLORTOT <- select(DOME,RLABO:SFLAG,PARAM)

# Make the sum of the abundance of aphiaID, because before it was different taxa it needs to be merge to avoid missleading thinking about the data
DOME_PP_FLORTOT <- DOME_PP_FLORTOT %>%
  group_by(across(-VALUE)) %>%  
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")


# Save it
if(nrow(filter(DOME_PP_FLORTOT, is.na(SPECI))) != 0){
  message("There is a taxa without aphiaID, Taxonomy_correspondance_FLORTOT_Dias_modif.csv must be updated")
} else {
  write.csv(DOME_PP_FLORTOT,file = "output/DOME_PP_REPHY_FLORTOT_Ready_version.csv",row.names = F,fileEncoding = "UTF-8",na = "")
}


# As the dataset is too large for the Data Conversion Tools we split it :

DOME_87_92 <- filter(DOME_PP_FLORTOT,CRUIS <= 1992)
write.csv(DOME_87_92,file = "output/DOME_PP_REPHY_FLORTOT_87_92.csv",row.names = F,fileEncoding = "UTF-8",na = "")

DOME_93_98 <- filter(DOME_PP_FLORTOT,CRUIS >= 1993 & CRUIS <= 1998)
write.csv(DOME_93_98,file = "output/DOME_PP_REPHY_FLORTOT_93_98.csv",row.names = F,fileEncoding = "UTF-8",na = "")

DOME_99_04 <- filter(DOME_PP_FLORTOT,CRUIS >= 1999 & CRUIS <= 2004)
write.csv(DOME_99_04,file = "output/DOME_PP_REPHY_FLORTOT_99_04.csv",row.names = F,fileEncoding = "UTF-8",na = "")

DOME_05_09 <- filter(DOME_PP_FLORTOT,CRUIS >= 2005 & CRUIS <= 2009)
write.csv(DOME_05_09,file = "output/DOME_PP_REPHY_FLORTOT_05_09.csv",row.names = F,fileEncoding = "UTF-8",na = "")

DOME_10_14 <- filter(DOME_PP_FLORTOT,CRUIS >= 2010 & CRUIS <= 2014)
write.csv(DOME_10_14,file = "output/DOME_PP_REPHY_FLORTOT_10_14.csv",row.names = F,fileEncoding = "UTF-8",na = "")

DOME_15_19 <- filter(DOME_PP_FLORTOT,CRUIS >= 2015 & CRUIS <= 2019)
write.csv(DOME_15_19,file = "output/DOME_PP_REPHY_FLORTOT_15_19.csv",row.names = F,fileEncoding = "UTF-8",na = "")

DOME_20_25 <- filter(DOME_PP_FLORTOT,CRUIS >= 2020)
write.csv(DOME_20_25,file = "output/DOME_PP_REPHY_FLORTOT_20_25.csv",row.names = F,fileEncoding = "UTF-8",na = "")



### Working on chla data ###
HYDRO <- filter(REPHY, PARAM == "CHLOROA")

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
REPHY_HYDRO <- pivot_wider(REPHY_HYDRO,names_from = "BODC",values_from = VALEUR)

#OCEAN formating

OCEAN <- REPHY_HYDRO
OCEAN$Cruise <- as.numeric(format(REPHY_HYDRO$DATETIME, "%Y"))
OCEAN$Station <- paste0(REPHY_HYDRO$SITE,"_")
OCEAN$Type <- "*"
OCEAN$`yyyy-mm-ddThh:mm:ss.sss` <- format(REPHY_HYDRO$DATETIME, "%Y-%m-%dT%H:%MZ")
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
OCEAN$`Chlorophyll a trichroma CPHLSSP1` <- REPHY_HYDRO$CPHLSSP1
OCEAN$`QV:ODV:Chlorophyll a trichroma CPHLSSP1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$CPHLSSP1),1,
                                                                      ifelse(REPHY_HYDRO$CPHLSSP1 == 0,4,0)))
OCEAN$`Chlorophyll a monochroma CPHLSXP1` <- REPHY_HYDRO$CPHLSXP1
OCEAN$`QV:ODV:Chlorophyll a monochroma CPHLSXP1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$CPHLSXP1),1,
                                                                       ifelse(REPHY_HYDRO$CPHLSXP1 == 0,4,0)))

OCEAN$`Chlorophyll a HPLC CPHLHPP1` <- REPHY_HYDRO$CPHLHPP1
OCEAN$`QV:ODV:Chlorophyll a HPLC CPHLHPP1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$CPHLHPP1),1,
                                                                      ifelse(REPHY_HYDRO$CPHLHPP1 == 0,4,0)))
OCEAN$`Chlorophyll a fluo CPHLFLP1` <- REPHY_HYDRO$CPHLFLP1
OCEAN$`QV:ODV:Chlorophyll a fluo CPHLFLP1` <- with(OCEAN,ifelse( is.na(REPHY_HYDRO$CPHLFLP1),1,
                                                                       ifelse(REPHY_HYDRO$CPHLFLP1 == 0,4,0)))


REPHY_OCEAN <- select(OCEAN,Cruise:`QV:ODV:Chlorophyll a fluo CPHLFLP1`)

REPHY_OCEAN <- unique(REPHY_OCEAN)
REPHY_OCEAN <- REPHY_OCEAN %>%
  filter(!(is.na(`Chlorophyll a trichroma CPHLSSP1`) & is.na(`Chlorophyll a monochroma CPHLSXP1`) & is.na(`Chlorophyll a HPLC CPHLHPP1`) & is.na(`Chlorophyll a fluo CPHLFLP1`)))


# Compute the mean by date and station as we cannot provide true replicates
REPHY_OCEAN <- REPHY_OCEAN |>
  group_by(`yyyy-mm-ddThh:mm:ss.sss`, `Longitude [degrees_east]`, `Latitude [degrees_north]`) |>
  mutate(
    `Chlorophyll a trichroma CPHLSSP1` = mean(`Chlorophyll a trichroma CPHLSSP1`, na.rm = TRUE),
    `Chlorophyll a monochroma CPHLSXP1` = mean(`Chlorophyll a monochroma CPHLSXP1`, na.rm = TRUE),
    `Chlorophyll a HPLC CPHLHPP1` = mean(`Chlorophyll a HPLC CPHLHPP1`, na.rm = TRUE),
    `Chlorophyll a fluo CPHLFLP1` = mean(`Chlorophyll a fluo CPHLFLP1`, na.rm = TRUE),
    
    `QV:ODV:Chlorophyll a trichroma CPHLSSP1` = min(`QV:ODV:Chlorophyll a trichroma CPHLSSP1`, na.rm = TRUE),
    `QV:ODV:Chlorophyll a monochroma CPHLSXP1` = min(`QV:ODV:Chlorophyll a monochroma CPHLSXP1`, na.rm = TRUE),
    `QV:ODV:Chlorophyll a HPLC CPHLHPP1` = min(`QV:ODV:Chlorophyll a HPLC CPHLHPP1`, na.rm = TRUE),
    `QV:ODV:Chlorophyll a fluo CPHLFLP1` = min(`QV:ODV:Chlorophyll a fluo CPHLFLP1`, na.rm = TRUE)
  ) |>
  ungroup() |>
  unique()


REPHY_OCEAN <- unique(REPHY_OCEAN)

REPHY_OCEAN$Station <- paste0(REPHY_OCEAN$Station,rownames(REPHY_OCEAN))
pre_header <- c(
  "//ICES_parameter_mapping",
  "//<subject>ICES:LOCAL:Depth [m]</subject><object>ICES:P01::DPSAZZ01</object><units>ICES:P06::ULAA</units>",
  "//<subject>ICES:LOCAL:Chlorophyll a trichroma CPHLSSP1</subject><object>ICES:P01::CPHLSSP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:Chlorophyll a monochroma CPHLSXP1</subject><object>ICES:P01::CPHLSXP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:Chlorophyll a HPLC CPHLHPP1</subject><object>ICES:P01::CPHLHPP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:Chlorophyll a fluo CPHLFLP1</subject><object>ICES:P01::CPHLFLP1</object><units>ICES:P06::UMMC</units>",
  "//",
  paste(colnames(REPHY_OCEAN), collapse = ",")
)

# Final dataset
write_lines(pre_header, "output/OCEAN_PH2_REPHY_Ready_version.csv",na = "") # metadata lines
write_excel_csv(REPHY_OCEAN,file = "output/OCEAN_PH2_REPHY_Ready_version.csv", append = TRUE,na = "") # complete dataset



