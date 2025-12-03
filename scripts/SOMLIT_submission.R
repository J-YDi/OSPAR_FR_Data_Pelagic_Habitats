#_______________________________________________________________________________
# Nom               : SOMLIT_submission.r
# Date de modif     : 28/11/2025
# Objet             : Mise en forme des donnees SOMLIT
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
packages_needed <- c("readr","dplyr","ggplot2","cowplot")

loadpackages(packages_needed)

#__________________________Loading Original Data________________________________

HYDRO <- read_delim("data/Somlit_Extraction_hydro_20250604_095825_cbc2ca969e04b37e.csv", 
                    delim = ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)

PICONANO <- read_delim("data/Somlit_Extraction_piconano_20250604_095949_af115101f74ae831.csv", 
                       delim = ";", escape_double = FALSE, col_names = FALSE,
                       trim_ws = TRUE)


#__________________________Cleaning column names________________________________

HYDRO <- HYDRO[-c(1:4),]
colnames(HYDRO) <- c("ID_SITE","DATE","HEURE","COEF_MAREE","MAREE","NIVEAU_PROFONDEUR","PROFONDEUR","SITE","LAT","LON",
                     "T","qT","S","qS","O","qO","PH","qPH","NH4","qNH4","NO3","qNO3","NO2","qNO2","PO4","qPO4","SIOH4",
                     "qSIOH4","COP","qCOP","NOP","qNOP","MES","qMES","DN15","qDN15","DC13","qDC13","CHLA","qCHLA")

PICONANO <- PICONANO[-c(1:4),]
colnames(PICONANO) <- c("ID_SITE","DATE","HEURE","COEF_MAREE","MAREE","NIVEAU_PROFONDEUR","PROFONDEUR","SITE","LAT","LON",
                        "TBACC","qTBACC","TBACSSC","qTBACSSC","TBACFLV","qTBACFLV","HNABACC","qHNABACC","HNABACSSC","qHNABACSSC",
                        "HNABACFLV","qHNABACFLV","LNABACC","qLNABACC","LNABACSSC","qLNABACSSC","LNABACFLV","qLNABACFLV","CRYC","qCRYC",
                        "CRYSSC","qCRYSSC","CRYFLR","qCRYFLR","CRYFLO","qCRYFLO","SYNC","qSYNC","SYNSSC","qSYNSSC","SYNFLR","qSYNFLR","SYNFLO",
                        "qSYNFLO","PROC","qPROC","PROSSC","qPROSSC","PROFLR","qPROFLR","PICOEC","qPICOEC","PICOESSC","qPICOESSC","PICOEFLR",
                        "qPICOEFLR","NANOEC","qNANOEC","NANOESSC","qNANOESSC","NANOEFLR","qNANOEFLR")

#_________________________Generate DATE-TIME column_____________________________

HYDRO$DATETIME <- as.POSIXct(paste(HYDRO$DATE,HYDRO$HEURE),format = "%Y-%m-%d %H:%M:%S",tz="UTC")
PICONANO$DATETIME <- as.POSIXct(paste(PICONANO$DATE,PICONANO$HEURE),format = "%Y-%m-%d %H:%M:%S",tz="UTC")

#________________________________Recoding NA's and Quality flags________________

# Keep quality flags 2,6 et 7
QF_HYDRO <- grep("q",colnames(HYDRO))
for (i in 1:length(QF_HYDRO)) {
  idx <- which(HYDRO[QF_HYDRO[i]] != 2 &
                 HYDRO[QF_HYDRO[i]] != 6 &
                 HYDRO[QF_HYDRO[i]] != 7)
  HYDRO[idx,QF_HYDRO[i]-1] <- NA
}

QF_PICONANO <- grep("q",colnames(PICONANO))
for (i in 1:length(QF_PICONANO)) {
  idx <- which(PICONANO[QF_PICONANO[i]] != 2 &
                 PICONANO[QF_PICONANO[i]] != 6 &
                 PICONANO[QF_PICONANO[i]] != 7)
  PICONANO[idx,QF_PICONANO[i]-1] <- NA
}

# Make it chronological
HYDRO <- HYDRO |>
  arrange(SITE,DATETIME) |>
  distinct()

PICONANO <- PICONANO |>
  arrange(SITE,DATETIME) |>
  distinct()

PICONANO <- PICONANO[,-QF_PICONANO] 

# Correct depth for Arcachon stations as there are all at the surface so there are not sampling at more than 1m for Bouee 13, Comprian and Eyrac stations
HYDRO$PROFONDEUR <- ifelse(HYDRO$SITE %in% c("Bouee 13","Comprian","Eyrac") & HYDRO$NIVEAU_PROFONDEUR == "S" & HYDRO$PROFONDEUR > 1,  1,  HYDRO$PROFONDEUR)
PICONANO$PROFONDEUR <- ifelse(PICONANO$SITE %in% c("Bouee 13","Comprian","Eyrac") & PICONANO$NIVEAU_PROFONDEUR == "S" & PICONANO$PROFONDEUR > 1,  1,  PICONANO$PROFONDEUR)

#____________________Make HYDRO data OCEAN compatible___________________________

HYDRO <- filter(HYDRO, SITE %in% c("Antioche" ,"Astan","Bizeux","Bouee 13","Comprian","Cézembre" ,"Estacade","Eyrac","Le Buron" , "Luc-sur-Mer",
                                                    "Point C", "Point L","Portzic","Smile", "pk 30", "pk 52","pk 86"))


OCEAN <- HYDRO
OCEAN$Cruise <- as.numeric(format(HYDRO$DATETIME, "%Y"))
OCEAN$Station <- paste0(HYDRO$SITE,"_",HYDRO$DATETIME)
OCEAN$Type <- "*"
OCEAN$`yyyy-mm-ddThh:mm:ss.sss` <- format(HYDRO$DATETIME, "%Y-%m-%dT%H:%MZ")
OCEAN$`Longitude [degrees_east]` <- HYDRO$LON
OCEAN$`Latitude [degrees_north]` <- HYDRO$LAT
OCEAN$`Bot. Depth [m]` <- NA
OCEAN$`Platform Code` <- "ZZ99"
OCEAN$`Device Category Code` <- "30" # Bottle and CTD
OCEAN$`Distributor Code` <- "548"
OCEAN$`Custodian Code` <- "548"
OCEAN$`Originator Code` <- "548"
OCEAN$`Project Code` <- NA
OCEAN$`Depth [m]` <- HYDRO$PROFONDEUR
OCEAN$`Chlorophyll a SOMLIT [ug/l] equals to [mg/m3]` <- ifelse((OCEAN$Station %in% c("Point L", "Point C")) & OCEAN$Cruise < 2009,  NA,  HYDRO$CHLA) # Protocole SOMLIT
OCEAN$`QV:ODV:Chlorophyll a SOMLIT [ug/l] equals to [mg/m3]` <- with(OCEAN,
                                                   ifelse(
                                                     Station %in% c("Point L", "Point C") & Cruise < 2009, 
                                                     1,                       # cas exclu
                                                     ifelse(
                                                       is.na(HYDRO$CHLA),            # donnée absente à l’origine
                                                       1,
                                                       0                             # sinon 0
                                                     )
                                                   )
)
OCEAN$`Chlorophyll a HPLC [ug/l] equals to [mg/m3]` <- ifelse((OCEAN$Station %in% c("Point L", "Point C")) & OCEAN$Cruise < 2009, HYDRO$CHLA, NA) # HPLC
OCEAN$`QV:ODV:Chlorophyll a HPLC [ug/l] equals to [mg/m3]` <- with(OCEAN,
                                                 ifelse(
                                                   (Station %in% c("Point L", "Point C")) & Cruise < 2009,
                                                   ifelse(is.na(HYDRO$CHLA), 1, 0),  # si CHLA dispo → 0, sinon NA
                                                   1                                # sinon (hors règle) → NA
                                                 )
)


SOMLIT_OCEAN <- select(OCEAN,Cruise:`QV:ODV:Chlorophyll a HPLC [ug/l] equals to [mg/m3]`)

pre_header <- c(
  "//ICES_parameter_mapping",
  "//<subject>ICES:LOCAL:Depth [m]</subject><object>ICES:P01::DPSAZZ01</object><units>ICES:P06::ULAA</units>",
  "//<subject>ICES:LOCAL:Chlorophyll a SOMLIT [ug/l] equals to [mg/m3]</subject><object>ICES:P01::CPHLFLP1</object><units>ICES:P06::UMMC</units>",
  "//<subject>ICES:LOCAL:Chlorophyll a HPLC [ug/l] equals to [mg/m3]</subject><object>ICES:P01::CPHLHPP1</object><units>ICES:P06::UMMC</units>",
  "//",
  paste(colnames(SOMLIT_OCEAN), collapse = ",")
)

# Final dataset
write_lines(pre_header, "output/OCEAN_PH2_SOMLIT_Ready_version_final.csv",na = "") # metadata lines
write_excel_csv(SOMLIT_OCEAN,file = "output/OCEAN_PH2_SOMLIT_Ready_version_final.csv", append = TRUE,na = "") # complete dataset

#_____________________PHYTOPLANKTON DATA TO DOME FORMAT_________________________####
# Keep only measurements that corresponds to a taxa and are under of scope of COBAM

PICONANO <- filter(PICONANO, SITE %in% c("Antioche" ,"Astan","Bizeux","Bouee 13","Comprian","Cézembre" ,"Estacade","Eyrac","Le Buron" , "Luc-sur-Mer",
                                   "Point C", "Point L","Portzic","Smile", "pk 30", "pk 52","pk 86"))

SOMLIT_DOME <- PICONANO |>
  pivot_longer(cols = TBACC:NANOEFLR,names_to = "TAXA",values_to = "VALEUR") |>
  filter(TAXA %in% c("CRYC","SYNC","PROC"))

# Replace 0 by NA 
SOMLIT_DOME[SOMLIT_DOME == 0] <- NA

# Delete 0 lines
SOMLIT_DOME <- SOMLIT_DOME |>
  filter(!is.na(VALEUR))


DOME <- SOMLIT_DOME

# Reporting laboratory
DOME$RLABO <- "BAMN" # BOREA

# Ship or platform code 
DOME$SHIPC <- "AA31" # Unknown

# Cruise identifier (series of sampling occasions) 
# "Make it up if you don't go on cruises - one name to be used for a year is fine."
DOME$CRUIS <- format(as.Date(SOMLIT_DOME$DATE),"%Y")

# Station identification /Sampling event ID
DOME$STNNO <- paste0(SOMLIT_DOME$SITE,"_",format(as.Date(SOMLIT_DOME$DATE),"%Y%m%d"))

# Latitude
DOME$LATIT <- SOMLIT_DOME$LAT

# Longitude
DOME$LONGI <- SOMLIT_DOME$LON

# Position system "WGS84 assumed if field is blank"
DOME$POSYS <- NA

# Station name
DOME$STATN <- SOMLIT_DOME$SITE

# Sample date
DOME$SDATE <- format(as.Date(SOMLIT_DOME$DATE),"%Y%m%d")

# Sample time
DOME$STIME <- NA

# Sounding depth in metres
DOME$WADEP <- NA

# Sample number / Sample identification 
DOME$SMPNO <- paste0(SOMLIT_DOME$SITE,"_",format(as.Date(SOMLIT_DOME$DATE),"%Y%m%d"))

# Factors potentially influencing guideline compliance and interpretation of data
DOME$FINFL <- NA

# Total sampled volume in litres
DOME$SMVOL <- NA

# Minimum depth - surface = 0 metres
DOME$MNDEP <- SOMLIT_DOME$PROFONDEUR
# Maximum depth in metres
DOME$MXDEP <- SOMLIT_DOME$PROFONDEUR

# Species from WoRMS
# manually indicate taxa without aphia ID if needed
DOME$SPECI <- ifelse(SOMLIT_DOME$TAXA == "PROC", 345515, #Prochlorochococcus
                     ifelse(SOMLIT_DOME$TAXA == "CRYC", 17639, #Cryptophytes
                            ifelse(SOMLIT_DOME$TAXA == "SYNC", 160572, # Synechococcus
                                   NA)))

# Species trophic status
DOME$TRPHY <- NA

# Stage of development
DOME$STAGE <- NA

# Parameter
DOME$PARAM <- "ABUNDNR" # Abundance number (number counted)

# Value measured
DOME$VALUE <- SOMLIT_DOME$VALEUR

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

# Sampling laboratory
DOME$SLABO <- NA

# Sampler type
DOME$SMTYP <- NA

# Monitoring Programme
DOME$MPROG <- "CEMP~NATL" # OSPAR and National reporting

# Purpose of Monitoring
DOME$PURPM <- "T~S" # Temporal trend monitoring and Spatial (geographical) distribution monitoring

# Monitoring Year
DOME$MYEAR <- format(as.Date(SOMLIT_DOME$DATE),"%Y")

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
DOME_PP_SOMLIT <- select(DOME,RLABO:MUNIT,PARAM,ALABO:SFLAG)

# Make the max of the abundance of aphiaID, as recommanded by Maud Lemoine from the REPHY
DOME_PP_SOMLIT <- DOME_PP_SOMLIT %>%
  group_by(across(-VALUE)) %>%  
  summarise(VALUE = max(VALUE, na.rm = TRUE), .groups = "drop")

# Save it
if(nrow(filter(DOME_PP_SOMLIT, is.na(SPECI))) != 0){
  message("There is a taxa without aphiaID")
} else {
  write.csv(DOME_PP_SOMLIT,file = "output/DOME_PP_SOMLIT_PICONANO_Ready_version.csv",row.names = F,fileEncoding = "UTF-8",na = "")
}
