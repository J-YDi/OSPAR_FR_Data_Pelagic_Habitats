#_______________________________________________________________________________
# Nom               : Extraction_REPHY_API_quadrige.r
# Date de modif     : 16/06/2025
# Objet             : Extraire les donnees REPHY via l'API Quadrige
# Auteurs           : Administration Quadrige, adapté par J-Y. Dias
# Version R         : 4.5.0
#_______________________________________________________________________________

#_______________________________________________________________________________

# Indiquer les dossiers ou la requete est enregistre et ou stocker les donnnes
# extraites

# Dossier contenant les requetes en format .json (cf Readme / a faire)
.REQUETES <- "D:/OSPAR/R/data" # A adapter
# Dossier ou les donnees sont enregistrees
.ORIGINAL.DATA.SETS <-  "D:/OSPAR/R/data" # A adapter

#_____________________Sous-Fonctions necessaires________________________________
# Originales, non modifiees par J-Y Dias

executeQuery <- function(query,apiUrl,apiToken = NULL) {
  if(!is.null(apiToken)){
    conn <- GraphqlClient$new(
      url = apiUrl,
      headers = list(Authorization = paste0("token ", apiToken))
    ) #Create the connection
  } else {
    conn <- GraphqlClient$new(
      url = apiUrl
      ,headers = "")
    #Create the connection
  }
  
  new <- Query$new()$query('url', query) #Pass the query
  result <- fromJSON(conn$exec(new$url),flatten = TRUE) #Parse the JSON output to a data.frame
  return(result)
}

executeResultExtraction <- function(Requete,apiUrl,apiToken = NULL) {
  result <- executeQuery(paste(
    'query {
    executeResultExtraction(
      filter: ',Requete
    ,') {
      id
      name
      startDate
      status
    }
  }'
    ,sep = "")
    ,apiUrl
    ,apiToken)
  return(result)
}

getExtraction <- function(jobId,apiUrl,apiToken = NULL) {
  result <- executeQuery(gsub('JOBID', jobId, 
                              'query {
      getExtraction(id: JOBID) {
        name
        status
        startDate
        endDate
        fileUrl
        error
      }
}
  ')
                         ,apiUrl
                         ,apiToken)
  return(result)
}

#________________________Fonction principale extraction_________________________
ExtractionAPI <- function(apiUrl = 'https://quadrige-core.ifremer.fr/graphql/public'
                          ,WDToken = NULL
                          ,NomRequete
                          ,YearRange){
  
  ## Loading packages
  ## -----------------------
  if(!require("ghql")){
    install.packages("ghql",repos="http://cran.irsn.fr/")
    require("ghql")
  }
  if(!require("jsonlite")){
    install.packages("jsonlite",repos="http://cran.irsn.fr/")
    require("jsonlite")
  }
  
  ## Recovering the token
  ## ---------------------
  apiToken <- NULL
  if(!is.null(WDToken)){
    if(dir.exists(WDToken)){
      WDini <- getwd()
      setwd(WDToken)
      
      FileToken <- c("TOKEN.txt","Token.txt","token.txt")
      FileToken <- FileToken[FileToken %in% list.files()]
      
      if(length(FileToken)>1){
        stop("Plusieurs fichiers Token.")
      }
      if(length(FileToken)>0){
        apiToken <- unlist(as.vector(read.delim(FileToken,header = FALSE)))
      }
      if(length(FileToken)==0){
        apiToken <- ""
      }
      
      setwd(WDini)
      
      rm(WDini,FileToken)
    }
  }
  
  ## Retrieving the extraction filter in json and modifying the dates
  ## ---------------------------------------------------------------------
  setwd(getAnywhere(".REQUETES")[1])
  Requete <- readLines(NomRequete,warn = FALSE)
  Requete[grep("startDate",Requete)] <- paste("    startDate : \""
                                              ,YearRange[1]
                                              ,"-01-01\","
                                              ,sep = "")
  Requete[grep("endDate",Requete)] <- paste("    endDate : \""
                                            ,YearRange[2]
                                            ,"-12-31\""
                                            ,sep = "")
  Requete <- paste(Requete
                   ,collapse = "")
  
  ## Recording extraction parameters in Original data sets
  # setwd(getAnywhere(".ORIGINAL.DATA.SETS")[1])
  # saveXML(SaveRoot
  #         ,file = NomRequete
  #         ,encoding = "ISO-8859-1")
  
  ## Start extraction
  ## ---------------------
  execution <- executeResultExtraction(Requete,apiUrl,apiToken)
  jobId <- execution$data$executeResultExtraction$id
  
  result <- getExtraction(jobId,apiUrl,apiToken)
  status <- result$data$getExtraction$status
  
  while(status=="RUNNING"){
    Sys.sleep(30) ## I wait 30 seconds.
    result <- getExtraction(jobId,apiUrl,apiToken)
    status <- result$data$getExtraction$status
  }
  
  ## Retrieve the zip
  ## ------------------
  if(status=="SUCCESS"){
    setwd(getAnywhere(".ORIGINAL.DATA.SETS")[1])
    
    myURL <- result$data$getExtraction$fileUrl
    File <- substr(myURL,gregexpr("/",myURL,perl = TRUE)[[1]][length(gregexpr("/",myURL,perl = TRUE)[[1]])]+1,nchar(myURL))
    
    ## Download the zip file
    download.file(url = myURL
                  ,destfile = File
                  ,method = 'auto')
    
    ## On d?compresse les fichiers
    unzip(File)
    
    ## Unzip the files
    file.rename(from = c(sub(".zip",".json",File)
                         ,sub(".zip","_RESULTAT.csv",File)
                         ,sub(".zip","_LISEZ_MOI.txt",File))
                ,to =  c(NomRequete
                         ,sub(".json",".csv",NomRequete)
                         ,sub(".json",".txt",NomRequete)))
    
    ## Delete the zip
    file.remove(File)
    
    rm("myURL","File")
  } ## End of SUCCESS
  
  ## Or we display the error message
  ## ---------------------------------
  if(status %in% c("FAILED","WARNING")){
    cat(paste("L'extraction a echouee car : "
              ,result$data$getExtraction$error
              ,sep = ""))
  } ## End of FAILED/WARNING
  
  #return(result)
}

#______________________________Extraction______________________________________
ExtractionAPI(apiUrl = 'https://quadrige-core.ifremer.fr/graphql/public'
              ,WDToken = "D:/OSPAR/R/data" ## Path to the folder containing the token
              ,NomRequete = "QUADRIGE_20250616_60457315_OSPAR_DIAS.json"
              ,YearRange = c(1987,2025)) # A modifier selon besoin


# Infos sur la requete .json:
# Contient les criteres d'extraction dans Quadrige:
# Demande d'extraire toutes les colonnes (filtre a posteriori)
# Impliquant le REPHY
# Pas de filtres sur les donnees 
# Uniquement les donnees ouvertes, non sous moratoire




