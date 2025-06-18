______________________________________________________________________________
Nom               : README_Extraction_REPHY_API_quadrige.txt
Date de modif     : 18/06/2025
Objet             : Extraire les donnees REPHY via l'API Quadrige
Auteurs           : J-Y. Dias
Contact           : jean-yves.dias@sorbonne-universite.fr
_______________________________________________________________________________

Ce script permet d'extraire toutes les donnees REPHY automatiquement.
Pour fonctionner le script a besoin d'un fichier .json, ce fichier correspond
a la requete initial lors de l'utilisation de l'API Quadrige directement sur 
le site https://quadrige-core.ifremer.fr/api/graphiql. 

Paragraphe suivant indique l'historique pour recuperer le fichier .json
Aller directement au suivant pour son utilisation. 

---------------RECUPERER LA REQUETE API QUADRIGE = FICHIER .JSON----------------
Pour recuperer ce fichier .json il faut se rendre sur le site precedent, et 
le code suivant a ete soumis: 
query {
  executeResultExtraction(
    filter: {
      name: "OSPAR_DIAS"
      fields: [MONITORING_LOCATION_ORDER_ITEM_LABEL,MONITORING_LOCATION_ORDER_ITEM_NAME,MONITORING_LOCATION_ID,MONITORING_LOCATION_LABEL,MONITORING_LOCATION_NAME,MONITORING_LOCATION_MIN_LATITUDE,MONITORING_LOCATION_MIN_LONGITUDE,SURVEY_LABEL,SURVEY_DATE,SURVEY_COMMENT,SAMPLING_OPERATION_LABEL,SAMPLING_OPERATION_TIME,SAMPLING_OPERATION_COMMENT,SAMPLING_OPERATION_DEPTH_LEVEL_NAME,SAMPLING_OPERATION_DEPTH,SAMPLE_LABEL,SAMPLE_COMMENT,MEASUREMENT_PROGRAMS_ID,MEASUREMENT_PMFMU_PARAMETER_ID,MEASUREMENT_PMFMU_PARAMETER_NAME,MEASUREMENT_PMFMU_METHOD_NAME,MEASUREMENT_PMFMU_UNIT_SYMBOL,MEASUREMENT_REFERENCE_TAXON_ID,MEASUREMENT_REFERENCE_TAXON_NAME,MEASUREMENT_REFERENCE_TAXON_LEVEL,MEASUREMENT_REFERENCE_TAXON_PARENT_NAME,MEASUREMENT_REFERENCE_TAXON_APHIAID,MEASUREMENT_NUMERICAL_VALUE,MEASUREMENT_COMMENT,MEASUREMENT_ANALYST_DEPARTMENT_LABEL,MEASUREMENT_ANALYST_DEPARTMENT_NAME, SURVEY_ID, SURVEY_QUALITY_FLAG_NAME, SAMPLING_OPERATION_ID, SAMPLING_OPERATION_QUALITY_FLAG_NAME, SAMPLE_ID, SAMPLE_QUALITY_FLAG_NAME, MEASUREMENT_ID, MEASUREMENT_QUALITY_FLAG_NAME]
      periods: [{ startDate: "2022-01-01", endDate: "2024-01-31" }]
      mainFilter: { program: { ids: ["REPHY"] } }
      options: { orderItemType: { ids: ["ZONESMARINES"] } }
      
    }
  ) {
    id
    name
    startDate
    status
  }
}


Avec ce code on recupere les colonnes presentes dans la base
Quadrige. Voir la correspondance des colonnes avec fichier EquivalenceChamp_SEANOE-API_20250617.xlsx
On recupere uniquement les donnees du REPHY. La periode 
demandee n'a pas d'importance car elle est modifiable via le script R.
Additionnellement les zones marines definies par le REPHY sont ajoutees 
pour permettre une filtration spatiale plus rapide. Voir le code de ces
zones sur https://archimer.ifremer.fr/doc/00814/92642/98991.pdf 
(mais aussi Zones_Marines_OK.csv dans le dossier Additional_data). 

Apres avoir soumis on recupere un numero de token ou "ID". Il est necessaire
d'ouvrir une nouvelle fenetre sur https://quadrige-core.ifremer.fr/api/graphiql
et taper le code suivant : 
query {
  getExtraction(id: 60457315) {
    name
    status
    startDate
    endDate
    fileUrl
    error
  }
}

Pour recuperer l'extraction, on recupere un lien, le copie colle et recupere le fichier
.json dont on a besoin pour repeter l'extraction autant de fois que necessaire via le script R
, en modifiant la date si necessaire, ainsi qu'un fichier .txt indiquant ce qui a ete extrait,
le nombre d'echantillon non extrait pour cause d'acces (moratoire) et le nom des responsables.
On recupere egalement un fichier .csv qui correspond aux donnees extraites pour la demande
formulee. 

-------------------------EXTRACTION AVEC SCRIPT R ET FICHIER .json ----------------------
Pour automatiser l'extraction via R seulement le fichier .json est necessaire, raison pour
laquelle il s'agit du seul fichier fourni avec le script R et le present README.

Le script R a ete fourni par l'administration Quadrige le 27/06/2025 a J-Y. Dias et adapte.
Le script R a besoin du dossier contenant le fichier .json ainsi du dossier ou sont 
enregistrees les donnees, par simplicite il apparait plus simple que ce dossier soit le meme.
/!\ Il est important de ne pas modifier le nom du fichier .json pour la reproductibilite du script
(sinon ligne R a adapter egalement). 