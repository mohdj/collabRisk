getAlgoWeights <- function(sliderWeight){
  algoWeightMap <- data.frame(sliderWeight=-5:5,algoWeight=c(-10^6,-100,-10,-1,-0.1,0,0.1,1,10,100,1000))
  tmp <- data.frame(sliderWeight=sliderWeight) %>% inner_join(algoWeightMap,by="sliderWeight")
  weight <- tmp$algoWeight
  names(weight) <- names(sliderWeight)
  return(weight)
}

createGroups <- function(dat,weight,colTypes,groupSize,importantTags=NULL,excludeTags=NULL,hardSepComb=NULL,assignTagWeightByFreq=T,numberOfGroup=NULL){
  dat[dat==""] <- NA # this is required to treat blanks as missing value (which matches with anything) else, separation/combination will try to match blanks
  if(!is.null(numberOfGroup)){ # numberOfGroup if mentioned than that gets priority, ideally both of them should not be allowed to be specified but to have minimum disrupt to old code, this hack is used.
    groupSize <- floor(nrow(dat)/numberOfGroup)
  } else {
    numberOfGroup <- floor(nrow(dat)/groupSize)
  }
  if(numberOfGroup==1) numberOfGroup <- 2 #atleast have two groups
  weight <- weight[weight!=0]
  typeOfClustering <- "hclust"
  colTypes <- colTypes[names(weight)]
  sliderWeight <- weight
  weight <- getAlgoWeights(weight)
  importantTags <- gsub(pattern = " ",replacement = "_",x = importantTags) # replace space by underscore since later on anyways they will be replaced
  clusInfo <- list(sliderWeight=sliderWeight,weight=weight,colTypes=colTypes,numberOfGroup=numberOfGroup,
                   idealClusterSize=groupSize,datColNames=names(dat),distanceType="SimilarityBased",
                   importantTags=importantTags,excludeTags=excludeTags,hardSepComb=hardSepComb)
  dat <- setupTimeZoneColumn(dat,clusInfo)
  dat <- setupDocumentTermFrequencyForTextCols(dat,clusInfo)
  clusInfo$columnTermFrequencyColumnMapping <- getColumnTermFrequencyColumnMapping(dat,clusInfo)
  clusInfo <- getAvgNumOfItemForMultiChoiceColumn(dat,clusInfo)
  clusInfo$numericColRangeDiff <- getNumericColumnRangeDifference(dat,clusInfo)
  if(length(importantTags)>0){
    clusInfo$importantTagDetail <- setupImportantTags(dat,clusInfo)
  }
  print(paste0("distance calculation starting... ",Sys.time()))
  clusInfo <- generateCluster(dat = dat,clusInfo=clusInfo,typeOfClustering = typeOfClustering)
  print(paste0("distance calculation & clustering completed... ",Sys.time()))
  clusInfo <- balanceClusterSize(clusInfo)
  print(paste0("Balance clustering done... ",Sys.time()))
  clusInfo <- performMovementsToImproveClusters(clusInfo)
  print(paste0("Randomised performance improvement done... ",Sys.time()))
  if(!is.null(clusInfo$hardSepComb)){
    clusInfo <- balanceExplicitCombConstraint(clusInfo)
    print(paste0("Explicit hard separation combination done... ",Sys.time()))
  }
  #   try({clusInfo$teamProfileData <- profileAllTeams(dat,clusInfo)
  #   clusInfo$clusterPerformance <- getClusteringPerformance(dat,clusInfo) 
  #   print(paste0("Team profiling done... ",Sys.time()))})
  
  return(clusInfo)
}