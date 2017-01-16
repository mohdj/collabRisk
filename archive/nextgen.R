sampleSet <- c("one","two","three","four","five","six")
dat <- getSimulatedBigData(N=5,sampleSet=sampleSet,numOfSingleChoiceCols=3,numOfMultiChoiceCols=2,
                           possibleNumOfItemsPerValue=1:3)
colTypes=c(x1="SingleChoice",x2="SingleChoice",x3="SingleChoice",x4="MultiChoice",x5="MultiChoice")
weight=c(x1=-5,x2=-4,x3=4,x4=5,x5=3)

groupSize <- 10
weight <- weight[weight!=0]
colTypes <- colTypes[names(weight)]
numberOfGroup <- floor(nrow(dat)/groupSize)
nr <- nrow(dat)
clusInfo <- list(weight=weight,colTypes=colTypes,numberOfGroup=numberOfGroup,
                 idealClusterSize=groupSize,datColNames=names(dat),distanceType="SimilarityBased")
dtm <- setupDocumentTermFrequencyForTextCols(dat,clusInfo)
clusInfo$columnTermFrequencyColumnMapping <- getColumnTermFrequencyColumnMapping(dtm,clusInfo)
#clusInfo$columnTermFrequencyColumnMappingIdx <- getColumnTermFrequencyColumnMappingIdx(dtm,clusInfo)
#clusInfo <- getAvgNumOfItemForMultiChoiceColumn(dat,clusInfo)

# singleChoiceColIdx <- which(clusInfo$colTypes %in% c("SingleChoice"))
# singleChoiceSimColIdx <- which(clusInfo$colTypes %in% c("SingleChoice") & clusInfo$weight > 0)
# singleChoiceDissimColIdx <- which(clusInfo$colTypes %in% c("SingleChoice") & clusInfo$weight < 0)
MultiChoiceSimColIdx <- which(clusInfo$colTypes %in% c("MultiChoice","PlainText") & clusInfo$weight > 0)
MultiChoiceDissimColIdx <- which(clusInfo$colTypes %in% c("MultiChoice","PlainText") & clusInfo$weight < 0)
MultiChoiceColIdx <- which(clusInfo$colTypes %in% c("MultiChoice","PlainText"))
askOfferMultiChoiceIdx <- which(clusInfo$colTypes %in% c("MultiChoiceAksOffer"))

matsc <- as.matrix(dtm[,which(clusInfo$colTypes %in% c("SingleChoice"))])
singleChoiceSimColIdx <- which(colnames(matsc) %in% names(weight)[weight>0])
singleChoiceDissimColIdx <- which(colnames(matsc) %in% names(weight)[weight<0])

MultiChoiceTermColNames <- unlist(clusInfo$columnTermFrequencyColumnMapping)
matmcterm <- as.matrix(dtm[,MultiChoiceTermColNames])
columnTermFrequencyColumnMappingIdx <- list()
for(i in which(clusInfo$colTypes %in% c("MultiChoice","PlainText","MultiChoiceAksOffer"))){
  nm <- names(clusInfo$colTypes)[i]
  columnTermFrequencyColumnMappingIdx[[i]] <- grep(pattern = paste0("__",nm),x = colnames(matmcterm))
}

dist <- matrix(nrow = nr,ncol = nr)

system.time({for(i in 1:(nr-2)){
  tmpdist <- matrix(nrow = (nr-i),ncol = length(colTypes))
  x <- matsc[1:(nr-i),]
  y <- matsc[(i+1):nr,]
  comp <- (x==y)*1
  tmpdist[,singleChoiceSimColIdx] <- 1-comp[,singleChoiceSimColIdx]
  tmpdist[,singleChoiceDissimColIdx] <- comp[,singleChoiceDissimColIdx]
  tmpdist[is.na(tmpdist)] <- 0 # missing values have 0 distance with everyone, so missing values means opportunity to combine with anything
  
  for(j in MultiChoiceColIdx){
    xj <- matmcterm[1:(nr-i),columnTermFrequencyColumnMappingIdx[[j]]]
    yj <- matmcterm[(i+1):nr,columnTermFrequencyColumnMappingIdx[[j]]]
    tmpdist[,j] <- getMultiChoiceColumnDistance_fast(xj,yj,j,clusInfo)
  }
  
  if(length(askOfferMultiChoiceIdx)>0){
    askColIdx <- askOfferMultiChoiceIdx[1]
    offerColIdx <- askOfferMultiChoiceIdx[2]
    
    # Ask column
    xj <- matmcterm[1:(nr-i),columnTermFrequencyColumnMappingIdx[[askColIdx]]]
    yj <- matmcterm[(i+1):nr,columnTermFrequencyColumnMappingIdx[[offerColIdx]]]
    tmpdist[,askColIdx] <- getMultiChoiceColumnDistance_fast(xj,yj,askColIdx,clusInfo)
    
    # Offer column
    xj <- matmcterm[1:(nr-i),columnTermFrequencyColumnMappingIdx[[offerColIdx]]]
    yj <- matmcterm[(i+1):nr,columnTermFrequencyColumnMappingIdx[[askColIdx]]]
    tmpdist[,offerColIdx] <- getMultiChoiceColumnDistance_fast(xj,yj,offerColIdx,clusInfo)
  }
  
  tmpdist <- tmpdist*abs(weight[colnames(tmpdist)])
  print(i)
}
})

getMultiChoiceColumnDistance_fast <- function(x,y,columnIdx,clusInfo){
#   xj <- matmcterm[1:(nr-i),columnTermFrequencyColumnMappingIdx[[j]]]
#   yj <- matmcterm[(i+1):nr,columnTermFrequencyColumnMappingIdx[[j]]]
  plusxy <- x+y
  multiplyxy <- x*y
  similar <- rowSums(multiplyxy)
  totalPresent <- rowSums(plusxy) - similar
  jacSim <- 1-similar/totalPresent # Jacard similarity
  themeSim <- 1 - similar/clusInfo$avgNumOfItemForMultiChoiceColumn[[columnIdx]]
  if(clusInfo$weight[columnIdx]>0){
    out <- pmin(themeSim,jacSim)*0.7 + 0.3*jacSim
  } else{
    out <- jacSim
  }
  return(out)
}