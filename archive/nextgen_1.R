library(tm)
library(dplyr)
library(data.table)
library(stringr)

getSimulatedBigData <- function(N,sampleSet,numOfSingleChoiceCols,numOfMultiChoiceCols,possibleNumOfItemsPerValue){
  # Generate single choice columns
  sampset <- eval(parse(text=paste0("c('",paste(paste0("a",sampleSet),collapse="','"),"')")))
  codestr <- paste0("data.frame(",paste(paste0("x",1:numOfSingleChoiceCols,"=sample(sampset,N,T)"),collapse=","),",stringsAsFactors=F)")
  dat <- eval(parse(text=codestr))
  #generate multi choice columns
  numOfCols <- numOfSingleChoiceCols+numOfMultiChoiceCols
  for(j in (numOfSingleChoiceCols+1):numOfCols){
    newcol <- list()
    for(i in 1:N){
      newcol[[i]] <- paste(paste0("a",sample(x = sampleSet,size = sample(x = possibleNumOfItemsPerValue,size = 1))),collapse=",")
    }
    newcol <- sapply(newcol,c)  
    dat[[paste0("x",j)]] <- newcol
  }  
  return(dat)
}

getMultiChoiceColumnDistance_array <- function(x,y,columnIdx,clusInfo){
  plusxy <- x+y
  multiplyxy <- x*y
  similar <- rowSums(multiplyxy)
  totalPresent <- rowSums(plusxy) - similar +0.00001 # just to avoid totalPresent as 0 when both are missing, this will add very slight error at 4th digit for all
  jacSim <- 1-similar/totalPresent # Jacard similarity
  themeSim <- 1 - similar/clusInfo$avgNumOfItemForMultiChoiceColumn[[names(clusInfo$weight)[columnIdx]]]
  themeSim <- themeSim*(themeSim>=0) # set negative distances to 0 distances
  if(clusInfo$weight[columnIdx]>0){
    out <- pmin(themeSim,jacSim)*0.7 + 0.3*jacSim
  } else{
    out <- jacSim
  }
  # Handling missing value
  bothPresent <- (rowSums(x)+rowSums(y))>1
  out <- out*bothPresent # handle missing value - if any one of them has missing value assign it 0 distance
  return(out)
}

getMultiChoiceColumnDistance_v1fast <- function(x,y,columnIdx,clusInfo){
  if(class(x)=="matrix")
    out <- getMultiChoiceColumnDistance_array(x,y,columnIdx,clusInfo)
  else {
    maxSimilarity <- clusInfo$avgNumOfItemForMultiChoiceColumn[[names(clusInfo$weight)[columnIdx]]]
    out <- getMultiChoiceColumnDistance_single(x,y,maxSimilarity,clusInfo)
  }
    
  return(out)
}

getMultiChoiceColumnDistance_single <- function(x,y,maxSimilarity,clusInfo){
  simDist <- getSimilarityBasedDistance(x,y,maxSimilarity,clusInfo)
  jacdist <- getJaccardDistance(x,y)
  simdist <- min(simDist,jacdist)
  dist <- 0.7*simdist + 0.3*jacdist
  return(dist)
}

sampleSet <- c("one","two","three","four","five","six")
dat <- getSimulatedBigData(N=100,sampleSet=sampleSet,numOfSingleChoiceCols=3,numOfMultiChoiceCols=2,
                           possibleNumOfItemsPerValue=1:3)
#dat <- datorig
colTypes=c(x1="SingleChoice",x2="SingleChoice",x3="SingleChoice",x4="MultiChoice",x5="MultiChoice")
weight=c(x1=-5,x2=-4,x3=4,x4=5,x5=3)

groupSize <- 2
weight <- weight[weight!=0]
colTypes <- colTypes[names(weight)]
numberOfGroup <- floor(nrow(dat)/groupSize)

clusInfo <- list(weight=weight,colTypes=colTypes,numberOfGroup=numberOfGroup,
                 idealClusterSize=groupSize,datColNames=names(dat),distanceType="SimilarityBased")
dtm <- setupDocumentTermFrequencyForTextCols(dat,clusInfo)
clusInfo$columnTermFrequencyColumnMapping <- getColumnTermFrequencyColumnMapping(dtm,clusInfo)
clusInfo <- getAvgNumOfItemForMultiChoiceColumn(dtm,clusInfo)
#clusInfo$columnTermFrequencyColumnMappingIdx <- getColumnTermFrequencyColumnMappingIdx(dtm,clusInfo)
#clusInfo <- getAvgNumOfItemForMultiChoiceColumn(dat,clusInfo)

# singleChoiceColIdx <- which(clusInfo$colTypes %in% c("SingleChoice"))
# singleChoiceSimColIdx <- which(clusInfo$colTypes %in% c("SingleChoice") & clusInfo$weight > 0)
# singleChoiceDissimColIdx <- which(clusInfo$colTypes %in% c("SingleChoice") & clusInfo$weight < 0)


#dist <- matrix(nrow = nr,ncol = nr)


populateDistance_v1fast <- function(dtm,clusInfo){
  nr <- nrow(dat)
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
  
  dist <- matrix(nrow = (nr-1),ncol = nr)
  dist[,1] <- 0
  for(i in 1:(nr-1)){
    tmpdist <- matrix(nrow = (nr-i),ncol = length(colTypes))
    #if(i == (nr-1)) browser()
    x <- matsc[1:(nr-i),]
    y <- matsc[(i+1):nr,]
    comp <- matrix(data = (x==y)*1,nrow = nr-i) # Need to explicitly make it matrix
    tmpdist[,singleChoiceSimColIdx] <- 1-comp[,singleChoiceSimColIdx]
    tmpdist[,singleChoiceDissimColIdx] <- comp[,singleChoiceDissimColIdx]
    tmpdist[is.na(tmpdist)] <- 0 # missing values have 0 distance with everyone, so missing values means opportunity to combine with anything
    
    for(j in MultiChoiceColIdx){
      xj <- matmcterm[1:(nr-i),columnTermFrequencyColumnMappingIdx[[j]]]
      yj <- matmcterm[(i+1):nr,columnTermFrequencyColumnMappingIdx[[j]]]
      tmpdist[,j] <- getMultiChoiceColumnDistance_v1fast(xj,yj,j,clusInfo)
    }
    
    if(length(askOfferMultiChoiceIdx)>0){
      askColIdx <- askOfferMultiChoiceIdx[1]
      offerColIdx <- askOfferMultiChoiceIdx[2]
      
      # Ask column
      xj <- matmcterm[1:(nr-i),columnTermFrequencyColumnMappingIdx[[askColIdx]]]
      yj <- matmcterm[(i+1):nr,columnTermFrequencyColumnMappingIdx[[offerColIdx]]]
      tmpdist[,askColIdx] <- getMultiChoiceColumnDistance_v1fast(xj,yj,askColIdx,clusInfo)
      
      # Offer column
      xj <- matmcterm[1:(nr-i),columnTermFrequencyColumnMappingIdx[[offerColIdx]]]
      yj <- matmcterm[(i+1):nr,columnTermFrequencyColumnMappingIdx[[askColIdx]]]
      tmpdist[,offerColIdx] <- getMultiChoiceColumnDistance_v1fast(xj,yj,offerColIdx,clusInfo)
    }
    
    tmpdist <- tmpdist %*% diag(abs(weight))
    tmpdist <- rowSums(tmpdist)
    tmpdist <- c(tmpdist,rep(NA,i-1))
    dist[,(i+1)] <- tmpdist
    #print(i)
  }
  distnew <- matrix(nrow = nr,ncol = nr)
  for(i in 1:(nr-1)){
    distnew[i,i:nr] <- dist[i,1:(nr-i+1)]
    distnew[i:nr,i] <- dist[i,1:(nr-i+1)]
  }
  distnew[nr,nr] <- 0
  return(distnew)
}

# comparing results
system.time({dist <- populateDistance_v1fast(dtm,clusInfo)})
distold <- populateDistanceMatrix(dtm,getDist,clusInfo)
for(i in 1:nrow(distold)){
  distold[i,i] <- 0
}
print(summary(as.vector(distold-dist)))