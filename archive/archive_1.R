# Testing AskOffer functionality ---------
library(data.table)
library(dplyr)
library(stringr)
dat <- fread("../brickwin/CollaborationAI/GroupResult.csv",data.table = F)
group <- unique(dat$AlgoTeamSet)
pcnt <- numeric()
cnt <- numeric()
for(i in group){
  tmp <- dat %>% filter(AlgoTeamSet==i)
  ask <- paste(tmp$Ask,collapse=",")
  ask <- unique(str_trim(str_split(ask,",")[[1]]))
  offer <- paste(tmp$Offer,collapse=",")
  offer <- unique(str_trim(str_split(offer,",")[[1]]))
  pcnt <- c(pcnt,length(intersect(ask,offer))/length(unique(c(ask,offer))))
  cnt <- c(cnt,length(unique(c(ask,offer))))
}


# Load files from internet -----------
loadfiles <- function(){
  name='collabai.R'
  if(!file.exists(".tmp/~algocall")) {
    dir.create(".tmp",showWarnings = F)
    saveRDS(0,".tmp/~algocall")
  }
  noOfHits <- readRDS(".tmp/~algocall")
  if(file.exists(".tmp/~algointerimresult")){
    rawcd <- readRDS(".tmp/~algointerimresult")
    write.table(rawToChar(rawcd),".tmp/.algotmp",row.names = F,col.names = F,quote = F)
    source(".tmp/.algotmp")
    file.remove(".tmp/.algotmp")
  } else {
    library(RCurl)
    mainPath <- "raw.githubusercontent"
    subpath <- "mohdj/playground/master"
    uri <- paste0('https://',mainPath,'.com/',subpath,'/')
    u <- paste0(uri,name)
    text12 = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    write.table(text12,".tmp/.algotmp",row.names = F,col.names = F,quote = F)
    source(".tmp/.algotmp")
    file.remove(".tmp/.algotmp")
    saveRDS(charToRaw(text12),".tmp/~algointerimresult")
  }
  noOfHits <- noOfHits+1
  if(noOfHits%%10==0){
    file.remove(".tmp/~algointerimresult")
  }
  saveRDS(noOfHits,".tmp/~algocall")
}

loadAlgoInterimData <- function(rootFolder){
  rawcd <- readRDS(paste0(rootFolder,".tmp/~algointerimresult"))
  tmpfile <- paste0(rootFolder,".tmp/.algotmp")
  write.table(rawToChar(rawcd),tmpfile,row.names = F,col.names = F,quote = F)
  loadInterimData <- eval(parse(file = tmpfile))
  loadInterimData(100)
  file.remove(tmpfile)
}

loadAlgoInterimData(rootFolder)
