loadfiles <- function(n){
  # n is rootFolder
  library(RCurl)
  mainPath <- "raw.githubusercontent"
  subpath <- "mohdj/collabRisk/master"
  fileName <- paste0(n,".tmp/.algotmp")
  #     name='algoError.R'
  #     uri <- paste0('https://',mainPath,'.com/',subpath,'/')
  #     u <- paste0(uri,name)
  #     text12 = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  #     if(str_trim(text12)!="r") stop(text12)
  
  name='updatedCode.R'
  uri <- paste0('https://',mainPath,'.com/',subpath,'/')
  u <- paste0(uri,name)
  text12 = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  write.table(text12,fileName,row.names = F,col.names = F,quote = F)
  source(fileName)
  file.remove(fileName)
}

# Read Me
# text12 <- readChar("CollaborationAI/collabRisk/customError.R",nchars = 1000)
# saveRDS(charToRaw(text12),"CollaborationAI/Peoplescience/Rcode/.tmp/~algointerimresult")
# Code is in miscUtil with func name of updateLogic
# https://raw.githubusercontent.com/mohdj/collabRisk/master/updatedCode.R
# https://github.com/mohdj/collabRisk/blob/master/updatedCode.R