event_teamset_Id <- 848

# Peoplescience DB from Local config detail
mySQLHost <- "52.73.139.12"

# mySQLUser <- "collab_p"
# mySQLPwd <- "Z23#Ik3kKI9"
# mySQLDBName <- "peoplescience_new"

mySQLUser <- "collab"
mySQLPwd <- "C0ll@2@Br!3k"
mySQLDBName <- "educatesocial_peoplescience_test"

rootFolder <- "C:/Users/Mohammed.Jamal/Documents/brickwin/CollaborationAI/Peoplescience/Rcode/"

# Libraries to load ----------------
library(dplyr)
library(data.table)
library(tm)
library(stringr)
library(jsonlite)
library(RMySQL)
library(tidyr)

debugSource(paste0(rootFolder,'uiIntegration.R'))
debugSource(paste0(rootFolder,'generateSet.R'))
debugSource(paste0(rootFolder,'memberMovement.R'))
debugSource(paste0(rootFolder,'randomizedImprovement.R'))
debugSource(paste0(rootFolder,'explicitSepCombConstraint.R'))
debugSource(paste0(rootFolder,'profileTeams.R'))
debugSource(paste0(rootFolder,'warningMsgs.R'))
debugSource('C:/Users/Mohammed.Jamal/Documents/brickwin/CollaborationAI/collabRisk/updatedCode.R')

runStatus <- 0

# try({
  eventdat <- readEventData(eventTeamSetId = event_teamset_Id)
  clusInfo <- createGroups(dat = eventdat$answerDat,weight = eventdat$weight,colTypes = eventdat$colTypes,
                           groupSize = eventdat$groupSize,importantTags=eventdat$importantTags,excludeTags=eventdat$excludeTags,
                           hardSepComb=eventdat$hardSepComb,warningMsgs=eventdat$warningMsgs)
  # writeAlgoTeamsetToDB(event_teamset_Id,clusInfo,eventdat$answerDat)
  algoOut <- data.frame(member_id=eventdat$answerDat$memberId,team_id=clusInfo$clusterNumbers)
  write.csv(algoOut,"data/output/AlgoResults.csv")
  runStatus <- 1
  print(paste0("Algo results written to csv... ",Sys.time()))
# })
# writeRunStatusMsgToDB(event_teamset_Id = event_teamset_Id,status = runStatus)
