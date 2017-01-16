eventTeamSetFileName <- "data/output/event_teamset_37.csv"
eventAnswersFileName <- "data/output/answers_event_73.csv"
# previousEventTeamSetFileName <- "data/output/event_teamset_members_348_349.csv"
outputFileName <- "data/output/AlgoResults.csv"
questionCategoryFileName <- "data/output/question_category_73.csv"
memberNamesFileName <- "data/output/attendies_event_73.csv"

# Libraries to load ----------------
library(dplyr)
library(data.table)
library(tm)
library(stringr)
library(jsonlite)
library(RMySQL)
library(tidyr)


# Local MYSQL detail ----------
mySQLUser <- "collab"
mySQLPwd <- "collab123"
mySQLHost <- "localhost"
mySQLDBName <- "peoplescience_new"

rootFolder <- "C:/Users/Mohammed.Jamal/Documents/brickwin/CollaborationAI/Peoplescience/Rcode/"
debugSource(paste0(rootFolder,'uiIntegration.R'))
debugSource(paste0(rootFolder,'generateSet.R'))
debugSource(paste0(rootFolder,'memberMovement.R'))
debugSource(paste0(rootFolder,'randomizedImprovement.R'))
debugSource(paste0(rootFolder,'explicitSepCombConstraint.R'))
debugSource(paste0(rootFolder,'profileTeams.R'))
debugSource('C:/Users/Mohammed.Jamal/Documents/brickwin/CollaborationAI/collabRisk/updatedCode.R')

readEventData_csv <- function(eventTeamSetFileName,eventAnswersFileName){
  customerInputJson <- read.csv(eventTeamSetFileName,stringsAsFactors = F)
  custInputJson <- customerInputJson$customer_input[1]
  custInput <- fromJSON(txt = custInputJson)

  answerDat <- fread(eventAnswersFileName,data.table = F,stringsAsFactors = F)
  answerDat <- answerDat %>% select(eventId=event_id,memberId=attendie_id,questionId=question_id,answer=tags)
  
  if(!is.null(questionCategoryFileName))
    questionCategory <- fread(questionCategoryFileName,data.table = F,stringsAsFactors = F)
  else questionCategory <- NULL

  if(ncol(custInput$q_weights)>1){
    #Below code removes duplicate answer entries by persons (in particular for quick connector)
    #Below line reverses the order of entry so the last entry is displayed first, this is implementation specific detail - 
    # since we are using duplicated which removes subsequent duplicates
    answerDat <- answerDat %>% mutate(EntryOrder=1:n()) %>% arrange(desc(EntryOrder)) %>% mutate(EntryOrder=NULL)
    answerDat <- answerDat[!duplicated(paste0(answerDat$memberId,"_",answerDat$questionId)),]
    
    answerDat <- spread(data = answerDat,key = questionId,value = answer)
  }
  names(answerDat) <- c(names(answerDat)[1:2],paste0("quest",names(answerDat)[3:ncol(answerDat)]))
  
  weight <- as.integer(custInput$q_weights$qwValue)
  names(weight) <- paste0("quest",custInput$q_weights$q_id)
  
  if(!is.null(custInput$previousEventTeamSets)){
    previousTSData <- fread(previousEventTeamSetFileName,data.table = F,stringsAsFactors = F)
    if(nrow(previousTSData)>0){
      prevTSAsAnswer <- data.frame(memberId=answerDat$memberId)
      for(tsid in custInput$previousEventTeamSets$event_teamset_id){
        prevTSAsAnswer <- prevTSAsAnswer %>% 
          inner_join(previousTSData %>% filter(event_teamset_id==tsid),by=c("memberId"="member_id"))
        names(prevTSAsAnswer)[which(names(prevTSAsAnswer)=="team_id")] <- paste0("ts",tsid)
      }
      answerDat <- answerDat %>% inner_join(prevTSAsAnswer,by="memberId")
      
      newNames <- c(names(weight),paste0("ts",custInput$previousEventTeamSets$event_teamset_id))
      weight <- c(weight,as.integer(custInput$previousEventTeamSets$qwValue))
      names(weight) <- newNames
    }
  }
  
  colTypes <- identifyColumnTypes(answerDat,questionCategory)
  colTypes <- unlist(colTypes)
  
  # if Network question is there, need to additionally pull member Names
#  if(TRUE){
 if(QuestionCategoryId$NetworkWhoKnowsWho$ColTypeName %in% colTypes){
#     selectSqlAttendiesName <- "SELECT a.id,a.first_name,a.last_name FROM attendies a inner join answers b on a.id = b.attendie_id where b.event_id = #event_id#"
#     selectSqlAttendiesName <- gsub(pattern = "#event_id#",replacement = eventId,x = selectSqlAttendiesName)
#     memberNames <- dbGetQuery(conn,selectSqlAttendiesName)
    memberNames <- fread(memberNamesFileName,select = c("id","first_name","last_name"),data.table = F,stringsAsFactors = F)
    memberNames$memberName <- paste(memberNames$last_name,memberNames$first_name,sep=" ")
    memberNames <- memberNames[,c("id","memberName")]
    memberNames <- unique(memberNames)
    # answerDat <- answerDat %>% inner_join(memberNames,by=c("memberId"="id"))
    answerDat <- memberNames %>% left_join(answerDat,by=c("id"="memberId"))
  }
  
  groupSize <- as.integer(custInput$groupSize)
  importantTags <- custInput$include_tags
  excludeTags <- custInput$exclude_tags
  
  if(!is.null(custInput$p_exclude)) {
    answerDat <- answerDat[which(!(answerDat$memberId %in% custInput$p_exclude)),]}
  
  hardSepComb <- custInput$p_together
  if(class(hardSepComb)=="matrix") hardSepComb <- split(hardSepComb,1:nrow(hardSepComb))
  
  if(!is.null(hardSepComb)){
    tmpMemberIds <- answerDat$memberId
    for(i in 1:length(hardSepComb)){
      for(j in 1:length(hardSepComb[[i]])){
        hardSepComb[[i]][j] <- which(tmpMemberIds==hardSepComb[[i]][j])
      }
    }
  }
  
  out <- list(weight=weight,colTypes=colTypes,groupSize=groupSize,answerDat=answerDat,
              importantTags=importantTags,excludeTags=excludeTags,hardSepComb=hardSepComb)
  return(out)
}

eventdat <- readEventData_csv(eventTeamSetFileName,eventAnswersFileName)
clusInfo <- createGroups(dat = eventdat$answerDat,weight = eventdat$weight,colTypes = eventdat$colTypes,
                         groupSize = eventdat$groupSize,importantTags=eventdat$importantTags,excludeTags=eventdat$excludeTags,
                         hardSepComb=eventdat$hardSepComb)
#writeAlgoTeamsetToDB(72,clusInfo,eventdat$answerDat)
algoOut <- data.frame(member_id=eventdat$answerDat$memberId,team_id=clusInfo$clusterNumbers)
write.csv(algoOut,outputFileName)
print("Algo output written to csv")



# Select Statments ------------------------
# peoplescience_new
# select *from peoplescience_new.event_teamsets where id=20;
# SELECT * FROM peoplescience_new.attendies  where event_id = 45;
# select * from peoplescience_new.answers where event_id = 45;
# SELECT id as questionId,category_flag as category FROM peoplescience_new.questions where category_flag > 0 and id in (select distinct(question_id) from peoplescience_new.answers where event_id=45);
# select event_teamset_id, member_id, team_id from event_teamset_members where event_teamset_id in (20)
# select * from peoplescience_new.categories;

# educatesocial_peoplescience_test
# select *from educatesocial_peoplescience_test.event_teamsets where id=442;
# SELECT * FROM educatesocial_peoplescience_test.attendies  where event_id = 556;
# select * from educatesocial_peoplescience_test.answers where event_id = 556;
# SELECT id as questionId,category_flag as category FROM educatesocial_peoplescience_test.questions where category_flag > 0 and id in (select distinct(question_id) from educatesocial_peoplescience_test.answers where event_id=556);
# select event_teamset_id, member_id, team_id from event_teamset_members where event_teamset_id in (442)
# select * from educatesocial_peoplescience_test.categories;
