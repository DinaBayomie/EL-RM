library(readr)
require(arules)
require(arulesViz)
require(RColorBrewer)

library(stringr)
library(dplyr)
library(plyr)

require (doParallel)
require(compiler)

### read rules from 2020 prepaid travel cost 
# rulesDF1 <- read_csv("output/ act_rules_binary PrepaidTravelCost2017P1.csv")
# rulesDF2 <- read_csv("output/ act_rules_binary PrepaidTravelCost2017P2.csv")
# rulesDF3 <- read_csv("output/ act_rules_binary PrepaidTravelCost2018P1.csv")
# rulesDF4 <- read_csv("output/ act_rules_binary PrepaidTravelCost2018P2.csv")
# 
# View(rulesDF1)
# View(rulesDF2)
# View(rulesDF3)
# View(rulesDF4)
# 
# rulesDF1v <- read_csv("output/ act_rules_value PrepaidTravelCost2017P1.csv")
# 
# 
# # rulesDF1 <- read_csv("output/ act_rules_binary trafficCT1month.csv")
# # rulesDF2 <- read_csv("output/ act_rules_binary trafficCT2-3years.csv")
# # rulesDF3 <- read_csv("output/ act_rules_binary trafficCT3months.csv")
# # rulesDF4 <- read_csv("output/ act_rules_binary trafficCT6-12Months.csv")
# # rulesDF5 <- read_csv("output/ act_rules_binary trafficCT6months.csv")
# 
# commonRules<-intersectRules(rulesDF1,rulesDF2,rulesDF3,rulesDF4)
# write.csv(commonRules,paste("output/","commonRules",outputName))

###This function takes the name of the rules files and compare them #
compareRules<-function(fileNames,outputName){
  print(fileNames)
  dfLi = list()
  dfLiAll = list()
  i = 1 
  for (fname in fileNames) {
    temp <-read_csv(fname)
  #  View(temp)
    dfLi[[i]]<- temp$rules
    dfLiAll[[i]] <- temp
    i = i+1
  }
  
  print(paste("**Finding common rules...", Sys.time()))
  commonRules<-intersectRules(dfLi)
  write.csv(commonRules,paste("output/","commonRules",outputName))
  
  commonRulesR<-Reduce(intersect,dfLi)
  write.csv(commonRulesR,paste("output/","commonRulesReduce",outputName))
  
  print(paste("**Gather union of all unique rules...", Sys.time()))
  allRules<-unionRules(dfLi)
  write.csv(allRules,paste("output/","allUnionRules",outputName))
  
  allRules<-Reduce(union, dfLi)
  write.csv(allRules,paste("output/","allUnionRulesReduce",outputName))
  
  
  
  print(paste("**summarize rules...", Sys.time()))
  ruleSummary<-summaryRules(dfLiAll)
  write.csv(ruleSummary,paste("output/","summaryRules",outputName))

  print(paste("**Finding unique different rules per each event log partition...", Sys.time()))
  
 
  for(i in 1:(length(dfLi))){
    # diff<-differentRules(dfLi[[i]]$rules,commonRules)
    if(i!=1){
    aa<-list(dfLi[[i]])
    aa <- append(aa, dfLi[1:i-1])
    if(i!=length(dfLi)){
    aa <- append(aa, dfLi[i+1:length(dfLi)])
    }
    }
    else{
      aa = dfLi
    }
    diff<- Reduce(setdiff,aa )
    write.csv(diff,paste("output/","diff_",i,"_",outputName))
  }
  for(i in 1:(length(dfLi))){
    diff<-differentRules(dfLi[[i]],commonRules)
  
    write.csv(diff,paste("output/","diff_common_",i,"_",outputName))
  }
  
  print(paste("**Combine rules...", Sys.time()))
  
  cumulativeR<-createCompulativeRules(commonRules)
  
  write.csv(diff,paste("output/","cumCommon_",outputName))
  
  cumulativeR<-createCompulativeRules(allRules)
  
  write.csv(diff,paste("output/","cumAll_",outputName))
  
  
  }

### will get the different rules vectors exists in a but not in b 
### a/b
differentRules<-function(rulesVa,rulesVb){
  
  return(setdiff(rulesVa,rulesVb))
  
}



intersectRules<-function(rulesDfs){
  #rulesDfs<-list(...)
  
  commonRules<-rulesDfs[[1]]
  for(i in 2:(length(rulesDfs)-1)){
    commonRules<-append(commonRules,intersect(commonRules,rulesDfs[[i]]))
  }
  return(unique(commonRules))
}



unionRules<-function(rulesDfs){
  # rulesDfs<-list(...)
  # 
  allRules<-vector()
  for(i in 1:(length(rulesDfs)-1)){
    allRules<-append(allRules,union(rulesDfs[[i]],rulesDfs[[i+1]]))
  }
  
  return(unique(allRules))
}


summaryRules<-function(rulesDfs){
  # rulesDfs<-list(...)
  
  allRules<-rulesDfs[[1]]
  for(i in 1:(length(rulesDfs))){
    allRules<-rbind(allRules, rulesDfs[[i]])
  }
  
  return(summary(allRules))
}


### drill downn and up in the rules using the binary and the value constraints 
### binaryRules_ is a vector \ list of the rules, the same for valueRules_

drillUpandDown <- function(binaryRules_,valueRules_,outputName,minVrules) {

  bRules <- splitRules(binaryRules_)
  vRules <- splitRules(valueRules_)
  print(paste("**building drill up and down rules dataframe...", Sys.time()))
  combineRules<-foreach(t = iter(bRules, by = 'row'), .combine = rbind) %dopar% {
  #  print(t$LHS)
     lhsFiltered <- vRules %>% filter(str_detect(LHS,t$LHS))
    
     if(!empty(df = lhsFiltered)){
      # print(t$RHS)
       rhsFiltered <- lhsFiltered %>% filter(str_detect(RHS,t$RHS))
       if(nrow(rhsFiltered)> 1)
       {
         if(!empty(df = rhsFiltered)){
           combineRules<-combineBandVrules(t,rhsFiltered)
         }
         }
     }
   }
  View(combineRules)
 colnames(combineRules) <-
  c("bLHS",
    "bRHS",
    "vRHS",
    "vRHS")
 
 write.csv(combineRules,paste("output/","combineRules",outputName))

 
  print(paste("**aggreating rules with a disjunction...", Sys.time()))
 cumulativeR<-createCompulativeRules(bRules)
 write.csv(cumulativeR,paste("output/","cumulativeRules",outputName))
 
 
 
# return(combineRules)
}

combineBandVrules <- function(bRule, filteredVrules) {

   combineRules<-data.frame(
    bLHS = character(),
    bRHS = character(),
    vLHS = character(),
    vRHS = character(), 
    stringsAsFactors = FALSE
  )
  colnames(combineRules) <-
    c("bLHS",
      "bRHS",
      "vRHS",
      "vRHS")


  for (i in 1:nrow(filteredVrules)) {
      temp<-data.frame(
      bLHS = bRule[1,1],
      bRHS = bRule[1,2],
      vLHS = filteredVrules[i,1],
      vRHS = filteredVrules[i,2],
      stringsAsFactors = FALSE
    )
    combineRules<-rbind(combineRules,temp)

  }
  
  return(combineRules)
}

###the generate left and right handside#
splitRules <- function(rules_) {
  
  LHSv = vector()
  RHSv = vector()

  rulesDFsplit <-foreach(t = iter(rules_, by = 'row'), .combine = rbind) %dopar% {
    rulesDFsplit <- splitRule(t)
  }
 
  
  colnames(rulesDFsplit) <-
    c("LHS",
      "RHS")
  return(rulesDFsplit)
}
splitRule <- function(rule_) {

    # temp=strsplit(rule_$rules,"=>")
    temp=strsplit(rule_,"=>")
    LHSv<- str_replace_all(temp[[1]][1], "[[:punct:]]", " ")
    RHSv<- str_replace_all(temp[[1]][2], "[[:punct:]]", " ")

  
  rulesDFsplit<-data.frame(
    LHS = LHSv,
    RHS = RHSv,
    stringsAsFactors = FALSE
  )
  
  colnames(rulesDFsplit) <-
    c("LHS",
      "RHS")
  return(rulesDFsplit)
}

### aggregate the different rules that affects the same 2 events in a disjunction relation 

createCompulativeRules<-function(rules_){
  splRules <- splitRules(rules_)
  
  rulesG_ <- split(splRules , f = splRules$LHS)
  rulesDFrulesDFcombine <- foreach(t = iter(rulesG_, by = 'row'), .combine = rbind) %dopar% {
    cRHS=paste(t$RHS, collapse = ' and ')
    rulesDFrulesDFcombine<-data.frame(
      LHS = t[1,1],
      RHS = cRHS,
      stringsAsFactors = FALSE
    )
    
  }
  
  colnames(rulesDFrulesDFcombine) <-
    c("LHS",
      "RHSDisjunction")
  return(rulesDFrulesDFcombine)
}


