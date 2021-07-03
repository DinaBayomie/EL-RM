library(readr)
require(arules)
require(arulesViz)
require(RColorBrewer)


require (doParallel)
require(compiler)



####These functions handle the transformation of the log into a transaction dataframe both binary and value dataframes    ####


###This function filter out the attributes that has one and only one value per a case
#and the attributes that change with every event as eid###
removeNullAttributes <- function (log) {
  drops <- c()
  
  
  for (j in colnames(log))
  {
    if (length(which(!is.na(log[, j]))) == length(log))
    {
      drops <- c(drops, j)
    }
  }
  
  updatedLog <-  log[, !(names(log) %in% drops)]
  View (updatedLog)
  return(updatedLog)
}



### return attributes that have common values
### Attr intersect Attr`
attributesToCompare <- function (log) {
  colNames_ <- colnames(log)
  colNames_ <-
    colNames_[lapply(colNames_, function(x)
      length(grep("Case", x, value = FALSE))) == 0]
  
  colNames_ <-
    colNames_[lapply(colNames_, function(x)
      length(grep("Timestamp", x, value = FALSE))) == 0]
  
  colNames_ <-
    colNames_[lapply(colNames_, function(x)
      length(grep("Activity", x, value = FALSE))) == 0]
  
  compare <- list()
  # colNames_<-colnames(log)
  for (j in 2:length(colNames_) - 1)
  {
    # print(paste("j", j))
    
    xx <- as.list(unique(log[, colNames_[j]]))
    for (k in j:length(colNames_))
    {
      # print(paste("k", k))
      if (j != k) {
        xxK <- as.list(unique(log[, colNames_[k]]))
        
        
        # print(intersect(unique(log[!is.na(log[,colNames_[j]]),colNames_[j]]),unique(log[!is.na(log[,colNames_[k]]),colNames_[k]])))
        #    print("unique(log[,j])")
        #   print(xx)
        #   print("unique(log[,k])")
        #    print(xxK)
        if (length(intersect(xx[[1]], xxK[[1]])) != 0)
        {
          #        print("equal")
          compare[[length(compare) + 1]] <-
            list(colNames_[j], colNames_[k])
        }
      }
    }
  }
  
  return(compare)
}




### convert value transaction table to binary transaction table 
buildBinaryTransTable <-function(valueTrans){
  # cmpUpdateRow <- cmpfun(updateRow)
  
  
    binaryTransDF <-foreach(r = iter(valueTrans,by='row'),.combine = rbind) %dopar% {
   # foreach(c = iter(r, by='col'), .combine = cbind) %dopar%{
   for(i in 1:ncol(r)){
     if(grepl("same",r[,i],ignore.case = TRUE)){

       r[,i] = "same"
     }
     else if (grepl("different",r[,i],ignore.case = TRUE)){

       r[,i] = "different"
     }
   }
   binaryTransDF<-r
   }
    #   binaryTransDF<-cmpUpdateRow(r)
    # }
  
  return(binaryTransDF)
}

updateRow<-function(r){
  #for(i in 1:ncol(r)){
  k<- foreach(c = iter(r,by='col'),.combine = cbind) %dopar% {
   if(grepl("same",r[,i],ignore.case = TRUE)){
      
      c = "same"
    }
    else if (grepl("different",r[,i],ignore.case = TRUE)){
      
      c= "different"
    }
    k<-c
  }
  return(k)
}


### T2: build DF for all the attributes state equal or not
## based on ei and ei+1
buildTransactionCaseDfAssOnly <- function(aCase, atts_, compare_) {
  ## creating the transcations of the dataframe with all the attributes
  transDf  <-
    data.frame(
      ActivityPre = character(),
      ActivitySucc = character(),
      stringsAsFactors = FALSE
    )
  
  for (j in atts_) {
    transDf[, j] = character()
  }
  additionalNames <- c()
  
  if (length(compare_) > 0) {
    for (j in compare_) {
      n <- paste(j[1], "=", j[2])
      additionalNames <- c(additionalNames, n)
      transDf[, n] = character()
    }
    
    colnames(transDf) <-
      c("ActivityPre",
        "ActivitySucc",
        atts_, additionalNames)
    
  }
  else{
    colnames(transDf) <-
      c("ActivityPre",
        "ActivitySucc",
        atts_,)
  }
  
  ## loop over events
  
  
  
  for (j in 1:(nrow(aCase) - 1))
  {
    tEqu <-
      data.frame(
        ActivityPre = as.character(aCase[j, ]$Activity),
        ActivitySucc = as.character(aCase[j + 1, ]$Activity),
        stringsAsFactors = FALSE
      )
    
    for (i in atts_) {
      #   print(paste(i,aCase[j, i],"=", aCase[j + 1, i]))
      if (is.na(aCase[j, i]) & is.na(aCase[j + 1, i]))
      {
        tEqu[, i] = as.character("same")
      }
      else if (is.na(aCase[j, i]) || is.na(aCase[j + 1, i]))
      {
        tEqu[, i] = as.character("different")
      }
      else if (aCase[j, i] == aCase[j + 1, i]) {
        tEqu[, i] = as.character("same")
      }
      else{
        tEqu[, i] = as.character("different")
      }
      
    }
    
    
    colnames(transDf) <-
      c("ActivityPre",
        "ActivitySucc",
        atts_)
    
    
    
    transDf <- rbind(transDf, tEqu)
  }
  
  
  return(transDf)
  
}


### build DF for all the attributes state equal or not
## based on ei and ei+1
## search for correlation between attributes
buildTransactionCaseDfCorrelation <-
  function(aCase, atts_, compare_) {
    ## creating the transcations of the dataframe with all the attributes
    transDf  <-
      data.frame(
        ActivityPre = character(),
        ActivitySucc = character(),
        stringsAsFactors = FALSE
      )
    
    for (j in atts_) {
      transDf[, j] = character()
    }
    additionalNames <- c()
    ### add the attributes that have common values, i.e., to define correlation between different attributes
    if (length(compare_) > 0) {
      for (j in compare_) {
        n <- paste(j[1], "=", j[2])
        additionalNames <- c(additionalNames, n)
        transDf[, n] = character()
      }
      
      colnames(transDf) <-
        c("ActivityPre",
          "ActivitySucc",
          atts_, additionalNames)
      
    }
    else{
      colnames(transDf) <-
        c("ActivityPre",
          "ActivitySucc",
          atts_)
    }
    
    
    colNamesY_ <- colnames(transDf)
    ## loop over events
    #additionalNames <- as.list(additionalNames)
    
    #print(additionalNames)
    ## works for finding asscoition over same attribute between ei and ei+1
    #View(aCase)
    for (j in 1:(nrow(aCase) - 1))
    {
      #print(paste( "ActivityPre ", as.character(aCase[j, ]$Activity)))
      #print( paste("ActivitySucc ", as.character(aCase[j + 1, ]$Activity))) 
      tEqu <-
        data.frame(
          ActivityPre = as.character(aCase[j, ]$Activity),
          ActivitySucc = as.character(aCase[j + 1, ]$Activity),
          stringsAsFactors = FALSE
        )
      
      for (i in atts_) {
        #   print(paste(i,aCase[j, i],"=", aCase[j + 1, i]))
        if (is.na(aCase[j, i]) & is.na(aCase[j + 1, i]))
        {
          tEqu[, i] = as.character("same")
        }
        else if (is.na(aCase[j, i]) || is.na(aCase[j + 1, i]))
        {
          tEqu[, i] = as.character("different")
        }
        else if (aCase[j, i] == aCase[j + 1, i]) {
          tEqu[, i] = as.character(paste("same"))
        }
        else{
          tEqu[, i] = as.character(paste("different"))
        }
        
      }
      
      ## correlation attributes as event id and offer id mslan
      if (length(compare_) > 0) {
        #index <- 1
        for (i in compare_) {
          att1 <- i[[1]]
          att2 <- i[[2]]
          #     print(paste(att1, " ", att2))
          
          n <- paste(att1, "=", att2)
          
          #  print(paste("aCase[j, att1], aCase[j + 1, att2]", aCase[j, att1], aCase[j + 1, att2]))
          #   print(paste("aCase[j, att2], aCase[j + 1, att1]", aCase[j, att2] , aCase[j + 1, att1]))
          bothEvNa <- 0
          if ((is.na(aCase[j, att1]) &
               is.na(aCase[j + 1, att2])) ||
              (is.na(aCase[j, att2]) & is.na(aCase[j + 1, att1])))
          {
            tEqu[, n] = as.character("YES")#additionalNames[index]
          }
          
          if (!is.na(aCase[j, att1]) &&
              !is.na(aCase[j + 1, att2])) {
            #      print("Not na1")
            #      print(paste("aCase[j, att1] == aCase[j + 1, att2]",aCase[j, att1], aCase[j + 1, att2],"=",(aCase[j, att1] == aCase[j + 1, att2])))
            if (aCase[j, att1] == aCase[j + 1, att2]) {
              #   print(paste(n,"yes"))
              tEqu[, n] = as.character(paste("YES", att1, "->", att2))
            }
            else{
              tEqu[, n] = as.character("NO")
            }
          }
          
          
          else if (!is.na(aCase[j, att2]) &&
                   !is.na(aCase[j + 1, att1])) {
            #     print("Not na2")
            #  print(paste("aCase[j, att2] == aCase[j + 1, att1]",aCase[j, att2], aCase[j + 1, att1],"=",(aCase[j, att2] == aCase[j + 1, att1])))
            
            if (aCase[j, att2] == aCase[j + 1, att1]) {
              # print(paste(n,"yes"))
              tEqu[, n] = as.character(paste("YES", att2, "->", att1))
            }
            else{
              tEqu[, n] = as.character("NO")
            }
          }
          
          else{
            tEqu[, n] = as.character("NO")
          }
          
          #}
          #  index <- index + 1
        }
        
        colnames(tEqu) <-
          c("ActivityPre",
            "ActivitySucc",
            atts_, additionalNames)
        
      }
      else{
        colnames(tEqu) <-
          c("ActivityPre",
            "ActivitySucc",
            atts_)
      }
      #  View(tEqu)
      # print (paste( "colnames(tEqu)",colnames(tEqu)," colnames(transDf)",colnames(transDf)))
      transDf <- rbind(transDf, tEqu)
      #  View(transDf)
    }
    
    # print("finished")
    
    #  View(transDf)
    return(transDf)
    
  }


### build DF for all the attributes state equal or not
## based on ei and ei+1
## search for correlation between attributes
buildTransactionCaseDfCorrelationValue <-
  function(aCase, atts_, compare_) {
    ## creating the transcations of the dataframe with all the attributes
    transDf  <-
      data.frame(
        ActivityPre = character(),
        ActivitySucc = character(),
        stringsAsFactors = FALSE
      )
    
    for (j in atts_) {
      transDf[, j] = character()
    }
    additionalNames <- c()
    ### add the attributes that have common values, i.e., to define correlation between different attributes
    if (length(compare_) > 0) {
      for (j in compare_) {
        n <- paste(j[1], "=", j[2])
        additionalNames <- c(additionalNames, n)
        transDf[, n] = character()
      }
      
      colnames(transDf) <-
        c("ActivityPre",
          "ActivitySucc",
          atts_, additionalNames)
      
    }
    else{
      colnames(transDf) <-
        c("ActivityPre",
          "ActivitySucc",
          atts_)
    }
    
    
    colNamesY_ <- colnames(transDf)
    ## loop over events
    #additionalNames <- as.list(additionalNames)
    
    #print(additionalNames)
    ## works for finding asscoition over same attribute between ei and ei+1
    #View(aCase)
    for (j in 1:(nrow(aCase) - 1))
    {
      #print(paste( "ActivityPre ", as.character(aCase[j, ]$Activity)))
      #print( paste("ActivitySucc ", as.character(aCase[j + 1, ]$Activity))) 
      tEqu <-
        data.frame(
          ActivityPre = as.character(aCase[j, ]$Activity),
          ActivitySucc = as.character(aCase[j + 1, ]$Activity),
          stringsAsFactors = FALSE
        )
      
      for (i in atts_) {
        #   print(paste(i,aCase[j, i],"=", aCase[j + 1, i]))
        if (is.na(aCase[j, i]) & is.na(aCase[j + 1, i]))
        {
          tEqu[, i] = as.character("same NA")
        }
        else if (is.na(aCase[j, i]) || is.na(aCase[j + 1, i]))
        {
          tEqu[, i] = as.character("different one is NA")
        }
        else if (aCase[j, i] == aCase[j + 1, i]) {
          tEqu[, i] = as.character(paste("same : ", aCase[j, i]))
        }
        else{
          tEqu[, i] = as.character(paste("different : ", aCase[j, i]," !=",aCase[j + 1, i] ))
        }
        
      }
      #  print("finish normal attributes")
      #   print(paste("len", length(additionalNames)))
      #   print(paste("len", length(compare_)))
      
      
      
      ## correlation attributes as event id and offer id mslan
      if (length(compare_) > 0) {
        #index <- 1
        for (i in compare_) {
          att1 <- i[[1]]
          att2 <- i[[2]]
          #     print(paste(att1, " ", att2))
          
          n <- paste(att1, "=", att2)
          
          #  print(paste("aCase[j, att1], aCase[j + 1, att2]", aCase[j, att1], aCase[j + 1, att2]))
          #   print(paste("aCase[j, att2], aCase[j + 1, att1]", aCase[j, att2] , aCase[j + 1, att1]))
          bothEvNa <- 0
          if ((is.na(aCase[j, att1]) &
               is.na(aCase[j + 1, att2])) ||
              (is.na(aCase[j, att2]) & is.na(aCase[j + 1, att1])))
          {
            tEqu[, n] = as.character("YES NA")#additionalNames[index]
          }
          
          if (!is.na(aCase[j, att1]) &&
              !is.na(aCase[j + 1, att2])) {
            #      print("Not na1")
            #      print(paste("aCase[j, att1] == aCase[j + 1, att2]",aCase[j, att1], aCase[j + 1, att2],"=",(aCase[j, att1] == aCase[j + 1, att2])))
            if (aCase[j, att1] == aCase[j + 1, att2]) {
              #   print(paste(n,"yes"))
              tEqu[, n] = as.character(paste("YES", att1, "->", att2))
            }
            else{
              tEqu[, n] = as.character("NO")
            }
          }
          
          
          else if (!is.na(aCase[j, att2]) &&
                   !is.na(aCase[j + 1, att1])) {
            #     print("Not na2")
            #  print(paste("aCase[j, att2] == aCase[j + 1, att1]",aCase[j, att2], aCase[j + 1, att1],"=",(aCase[j, att2] == aCase[j + 1, att1])))
            
            if (aCase[j, att2] == aCase[j + 1, att1]) {
              # print(paste(n,"yes"))
              tEqu[, n] = as.character(paste("YES", att2, "->", att1, " : ", aCase[j, att2]))
            }
            else{
              tEqu[, n] = as.character("NO")
            }
          }
          
          else{
            tEqu[, n] = as.character("NO")
          }
          
          #}
          #  index <- index + 1
        }
        
        # print("finish correlation attributes")
        #print (paste( "colnames(tEqu)",colnames(tEqu)))
        #  print(tEqu)
        #  print(c("ActivityPre",
        #         "ActivitySucc",
        #        atts_, additionalNames))
        colnames(tEqu) <-
          c("ActivityPre",
            "ActivitySucc",
            atts_, additionalNames)
        
      }
      else{
        colnames(tEqu) <-
          c("ActivityPre",
            "ActivitySucc",
            atts_)
      }
      #  View(tEqu)
      # print (paste( "colnames(tEqu)",colnames(tEqu)," colnames(transDf)",colnames(transDf)))
      transDf <- rbind(transDf, tEqu)
      #  View(transDf)
    }
    
    # print("finished")
    
    #  View(transDf)
    return(transDf)
    
  }

#### build dataframe that satisfy type 1, 2 ,3,4

buildDFSimilarity2eventsLog<-function(log_){
  cmpTransCasesEi <- cmpfun(buildTransactionDfForDiffCasesAtt)
  cmpTransCasesEiEj <- cmpfun(buildTransactionDfForDiffCasesAttEiEj)
  
  
  eAtt <- colnames(log_)
  eAtt <-
    eAtt[lapply(eAtt, function(x)
      length(grep("Case", x, value = FALSE))) == 0]
  
  eAtt <-
    eAtt[lapply(eAtt, function(x)
      length(grep("Timestamp", x, value = FALSE))) == 0]
  
  eAtt <-
    eAtt[lapply(eAtt, function(x)
      length(grep("Activity", x, value = FALSE))) == 0]
  
  
  compare <- attributesToCompare(log_)
  cases_ <- split(log_ , f = log_$'Case ID')
  
  trans.dataframe <- cmpTransCasesEi(cases_,eAtt,compare)
  trans.dataframe2 <- cmpTransCasesEiEj(cases_,eAtt,compare)
  trans.dataframe <- rbind(trans.dataframe, trans.dataframe2)
  return (trans.dataframe)
  
}




#### BUILD transDF from different cases [ working on ei and ei+1]
### Build DF for option 2 and 6
buildTransactionDfForDiffCasesAtt <- function(log_,eAtt_,compare_) {
  cmpTransCaseAtts <- cmpfun(buildTransactionCaseDfCorrelation)
  
  #  View(log_)
  
#  i = 0
  trans.dataframe <-
    foreach(t = iter(log_, by = 'row'), .combine = rbind) %dopar% {
     #    print(i)
     # i <- i + 1
      #View(t)
      if(nrow(t)>1){
      trans.dataframe <-
        cmpTransCaseAtts(t, eAtt_, compare_)
      }
    }
  
  return(trans.dataframe)
  
}

buildTransactionDfForDiffCasesAttNormalFor <- function(log_,atts_, compare_) {
  trans.dataframe  <-
    data.frame(
      ActivityPre = character(),
      ActivitySucc = character(),
      stringsAsFactors = FALSE
    )
  
  for (j in atts_) {
    trans.dataframe[, j] = character()
  }
  additionalNames <- c()
  ### add the attributes that have common values, i.e., to define correlation between different attributes
  if (length(compare_) > 0) {
    for (j in compare_) {
      n <- paste(j[1], "=", j[2])
      additionalNames <- c(additionalNames, n)
      trans.dataframe[, n] = character()
    }
    
    colnames(trans.dataframe) <-
      c("ActivityPre",
        "ActivitySucc",
        atts_, additionalNames)
    
  }
  else{
    colnames(trans.dataframe) <-
      c("ActivityPre",
        "ActivitySucc",
        atts_)
  }
  
  
   
for (t in log_) {
 if(nrow(t)>1){ 
  tempDF <-
        buildTransactionCaseDfCorrelation(t, atts_, compare_)
      
  rbind(trans.dataframe,tempDF)
 }
    }
  
  return(trans.dataframe)
  
}




buildTransactionDfForDiffCasesAttEiEj <- function(log_,eAtt_,compare_) {
  cmpTransCaseAttsEiEj <- cmpfun(buildTransactionCaseDfCorrelationEiEj)
  
  #  View(log_)
  
  
  trans.dataframe <-
    foreach(t = iter(log_, by = 'row'), .combine = rbind) %dopar% {
      trans.dataframe <-  cmpTransCaseAttsEiEj(t, eAtt_, compare_)
      
    }
  
  return(trans.dataframe)
  
}



### build DF for all the attributes state equal or not and
## search for correlation between attributes
## based on ei and ej
buildTransactionCaseDfCorrelationEiEj <-
  function(aCase, atts_, compare_) {
    ## creating the transcations of the dataframe with all the attributes
    transDf  <-
      data.frame(
        ActivityPre = character(),
        ActivitySucc = character(),
        stringsAsFactors = FALSE
      )
    
    for (j in atts_) {
      transDf[, j] = character()
    }
    additionalNames <- c()
    ### add the attributes that have common values, i.e., to define correlation between different attributes
    if (length(compare_) > 0) {
      for (j in compare_) {
        n <- paste(j[1], "=", j[2])
        additionalNames <- c(additionalNames, n)
        transDf[, n] = character()
      }
      
      colnames(transDf) <-
        c("ActivityPre",
          "ActivitySucc",
          atts_, additionalNames)
      
    }
    else{
      colnames(transDf) <-
        c("ActivityPre",
          "ActivitySucc",
          atts_)
    }
    
    
    colNamesY_ <- colnames(transDf)
    ## loop over events
    #additionalNames <- as.list(additionalNames)
    
    #print(additionalNames)
    ## works for finding asscoition over same attribute between ei and ej
    #  print(aCase)
    for (attr in atts_) {
      temp <- split(aCase, f = aCase[, attr])
      for (dfTemp in temp) {
        #dfTemp <- as (s, "data.frame")
        #  print(dfTemp)
        if (nrow(dfTemp) > 1) {
          for (j in 1:(nrow(dfTemp) - 1))
          {
            #  print(paste("event num",j))
            tEqu <-
              data.frame(
                ActivityPre = as.character(dfTemp[j, ]$Activity),
                ActivitySucc = as.character(dfTemp[j + 1, ]$Activity),
                stringsAsFactors = FALSE
              )
            
            for (i in atts_) {
              #   print(paste(i,dfdfTemp[j, i],"=",dfTemp[j + 1, i]))
              if (is.na(dfTemp[j, i]) & is.na(dfTemp[j + 1, i]))
              {
                tEqu[, i] = as.character("same ")
              }
              else if (is.na(dfTemp[j, i]) || is.na(dfTemp[j + 1, i]))
              {
                tEqu[, i] = as.character("different")
              }
              else if (dfTemp[j, i] == dfTemp[j + 1, i]) {
                tEqu[, i] = as.character(paste("same "))
              }
              else{
                tEqu[, i] = as.character(paste("different"))
              }
              
            }
            
            
            #  print("finish normal attributes")
            #   print(paste("len", length(additionalNames)))
            #   print(paste("len", length(compare_)))
            
            
            
            ## correlation attributes as event id and offer id mslan
            if (length(compare_) > 0) {
              #index <- 1
              for (i in compare_) {
                att1 <- i[[1]]
                att2 <- i[[2]]
                #     print(paste(att1, " ", att2))
                
                n <- paste(att1, "=", att2)
                
                #  print(paste("dfTemp[j, att1], dfTemp[j + 1, att2]", dfTemp[j, att1], dfTemp[j + 1, att2]))
                #   print(paste("dfTemp[j, att2], dfTemp[j + 1, att1]", dfTemp[j, att2] , dfTemp[j + 1, att1]))
                bothEvNa <- 0
                if ((is.na(dfTemp[j, att1]) &
                     is.na(dfTemp[j + 1, att2])) ||
                    (is.na(dfTemp[j, att2]) &
                     is.na(dfTemp[j + 1, att1])))
                {
                  tEqu[, n] = as.character("YES ")#additionalNames[index]
                }
                
                if (!is.na(dfTemp[j, att1]) &&
                    !is.na(dfTemp[j + 1, att2])) {
                  #      print("Not na1")
                  #      print(paste("dfTemp[j, att1] == dfTemp[j + 1, att2]",dfTemp[j, att1], dfTemp[j + 1, att2],"=",(dfTemp[j, att1] == dfTemp[j + 1, att2])))
                  if (dfTemp[j, att1] == dfTemp[j + 1, att2]) {
                    #   print(paste(n,"yes"))
                    tEqu[, n] = as.character(paste("YES", att1, "->", att2))
                  }
                  else{
                    tEqu[, n] = as.character("NO")
                  }
                }
                
                
                else if (!is.na(dfTemp[j, att2]) &&
                         !is.na(dfTemp[j + 1, att1])) {
                  #     print("Not na2")
                  #  print(paste("dfTemp[j, att2] == dfTemp[j + 1, att1]",dfTemp[j, att2], dfTemp[j + 1, att1],"=",(dfTemp[j, att2] == dfTemp[j + 1, att1])))
                  
                  if (dfTemp[j, att2] == dfTemp[j + 1, att1]) {
                    # print(paste(n,"yes"))
                    tEqu[, n] = as.character(paste("YES", att2, "->", att1))
                  }
                  else{
                    tEqu[, n] = as.character("NO")
                  }
                }
                
                else{
                  tEqu[, n] = as.character("NO")
                }
                
                #}
                #  index <- index + 1
              }
              
              # print("finish correlation attributes")
              #print (paste( "colnames(tEqu)",colnames(tEqu)))
              #  print(tEqu)
              #  print(c("ActivityPre",
              #         "ActivitySucc",
              #        atts_, additionalNames))
              colnames(tEqu) <-
                c("ActivityPre",
                  "ActivitySucc",
                  atts_,
                  additionalNames)
              
            }
            else{
              colnames(tEqu) <-
                c("ActivityPre",
                  "ActivitySucc",
                  atts_)
            }
            #  View(tEqu)
            # print (paste( "colnames(tEqu)",colnames(tEqu)," colnames(transDf)",colnames(transDf)))
            transDf <- rbind(transDf, tEqu)
            #  View(transDf)
          }
        }
      }
    }
    
    # print("finished")
    
    #  View(transDf)
    return(transDf)
    
  }





### build DF for all the attributes state equal or not and
## search for correlation between attributes with value
## based on ei and ej
buildTransactionCaseDfCorrelationEiEjValue <-
  function(aCase, atts_, compare_) {
    ## creating the transcations of the dataframe with all the attributes
    transDf  <-
      data.frame(
        ActivityPre = character(),
        ActivitySucc = character(),
        stringsAsFactors = FALSE
      )
    
    for (j in atts_) {
      transDf[, j] = character()
    }
    additionalNames <- c()
    ### add the attributes that have common values, i.e., to define correlation between different attributes
    if (length(compare_) > 0) {
      for (j in compare_) {
        n <- paste(j[1], "=", j[2])
        additionalNames <- c(additionalNames, n)
        transDf[, n] = character()
      }
      
      colnames(transDf) <-
        c("ActivityPre",
          "ActivitySucc",
          atts_, additionalNames)
      
    }
    else{
      colnames(transDf) <-
        c("ActivityPre",
          "ActivitySucc",
          atts_)
    }
    
    
    colNamesY_ <- colnames(transDf)
    ## loop over events
    #additionalNames <- as.list(additionalNames)
    
    #print(additionalNames)
    ## works for finding asscoition over same attribute between ei and ej
    #  print(aCase)
    for (attr in atts_) {
      temp <- split(aCase, f = aCase[, attr])
      for (dfTemp in temp) {
        #dfTemp <- as (s, "data.frame")
        #  print(dfTemp)
        if (nrow(dfTemp) > 1) {
          for (j in 1:(nrow(dfTemp) - 1))
          {
            #  print(paste("event num",j))
            tEqu <-
              data.frame(
                ActivityPre = as.character(dfTemp[j, ]$Activity),
                ActivitySucc = as.character(dfTemp[j + 1, ]$Activity),
                stringsAsFactors = FALSE
              )
            
            for (i in atts_) {
              #   print(paste(i,dfdfTemp[j, i],"=",dfTemp[j + 1, i]))
              if (is.na(dfTemp[j, i]) & is.na(dfTemp[j + 1, i]))
              {
                tEqu[, i] = as.character("same NA")
              }
              else if (is.na(dfTemp[j, i]) || is.na(dfTemp[j + 1, i]))
              {
                tEqu[, i] = as.character("different one is NA")
              }
              else if (dfTemp[j, i] == dfTemp[j + 1, i]) {
                tEqu[, i] = as.character(paste("same :",dfTemp[j, i]))
              }
              else{
                tEqu[, i] = as.character(paste("different :",dfTemp[j, i] ,"!=", dfTemp[j + 1, i]))
              }
              
            }
            
            
            #  print("finish normal attributes")
            #   print(paste("len", length(additionalNames)))
            #   print(paste("len", length(compare_)))
            
            
            
            ## correlation attributes as event id and offer id mslan
            if (length(compare_) > 0) {
              #index <- 1
              for (i in compare_) {
                att1 <- i[[1]]
                att2 <- i[[2]]
                #     print(paste(att1, " ", att2))
                
                n <- paste(att1, "=", att2)
                
                #  print(paste("dfTemp[j, att1], dfTemp[j + 1, att2]", dfTemp[j, att1], dfTemp[j + 1, att2]))
                #   print(paste("dfTemp[j, att2], dfTemp[j + 1, att1]", dfTemp[j, att2] , dfTemp[j + 1, att1]))
                bothEvNa <- 0
                if ((is.na(dfTemp[j, att1]) &
                     is.na(dfTemp[j + 1, att2])) ||
                    (is.na(dfTemp[j, att2]) &
                     is.na(dfTemp[j + 1, att1])))
                {
                  tEqu[, n] = as.character("YES NA")#additionalNames[index]
                }
                
                if (!is.na(dfTemp[j, att1]) &&
                    !is.na(dfTemp[j + 1, att2])) {
                  #      print("Not na1")
                  #      print(paste("dfTemp[j, att1] == dfTemp[j + 1, att2]",dfTemp[j, att1], dfTemp[j + 1, att2],"=",(dfTemp[j, att1] == dfTemp[j + 1, att2])))
                  if (dfTemp[j, att1] == dfTemp[j + 1, att2]) {
                    #   print(paste(n,"yes"))
                    tEqu[, n] = as.character(paste("YES", att1, "->", att2))
                  }
                  else{
                    tEqu[, n] = as.character("NO")
                  }
                }
                
                
                else if (!is.na(dfTemp[j, att2]) &&
                         !is.na(dfTemp[j + 1, att1])) {
                  #     print("Not na2")
                  #  print(paste("dfTemp[j, att2] == dfTemp[j + 1, att1]",dfTemp[j, att2], dfTemp[j + 1, att1],"=",(dfTemp[j, att2] == dfTemp[j + 1, att1])))
                  
                  if (dfTemp[j, att2] == dfTemp[j + 1, att1]) {
                    # print(paste(n,"yes"))
                    tEqu[, n] = as.character(paste("YES", att2, "->", att1, " : ",dfTemp[j, att2]))
                  }
                  else{
                    tEqu[, n] = as.character("NO")
                  }
                }
                
                else{
                  tEqu[, n] = as.character("NO")
                }
                
                #}
                #  index <- index + 1
              }
              
              # print("finish correlation attributes")
              #print (paste( "colnames(tEqu)",colnames(tEqu)))
              #  print(tEqu)
              #  print(c("ActivityPre",
              #         "ActivitySucc",
              #        atts_, additionalNames))
              colnames(tEqu) <-
                c("ActivityPre",
                  "ActivitySucc",
                  atts_,
                  additionalNames)
              
            }
            else{
              colnames(tEqu) <-
                c("ActivityPre",
                  "ActivitySucc",
                  atts_)
            }
            #  View(tEqu)
            # print (paste( "colnames(tEqu)",colnames(tEqu)," colnames(transDf)",colnames(transDf)))
            transDf <- rbind(transDf, tEqu)
            #  View(transDf)
          }
        }
      }
    }
    
    # print("finished")
    
    #  View(transDf)
    return(transDf)
    
  }



defineGeneralEqualityConstraints<-function(transDF){
  i=1
  caseAttributes<-list()
  nam<-names(trans.dataframe)
  for(k in 1:length(transDF)){
    u<-unique(transDF[,k])
    if(length(u)==1 && grepl("same",u[1], ignore.case = TRUE))
    {
      caseAttributes[i]<-nam[k]
      i=i+1
    }
  }
  return(caseAttributes)
}

###This function will remove all the compare columns that are always false.
###Also it will remove all attribute that always different##
cleanTransactionTable<-function(transTable){
  
 # drops = foreach(attr_ = iter(transTable,by='col'),.combine = 'cfun') %dopar% {
  drops = vector()
  for(i in 1:ncol(transTable)){
    u <- as.list(unique(transTable[,i]))
    #print(nrow(u))
    if(length(u) == 1){
    # if(nrow(u)==1){
     # print(u)
      if((grepl("different",u[1],ignore.case = TRUE) || grepl("NO",u[1],ignore.case = TRUE)))
      {
        drops <- append(drops,names(transTable)[i])
        
      }
    # }
    }
    }
  updatedTranstable<- transTable[, !names(transTable) %in% drops]
  return(updatedTranstable)
}

cfun<-function(df,v){

  #View(v)
  if(length(v)>1)
  {
    df <- cbind(v, df) 
   # 1View(df)
   }
  return(df)
}


checkAttribute<-function(attr_){
  u <- unique(attr_)
  if(nrow(u)==1){
    print(u)
    if((grepl("different",u[1],ignore.case = TRUE) || grepl("NO",u[1],ignore.case = TRUE)))
       {return(attr_)}
  }
  return(vector())
}

