buildModel <- function(start_ind, end_ind, start_dep, end_dep, evaluate=TRUE){
  if(!require("randomForest")){
    install.packages("randomForest")
    require("randomForest")
  }
  if(!require("leaps")){
    install.packages("leaps")
    require("leaps")
  }
  if(!require("onehot")){
    install.packages("onehot")
    require("onehot")
  }
  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }
  if(!require("lubridate")){
    install.packages("lubridate")
    require("lubridate")
  }
  if(!require("rpart")){
    install.packages("rpart")
    require("rpart")
  }
  if(!require("pROC")){
    install.packages("pROC")
    require("pROC")
  }
  if(!require("lift")){
    install.packages("lift")
    require("lift")
  }
  
  cat("Reading in the data:")
  time <- Sys.time()
  options(warn=-1)
  
  #read in all data tables
  complaints <- read.table("http://ballings.co/hidden/aCRM/data/chapter6/complaints.txt", sep = ";", header = TRUE)
  credit <- read.table("http://ballings.co/hidden/aCRM/data/chapter6/credit.txt", sep = ";", header = TRUE)
  customers <- read.table("http://ballings.co/hidden/aCRM/data/chapter6/customers.txt", sep = ";", header = TRUE)
  delivery <- read.table("http://ballings.co/hidden/aCRM/data/chapter6/delivery.txt", sep = ";", header = TRUE)
  formula <- read.table("http://ballings.co/hidden/aCRM/data/chapter6/formula.txt", sep = ";", header = TRUE)
  subscriptions <- read.table("http://ballings.co/hidden/aCRM/data/chapter6/subscriptions.txt", sep = ";", header = TRUE)
  cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
      attr(Sys.time()- time,"units"), "\n")
  
  cat("Preparing the data:")
  time <- Sys.time()
  
  subscriptions$StartDate <- dmy(subscriptions$StartDate)
  subscriptions$EndDate <- dmy(subscriptions$EndDate)
  subscriptions$RenewalDate <- dmy(subscriptions$RenewalDate)
  
  #time interval calculation
  subscriptions$t4 <- end_dep
  subscriptions$t3 <- start_dep
  subscriptions$t2 <- end_ind
  subscriptions$t1 <- start_ind
  
  
  #we only want subscriptions from this time interval
  custIDs <- unique(subscriptions[which(!(subscriptions$StartDate <= subscriptions$t2 & subscriptions$EndDate >= subscriptions$t3)),2])
  subscriptions <- subset(subscriptions, StartDate <= t2 & EndDate >= t3)
  
  
  #merged the above two
  subForm <- merge(subscriptions,formula, all.x = TRUE, by = "FormulaID")
  
  #format dates using lubridate package
  delivery$StartDate <- dmy(delivery$StartDate)
  delivery$EndDate <- dmy(delivery$EndDate)
  delivery$Duration <- as.numeric(delivery$EndDate - delivery$StartDate)
  
  #one hot encoding
  deliveryOneHot <- onehot(delivery)
  deliveryOneHot <- predict(deliveryOneHot, delivery)
  deliveryOneHot <- deliveryOneHot[,-c(16:18)]
  deliveryOneHot <- as.data.frame(deliveryOneHot)
  deliveryOneHot <- merge(delivery, deliveryOneHot, all.x = TRUE, by = c("DeliveryID","SubscriptionID"))
  deliveryOneHot <- deliveryOneHot[,-c(3:5)]
  
  
  names(deliveryOneHot)[6:18] <- c("TypeDI","TypeDR","TypeMD","ClassBlank","ClassABN","ClassNOR","ContextBlank",
                                   "ContextACH","ContextNPA","ContextOTH","ContextPCH","ContextREN","ContextVAC")
  
  
  #aggregate delivery information on subscription
  deliveryAgg <- deliveryOneHot %>%     
    group_by(SubscriptionID) %>% 
    summarize(numDelChanges = n(), 
              numDI = sum(TypeDI),
              numDR = sum(TypeDR),
              numMD = sum(TypeMD),
              numClassBlank = sum(ClassBlank),
              numABN = sum(ClassABN),
              numNOR = sum(ClassNOR),
              numContextBlank = sum(ContextBlank),
              numACH = sum(ContextACH),
              numNPA = sum(ContextNPA),
              numOTH = sum(ContextOTH),
              numPCH = sum(ContextPCH),
              numREN = sum(ContextREN),
              numVAC = sum(ContextVAC),
              maxDur = max(Duration),
              earliestStart = min(StartDate),
              latestEnd = max(EndDate))
  
  #merge delivery and subscription on subscription ID
  DelSubForm <- merge(subForm, deliveryAgg, all.x = TRUE, by = "SubscriptionID")
  
  
  #one hot encode and aggregate credit
  credit <- credit[,-4]
  creditOneHot <- onehot(credit)
  creditOneHot <- predict(creditOneHot, credit)
  creditOneHot <- as.data.frame(creditOneHot)
  
  creditAgg <- creditOneHot %>%
    group_by(SubscriptionID) %>%
    summarize(numCreds = n(),
              TotalCredAmt = sum(Amount),
              numCC = sum(`ActionType=CC`),
              numEN = sum(`ActionType=EN`),
              numPO = sum(`ActionType=PO`),
              numCOM = sum(`CreditSource=COM`),
              numSUB = sum(`CreditSource=SUB`),
              numTRA = sum(`CreditSource=TRA`))
  
  
  #merging credit and the current working table
  DelSubFormCred <- merge(DelSubForm, creditAgg, all.x = TRUE, by = "SubscriptionID")
  
  
  DelSubFormCred$numCreds <- ifelse(is.na(DelSubFormCred$numCreds),0,DelSubFormCred$numCreds)
  DelSubFormCred$TotalCredAmt <- ifelse(is.na(DelSubFormCred$TotalCredAmt),0,DelSubFormCred$TotalCredAmt)
  DelSubFormCred$numCC <- ifelse(is.na(DelSubFormCred$numCC),0,DelSubFormCred$numCC)
  DelSubFormCred$numEN <- ifelse(is.na(DelSubFormCred$numEN),0,DelSubFormCred$numEN)
  DelSubFormCred$numPO <- ifelse(is.na(DelSubFormCred$numPO),0,DelSubFormCred$numPO)
  DelSubFormCred$numCOM <- ifelse(is.na(DelSubFormCred$numCOM),0,DelSubFormCred$numCOM)
  DelSubFormCred$numSUB <- ifelse(is.na(DelSubFormCred$numSUB),0,DelSubFormCred$numSUB)
  DelSubFormCred$numTRA <- ifelse(is.na(DelSubFormCred$numTRA),0,DelSubFormCred$numTRA)
  
  
  #aggregate complaints on Customer ID
  complaints$ComplaintType <- as.factor(complaints$ComplaintType)
  complaints$SolutionType <- as.factor(complaints$SolutionType)
  complaints$FeedbackType <- as.factor(complaints$FeedbackType)
  complaints$ProductID <- as.factor(complaints$ProductID)
  complaints$ComplaintDate <- dmy(complaints$ComplaintDate)
  
  
  #one hot encoding and aggregating complaints on customers
  complaintsOneHot <- onehot(complaints[,-4])
  complaintsOneHot <- predict(complaintsOneHot, complaints)
  complaintsOneHot <- as.data.frame(complaintsOneHot)
  complaintsOneHot$ComplaintDate <- complaints[,4]
  
  compl <- complaintsOneHot %>%
    group_by(CustomerID) %>%
    summarize(numComps = n(),
              numProdID1 = sum(`ProductID=1`),
              numProdID2 = sum(`ProductID=2`),
              numProdID3 = sum(`ProductID=3`),
              numProdID4 = sum(`ProductID=4`),
              numProdID5 = sum(`ProductID=5`),
              numProdID6 = sum(`ProductID=6`),
              numProdID7 = sum(`ProductID=7`),
              numProdID8 = sum(`ProductID=8`),
              numCompType1 = sum(`ComplaintType=1`),
              numCompType2 = sum(`ComplaintType=2`),
              numCompType3 = sum(`ComplaintType=3`),
              numCompType4 = sum(`ComplaintType=4`),
              numCompType5 = sum(`ComplaintType=5`),
              numCompType6 = sum(`ComplaintType=6`),
              numCompType7 = sum(`ComplaintType=7`),
              numCompType8 = sum(`ComplaintType=8`),
              numCompType9 = sum(`ComplaintType=9`),
              numSolType1 = sum(`SolutionType=1`),
              numSolType2 = sum(`SolutionType=2`),
              numSolType3 = sum(`SolutionType=3`),
              numSolType4 = sum(`SolutionType=4`),
              numFeedbackType1 = sum(`FeedbackType=1`),
              numFeedbackType2 = sum(`FeedbackType=2`),
              numFeedbackType3 = sum(`FeedbackType=3`),
              numFeedbackType4 = sum(`FeedbackType=4`),
              numFeedbackType5 = sum(`FeedbackType=5`),
              numFeedbackType6 = sum(`FeedbackType=6`),
              mostRecentComp = max(ComplaintDate))
  
  
  #merge complaints and customers
  custCompl <- merge(customers, compl, by = "CustomerID",all.x = TRUE)
  
  
  #we only want subscriptions from this time interval
  latestDate <- max(DelSubFormCred$EndDate)
  DelSubFormCred$timeSinceLastRenew <- latestDate - DelSubFormCred$RenewalDate
  
  #aggregating on customer
  subAgg <- DelSubFormCred %>%
    group_by(CustomerID) %>%
    summarize(numSubscriptions = n(),
              earliestStart = min(StartDate),
              latestStart = max(StartDate),
              latestEnd = max(EndDate),
              latestRenewal = max(RenewalDate),
              numNewspapers = sum(NbrNewspapers),
              avgNbrStart = mean(NbrStart),
              timeSinceLastRenew = max(timeSinceLastRenew),
              totFormulaPrice = sum(GrossFormulaPrice),
              totNetFormulaPrice = sum(NetFormulaPrice),
              totNetNewspaperPrice = sum(NetNewspaperPrice),
              numProdDiscount = sum(ProductDiscount),
              totDiscount = sum(TotalDiscount),
              totPrice = sum(TotalPrice),
              totCredit = sum(TotalCredit),
              numDelChanges = sum(numDelChanges),
              numAbnormal = sum(numABN),
              maxDur = max(maxDur),
              numCreds = sum(numCreds),
              totCreditAmt = sum(TotalCredAmt))
  
  
  #merging to create the final base table
  finalBase <- merge(customers, subAgg, by = "CustomerID",all.y = TRUE)
  #removing unecessary datasets
  rm(compl);rm(complaints);rm(complaintsOneHot);rm(credit);rm(creditAgg)
  rm(creditOneHot);rm(custCompl);rm(customers);rm(delivery);rm(deliveryAgg)
  rm(deliveryOneHot);rm(DelSubForm);rm(DelSubFormCred);rm(formula);rm(subAgg)
  rm(subForm);rm(subscriptions)
  
  finalBase$DOB <- dmy(finalBase$DOB)
  
  #we defined churn as someone who hadn't renewed their subscription within ~3 months of the last date in dataset
  #3/2/11
  finalBase$churn <- ifelse((finalBase$timeSinceLastRenew>100 | is.na(finalBase$timeSinceLastRenew)), 1, 0)
  
  levels(finalBase$Gender)[1] <- "Unknown"
  
  finalBase$DOB[which(finalBase$DOB=="2222-01-01")] <- NA
  finalBase$DOB[which(finalBase$DOB=="1900-01-01")] <- NA
  finalBase$age <- as.numeric(latestDate - finalBase$DOB)/365
  
  #making a categorical variable out of age
  for(i in 1:nrow(finalBase)){
    if(is.na(finalBase$age[i])){ finalBase$ageCat[i] <- "Unknown" }
    else if(finalBase$age[i] <= 40){ finalBase$ageCat[i] <- "Youngsters" }
    else if(finalBase$age[i] > 40 & finalBase$age[i] <= 60){ finalBase$ageCat[i] <- "MiddleAge"}
    else if(finalBase$age[i] > 60 & finalBase$age[i] <= 80){ finalBase$ageCat[i] <- "Oldish" }
    else if(finalBase$age[i] > 80){ finalBase$ageCat[i] <- "Oldest" }
  }
  
  
  finalBase <- finalBase[,-c(1,3,4,6,11,14,28)]
  finalBase <- finalBase[,-c(4,5,6,9,13)] # removed earliestStart, latestStart, totFormulaPrice, totDiscount
  
  cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
      attr(Sys.time()- time,"units"), "\n")
  
  if (evaluate==TRUE){
    cat("Creating model:")
    time <- Sys.time()
    
    set.seed(1)
    train_ind <- sample(1:nrow(finalBase), nrow(finalBase)*.70)
    
    train <<- finalBase[train_ind,]
    test <<- finalBase[-train_ind,]
    
    train$ageCat <<- as.factor(train$ageCat)
    test$ageCat <<- as.factor(test$ageCat)
    
    rf <- randomForest(churn ~ ., data = train)
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    
  }
  return(rf)
  
}


predictChurn <- function(object=rf){
  preds <- predict(rf, newdata = test)
  return(paste0("auc is: ", auc(test$churn,preds), " || Top Decile Lift is ", 
                TopDecileLift(preds, test$churn)))
}

rf = buildModel("2006-01-02","2010-02-23","2010-02-24","2011-02-24",evaluate = T)
predictChurn(rf)