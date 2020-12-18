


rankhospital <- function(state,outcome,num) {
  
  #read dim_table to find state of hospital.
  #dim_hospital <- read.csv("hospital-data.csv")
  #dim_hospital <- dim_hospital[c("Hospital.Name","State")]
  
  ## Read outcome data
  outcome_all <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #filter the relevantcolumns
  outcome_all <- outcome_all[c(2,7,11,17,23)]
  colnames(outcome_all)[c(3,4,5)] <- c("heart attack","heart failure", "pneumonia")
  
  ## Check that function inputs: state and outcome are valid
  if (!(outcome %in% c("heart attack","heart failure", "pneumonia"))) stop("Invalid outcome entered")
  if (!(state %in% outcome_all$State)) stop("Invalid State entered")
  
  #filter the state column
  outcome_all <- outcome_all[outcome_all$State==state,]
  
  #only keep the right outcome column
  if(outcome=="heart attack"){
    outcome_all <- outcome_all[c(1,2,3)]
  }else if(outcome=="heart failure") {
    outcome_all <- outcome_all[c(1,2,4)]
  }else{
    outcome_all <- outcome_all[c(1,2,5)]
  }
  #make numeric & exclude NA.
  suppressWarnings(
  outcome_all[,3] <- as.numeric(outcome_all[,3])
  )
  outcome_all <- na.omit(outcome_all)
  #outcome_all <- outcome_all[order(outcome_all[,3]),]
  
  #get the right ranking number
  if(num=="best") { 
    num<-1
  } else if (num=="worst") {
    num<-length(outcome_all[,3])
  } else if (num>length(outcome_all[,3])) { 
    outcome_all<-NA
    print(outcome_all)
    stop("Cannot proceed: num > amount of hospitals in list")
  } else { num <-num}
  

  
  ## ranking
  outcome_all <- outcome_all[ order(outcome_all[1]),]
  outcome_all <- outcome_all[ order(outcome_all[3]),]
  outcome_all$Rank <- seq.int(nrow(outcome_all))
  
  #return 
  outcome_all[num,1]
  
  
}
