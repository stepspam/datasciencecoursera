


rankall <- function(outcome,num="best") {
  
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
  
  ## ranking
  outcome_all <- outcome_all[ order(outcome_all[1]),]
  outcome_all <- outcome_all[ order(outcome_all[3]),]
  outcome_all <- outcome_all[ order(outcome_all[2]),]
  #splits the column per state
  outcome_split <- split(outcome_all,outcome_all$State)
  #outcome_all <- mapply(cbind, outcome_all, "SampleID"=seq.int(nrow(outcome_all)), SIMPLIFY=F)
  
  #add ranking to all list items
  outcome_split<-lapply(outcome_split, function(x) { rl <- seq.int(nrow(x))
                            x <- cbind( x, rl=rl) })
  #get the right ranking number
  if(num=="best") { 
    num<-rep(1,length(unique(outcome_all$State)))
     
  } else if (num=="worst") {
    num<-sapply(outcome_split,function(x) {x<-length(x[,3])} )
  } else { num <-num<-rep(num,length(unique(outcome_all$State)))}
  
  
  ##return dataframe
  y <- data.frame(cbind(rep(NA,length(outcome_split)),rep(NA,length(outcome_split))))
  colnames(y) <- c("Hospital", "State")
#  z <- rep(NA,length(outcome_split))
  for (i in 1:length(outcome_split) ) {
    if (is.na(outcome_split[[i]][num[i],1])){
        y[i,1] <- NA
        y[i,2] <- outcome_split[[i]][1,2]
    }else{
        y[i,] <- outcome_split[[i]][num[i],c(1,2)]
    }
 #   z[i] <- outcome_split[[i]][num[i],2]
  }
  
  #return
  y
}
