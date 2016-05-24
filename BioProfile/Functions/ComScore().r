# Function Name: ComScore() 
# Purpose: To calculate composite scores for suture closure
# Linked Functions: output used in Adult()
# Example Use: ComScore(P,MC,SF,IST,SST) 
######################################################################################################################################################################################

ComScore<-function(A,B,C,D,E){# each of the inputs are either NA,0,1,2 or 3
  #Step 1: combine input and convert to numeric (input from App is character vector)
  V<-c(A,B,C,D,E)
  V<-as.numeric(V)
  
  #Step2:count number of NAs (missing values)
  na<-length(is.na(V)[is.na(V)==TRUE])
  
  #Step3: add together non nas
  composite<-sum(V,na.rm=TRUE)
  
  if(na==0){composite<-composite# if their are no missing values then the composite score is just the sum of the input
  }else{
	#if don't have all required values (i.e there are NAs) then missing values are estimated using mean of recorded values
	composite<-round(composite+(mean(V,na.rm=TRUE)*na),0)
    # the composite score becomes the sum of the enter values plus the estimated score times the number of missing values
  }
  
  #Step 4: output 
  list(na=na,score=composite)# a list giving the number of NAs and the composite score
}