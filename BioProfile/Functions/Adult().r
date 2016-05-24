# Function Name: Adult() 
# Purpose:  To estimate the age of adult remains usinf recognized osteological markers 
# Linked Functions: The vault and LA input are assumed to be the output of ComScore(). Sex()$Sex can be used in the sex input
# Example Use: Adult(Sex=Sex(x)$Sex,todd="p1", SucheyBrookes="p1", lovejoy="p2",vault=ComScore(ML,L,O,AS,B),LA= ComScore(P,MC,SF,IST,SST))
############################################################################################

Adult<-function(sex,todd, SucheyBrookes,lovejoy,vault,LA){
 
 # Step 1: create blank a data frame of results to be filled through the course of the function
  results<-data.frame(trait=c("Todd","Suchey-Brookes","Lovejoy","Vault","Lateral-Anterior"),
                      phase=c(NA,NA,NA,NA,NA),
                      min=c(NA,NA,NA,NA,NA),
                      max=c(NA,NA,NA,NA,NA),
                      average=c(NA,NA,NA,NA,NA))

  #Step 2: constructing reference data frames, these are used to look up ages they are taken from Buikstra & Uberlaker (94) and Byers(10)
  # cranial suture scores(vault) 
  cvault<-data.frame(composite=c(1:20,"NaN"),# composite to be compared to the score entered as input into the function  
                     s=c("s1","s1","s2","s2","s2","s2","s3","s3","s3","s3","s3","s4","s4","s4","s4","s5","s5","s5","s6","s6",NA),# will become phase in results data frame
                     min=c(19,19,23,23,23,23,28,28,28,28,28,31,31,31,31,35,35,35,34,34,NA),
                     max=c(44,44,45,45,45,45,44,44,44,44,44,61,61,61,61,60,60,60,61,61,NA),
                     average=c(31,31,34,34,34,34,38,38,38,38,38,44,44,44,44,49,49,49,52,52,NA))
  #cranial suture scores (Lateral-aneterior)
  cLA<-data.frame(composite=c(1:14,"NaN"),# NaN = potential output of the ComScore function
                  s=c("s1","s2","s3","s3","s3","s4","s5","s5","s6","s6","s7","s7","s7","s7",NA),
                  min=c(20,29,28,28,28,31,35,35,39,39,49,49,49,49,NA),
                  max=c(41,44,51,51,51,54,56,56,61,61,61,61,61,61,NA),
                  average=c(32,37,40,40,40,43,46,46,52,52,55,55,55,55,NA))
  # Todd pubic synthesis score
  Todd<-data.frame(phase=c("NA","p1","p2","p3","p4","p5","p6","p7","p8","p9","p10"),
                   min=c(NA,18,20,22,25,27,30,35,40,45,50),
                   max=c(NA,19,21,24,26,30,35,39,45,49,61),
                   average=c(NA,18.5,20.5,23,25.5,28.5,32.5,37,42.5,47,61))
  # Suchey-Brookes auricular surface 
  SB<-data.frame(phase=c("NA","p1","p2","p3","p4","p5","p6"),
                 minF=c(NA,15,19.5,21,26,26.5,42),# female min age
                 maxF=c(NA,24,40,53,61,61,61),# female max age 
                 averageF=c(NA,18.5,25.5,32,38,49,59),# female average 
                 minM=c(NA,15,19,21,23,27,34),
                 maxM=c(NA,28,34,44,58,61,61),
                 averageM=c(NA,18.5,24,28,35,45,60))
  # Lovejoy auricular surface 
  Lovejoy<-data.frame(phase<-c("NA","p1","p2","p3","p4","p5","p6","p7","p8"),
                      min=c(NA,20,25,30,35,40,45,50,60),
                      max=c(NA,24,29,34,39,44,49,59,61),
                      average=c(NA,22,27,32,37,42,47,54.5,61))
  
  #Step 3:filling the  results data.frame based on input 
  #Todd 
  results[,-1][results$trait=="Todd",]<-Todd[Todd$phase==todd,]
  # the row in the results frame where the trait column is equal to Todd (minus the trait column) is filled with the data from the Todd reference table where the phase matches the input
                     results[1,2]<-todd# the phase column in the Todd row of results becomes equal to the inputted value
  
  #Lovejoy 
  results[,-1][results$trait=="Lovejoy",]<-Lovejoy[Lovejoy$phase==lovejoy,]
                     results[3,2]<-lovejoy
  
  #Vault
  if(is.na(vault$score)){# if the vault score entered is NA (or missing)
    results[,-1][results$trait=="Vault",]<-c(NA,NA,NA,NA)# fill with NA values 
    results[4,2]<-NA
  }else{# if not NA
    results[,-1][results$trait=="Vault",]<-cvault[cvault$composite==vault$score,][-1]
    results[4,2]<-as.character(cvault[cvault$composite==vault$score,2])
  }
   
  #LA 
  if(is.na(LA$score)){
    results[,-1][results$trait=="Lateral-Anterior",]<-c(NA,NA,NA,NA)
    results[5,2]<-NA
  }else{
    results[,-1][results$trait=="Lateral-Anterior",]<-cLA[cLA$composite==LA$score,][-1]
    results[5,2]<-as.character(cLA[cLA$composite==LA$score,2])
  }

  #Suchey-Brookes
  if(sex=="female"|sex=="possible female"){# if inputed sex is female 
    results[,-1][results$trait=="Suchey-Brookes",]<-SB[SB$phase==SucheyBrookes,c(1,2,3,4)]# takes data from minF, maxF and averageF columns
    results[2,2]<-SucheyBrookes
  }else{
	if(sex=="male"|sex=="possible male"){# if male
      results[,-1][results$trait=="Suchey-Brookes",]<-SB[SB$phase==SucheyBrookes,c(1,5,6,7)]# takes data from, minM, maxM and averageM columns 
      results[2,2]<-SucheyBrookes
	}else{# if not male or female (i.e unknown sex)
      results[,-1][results$trait=="Suchey-Brookes",]<-c(NA,NA,NA,NA)# can't calculate sucheyBrookes score without knowing sex so returns NA values 
      results[2,2]<-NA
    }
  }

  #Step 4: NA's and warnings
  w1<-if(vault$na>0|LA$na>0){paste("you are missing ",as.character(sum(vault$na,LA$na))," suture closure scores. The composite score uses the average of recorded scores to estimate missing values. Therefore the ",if(LA$na>0)"Lateral-Anterior",if(vault$na>0 & LA$na>0)" and ",if(vault$na>0) "vault ","age range",if(vault$na>0 & LA$na>0)"s", "will be less accurate")}
  # warning 1 is created if there were any missing values in the calculation of the composite scores (see ComScore())
  #it wil tell the user that the result will be less accurate and tell the user what value is effected (i.e the vault, the lateral anterior or both)
  
  w2<-if(anyNA(results)){paste("best results come from complete analysis.You are missing: ",if(anyNA(results[results$trait=="Suchey-Brookes"]))"Suchey-Brookes ", if(anyNA(results[results$trait=="Todd"]))"Todd ",if(anyNA(results[results$trait=="Lovejoy"]))"Lovejoy ",if(anyNA(results[results$trait=="Vault"]))"Vault ",if(anyNA(results[results$trait=="Lateral-Anterior"]))"Lateral-Anterior ")}
  # warning 2 is created if there are any missing values in the results dataframe after it's been filled. 
  # It will tell the user that better results come from complete analysis and give the user the missing fields
  
  #Step 5: calculate age ranges (total and average)
  tRange<-paste(as.character(min(results$min,na.rm=TRUE))," - ",as.character(max(results$max,na.rm=TRUE)))# this give the user the maximum possible range (i.e the lowest minimun age and the highest maximum age)
  aRange<-paste(as.character(min(results$average,na.rm=TRUE))," - ",as.character(max(results$average,na.rm=TRUE)))# this gives the range of the average values   
  
  #Step 6: create the output
  output<-list(TotalRange=tRange,AverageRange=aRange,Table=results,Warnings=c(w1,w2))# this is a list containing the two ranges the filled in results dataframe and the warnings
  output
}