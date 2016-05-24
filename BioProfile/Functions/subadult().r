# Function Name: subadult() 
# Purpose:to determine adult from sub adult using epiphyseal closure, and provide an estimated age range for subadults
# Linked Functions: used as input for sex()
# Example Use: ComScore(P,MC,SF,IST,SST) 
######################################################################################################################################################################################

subadult<-function(epiphysis){# epiphysis is a character vector containing the names of any open epiphyses
  # Step 1: Creating reference data frame containing known closure ranges for each epiphysis 
  ossification<-data.frame(epiphysis=c("pRadius","dFibula","dTibia","dFemur","pFibula","acromion","iliac","hHead","fHead","lTrochanter","pTibia","gTrochanter","dradius","s3s5","s2s3","s1s2","clavicle","SOS"),
                           max=c(18.5,19.5,19.5,21.0,21.0,21.5,21.5,23.0,19.5,19.5,22.0,19.5,22.0,23.5,24.5,32.0,30.0,25.5),
                           min=c(14.5,14.5,14.5,14.5,14.5,14.5,14.5,14.5,15.5,15.5,15.5,17.0,17.0,17.0,17.5,17.5,18.5,19.5),
                           fullname=c("proximal radius","distal fibula", "distal Tibia","distal femur","proximal fibula", "acromion process","illiac crest","humeral head", "femoral head", "lesser trochanter",
                                      "proximal Tibia","greater trochnater","distal radius","sacrum s3-s5","sacrum s3-s2","sacrum s1-s2","sternal end of clavicle","spheno-occipital synchondrosis"))
  
  #Step2: setting value of sacrum and base value of  message 
  sacrum<-"s1s2"
  Message<-NULL
  
  #Step 3: assesment of input
  # if epiphyses is empty (i.e all are closed)
  if(is.null(epiphysis)){age<-"Adult";range<-NULL;Message<-"no epiphyses open so remains are adult"
  }else{
	if(identical(epiphysis,sacrum)){# if only s1s2 is selected (only unfused epiphysis)
      Message<-"non fusion of s1 and s2 is very common if no other epiphyses are open the remains are likely adult" # unique message 
      age<-"Adult"
	}else{# if they are more epiphyses open
      age<-"SubAdult" # set age 
	  #getting data from reference 
	  max<-ossification$max[ossification$epiphysis %in% epiphysis]# maximum age at which epiphyses that are open close
      min<-ossification$min[!(ossification$epiphysis %in% epiphysis)]# minimum ages at which the epiphyses that are closed close
      maxEp<-ossification$fullname[ossification$max==min(max)]# name of the epiphysis that has the lowest max age
      
	  #Step4: creating range
	  range<-paste(as.character(max(min))," - ",as.character(min(max)))
      # the range goes from the highest minimum closing age of the epiphyses that are closed to the lowest maxium age for the epiphyses that are open
	  
	  #Step 5: warning if min is larger than max 
	  Message<-if(min(max)<max(min)){paste("maximun is larger than minimum, this could be congential nonfusion please untick ", maxEp, " and try again")}
	  # for potential congential non-fusion issues 
    }}
  
  #Step 6:output 
  list(age=age,range=range,Mesage=Message# list of charcter vector (either "Adult" or "SubAdult), the range and ny warnings 
}
