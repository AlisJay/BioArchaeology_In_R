# Function Name: Ancestory() 
# Purpose:  To determine ancestory of remains using cranial traits
# Linked Functions: output can be used in Sex()
# Example Use: Ancestry(orbit="angular",nRoot="high",LNB="guttered",Palate="hyperbolic",profile="intermediate",nWidth="wide",BR=TRUE,VS=TRUE,PBD=FALSE,SI="undeterminable",nBridge=FALSE,LEB=TRUE)
######################################################################################################################################################################################

Ancestory<-function(orbit="NA",nRoot="NA",LNB="NA",palate="NA",profile="NA",nWidth="NA",BR="NA",VS="NA",PBD="NA",SI="NA",nBridge="NA",LEB="NA"){
  
#Step1: setting base values for each possible outcome
  Asian<-0;African<-0;European<-0;unknown<-0
 
#Step 2: Trait scoring;
  #part1: multiple choice
  #orbit shape
  if(orbit=="angular"){European<-European+1# if input = angular then add 1 point to the european
  }else{
  if(orbit=="rectangular"){African<-African+1# if input= rectangular add 1 point to the African
  }else{
  if(orbit=="round"){Asian<-Asian+1# if input = round add 1 point to Asian
  }else{
  unknown<-unknown+1# if input is anything other than "angular", "rectangular" or "round" add 1 point to unknown
  }}}
 
 #Nasal Root
  if(nRoot=="high"){European<-European+1
  }else{
  if(nRoot=="low round"){African<-African+1
  }else{
  if(nRoot=="low ridged"){Asian<-Asian+1
  }else{
  unknown<-unknown+1
  }}}
  
  #Lower Nasal Border
  if(LNB=="sharp sill"){European<-European+1
  }else{
  if(LNB=="guttered"){African<-African+1
  }else{
  if(LNB=="flat"){Asian<-Asian+1
  }else{
  unknown<-unknown+1
  }}}
  
  #Palate shape
  if(palate=="parabolic"){European<-European+1
  }else{
  if(palate=="hyperbolic"){African<-African+1
  }else{
  if(palate=="ellipitical"){Asian<-Asian+1
  }else{
  unknown<-unknown+1
  }}}
  
  #Profile
  if(profile=="straight"){European<-European+1
  }else{
  if(profile=="projecting"){African<-African+1
  }else{
  if(profile=="intermediate"){Asian<-Asian+1
  }else{
  unknown<-unknown+1
  }}}
  
  #Nasal Width
  if(nWidth=="narrow"){European<-European+1
  }else{
  if(nWidth=="wide"){African<-African+1
  }else{
  if(nWidth=="intermediate"){Asian<-Asian+1
  }else{
  unknown<-unknown+1
  }}}
  
  #Part 2: specific ancestry traits, TRUE/FALSE inputs
  #Prominant Brow ridge
  if(BR==TRUE){European<-European+1}# if present (i.e. input= TRUE) then add 1 to European
  if(BR=="undeterminable"|BR=="NA"){unknown<-unknown+1}# if the input is undeterminable (if it is not possible to judge presence) add 1 to unknown
  # if the input is FALSE (the trait is definately not present) then nothing happens 
  
  #Complex vault structure
  if(VS==TRUE){Asian<-Asian+1}
  if(VS=="undeterminable"|VS=="NA"){unknown<-unknown+1}
  
  #Post Bregma Depression
  if(PBD==TRUE){African<-African+1}
  if(PBD=="undeterminable"|PBD=="NA"){unknown<-unknown+1}
  
  #Shovel shapped incisors 
  if(SI==TRUE){Asian<-Asian+1}
  if(SI=="undeterminable"|SI=="NA"){unknown<-unknown+1}
  
  #Nasal Bridge
  if(nBridge==TRUE){European<-European+1}
  if(nBridge=="undeterminable"|nBridge=="NA"){unknown<-unknown+1}
  
  #LEB
  if(LEB==TRUE){Asian<-Asian+1}
  if(LEB=="undeterminable"|LEB=="NA"){unknown<-unknown+1}
  
# Step 3: Assessment
  #creating data frames from accumlated scores 
  scores<-data.frame(asian=Asian,african=African,european=European)
  unknown<-data.frame(undeterminable=unknown)
  
  #assessment test
  other<-sum(scores)-max(scores)# then number of scores that are not for the most popular
  # i.e if the European category had the highest number of point this would be to total of the Asian and African scores 
  
  outcome<-if(unknown>8){as.character("unknown")# if there is more than 8 unkown value then an assement can be made so the function returns an unkown result
  }else{
    if(unknown>6 & max(scores)<other){as.character("unknown")# if the unkown value is 7 or 8 and highest scoring category has less than the other two combined then ancestory is also unknown
    }else{
    if(unknown>6){paste("probable",as.character(names(scores)[which.max(scores)]))# if the unkown value is 7 or 8 and the highest scoring cateogory is more than others then it is probably that category
    }else{# if unknown is 6 or less
      if(max(scores)>other){as.character(names(scores)[which.max(scores)])# if the highest scoring cateogory is greater than the other two combined then ancestory is reported as that cateogory
      }else{paste("probable",as.character(names(scores)[which.max(scores)]))}# if the highest scoring cateogory is not greater than the other two combined then ancestory is reported as probably that cateogory
    }
  }} 
 
#Step 4: Warnings 
  w<-NA; w<-if(unknown>0){"Your input was incomplete. The most reliable results come from complete analysis."}
# if there are any unkown values then a warning is created telling the user that complete analysis yeilds the best results

#Step 5: output
  list(scores=cbind(scores,unknown),outcome=outcome,warnings=w)# this is a list containing a table of results, the written assement (eg. "probable European") and the warning
}