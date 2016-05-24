#Functions to calculate the Health Index
#HealthIndex() function for individual populations 
# MultiHI() for dataframes with mulitple populations 
#####################################################################################
HealthIndex<-function(Pop){# pop= dataframe with Info for a single population
#source functions/ data
source('HIfunctions.R')# custom functions 
Maresh<-read.table("~data/Maresh.txt",header=TRUE)# Maresh stature data 

#adjusting input dataframe
PopA<-Pop[!(is.na(Pop$Age)),]#rm observations without age
PopA[,39:45]<-apply(PopA[,39:45],2,FUN=Fact2Num)# change trauma field to factors

#set up empty dataframes to fill
D1<-data.frame(Individual=PopA$ID,Age=NA,B2D=NA,Ten=NA,Stature=NA,Hypoplasia=NA,Anemia=NA,DentalHealth=NA,Infections=NA,DJD=NA,Trauma=NA)
D2<-data.frame(Trait=c("Stature","Hypoplasia","Anemia","DentalHealth","Infections","DJD","Trauma"),contributors=NA,Age0_4=NA,Age5_14=NA,Age15_24=NA,Age25_34=NA,Age35_44=NA,Age45=NA)
HI<-data.frame(Pop=PopA$Site[1],num=length(PopA$Age),HI=NA,QALY=NA,lifeEx=NA,Pyl=NA,Stature=NA,Hypoplasia=NA,Anemia=NA,Dental=NA,Infection=NA,DJD=NA,Trauma=NA)

#filling D1: Individual scores 
for(i in 1:length(D1$Individual)){# cycles through every indidual 
  #Age
  D1$Age[D1$Individual==PopA$ID[i]]<-round(LovejoyAdj(PopA$Age[i]))
  
  #B2D
  if(D1$Age[i]%in% 0:4){D1$B2D[i]<-1}
  if(D1$Age[i]%in% 5:14){D1$B2D[i]<-2}
  if(D1$Age[i]%in% 15:24){D1$B2D[i]<-3}
  if(D1$Age[i]%in% 25:34){D1$B2D[i]<-4}
  if(D1$Age[i]%in% 35:44){D1$B2D[i]<-5}
  if(D1$Age[i]>= 45){D1$B2D[i]<-6}
  
  #Ten
  if((D1$Age[i]<=4)& (D1$Age[i]-10<=4)){D1$Ten[i]<-01}
  if((D1$Age[i]%in% 5:14)& ((D1$Age[i]-10)<=4)){D1$Ten[i]<-12}
  if((D1$Age[i]%in% 15:24)& ((D1$Age[i]-10)%in% 5:14)){D1$Ten[i]<-23}
  if((D1$Age[i]%in% 25:34)& ((D1$Age[i]-10)%in% 15:24)){D1$Ten[i]<-34}
  if((D1$Age[i]%in% 35:44)& ((D1$Age[i]-10)%in% 25:34)){D1$Ten[i]<-45}
  if((D1$Age[i]>= 45)& ((D1$Age[i]-10)%in% 35:44)){D1$Ten[i]<-56}
  if((D1$Age[i]>=45)& ((D1$Age[i]-10)>=45)){D1$Ten[i]<-60}
  
  #Stature
  if(PopA$Age[i]>=18){D1$Stature[D1$Individual==PopA$ID[i]]<-Stature(PopA$FLEN[i],PopA$Age[i],PopA$Sex[i],Maresh)# if 18 or older use femur length
  }else{D1$Stature[D1$Individual==PopA$ID[i]]<-Stature(PopA$FDIAP[i],PopA$Age[i],PopA$Sex[i],Maresh)}# if under 18 use femur diaphysis length
  
  #hypoplasia
  x<-c(Cat2Percent(PopA$LDI[i],3),Cat2Percent(PopA$LDC[i],3), Cat2Percent(PopA$LPI[i],3),Cat2Percent(PopA$LPC[i],3))# calculate score for all measures
  if(sum(is.na(x))==length(x)){D1$Hypoplasia[D1$Individual==PopA$ID[i]]<-NA# if all NA
  }else{D1$Hypoplasia[D1$Individual==PopA$ID[i]]<-min(x,na.rm=TRUE)}# select lowest score 
  
  #Anemia
  x<-c(Cat2Percent(PopA$CROB[i],3),Cat2Percent(PopA$PORHY[i],2))
  if(sum(is.na(x))==length(x)){D1$Anemia[D1$Individual==PopA$ID[i]]<-NA
  }else{D1$Anemia[D1$Individual==PopA$ID[i]]<-min(x,na.rm=TRUE)}
  
  #DentalHealth
  D1$DentalHealth[D1$Individual==PopA$ID[i]]<-DentalHealth(PopA$SUMTET[i],PopA$SUMPRE[i],PopA$SUMCAV[i],PopA$SUMABS[i])
  
  #Infections
  x<-c(Cat2Percent(PopA$SKELINF[i],3),Cat2Percent(PopA$TIBINF[i],4))
  if(sum(is.na(x))==length(x)){D1$Infections[D1$Individual==PopA$ID[i]]<-NA
  }else{D1$Infections[D1$Individual==PopA$ID[i]]<-min(x,na.rm=TRUE)}
  
  #DJD
  x<-c(Cat2Percent(PopA$DJSH[i],5),Cat2Percent(PopA$DJHK[i],5),Cat2Percent(PopA$DJCER[i],4),Cat2Percent(PopA$DJTHO[i],4),Cat2Percent(PopA$DJLUM[i],4),Cat2Percent(PopA$DJTMJ[i],2),Cat2Percent(PopA$DJWR[i],2),Cat2Percent(PopA$DJHAN[i],2))
  if(sum(is.na(x))==length(x)){D1$DJD[D1$Individual==PopA$ID[i]]<-NA
  }else{D1$DJD[D1$Individual==PopA$ID[i]]<-min(x,na.rm=TRUE)}
  
  #Trauma
  if(PopA$TRARM[i]>1|PopA$TRLEG[i]>1|PopA$TRNAS[i]>1|PopA$TRFAC[i]>1|PopA$TRSKUL[i]>1|PopA$TRHAN[i]>1|PopA$TRWEAP[i]>1){# if any trauma score 0
  D1$Trauma[D1$Individual==PopA$ID[i]]<-0
  }else{
    if(sum(c(PopA$TRARM[i],PopA$TRLEG[i],PopA$TRNAS[i],PopA$TRFAC[i],PopA$TRSKUL[i],PopA$TRHAN[i],PopA$TRWEAP[i]),na.rm=TRUE)==0){#if all unrecorded
      D1$Trauma[D1$Individual==PopA$ID[i]]<-NA
    }else{D1$Trauma[D1$Individual==PopA$ID[i]]<-100}}# otherwise trauma =100
}

#filling in D2: age specfic attribute scores 
D2<-B2DTotal(D1,D2,D2row=1,D1column=5)# stature
D2<-B2DTotal(D1,D2,D2row=2,D1column=6)# Hypoplasia
D2<-B2DTotal(D1,D2,D2row=3,D1column=7)# anemia
D2<-TenTotal(D1,D2,D2row=5,D1column=9)# infection
D2<-TenTotal(D1,D2,D2row=7,D1column=11)# Trauma
D2<-TenTotal2(D1,D2,D2row=4,D1column=8)# DentalHealth
D2<-TenTotal2(D1,D2,D2row=6,D1column=10)# DJD
D2[,3:8]<-D2[,3:8]/D2[,2]

#Filling HI: calculating the health Index

#Person Years live and life expectancy
pyl<-PYL(D1)
HI$lifeEx<-sum(pyl$Averagepyl)# proxy for life expectancy
HI$Pyl<-sum(pyl$pyl)

#Average attribute scores 
for(i in 1:7){
  if(D2$contributors[i]==0){D2$AverageScore[i]<-NA}
  else{D2$AverageScore[i]<-sum(D2[i,3:8]*pyl$Averagepyl,na.rm=TRUE)/HI$lifeEx}
}
HI[7:13]<-D2$AverageScore

#Health Index
HI$HI<-mean(D2$AverageScore,na.rm=TRUE)
HI$QALY<-26.38*(HI$HI/100)
HI<-round(HI[,sapply(HI, is.numeric)],2)# round all to 2 decimal places

#return HI dataset
HI
}
#####################################################################################
MultiHI<-function(x){# x= dataframe containing multiple populations
  x<-x[!(is.na(x$Age)),]#rm no age
  x<-split(x,x$Site)#split by site to create list of separate dataframes
  x<-Filter(function(x)length(x[,1])>0,x)# rm empty dataframes
  x<-lapply(x,FUN=HealthIndex)#applies HealthIndex() function to every dataframe 
  x<-do.call("rbind",x)#joins output into single dataframe
  x
}