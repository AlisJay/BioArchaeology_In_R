#Collection of all the functions used in the HealthIndex
#####################################################################################
#1. LovejoyAdj
LovejoyAdj<-function(age,info=FALSE){# age= numeric, info=logical
  #info function provides details of the method used and reference
  #default value= FALSE
  if (info==TRUE){
    output<-"Age at death estimations from skeletal remains have been show to be inaccurate. 
    Spefically there is a tendency to underestimate, particularly in older individuals. 
    This function adjusts reported age using the method outlined in lovejoy et al. (1985). 
    The function takes a single numeric input. To apply to dataframe the following steps are required:
    Dataframe$Adjusted<-NA
    for(i in 1:length(Dataframe$Age)){Dataframe$Adjusted[i]<-LovejoyAdj(Dataframe$Age[i])}"
    output<-strwrap(output)
  }else{
    if(is.na(age)){
      output<-NA
    }else{
      if(age>=18 & age<30){
        output<-age-1.4
      }else{
        if(age>=30 & age<40) {
          output<-age+0.8
        }else{
          if(age>=40 & age<50){
            output<-age+7.4
          }else{
            if(age>=50 & age<60){
              output<-age+6.8
            }else{
              if(age>=60){
                output<-age+13.6
              }else{output<-age}}}}}}}
  output}
#####################################################################################
#2.StatureLogic
StatureLogic<-function(x,A,B){# x = value, A=modern standard, B= 3SD below 
if(x>=A){output<-100}# if reaches or exceeds modern standard 
else{
if(x<=B){output<-0}# if less than or equal to 3rd SD
else{output<-100-((A-x)/(A-B)*100)}# overwise graded score between 0 and 100
}
output
}
#####################################################################################
#3. Stature 
Stature<-function(femur,Age,Sex,Maresh){# reference data, femur length age and sex
  if(anyNA(c(Age,femur))|round(Age) %in% 12:17){# age excluded in S&R 11.75-17.75
    s<-NA
  }else{
    if(round(Age)>=18){# adult values 
      if(Sex==6|is.na(Sex)|Sex==5){s<-StatureLogic(femur,Maresh$UL[Maresh$age==18],Maresh$U3SD[Maresh$age==18])}#unknown sex
	    if(Sex==1|Sex==2){s<-StatureLogic(femur,Maresh$FL[Maresh$age==18],Maresh$F3SD[Maresh$age==18])}#female
	    if(Sex==3|Sex==4){s<-StatureLogic(femur,Maresh$ML[Maresh$age==18],Maresh$M3SD[Maresh$age==18])}#male
    }else{s<-StatureLogic(femur,Maresh$UL[which.min(abs(Maresh$age-Age))],Maresh$U3SD[which.min(abs(Maresh$age-Age))])}#juvenilles
    }
  s
}
#####################################################################################
#4. Cat2Percent
Cat2Percent<-function(x,c){# value and number of categories
ref<-data.frame(cat=c(0,1,2,3,4,5),two=c(NA,100,0,NA,NA,NA),three=c(NA,100,50,0,NA,NA),four=c(NA,100,67,33,0,NA),five=c(NA,100,75,50,25,0))
ref[ref$cat==x,c]
}
#####################################################################################
#5.DentalHealth
DentalHealth<-function(SUMTET,SUMPRE,SUMCAV,SUMABS){
x<-c(SUMTET,SUMPRE,SUMCAV,SUMABS)
if(sum(is.na(x))>0|SUMTET+SUMPRE<8){DH<-NA}
else{
completness<-1-((SUMPRE+SUMCAV)/(SUMPRE+SUMTET))
if(SUMABS==0){Abcess<-1}
if(SUMABS==1){Abcess<-0.5}
if(SUMABS>1){Abcess<-0}
DH<-(completness*75)+(Abcess*25)
}
DH
}
#####################################################################################
#6.Fact2NUM
Fact2Num<- function(x){
  a<-as.character(x)
  a<-as.numeric(a)# will replace . with NA by coercion and all other entries will become numbers 
  a
}
#####################################################################################
#7.B2DTotal
B2DTotal<-function(D1,D2,D2row,D1column){#D2row & D1 Column= number
D2$contributors[D2row]<-length(D1[!(is.na(D1[,D1column])),D1column])
if(D2$contributors[D2row]==0){D2<-D2}# if no contributors don't change 
else{
D2$Age0_4[D2row]<-sum(D1[,D1column],na.rm=TRUE)# everyone who can contributes to the first age period
D2$Age5_14[D2row]<-sum(D1[D1$B2D %in% 2:6,D1column],na.rm=TRUE)#everyone not dead
D2$Age15_24[D2row]<-sum(D1[D1$B2D %in% 3:6,D1column],na.rm=TRUE)
D2$Age25_34[D2row]<-sum(D1[D1$B2D %in% 4:6,D1column],na.rm=TRUE)
D2$Age35_44[D2row]<-sum(D1[D1$B2D %in% 5:6,D1column],na.rm=TRUE)
D2$Age45[D2row]<-sum(D1[D1$B2D==6,D1column],na.rm=TRUE)
}
D2
}
#####################################################################################
#8.TenTotal
TenTotal<-function(D1,D2,D2row,D1column){
D2$contributors[D2row]<-length(D1[!(is.na(D1[,D1column])),D1column])
D1<- D1[!(is.na(D1[,D1column])),]
if(D2$contributors[D2row]==0){D2<-D2# if no contributors don't change
}else{
D2$Age0_4[D2row]<-(100*D2$contributors[D2row])-(100*length(D1[D1$Ten %in% c(01,12),D1column]))+sum(D1[D1$Ten %in% c(01,10),D1column])
D2$Age5_14[D2row]<-(100*D2$contributors[D2row])-(100*length(D1[D1$Ten %in% c(01,12,23),D1column]))+sum(D1[D1$Ten %in% c(12,23),D1column])
D2$Age15_24[D2row]<-(100*D2$contributors[D2row])-(100*length(D1[D1$Ten %in% c(01,12,23,34),D1column]))+sum(D1[D1$Ten %in% c(23,34),D1column])
D2$Age25_34[D2row]<-(100*D2$contributors[D2row])-(100*length(D1[D1$Ten %in% c(01,12,23,34,45),D1column]))+sum(D1[D1$Ten %in% c(34,45),D1column])
D2$Age35_44[D2row]<-(100*D2$contributors[D2row])-(100*length(D1[D1$Ten %in% c(01,12,23,34,45,56),D1column]))+sum(D1[D1$Ten %in% c(45,56),D1column])
D2$Age45[D2row]<-sum(D1[D1$Ten %in% c(56,60),D1column])
}
D2
}
#####################################################################################
#9.TenTotal2
TenTotal2<-function(D1,D2,D2row,D1column){
  D2$contributors[D2row]<-length(D1[!(is.na(D1[,D1column])),D1column])
  D1<- D1[!(is.na(D1[,D1column])),]
  if(D2$contributors[D2row]==0){D2<-D2
  }else{
    D2$Age0_4[D2row]<-(100*D2$contributors[D2row])#rm contributors, i.e those whose 10 years include this period
    D2$Age5_14[D2row]<-(100*D2$contributors[D2row])-(100*length(D1[D1$Ten== 01,D1column]))# rm contributors
    D2$Age15_24[D2row]<-(100*D2$contributors[D2row])-(100*length(D1[D1$Ten %in% c(01,12,23,34),D1column]))+sum(D1[D1$Ten %in% c(23,34),D1column])
    D2$Age25_34[D2row]<-(100*D2$contributors[D2row])-(100*length(D1[D1$Ten %in% c(01,12,23,34,45),D1column]))+sum(D1[D1$Ten %in% c(34,45),D1column])
    D2$Age35_44[D2row]<-(100*D2$contributors[D2row])-(100*length(D1[D1$Ten %in% c(01,12,23,34,45,56),D1column]))+sum(D1[D1$Ten %in% c(45,56),D1column])
    D2$Age45[D2row]<-sum(D1[D1$Ten %in% c(56,60),D1column])
  }
  D2
}
#####################################################################################
#10. PYL
PYL<-function(D1){# uses info in D1 database to calculate the person years live in each age category
  pyl<-data.frame(age=c("0_4","5_14","15_24","25_34","35_44","45+"),pyl=NA,Averagepyl=NA)
  #formula for working out pyl= (number of years in period x number of people who survived)+(the years they lived during that period by those who died in that period)
  pyl$pyl[1]<-(4*length(D1$Age[D1$B2D!=1]))+sum(D1$Age[D1$B2D==1])# 0-4
  pyl$pyl[2]<-(10*length(D1$Age[D1$B2D %in% 3:6]))+sum((D1$Age[D1$B2D==2]-4))#5-14 (-4= years before this period)
  pyl$pyl[3]<-(10*length(D1$Age[D1$B2D %in% 4:6]))+sum((D1$Age[D1$B2D==3]-14))
  pyl$pyl[4]<-(10*length(D1$Age[D1$B2D %in% 5:6]))+sum((D1$Age[D1$B2D==4]-24))
  pyl$pyl[5]<-(10*length(D1$Age[D1$B2D %in% 6]))+sum((D1$Age[D1$B2D==5]-34))
  pyl$pyl[6]<-sum((D1$Age[D1$B2D==6]-44))# no survivors everyone who isn't already dead must die during this period
  pyl$Averagepyl<-pyl$pyl/length(D1$Age)# number of years lived in the category by the average person
  pyl
}
#####################################################################################


