# Function Name: sex() 
# Purpose: to determine sex from pelvic and cranial traits  
# Linked Functions: ancestory()$outcome can be used as ancestry input, uses output of subadult(). Employs dfchoose(). Can be used in Adult()
# Example Use: sex(ancestory=ancestry()$outcome,subAdult=subadult(),pelvis,cranial) 
######################################################################################################################################################################################

sex<-function(ancestory,subAdult,pelvis,cranial){# pelvis and cranial are spefic objects created by the App that contain the trait scores 
  
  #Step 1: set up blank results data frame to be filled 
  results<-data.frame(trait=c("ventral arch","subpubic concavity","ishiopubic ramus","sciatic notch","prearicular sulcus","ishiopubic index","nuchal crest","mastoid process","supra-orbital margin","glabella","mental eminence","craniometrics"),
                                  sex=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  
  #Step 2: creating Reference data frames 
  #discriminate functions table from Gile(1970)(as reported in Byers,2010) for white crania
  DFW<-data.frame(measurement=c("ML","MB","BABR","BaNA","BB","BaPr","NaAl","PB","LM","sectioning point","percent correct"),
                  DF1=c(3.107,-4.643,5.786,NA,14.821,1.000,2.714,-5.179,6.071,2672.39,86.6),
                  DF2=c(3.400,-3.833,5.433,-0.167,12.200,-0.100,2.200,NA,5.367,2592.32,86.4),
                  DF3=c(1.800,-1.783,2.767,-0.100,6.300,NA,NA,NA,2.833,1296.20,86.4),
                  DF4=c(NA,NA,NA,10.714,16.381,-1.00,4.333,-6.571,14.810,3348.27,84.5),
                  DF5=c(1.236,-1.000,NA,NA,3.291,NA,NA,NA,1.528,536.93,85.5),
                  DF6=c(9.875,NA,NA,7.062,19.062,-1.000,4.375,NA,NA,5066.69,84.9))
  #discriminate functions table from Gile(1970)(as reported in Byers,2010) for black crania
  DFB<-data.frame(measurement=c("ML","MB","BABR","BaNA","BB","BaPr","NaAl","PB","LM","sectioning point","percent correct"),
                  DF1=c(9.222,7.000,1.000,NA,31.111,5.889,20.222,-30.556,47.111,8171.53,87.6),
                  DF2=c(3.895,3.632,1.000,-2.053,12.947,1.368,8.158,NA,19.947,4079.12,86.6),
                  DF3=c(3.533,1.667,0.867,0.100,8.700,NA,NA,NA,14.367,2515.91,86.5),
                  DF4=c(NA,NA,NA,1.000,19.389,2.778,11.778,-14.333,23.667,3461.46,87.5),
                  DF5=c(2.111,1.000,NA,NA,4.936,NA,NA,NA,8.037,1387.72,85.3),
                  DF6=c(2.867,NA,NA,-0.100,12.367,-0.233,6.900,NA,NA,2568.97,85.0))
  #discriminate functions table from Gile(1970)(as reported in Byers,2010) for Asian crania
  DFA<-data.frame(measurement=c("ML","MB","BABR","BaNA","BB","BaPr","NaAl","PB","LM","sectioning point","percent correct"),
                  DF1=c(1.000,-0.062,1.865,NA,12.57,NA,NA,NA,NA,579.96,86.4),
                  DF2=c(1.000,0.221,NA,NA,1.095,NA,0.504,NA,NA,380.84,83.1))
  #scoring for 5 point scale (e.g. Mastoid process)
  match5<-data.frame(no=c(0:5),sex=c(NA,"f","pf","u","pm","m"))
  #scoring for 3 point scale (eg.phenice traits)
  match3<-data.frame(no=c(0:3),sex=c("NA","f","u","m"))
  #Ishiopubic index scores
  IPI<-data.frame(ancestory=c("european","possible european","african","possible african","asian","possible asian","unknown"),
                  over=c(95,95,88,88,94,94,94),
                  under=c(91,91,83,83,84,84,84))
  
  #Step3: setting base values of warnings 
  w1<-NULL;w2<-NULL;w3<-NULL
  
  #Step 4: test if remains are sub-adult  (if so finish analysis, as can't sex sub-adults)
  if(subAdult$age=="SubAdult"){sex<-"unknown"# uses output of subAdult()
  w1<-"We do not sex subadult"# unique warning message for subadults 
  }else{

#Step 5: filling in results table for non juvenille remains: Pelvis  
    #Part 1: non metric traits with 0-5 sliders (0 is unrecordable)
    results$sex[results$trait=="nuchal crest"]<-as.character(match5$sex[match5$no==cranial$nonmetric[1,2]])
    
    results$sex[results$trait=="mastoid process"]<-as.character(match5$sex[match5$no==cranial$nonmetric[2,2]])
    
    results$sex[results$trait=="supra-orbital margin"]<-as.character(match5$sex[match5$no==cranial$nonmetric[3,2]])
    
    results$sex[results$trait=="glabella"]<-as.character(match5$sex[match5$no==cranial$nonmetric[4,2]])
    
    results$sex[results$trait=="mental eminence"]<-as.character(match5$sex[match5$no==cranial$nonmetric[5,2]])
    
    results$sex[results$trait=="sciatic notch"]<-as.character(match5$sex[match5$no==pelvis$nonmetric[4,2]])
    
    #Part 2: non metric traits with 0-3 sliders 
	results$sex[results$trait=="ventral arch"]<-as.character(match3$sex[match3$no==pelvis$nonmetric[1,2]])
    
    results$sex[results$trait=="subpubic concavity"]<-as.character(match3$sex[match3$no==pelvis$nonmetric[2,2]])
    
    results$sex[results$trait=="ishiopubic ramus"]<-as.character(match3$sex[match3$no==pelvis$nonmetric[3,2]])
    
    #Part3: Prearicular sulcus
    if(!(is.na(pelvis$nonmetric[5,2]))){# if preauricular score isn't an NA
      if(pelvis$nonmetric[5,2]=="yes"){# and if it is yes
        results$sex[results$trait=="prearicular sulcus"]<-"f"
      }else{
        if(pelvis$nonmetric[5,2]=="no"){# if preauricular is no
          results$sex[results$trait=="prearicular sulcus"]<-"m"
      }else{results$sex[results$trait=="prearicular sulcus"]<-"u"}# if preauricular score is NA
    }}
   
    #Part 4: Ishiopubic index
    if(!(anyNA(pelvis$metric))){# if neither pubic or ishial length are NA
      index<-(pelvis$metric[1,2]/pelvis$metric[2,2])*100# ishiopubic Index calcultion
      
      if(index>IPI[IPI$ancestory==ancestory,2]){results$sex[results$trait=="ishiopubic index"]<-"f"# if above highest value for the ancestry given then female
      }else{
        if(index<IPI[IPI$ancestory==ancestory,3]){results$sex[results$trait=="ishiopubic index"]<-"m"# if below lowest value for the ancestry given then male
        }else{results$sex[results$trait=="ishiopubic index"]<-"u"}# if inbetween two values then unknown sex
      }
    }
    
#Step 6: filling in results table for non juvenille remains: Cranial   
	#part 1: picking reference table based on ancestry then using dfchoose() to pick discrimate function to use
    if(ancestory=="asian"|ancestory=="possible asian"){# if asian 
      x<-dfchoose(DFA,cranial$metric)# use DFA reference table in df choose
      y<-DFA# store reference table used as y
    }else{
      if(ancestory=="african"|ancestory=="possible african"){
        x<-dfchoose(DFB,cranial$metric)# x will give the name of function to use and what rows are involved in it calculation (see dfchoose())
        y<-DFB
      }else{
        if(ancestory=="european"|ancestory=="possible european"){
          x<-dfchoose(DFW,cranial$metric)
          y<-DFW
        }else{x<-list(df=NA,rows=NA)}
      }}
    
	#Part 2: use output of dfchoose() to apply discriminate function and compare to reference table 
    if(!(is.na(x$df))){# if output of last part not NA, i.e if there is a discriminate function that can be applied
      sum<-sum(y[x$rows,names(y)==x$df]*cranial$metric[x$rows,2])# sum of discrimate function values time the measurements recorded
      if(sum>y[10,names(y)==x$df]){# if above the relevant sectioning point
        results$sex[results$trait=="craniometrics"]<-"m"
      }else{results$sex[results$trait=="craniometrics"]<-"f"}# if below sectioning point then female
    }
    
    #Step 7: assessments and warnings
    results$sex<-as.factor(results$sex)# change to factors from character
	
	if(sum(summary(results$sex)["f"],summary(results$sex)["pf"],na.rm=TRUE)>6){#if mostly female or possible female 
      
	  if(all(is.na(results[c(1:6),2]))){#if no pelvic measurements then use possible female 
        sex<-"possible female"
        w2<-"without pelvis assessment sex determination can only give possible results" # produce warning explaining that pelvic measurments are need to give more than a possible result
      }else{# if there are pelvic measurements
		if(summary(results$sex)["f"]>6){# if female is the majority then return female
          sex<-"female"
		}else{sex<-"possible female"}#if female isn't majority return possible female 
      }
    }else{
	  if(sum(summary(results$sex)["m"],summary(results$sex)["pm"],na.rm=TRUE)>6){# if male and possible male are the majority 
		if(all(is.na(results[c(1:6),2]))){#if no pelvic measurments use possible male 
          sex<-"possible male"
          w2<-"without pelvis assessment sex determination can only give possible results"# create warning 
        }else{
		  if(summary(results$sex)["m"]>6){# is male is the majority
            sex<-"male"
		  }else{sex<-"possible male"}# if male not a majority
        }
	}else{sex<-"unknown"}# if no majority by either male or female side return unknown 
  }
  }

  #incomplete analysis warnng 
  w3<-if(anyNA(results)){"Your input was incomplete please note that the most reliable results come from complete analysis"}# create warning is any missing data 

#output
list(Sex=sex,Table=results,Warnings=c(w1,w2,w3))# list of character vector with sex (e.g. "Male"), the table of results and any warnings
}