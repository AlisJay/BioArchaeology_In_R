# Function Name: dfchoose() 
# Purpose: to determine what discriminate function to use for craniometric sex assement based on what data is available
# Linked Functions: used inside the function sex() as part of the assessment of the craniometric data 
# Example Use: ComScore(P,MC,SF,IST,SST) 
######################################################################################################################################################################################

dfchoose<-function(x,y){# x= the reference table and y = the users input
  
  #Step 1: creating blank fields to be filled 
  df<-NA# this will tell you what function to use
  rows<-NA# this will tell you what rows are used in implementation of that function
  
  #Step 2: Picking the function
  # Part1: If there are 3 columns in the reference table 
  # determining what input is needed for each discriminate function
  if(ncol(x)==3){a<-head(row.names(x)[!(is.na(x[,2]))==TRUE],-2)# gives the fields need for first discrimate function option
                 b<-head(row.names(x)[!(is.na(x[,3]))==TRUE],-2)# second option
                 
				 #choosing which to used by matching available input 
				 if(anyNA(y[a,2])==FALSE){df<-"DF1"# if have all the avaible data for option 1
                                          rows<-a
                 }else{if(anyNA(y[b,2])==FALSE){df<-"DF2"# if don't have data need for first DF but do for the second
                                                rows<-b
                 }else{df<-NA}# if don't have data needed for either
                 }
  }
  
  #Part 2: if there are 7 options in the reference table, same pattern as above just more options 
  #determining what input is needed
  if(ncol(x)==7){a<-head(row.names(x)[!(is.na(x[,2]))==TRUE],-2)
                 b<-head(row.names(x)[!(is.na(x[,3]))==TRUE],-2)
                 c<-head(row.names(x)[!(is.na(x[,4]))==TRUE],-2)
                 d<-head(row.names(x)[!(is.na(x[,5]))==TRUE],-2)
                 e<-head(row.names(x)[!(is.na(x[,6]))==TRUE],-2)
                 f<-head(row.names(x)[!(is.na(x[,7]))==TRUE],-2)
                 
				 #choosing which to used by matching available input 
				 if(anyNA(y[a,2])==FALSE){df<-"DF1"
                                          rows<-a
                 }else{if(anyNA(y[b,2])==FALSE){df<-"DF2"
                                                rows<-b
                 }else{if(anyNA(y[c,2])==FALSE){df<-"DF3"
                                                rows<-c
                 }else{if(anyNA(y[d,2])==FALSE){df<-"DF4"
                                                rows<-d
                 }else{if(anyNA(y[e,2])==FALSE){df<-"DF5"
                                                rows<-e
                 }else{if(anyNA(y[f,2])==FALSE){df<-"DF6"
                                                rows<-f
                 }else{df<-NA}
                 }}}}}}
  
  #Step 3: output
  list(df=df,rows=rows)# a list of the function to use and the row involved 
}