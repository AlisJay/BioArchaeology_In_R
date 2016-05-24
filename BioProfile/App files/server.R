#sourcing functions from the helper file
source("helpers.R")

shinyServer(function(input, output) {
  
  # known value outputs 
  #ancestory
  output$Kan <- renderPrint({ input$kan })
  #age
  output$Kag<-renderPrint({input$kage})
  output$Kagr<-renderPrint({paste(as.character(input$krange[1])," - ", as.character(input$krange[2]))})
  #sex
  output$Ks<-renderPrint({input$ksex})
  
  
  #unknown values by function call 
  
  #Ancestory()
  #function call
  an<-reactive({
    Ancestory(input$orbit,input$nRoot,input$LNB,input$palate,input$profile,input$nwidth,input$BR,input$VS,input$PBD,input$SI,input$nBridge,input$LEB)
  })
  #outputs 
  output$an<-renderPrint({
    an()$outcome
  })
  output$anT<-renderTable({
    an()$scores
  })
  output$anw<-renderPrint({an()$warnings})
  
  
  #subadult()
  #outputs 
  #(n.b subadult(input$epiphysis) function call not stored a separate object as it is simple and easily readable)
  output$ag<-renderPrint({
    subadult(input$epiphysis)$age
  })
  output$agr4<-renderPrint({subadult(input$epiphysis)$range})
  output$agw4<-renderPrint({subadult(input$epiphysis)$Mesage})
  
  #sex()
  #creating the pelvis and cranial field from the input
  p<-reactive({list(nonmetric=data.frame(trait=c("ventral arch","subpubic concavity","ishiopubic ramus","sciatic notch","prearicular sulcus"),score=c(input$va,input$spc,input$ipr,input$sn,input$pas)),
                         metric=data.frame(trait=c("pubic length", "ischium length"),measurement=c(input$pLength,input$iLength)))})
  
  cr<-reactive({list(nonmetric= data.frame(trait=c("nuchal crest","mastoid process","supra-orbital margin","glabella","mental eminence"),
                                          score=c(input$nc,input$m,input$som,input$g,input$me)),
                    metric=data.frame(trait=c("maximum length","maximum breadth","basion bregma","basion nasion","bizygomatic breadth","basion prosthion","nasion alveolar","palate breadth","mastoid length"),
                                            measurement=c(input$maxLength,input$maxBreadth,input$BaBr,input$BaNa,input$BB,input$BaPr,input$NaAl,input$pBreadth,input$mLength)))})
  #(ancestory,subAdult,pelvis,cranial)
  #options for sex(), using known and unknown values
  s1<-reactive({sex(input$kan,subAdult=list(age=input$kage),pelvis=p(),cranial=cr())})
  s2<-reactive({sex(ancestory=input$kan,subAdult=subadult(input$epiphysis),pelvis=p(),cranial=cr())})
  s3<-reactive({sex(ancestory=an()$outcome,subAdult=list(age=input$kage),pelvis=p(),cranial=cr())})
  s4<-reactive({sex(ancestory=an()$outcome,subAdult=subadult(input$epiphysis),pelvis=p(),cranial=cr())})
  
  #output options for sex()
  output$sx1<-renderPrint({s1()$Sex})
  output$st1<-renderTable({s1()$Table})
  output$sw1<-renderPrint({s1()$Warnings})
  
  output$sx2<-renderPrint({s2()$Sex})
  output$st2<-renderTable({s2()$Table})
  output$sw2<-renderPrint({s2()$Warnings})
  
  output$sx3<-renderPrint({s3()$Sex})
  output$st3<-renderTable({s3()$Table})
  output$sw3<-renderPrint({s3()$Warnings})
  
  output$sx4<-renderPrint({s4()$Sex})
  output$st4<-renderTable({s4()$Table})
  output$sw4<-renderPrint({s4()$Warnings})
  
  
  #adult()
  #creating vault and LA from input
  vault<-reactive({ComScore(input$ML,input$L,input$O,input$AS,input$B)})
  LA<-reactive({ComScore(input$P,input$MC,input$SF,input$IST,input$SST)})
  
  #adult options known and unknown sex
  a1<-reactive({Adult(sex=input$ksex,todd=input$todd,SucheyBrookes=input$SucheyBrookes,lovejoy=input$lovejoy,vault=vault(),LA=LA())})
  a2<-reactive({Adult(sex=s4()$Sex,todd=input$todd,SucheyBrookes=input$SucheyBrookes,lovejoy=input$lovejoy,vault=vault(),LA=LA())})
  a3<-reactive({Adult(sex=s2()$Sex,todd=input$todd,SucheyBrookes=input$SucheyBrookes,lovejoy=input$lovejoy,vault=vault(),LA=LA())})
  
  #age output options
  output$agr1<-renderPrint({paste(as.character(a1()$TotalRange)," (",as.character(a1()$AverageRange),")")})
  output$agt1<-renderTable({a1()$Table})
  output$agw1<-renderPrint({a1()$Warnings})
  
  output$agr2<-renderPrint({paste(as.character(a2()$TotalRange)," (",as.character(a2()$AverageRange),")")})
  output$agt2<-renderTable({a2()$Table})
  output$agw2<-renderPrint({a2()$Warnings})
  
  output$agr3<-renderPrint({paste(as.character(a3()$TotalRange)," (",as.character(a3()$AverageRange),")")})
  output$agt3<-renderTable({a3()$Table})
  output$agw3<-renderPrint({a3()$Warnings})
  
  
  
})
