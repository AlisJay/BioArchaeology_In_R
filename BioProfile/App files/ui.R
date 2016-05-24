library(shiny);library(markdown)
shinyUI(navbarPage("Bio-Profiler",
                   tabPanel("Ancestry",
                            fluidPage(
                              titlePanel("Ancestry Input"),
                              selectInput("knownac", "Known?", c("known","unknown"),selected="known"),
                              conditionalPanel("input.knownac=='known'",
                                               radioButtons("kan", label = h3("ancestry"),
                                                            choices = list("Asian" = "asian", "European" = "european", "African" = "african"), 
                                                            selected = 1)),
                              conditionalPanel("input.knownac=='unknown'",
                                               fluidRow(
                                                 column(width=6,
                                                        selectInput("orbit","orbit",c("angular","rectangular","round","undeterminable")),
                                                        selectInput("nRoot","nasal root",c("high","low round","low ridged","undeterminable")),
                                                        selectInput("LNB","lower nasal border",c("sharp sill","guttered","flat","undeterminable")),
                                                        selectInput("palate","palate",c("parabolic","hyperbolic","ellipitical","undeterminable")),
                                                        selectInput("profile","profile",c("straight","projecting","intermediate","undeterminable")),
                                                        selectInput("nwidth","nasal width",c("narrow","wide","intermediate","undeterminable"))
                                                 ),
                                                 column(width=6,
                                                        selectInput("BR","prominant brow ridge",c("TRUE","FALSE","undeterminable")),
                                                        selectInput("VS","complex vault structure",c("TRUE","FALSE","undeterminable")),
                                                        selectInput("PBD","post bregma depression",c("TRUE","FALSE","undeterminable")),
                                                        selectInput("SI","shovel shaped incisors",c("TRUE","FALSE","undeterminable")),
                                                        selectInput("nBridge","nose bridge",c("TRUE","FALSE","undeterminable")),
                                                        selectInput("LEB","LEB",c("TRUE","FALSE","undeterminable"))
                                                 )
                                               ))
                              
                            )
                   ),
                   tabPanel("Age",
                            fluidPage(
                              titlePanel("Age Input"),
                              selectInput("knownage", "Known?", c("known","unknown")),
                              conditionalPanel("input.knownage=='known'",
                                               radioButtons("kage","Age group",choices=list("adult"="Adult","subadult"="SubAdult"),select=1),
                                               sliderInput("krange","Age range",min=0,max=100,value=c(40,60))
                              ),
                              conditionalPanel("input.knownage=='unknown'",
                                               fluidRow(
                                                 column(width=4,
                                                        h4("epiphyses closure"),
                                                        checkboxGroupInput("epiphysis","tick all open epiphyses",
                                                                           choices=list("proximal radius"="pRadius","distal fibula"="dFibula","distal tibia"="dTibia","distal femur"="dFemur","proximal fibula"="pFibula","acromion process"="acromion",
                                                                                        "illiac crest"="iliac","humeral head"="hHead","femoral head"="fHead","lesser trochanter"="lTrochanter","proximal tibia"="pTibia",
                                                                                        "greater trochanter"="gTrochanter","distal radius"="dradius","sacrum s3-s5"="s3s5","sacrum s3-s2"="s2s3","sacrum s1-s2"="s1s2","sternal end of clavicle"="clavicle","spheno-occipital synchondrosis"="SOS"))
                                                 ),
                                                 column(width=4,
                                                        h4("phase scoring"),
                                                        selectInput("todd","pubic synthesis Todd",c("NA","p1","p2","p3","p4","p5","p6","p7","p8","p9","p10")),
                                                        selectInput("SucheyBrookes","pubic synthesis Suchey-Brookes",c("NA","p1","p2","p3","p4","p5","p6")),
                                                        selectInput("lovejoy","auricular surface Lovejoy",c("NA","p1","p2","p3","p4","p5","p6","p7","p8"))
                                                 ),
                                                 column(width=4,
                                                        h4("suture closure"),
                                                        selectInput("ML","midlambdoid",c("NA","0","1","2","3")),
                                                        selectInput("L","lambda",c("NA","0","1","2","3")),
                                                        selectInput("O","obelion",c("NA","0","1","2","3")),
                                                        selectInput("AS","anterior sagittal",c("NA","0","1","2","3")),
                                                        selectInput("B","bregma",c("NA","0","1","2","3")),
                                                        selectInput("P","pterion",c("NA","0","1","2","3")),
                                                        selectInput("MC","midcoronal",c("NA","0","1","2","3")),
                                                        selectInput("SF","sphenofrontal",c("NA","0","1","2","3")),
                                                        selectInput("IST","inferior sphenotemporal",c("NA","0","1","2","3")),
                                                        selectInput("SST","superior sphenotemporal",c("NA","0","1","2","3"))
                                                        
                                                 )
                                               ))
                            )      
                   ),
                   tabPanel("Sex",
                            fluidPage(
                              titlePanel("Sex Input"),
                              selectInput("knownsex", "Known?", c("known","unknown")),
                              conditionalPanel("input.knownsex=='known'",
                                               radioButtons("ksex","sex",choices=list("male"="male","female"="female"),select=1)
                              ),
                              conditionalPanel("input.knownsex=='unknown'",
                                               fluidRow(
                                                 column(width=6,
                                                        h3("non metric traits"),
                                                        h4("pelvis"),
                                                        sliderInput("va","ventral arch",min=0,max=3,value=0),
                                                        sliderInput("spc","subpubic concavity",min=0,max=3,value=0),
                                                        sliderInput("ipr","ishio-pubic ramus",min=0,max=3,value=0),
                                                        sliderInput("sn","sciatic notch",min=0,max=5,value=0),
                                                        selectInput("pas","preauricular sulcus",c("NA","yes","no")),
                                                        h4("cranial"),
                                                        sliderInput("nc","nuchal crest",min=0,max=5,value=0),
                                                        sliderInput("m","mastoid process",min=0,max=5,value=0),
                                                        sliderInput("som","supraorbital margin",min=0,max=5,value=0),
                                                        sliderInput("g","glabella",min=0,max=5,value=0),
                                                        sliderInput("me","mental eminence",min=0,max=5,value=0)
                                                 ),
                                                 column(width=6,
                                                        h3("metric assessment"),
                                                        h4("pelvis"),
                                                        numericInput("pLength","pubis length",value=NA),
                                                        numericInput("iLength","ishial length",value=NA),
                                                        h4("cranial"),
                                                        numericInput("maxLength","maximum length",value=NA),
                                                        numericInput("maxBreadth","maximum breadth",value=NA),
                                                        numericInput("BaBr","basion to bregma",value=NA),
                                                        numericInput("BaNa","basion to nasion",value=NA),
                                                        numericInput("BB","bizygomatic breadth",value=NA),
                                                        numericInput("BaPr","basion to prosthion",value=NA),
                                                        numericInput("NaAl","nasion to alveolar border",value=NA),
                                                        numericInput("pBreadth","palate breadth",value=NA),
                                                        numericInput("mLength","mastoid length",value=NA)
                                                 )
                                               )
                              )
                            )
                   ),
                   tabPanel("Result",
                            fluidPage(
                              titlePanel("Results"),
                              mainPanel(
                                h4("Ancestry"),
                                conditionalPanel("input.knownac=='known'",
                                                 h5("Known ancestry:"),
                                                 h5(textOutput("Kan"))),
                                conditionalPanel("input.knownac=='unknown'",
                                                 h5("Calculated ancestry:"),
                                                 h5(textOutput("an")),
                                                 tableOutput("anT"),
                                                 h5(textOutput("anw"))),
                                h4("Age"),
                                conditionalPanel("input.knownage=='known'",
                                                 h5("Known age:"),
                                                 h5(textOutput("Kag")),
                                                 h5(textOutput("Kagr"))),
                                conditionalPanel("input.knownage=='unknown'",
                                                 h5("Calculated age:"),
                                                 h5(textOutput("ag")),
                                                 h5(textOutput("agw4")),
                                                 conditionalPanel("input.epiphysis!='' & input.epiphysis!='s1s2'",
                                                                  h5(textOutput("agr4"))),
                                                 conditionalPanel("input.epiphysis==''|input.epiphysis=='s1s2'",
                                                                  conditionalPanel("input.knownsex=='known'",
                                                                                   h5(textOutput("agr1")),
                                                                                   tableOutput("agt1"),
                                                                                   h5(textOutput("agw1"))),
                                                                  conditionalPanel("input.knownsex=='unknown'& input.knownac=='known'",
                                                                                   h5(textOutput("agr3")),
                                                                                   tableOutput("agt3"),
                                                                                   h5(textOutput("agw3"))),
                                                                  conditionalPanel("input.knownsex=='unknown'& input.knownac=='unknown'",
                                                                                   h5(textOutput("agr2")),
                                                                                   tableOutput("agt2"),
                                                                                   h5(textOutput("agw2")))
                                                                  
                                                 )
                                ),
                                h4("Sex"),
                                conditionalPanel("input.knownsex=='known'",
                                                 h5("Known sex:"),
                                                 h5(textOutput("Ks"))),
                                conditionalPanel("input.knownsex=='unknown'",
                                                 h5("Calculated sex:"),
                                                 conditionalPanel("input.knownac=='known'&input.knownage=='known'",
                                                                  h5(textOutput("sx1")),
                                                                  tableOutput("st1"),
                                                                  h5(textOutput("sw1"))),
                                                 conditionalPanel("input.knownac=='known'&input.knownage=='unknown'",
                                                                  h5(textOutput("sx2")),
                                                                  tableOutput("st2"),
                                                                  h5(textOutput("sw2"))),
                                                 conditionalPanel("input.knownac=='unknown'&input.knownage=='known'",
                                                                  h5(textOutput("sx3")),
                                                                  tableOutput("st3"),
                                                                  h5(textOutput("sw3"))),
                                                 conditionalPanel("input.knownac=='unknown'&input.knownage=='unknown'",
                                                                  h5(textOutput("sx4")),
                                                                  tableOutput("st4"),
                                                                  h5(textOutput("sw4")))
                                                 
                                                 
                                                 
                                )
                                )
                              
                            )
                   ),
                   tabPanel("Help",
                            fluidPage(
                              titlePanel("Help"),
                              mainPanel(
                                img(src="pic.png",height=296,width=432),
                                p("BioProfile is an app that uses the results of standard osteological methods to create a basic biological profile (age, sex and ancestry)"),
                                p("There are three input tabs; Ancestry, Age and Sex. The first option for each is whether that aspect is known or unknown (i.e. needs to be calculated).
                                  If known the app provides widgets to input the known data. If unknown BioProfile will provide the user with widgets to input data from various relevent standardised analysis.
                                  (all methods used are taken from Standards for data collection from human skeletal remains (Buikstra and uberlaker,1994) and Introduction to forensic anthropology (Byers 2010))"),
                                p("The output of the analysis is displayed in the results tab under the relevant headings. The first line of the output will read either known..: or calculated..:
                                  If the information was known the output will simply mirror the inputted data.
                                  If the results were calculated then the output will include;"),
                                p("1. A summary value (e.g. for sex it will be one of the following male, possible male, female, possible female, unknown"),
                                p("2. A results table"),
                                p("3. Any warnings generated"),
                                p("further information can be found at https://github.com/AlisJay/BioProfile")
                                )
                              ))
)
)