#########################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################
# This ShinyApp has been developed as part of a project tiled: "Victims’ Access to Justice through English Criminal Courts, 1675 to the present". 
# This has been funded by the Economic and Social Research Council (grant n. ES/R006962/1). 
# Further information regarding the project can be found at: http://esrcvictims.org
##########################################################################################################################################################################################################################################################
# This app has been developed as part of the Impact Acceleration Award (DT04308), University of Essex.
########################################################################################################################################################################################################################################################### 
# For the purpose of our project, we have compiled a special datatset that merges and appends a selection of variables extracted from the Crime Survey for England and Wales (CSEW):
# Impara, E. & Cox, P. (2020). 'Aggregated Crime Survey for England and Wales, 1982-2017, with Access to Justice Focus'. [data collection]. Office for National Statistics, [original data producer(s)]. Office for National Statistics. SN: 8716  
# The dataset used in this work can be accessed at UK Data Service (via registration:  https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8716
#########################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(finalfit)
library(foreign)
library(MASS)
library(reshape2)
library(DT)
library(effects)
library(car)
library(nnet)
library(glmnet)
library(brant)
library(caret)
library(ordinalNet)


cols <- list("jobcps","jobjud", "jobmag", "jobjuv", "cjscpsb", "cjscrt2a", "cjscrt2b", "cjsovb1", "fairatt1", "fairova1", "confoff", "confvict")
names(cols) <- c("How good a job do you think the CPS are doing?",
                 "How good a job do you think judges are doing?",
                 "How good a job do you think magistrates are doing?",
                 "How good a job do you think the juvenile courts are doing?",
                 "How confident are you Crown Prosecution Service is effective at prosecuting?",
                 "How confident are you Courts are dealing with cases promptly?",
                 "How confident are you Courts are effective at giving punishments?",
                 "How confident are you - Criminal Justice System as a whole is effective?",
                 "How much do you agree or disagree that the Criminal Justice System gives victims and witnesses the support they need?",
                 "How confident are you that the Criminal Justice System as a whole is fair?",
                 "How confident you are that CJS is effective in bringing people to justice?",
                 "How confident are you that the CJS meets the needs of victim?")


cols1 <- list("recode_reported", "victim_bgl","victim_prp", "victim_veh", "victim_vio", "victim_vio2", "sex", "recode_race", "recode_age", "recode_educat3", "recode_income", "gor")
names(cols1) <- c("Police knew", "Burglary", "Property Crime", "Vehicle Crime", "Violent Crime", "Sex Crime", "Sex", "Race", "Age", "Education", "Income", "Location")

ui <- fluidPage(
  titlePanel("Logistic Regression Models"),
  p(HTML("This app is one of the outcomes of the ESRC-funded project: 'Victims’ Access to Justice through English Criminal Courts, 1675 to the present'. 
         A special aggregated and appended subset of the CSEW is used here. This only includes respondents identified as victims.")),
  sidebarLayout(
    sidebarPanel(
      
      # Use SelectInput for survey years only
      selectInput(inputId="year", label = "Year", 
                  choices=c(1982, 1984, 1988, 1992, 1994, 1996, 1998, 
                            2000, 2001,  2002,  2003,  2004, 2005, 2006,  2007,  2008,  2009,  
                            2010,  2011, 2012, 2013, 2014, 2015, 2016, 2017), 
                  multiple=T),
      
      selectInput(inputId="models", label = "Choose Model", choices= c("Proportional Odds model (default)", 
                                                                       "Multinomial model (if prop odd assumption violated)",
                                                                       "Ridge Regression Model (if multicollinearity present)"), multiple=F),
      
      selectInput(inputId="outcome",label = "Response Variables",choices=cols, multiple=F),
      
      selectInput(inputId="features", label = "Explanatory Variables",choices=cols1, multiple=TRUE)
      
    ),
    
    mainPanel(
      h5(),
      tabsetPanel(type = "tabs", 
                  tabPanel("Model Summary", verbatimTextOutput("summary1"), verbatimTextOutput("summary1_bis")), 
                  tabPanel("Assumptions testing", verbatimTextOutput("summary2"), verbatimTextOutput("summary3")),
                  tabPanel("Predicted Probabilities & Plots", plotOutput("plot")),
                  
                  tabPanel("Model Specification", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2("Model Description"),
                                           includeMarkdown("The_model.Rmd"),
                           ))),
                  tabPanel("Brief Tutorial", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2("How to use the app"),
                                           includeMarkdown("tutorial_app5.Rmd"),
                           ))),
                  tabPanel("Dataset Info", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2("A special aggregated dataset"),
                                           includeMarkdown("dataset_info.Rmd"),
                           ))),
                  tabPanel("Useful Resources", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2("Suggested Bibliography"),
                                           includeMarkdown("resources.Rmd"),
                           ))),
                  
                  tabPanel("About", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2(),
                                           includeMarkdown("about.Rmd"),
                           ))),
                  
                  textOutput("text"),
                  verbatimTextOutput("summary"),
                  p(HTML("DISCLAIMER: this modelling has exclusively research and educational purposes. It is not intended for decision-making. Even though a lot of work went into making sure no mistakes are present, we might have missed something. Please email us at <a href=http://esrcvictimsproject.shiny@gmail.com>  esrcvictimsproject.shiny@gmail.com </a> if you notice anything inaccurate"))
      )
    )
  ))


server <- function(input, output) {
  
  df <- read.csv("./ESRC_df_app.csv", sep = ",", header=TRUE, na.strings = c("","NA","-7", "-8", "-9", "98", "99"))

  df$recode_reported <- factor(df$recode_reported, c("no", "yes"), levels = c("no", "yes"))
  df$victim_bgl <- factor(df$victim_bgl, c("no", "yes"), levels = c("no", "yes"))
  df$victim_prp <- factor(df$victim_prp, c("no", "yes"), levels = c("no", "yes"))
  df$victim_veh <- factor(df$victim_veh, c("no", "yes"), levels = c("no", "yes"))
  df$victim_vio <- factor(df$victim_vio, c("no", "yes"), levels = c("no", "yes"))
  df$victim_vio2 <- factor(df$victim_vio2, c("no", "yes"), levels = c("no", "yes"))
  
  df$sex <- factor(df$sex, c("male", "female"), 
                   levels = c("male", "female"))
  df$recode_race <- factor(df$recode_race, c("white", "black", "asian", "other"), 
                           levels = c("white", "black", "asian", "other"))
  df$recode_age <- factor(df$recode_age, c("under 25", "26-39", "40-59", "60-79", "over 80"), 
                          levels = c("under 25", "26-39", "40-59", "60-79", "over 80"))
  df$recode_educat3 <- factor(df$recode_educat3, c("high", "A-levels", "GCSE", "other/no"), 
                              levels = c("high", "A-levels", "GCSE", "other/no"))
  df$recode_income <- factor(df$recode_income, c("low", "mid", "top"), levels = c("low", "mid", "top"))
  df$gor <- factor(df$gor, c("north east", "north west", "Yorks&Humberside", "east midlands", "west midlands", "east of England", 
                             "London", "south east", "south west", "Wales", "Scotland"), 
                   levels = c("north east", "north west", "Yorks&Humberside", "east midlands", "west midlands", "east of England", 
                              "London", "south east", "south west", "Wales", "Scotland"))

  df$jobcps <- ordered(df$jobcps, levels = c("very poor", "poor", "fair", "good", "excellent"))
  df$jobjud<- ordered(df$jobjud, levels = c("very poor", "poor", "fair", "good", "excellent"))
  df$jobmag <- ordered(df$jobmag, levels = c("very poor", "poor", "fair", "good", "excellent"))
  df$jobjuv <- ordered(df$jobjuv, levels = c("very poor", "poor", "fair", "good", "excellent"))
  
  df$cjscpsb <- ordered(df$cjscpsb, levels = c("not at all", "not very", "fairly", "very"))
  df$cjscrt2a <- ordered(df$cjscrt2a, levels = c("not at all", "not very", "fairly", "very"))
  df$cjscrt2b <- ordered(df$cjscrt2b, levels = c("not at all", "not very", "fairly", "very"))
  df$cjsovb1 <- ordered(df$cjsovb1, levels = c("not at all", "not very", "fairly", "very"))
  df$fairatt1 <- ordered(df$fairatt1, levels = c("not at all", "not very", "fairly", "very"))
  df$fairova1 <- ordered(df$fairova1, levels = c("not at all", "not very", "fairly", "very"))
  df$confoff <- ordered(df$confoff, levels = c("not at all", "not very", "fairly", "very"))
  df$confvict <- ordered(df$confvict, levels = c("not at all", "not very", "fairly", "very"))
  
  ########################## Regression output ##########################
  output$summary1 <- renderPrint({
    vf_year <- input$year
    vf_outcome <- input$outcome
    vf_features <- input$features
    Modelling <- input$models
    
    if(is.null(vf_year) | is.null(vf_outcome) | is.null(vf_features)){
      return()
    }
    
    df_filtered <- df[df$year %in% vf_year, c(vf_outcome, vf_features)]
    df_filtered[[vf_outcome]] <-as.factor(df_filtered[[vf_outcome]]) # needed by polr()
    
    partitionTarget <- df_filtered[[vf_outcome]]
    set.seed(1981)
    trainIndex <- caret::createDataPartition(partitionTarget, p=0.7)$Resample1
    train <- df_filtered[trainIndex, ]
    test <- df_filtered[-trainIndex, ]
    
    
    if(Modelling=="Proportional Odds model (default)") {
      fit <- reactive(polr(as.formula(paste(vf_outcome, "~", paste(vf_features, collapse="+"))), data = train, Hess = TRUE))
      summary(fit())
    } 
    else if(Modelling=="Multinomial model (if prop odd assumption violated)") {
      fit <- reactive(multinom(as.formula(paste(vf_outcome, "~", paste(vf_features, collapse="+"))), data = train))
      summary(fit())
    }
    else if(Modelling== "Ridge Regression Model (if multicollinearity present)") {
      if(length(vf_features) <= 1 & length(levels(df_filtered[[vf_features[1]]])) <= 2) {
        return("You need to select more than one variable, or a variable with more than 2 levels")
      }
      
      df_nona <- na.omit(df_filtered)
      form <- as.formula(paste(vf_outcome," ~ ", paste(vf_features, collapse="+")))
      mf <- model.frame(form, data=df_nona)
      
      predictors <- as.data.frame(model.matrix(form, mf))
      predictors <- sapply(predictors[,-1], as.integer)
      
      y <- model.response(mf)
      x <- predictors
      
      # Fit model
      set.seed(1981)
      fit <- ordinalNetTune(x, y, nFolds = 3, alpha = 0)
      bestLam <- which.max(rowMeans(fit$loglik))
      coef(fit$fit, whichLambda = bestLam, matrix = TRUE)
    }
    else { NULL }
  })
  
  ########################## Accuracy output ##########################
  
  output$summary1_bis <- renderPrint({
    vf_year <- input$year
    vf_outcome <- input$outcome
    vf_features <- input$features
    Modelling <- input$models
    
    if(is.null(vf_year) | is.null(vf_outcome) | is.null(vf_features)){
      return()
    }
    
    df_filtered <- df[df$year %in% vf_year, c(vf_outcome, vf_features)]
    
    partitionTarget <- df_filtered[[vf_outcome]]
    set.seed(1981)
    trainIndex <- caret::createDataPartition(partitionTarget, p=0.7)$Resample1
    train <- df_filtered[trainIndex, ]
    test <- df_filtered[-trainIndex, ]
    
    if(Modelling=="Proportional Odds model (default)") {
      fit <- polr(as.formula(paste(vf_outcome, "~", paste(vf_features, collapse="+"))), data = train, Hess = TRUE)
      # ACCURACY 
      trainPred <- predict(fit, train)
      trainTable <- table(train[[vf_outcome]], trainPred)
      testPred <- predict(fit, newdata=test)
      testTable <- table(test[[vf_outcome]], testPred) 
      trainAcc <- sum(diag(trainTable)) / sum(trainTable) # accuracy rate on test data
      testAcc <- sum(diag(testTable)) / sum(testTable) # accuracy rate on test data
      paste("ACCURACY ON TEST SET:", testAcc)
    } 
    else if(Modelling=="Multinomial model (if prop odd assumption violated)") {
      fit <- multinom(as.formula(paste(vf_outcome, "~", paste(vf_features, collapse="+"))), data = train)
      # ACCURACY 
      trainPred <- predict(fit, train)
      trainTable <- table(train[[vf_outcome]], trainPred)
      testPred <- predict(fit, newdata=test)
      testTable <- table(test[[vf_outcome]], testPred) 
      trainAcc <- sum(diag(trainTable)) / sum(trainTable) # accuracy rate on test data
      testAcc <- sum(diag(testTable)) / sum(testTable) # accuracy rate on test data
      paste("ACCURACY ON TEST SET:", testAcc)
    }
    else if(Modelling== "Ridge Regression Model (if multicollinearity present)") {
      if(length(vf_features) <= 1 & length(levels(df_filtered[[vf_features[1]]])) <= 2) {
        return("You need to select more than one variable, or a variable with more than 2 levels")
      }
      
      df_nona <- na.omit(df_filtered)
      form <- as.formula(paste(vf_outcome," ~ ", paste(vf_features, collapse="+")))
      mf <- model.frame(form, data=df_nona)
      
      predictors <- as.data.frame(model.matrix(form, mf))
      predictors <- sapply(predictors[,-1], as.integer)
      
      y <- model.response(mf)
      x <- predictors
      
      # Fit model
      set.seed(1981)
      fit <- ordinalNetTune(x, y, nFolds = 3)
      bestLam <- which.max(rowMeans(fit$loglik))
      paste("ACCURACY ON TEST SET:", 1 - mean(fit$misclass))
    }
    else { NULL }
  })
  
  
  ##########################Assumptions output ##########################
  
  output$summary2 <- renderPrint({
    vf_year <- input$year
    vf_outcome <- input$outcome
    vf_features <- input$features
    Modelling <- input$models
    
    if(is.null(vf_year) | is.null(vf_outcome) | is.null(vf_features)){
      return("not enough data")
    }
    
    df_filtered <- df[df$year %in% vf_year, c(vf_outcome, vf_features)]
    partitionTarget <- df_filtered[[vf_outcome]]
    set.seed(1981)
    trainIndex <- caret::createDataPartition(partitionTarget, p=0.7)$Resample1
    train <- df_filtered[trainIndex, ]
    test <- df_filtered[-trainIndex, ]
    
    
    if(Modelling=="Proportional Odds model (default)") {
      fit <- polr(as.formula(paste(vf_outcome, "~", paste(vf_features, collapse="+"))), data = train, Hess = TRUE)
      vif(fit)
    } 
    else if(Modelling=="Multinomial model (if prop odd assumption violated)") {
      fit <- multinom(as.formula(paste(vf_outcome, "~", paste(vf_features, collapse="+"))), data = train)
      vif(fit)
    }
    else { return("no assumption testing available") }
  })
  
  output$summary3 <- renderPrint({
    vf_year <- input$year
    vf_outcome <- input$outcome
    vf_features <- input$features
    Modelling <- input$models
    
    if(is.null(vf_year) | is.null(vf_outcome) | is.null(vf_features)){
      return()
    }
    
    df_filtered <- df[df$year %in% vf_year, c(vf_outcome, vf_features)]
    
    partitionTarget <- df_filtered[[vf_outcome]]
    set.seed(1981)
    trainIndex <- caret::createDataPartition(partitionTarget, p=0.7)$Resample1
    train <- df_filtered[trainIndex, ]
    test <- df_filtered[-trainIndex, ]
    
    if(Modelling=="Proportional Odds model (default)") {
      fit <- polr(as.formula(paste(vf_outcome, "~", paste(vf_features, collapse="+"))), data = train, Hess = TRUE)
      brant(fit)
    }
    else { return("no assumption testing available") }
    
  })
  

  ########################## Plots output ##########################
  
  output$plot <- renderPlot({
    vf_year <- input$year
    vf_outcome <- input$outcome
    vf_features <- input$features
    Modelling <- input$models
    
    if(is.null(vf_year) | is.null(vf_outcome) | is.null(vf_features)){
      return("not enough data")
    }
    
    df_filtered <- df[df$year %in% vf_year, c(vf_outcome, vf_features)]
    partitionTarget <- df_filtered[[vf_outcome]]
    set.seed(1981)
    trainIndex <- caret::createDataPartition(partitionTarget, p=0.7)$Resample1
    train <- df_filtered[trainIndex, ]
    test <- df_filtered[-trainIndex, ]
    
    if(Modelling=="Proportional Odds model (default)") {
      fit <- polr(as.formula(paste(vf_outcome, "~", paste(vf_features, collapse="+"))), data =train, Hess = TRUE)
      plot(allEffects(fit), lines = list(multiline=T))
    } 
    else if(Modelling=="Multinomial model (if prop odd assumption violated)") {
      fit <- multinom(as.formula(paste(vf_outcome, "~", paste(vf_features, collapse="+"))), data = train)
      plot(allEffects(fit), lines = list(multiline=T))
      
    }
    else if(Modelling== "Ridge Regression Model (if multicollinearity present)") {
      df_nona <- na.omit(df_filtered)
      form <- as.formula(paste(vf_outcome," ~ ", paste(vf_features, collapse="+")))
      mf <- model.frame(form, data=df_nona)
      
      predictors <- as.data.frame(model.matrix(form, mf))
      predictors <- sapply(predictors[,-1], as.integer)
      
      y <- model.response(mf)
      x <- predictors
      
      set.seed(1981)
      fit <- ordinalNetTune(x, y, nFolds = 3)
      plot(fit, type = "loglik")
    }
    else { return() }
    
  })
  
}

shinyApp(ui = ui, server = server)




