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
library(dplyr)
library(ggplot2)


cols <- list("recode_jobcps","recode_jobjud", "recode_jobmag", "recode_jobprob", "recode_jobjuv", "recode_cjscpsb", "recode_cjscrt2a", "recode_cjscrt2b", "recode_cjsovb1", 
             "recode_fairatt1", "recode_fairova1", "recode_confoff", "recode_confvict")
names(cols) <- c("How good a job do you think the CPS are doing?",
                 "How good a job do you think judges are doing?",
                 "How good a job do you think magistrates are doing?",
                 "How good a job do you think the probation services are doing?",
                 "How good a job do you think the juvenile courts are doing?",
                 "How confident are you Crown Prosecution Service is effective at prosecuting?",
                 "How confident are you Courts are dealing with cases promptly?",
                 "How confident are you Courts are effective at giving punishments?",
                 "How confident are you - Criminal Justice System as a whole is effective?",
                 "How much do you agree or disagree that the Criminal Justice System gives victims and witnesses the support they need?",
                 "How confident are you that the Criminal Justice System as a whole is fair?",
                 "How confident you are that CJS is effective in bringing people to justice?",
                 "How confident are you that the CJS meets the needs of victim?")

ui <- fluidPage(
  titlePanel("Attitudes towards the CJS: Patterns over time"),
  p(HTML("This app is one of the outcomes of the ESRC-funded project: 'Victims’ Access to Justice through English Criminal Courts, 1675 to the present'. 
         A special aggregated and appended subset of the CSEW is used here. This only inclused respondents identified as victims. Please note, no survey weights were used in this app.")),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId="variables_cjs",label = "CJS Variables",choices=cols, multiple=FALSE),
      selectInput(inputId="recode_reported", label = "Did police know about the incident?",choices=c("no", "yes"), multiple=FALSE),
      selectInput(inputId="sex", label = "Victim's Sex",choices=c("male", "female"), multiple=TRUE),
      selectInput(inputId="recode_race", label = "Victim's Race",choices=c("white", "black", "asian", "other"), multiple=TRUE),
      selectInput(inputId="recode_income", label = "Victim's Income",choices=c("low", "mid", "top"), multiple=TRUE),
      selectInput(inputId="gor", 
                  label = "Victim's Geographical Area",
                  choices=c("north east", "north west", "Yorks&Humberside", "east midlands", "west midlands", 
                            "east of England", "London", "south east", "south west", "Wales", "Scotland"), 
                  multiple=TRUE),
    ),
    mainPanel(
      h5(),
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("chart")),
                  tabPanel("Brief Tutorial", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2("How to use the app"),
                                           includeMarkdown("tutorial_app3.Rmd"),
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
  
  df <- read.csv("./ESRC_df_app_numeric.csv", sep = ",", header=TRUE, na.strings = c("","NA","-7", "-8", "-9", "98", "99"))
  
  output$chart <- renderPlot({
    vf_var_cjs <- toString(input$variables_cjs)
    vf_var_cops <- input$recode_reported # "yes" = 1, "no" = 0
    vf_var_sex <- input$sex              # "male" = 1, "female" = 2
    vf_var_race <- input$recode_race     # "white"= 1,"black"= 2, "Indian/Pakistani/Bamgladeshi"= 3, "Mixed/Other"= 4
    vf_var_age <- input$recode_age       # "under 25" = 1, "26-39"=2, "40-59"=3, "60-79"=4, "over 80"=5
    vf_var_edu <- input$recode_educat3   # "high"=1, "A-levels"=2, "GCSE"=3, "other/no"=4
    vf_var_income <- input$recode_income # "low"=1, "mid"=2, "top"=3
    vf_var_gor <- input$gor              # "north east"=1, "north west"=2, "Yorks&Humberside"=3, "east midlands"=4, "west midlands"=5, "east of England"=6, 
                                         # "London"=7, "east south"=8, "south west"=9, "Wales"=10, "Scotland"=11

    if(is.null(vf_var_cjs) |
       is.null(vf_var_cops) |
       is.null(vf_var_sex) |
       is.null(vf_var_race) |
       is.null(vf_var_income) |
       is.null(vf_var_gor)) {
      return()
    }
   
    df_filtered <- df[df$recode_reported==vf_var_cops &
                        df$sex==vf_var_sex &
                        df$recode_race==vf_var_race &
                        df$recode_income==vf_var_income &
                        df$gor==vf_var_gor,]
    
    likert_values <- sort(levels(factor(df[,vf_var_cjs])))

    result <- matrix(nrow=length(likert_values))
    yearNames <- c()
    for (yy in 1982:2017) {
      col_year_result <- c()
      for(likert in likert_values) {
        temp <- df_filtered[df_filtered$year==yy, vf_var_cjs]
        num_values <- length(temp[!is.na(temp)]) # number people responding
        temp <- temp[temp==likert & !is.na(temp)]
        if(num_values > 0) { 
          avg_for_likert <- length(temp) / num_values 
        } else { 
          avg_for_likert <- 0 
        }
        col_year_result <- append(col_year_result, avg_for_likert)
      }
      yearNames <- append(yearNames, as.character(yy))
      result <- cbind(result,col_year_result)
    }
    # Removing first column - which is a column of NA ...
    result <- result[,-1]
    dimnames(result) <- list(likert_values,yearNames)
   
    par(mfrow=c(2,1))
    grp <- df_filtered %>% dplyr::filter(!is.na(df_filtered[,vf_var_cjs])) %>% dplyr::group_by(year)
    chartData <- dplyr::summarise(grp, mean=mean(get(vf_var_cjs)), sd=sd(get(vf_var_cjs)))

    plot(chartData$mean ~ chartData$year, type='l', xlim=c(1980, 2018), ylim=c(0, 5), xlab='Year', ylab='Sentiment', main ='Average Likert Score across Years')
    lines(chartData$mean+1.96*chartData$sd ~ chartData$year, type='l', lty=3)
    lines(chartData$mean-1.96*chartData$sd ~ chartData$year, type='l', lty=3)
    matplot(t(result[,2:ncol(result)]),
            type="b",
            main="Share of Responses per Likert Category across Years", 
            at = 0:(length(yearNames)-1), 
            labels = yearNames, 
            xlab='Year', 
            ylab='%  Responses per Likert Category')
  })
}

shinyApp(ui = ui, server = server)


