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

cols <- list("jobcps","jobjud", "jobmag", "jobprob", "jobjuv", "cjscpsb", "cjscrt2a", "cjscrt2b", "cjsovb1", 
             "fairatt1", "fairova1", "confoff", "confvict")
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


cols2 <- list("sex", "recode_race", "recode_age", "recode_income", "recode_educat3", "recode_workstat1", "recode_marstat2", "incity", "gor")

names(cols2) <- c("Sex", "Race", "Age", "Income", "Education", "Work Status", "Marital status", "Inner/non-inner city", "Location")


ui <- fluidPage(
  titlePanel("Frequency Distributions: Victims' attitudes to the Criminal Justice System"),
  p(HTML("This app is one of the outcomes of the ESRC-funded project: 'Victims’ Access to Justice through English Criminal Courts, 1675 to the present'. 
         A special aggregated and appended subset of the CSEW is used here. This only inclused respondents identified as victims. Please note, no survey weights were used in this app.")),
  sidebarLayout(
    sidebarPanel(
      
      # Use SelectInput for survey years only
      selectInput(inputId="year", label = "Year", 
                  choices=c(1982, 1984, 1988, 1992, 1994, 1996, 1998, 
                            2000, 2001,  2002,  2003,  2004, 2005, 2006,  2007,  2008,  2009,  
                            2010,  2011, 2012, 2013, 2014, 2015, 2016, 2017), 
                  multiple=T),
      selectInput(inputId="variables_cjs","CJS Variables",choices=cols, multiple=FALSE),
      selectInput(inputId="variables_dem","Demographic Variables",choices=cols2, multiple=FALSE),
    )
    
    
    ,
    mainPanel(
      h5(),
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("distPlot")),
                  tabPanel("Brief Tutorial", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2("How to use the app"),
                                           includeMarkdown("tutorial_app1.Rmd"),
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
                  p(HTML("DISCLAIMER: this modelling has exclusively research and educational purposes. It is not intended for decision-making. Even though a lot of work went into making sure no mistakes are present, we might have missed something. Please email us at  <a href=http://esrcvictimsproject.shiny@gmail.com>  esrcvictimsproject.shiny@gmail.com </a> if you notice anything inaccurate"))
    )
  )
))


server <- function(input, output) {
  
  df <- read.csv("./ESRC_df_app.csv", sep = ",", header=TRUE, na.strings = c("","NA","-7", "-8", "-9", "98", "99"))
  
  output$distPlot <- renderPlot({
    vf_year <- input$year
    vf_var_cjs <- input$variables_cjs
    vf_var_dem <- input$variables_dem
    
    if(is.null(vf_year) | is.null(vf_var_cjs) | is.null(vf_var_dem)) {
      return()
    }

    df_filtered <- df[df$year %in% vf_year,]
    if( length(unique(df_filtered[,vf_var_dem]))==1) {
      
      text = paste("Not enough data for selected year. Please choose another combination.")
      ggplot() + annotate("text", x = 4, y = 25, size=4, label = text) + theme_void()
      
    } else {
      
      ggplot(df_filtered, aes(x=df_filtered[,vf_var_cjs], fill=as.factor(df_filtered[,vf_var_dem]))) +
        geom_bar(position = "stack", stat="count") +
        xlab(label="Level of confidence/satisfaction") + ylab(label="Number of responses (count)") +
        theme(legend.title = element_blank()) 
    }
  })
}

shinyApp(ui = ui, server = server)



