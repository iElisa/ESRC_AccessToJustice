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
library(clustMixType)
library(ggplot2)
library(klaR)
library(DT)

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


cols1 <- list("recode_reported", "victim_bgl","victim_prp", "victim_veh", "victim_vio", "victim_vio2")
names(cols1) <- c("Police knew", "Burglary", "Property Crime", "Vehicle Crime", "Violent Crime", "Sex Crime")

cols2 <- list("sex", "recode_race", "recode_age", "recode_educat3", "recode_income", "gor")
names(cols2) <- c("Sex", "Race", "Age", "Education", "Income", "Location")

ui <- fluidPage(
  titlePanel("Clustering"),
  p(HTML("This app is one of the outcomes of the ESRC-funded project: 'Victims’ Access to Justice through English Criminal Courts, 1675 to the present'. 
         A special aggregated and appended subset of the CSEW is used here. This only includes respondents identified as victims")),
  sidebarLayout(
    sidebarPanel(
      
      # Use SelectInput for survey years only
      selectInput(inputId="year", label = "Year", 
                  choices=c(1982, 1984, 1988, 1992, 1994, 1996, 1998, 
                            2000, 2001,  2002,  2003,  2004, 2005, 2006,  2007,  2008,  2009,  
                            2010,  2011, 2012, 2013, 2014, 2015, 2016, 2017), 
                  multiple=T),
      selectInput(inputId="variables_cjs",label = "CJS Variables",choices=cols, multiple=FALSE),
      selectInput(inputId="variables_crime", label = "Crime Variables",choices=cols1, multiple=TRUE),
      selectInput(inputId="variables_demo", label = "Demographic Variables",choices=cols2, multiple=TRUE),
      sliderInput(inputId="clusters", label = "Number of clusters", min = 1, max = 35, value = 1)
    ),
    mainPanel(
      h5(),
      tabsetPanel(type = "tabs", 
                  tabPanel("Summary of cluster centroids", DT::dataTableOutput("table")),
                  tabPanel("The Algorithm", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2("Brief Explanation of the Algorithm"),
                                           includeMarkdown("kproto.Rmd"),
                           ))),
                  tabPanel("Brief Tutorial", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2("How to use the app"),
                                           includeMarkdown("tutorial_app4.Rmd"),
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
  
  output$table <- renderDataTable({
    vf_year <- input$year
    vf_var_crime <- append(input$variables_crime, c("ncrime1"))
    vf_var_demo <- input$variables_demo
    vf_var_cjs <- input$variables_cjs
    vf_var_clusters <- input$clusters
    
    if(is.null(vf_year) | is.null(vf_var_crime) | is.null(vf_var_demo) | is.null(vf_var_cjs) | is.null(vf_var_clusters)) {
      return()
    }
    
  
    # Filtering by year and selected variables - removing NA 
    df_filtered <- df[df$year %in% vf_year, c(vf_var_crime, vf_var_demo, vf_var_cjs)]
    df_filtered <- data.frame(df_filtered )
    df_filtered <- na.omit(df_filtered)
     
    if(dim(df_filtered)[1]==0) {
       return("Not enough data to process.")
    }
     
    set.seed(1981)
    clustMixType::lambdaest(df_filtered, num.method = 1, fac.method = 1, outtype = "numeric")
      
    set.seed(2019)
    kpres <- clustMixType::kproto(df_filtered, k=vf_var_clusters, lambda = clustMixType::lambdaest(df_filtered), na.rm=F)
      
    kpres$centers
  })
}

shinyApp(ui = ui, server = server)




