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

cols <- list("victim_bgl","victim_prp","victim_vio2","victim_veh", "victim_vio")
names(cols) <- c("Burglary", "Property crime","Sex crime","Vehicle crime", "Violent crime")

cols2 <- list( "recode_age", "recode_educat3", "recode_income", "incity", "gor","recode_marstat2","recode_race", "sex",  "recode_workstat1")
names(cols2) <- c("Age", "Education", "Income", "Inner/non-inner city","Location", "Marital status", "Race", "Sex", "Work Status")

ui <- fluidPage(
  titlePanel("Frequency Distributions: crime types and victims"),
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
      
      radioButtons(inputId="variables_cops","Did police know about the incident?", choices=c("no", "yes"), selected = "single"),
      radioButtons(inputId="variables_crime","Offence type",choices=cols, selected = "single"),
      radioButtons(inputId="variables_dem","Demographic Variables",choices=cols2, selected = "single"),
    ),
    
    mainPanel(
      h5(),
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("distPlot")),
                  tabPanel("Brief Tutorial", br(),
                           fluidRow(column(12,
                                           withMathJax(),
                                           h2("How to use the app"),
                                           includeMarkdown("tutorial_app2.Rmd"),
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
    vf_var_crime <- input$variables_crime
    vf_var_dem <- input$variables_dem
    vf_var_cops <- input$variables_cops
    
    if(is.null(vf_year) | is.null(vf_var_crime) | is.null(vf_var_dem) | is.null(vf_var_cops)) {
      return()
    }
    
    df_filtered <- df[df$year %in% vf_year & df$recode_reported==vf_var_cops,]
    if( length(unique(df_filtered[,vf_var_dem]))==1) {
      text = paste("Not enough data for selected year. Please choose another combination.")
      ggplot() + annotate("text", x = 4, y = 25, size=4, label = text) + theme_void()
    } else {
      
      ggplot(df_filtered, aes(x=df_filtered[,vf_var_crime], fill=as.factor(df_filtered[,vf_var_dem]))) +
        geom_bar(position = "stack", stat="count") +
        xlab(label="Victim of a particular crime") + ylab(label = "Number of respondents (count)") +
        theme(legend.title = element_blank())
    }
  })
}

shinyApp(ui = ui, server = server)
