# Loading packages

library(shiny)
library(shinydashboard)
suppressPackageStartupMessages(library(tidyverse))
library(BayesFactor)
library(gsheet)


setwd("C:/Users/kekec/Dropbox/_Real_time")


source("Continuous_research_demo/modules/scatter_module.R")
source("Continuous_research_demo/modules/violin_module.R")
source("Continuous_research_demo/modules/cor_module.R")
source("Continuous_research_demo/modules/bayes_plot_module.R")
source("Continuous_research_demo/modules/notification_module.R")
source("Continuous_research_demo/modules/generate_preprint_module.R")


# Define server logic

shinyServer(function(input, output) {
  

  # Read in the url of the google spreadsheet that contains the output of your google form
  
  url <- "https://docs.google.com/spreadsheets/d/1pafm2WKsfdXiJfw1K5aJm1rggRm-Zt9UAbN32y1MdqI/edit?usp=sharing"
  
##  # Create connection through the url with the spreadsheet
  
##  ss <- suppressMessages(gs_url(url, visibility = "public"))
  
  # Set the time interval for the refresh of the app (in milliseconds)
  
  refresh_time <- 3000
  
  # Create an empty reactive value for storing the output values
  
  values <- reactiveValues()
  
  # Set the default trigger value
  
  values$trigger <- NULL
  
  # Create an empty dataframe for storing the data
  
  values$form_data <- tibble()
  
  # Create a dataframe for storing the results of the Bayes factor calculation
  
  values$bf_data <- tibble(BF = numeric(0),
                           n_participant = integer(0))
  
  # Create a reactive expression for reading in the data from the spreadsheet
  
  read_data <- reactive({
    
    # Run the reactive expression after the time specified in [refresh_time]
    invalidateLater(refresh_time)
    
    # Read in data and transform variable names
    temp <- gsheet2tbl(url)
    names(temp) = c("Timestamp", "Gender", "Shoesize", "Height")
  
    temp
  })
  
  # Create an expression that triggers the analysis if there are new responses to the form
  
  observe({
    
    invalidateLater(refresh_time)
    
    if(!identical(values$form_data, read_data())){
      
      values$trigger <-1
      values$form_data <- read_data()
      
    }else{
      
      values$trigger <- NULL}
  })
  
  # Create an expression for plotting the results of the analysis that is triggered by new responses
  
  observeEvent(values$trigger,{
    
    # Create a scatterplot 
    callModule(scatter_module, "scatter", data = reactive(values$form_data))
    
    # Create a table that shows correlation between variables
    callModule(cor_module, "cor", data = reactive(values$form_data))
    
    # Create a violinplot
    callModule(violin_module, "violin", data = reactive(values$form_data))
    
  })

  
  # refresh time text
  output$refresh_time_text <- renderText({
    invalidateLater(refresh_time)
    
    paste("The data on this page was last refreshed at:  ", Sys.time(), "  (time zone:  ", Sys.timezone(), " ).", sep = "")
    
  })
  
  
    
  # Create An expression for the Bayes factor analysis
  observeEvent(values$trigger,{
    
    # Save data for the BF analysis
    bf_temp <- values$form_data %>% 
      filter(!is.na(Shoesize)) %>% 
      mutate(Gender = as.factor(Gender))
    
    # Count the number of observations
    bf_temp_n <- bf_temp %>%
      count() %>% 
      pull(n)
    
    # Set the minimum number of participants required for the analysis
    bf_min_participant <- 5
    
    
    if(bf_temp_n > bf_min_participant){
      
      output$bf_warning <- NULL
      if(nrow(values$bf_data) == 0){
        
        for(i in bf_min_participant:bf_temp_n){
          bf_temp_sub <- bf_temp[1:i,]
          
          new_line <- tibble(BF = as.numeric(as.vector(correlationBF(bf_temp_sub$Shoesize, bf_temp_sub$Height))),
                             n_participant = as.numeric(i))
          
          values$bf_data <- bind_rows(values$bf_data, new_line)}
        
        }else{
          
        new_participant_n <- max(values$bf_data$n_participant)
        
        for(i in new_participant_n:bf_temp_n){
          bf_temp_sub <- bf_temp[1:i,]
          new_line <- tibble(BF = as.numeric(as.vector(correlationBF(bf_temp_sub$Shoesize, bf_temp_sub$Height))),
                             n_participant = as.numeric(i))
          values$bf_data <- bind_rows(values$bf_data, new_line)}
        }
     
      
      
    # Plot Bayes factors
    
    callModule(bayes_plot_module, "bayes", data = reactive(values$bf_data), bf_min_participant = bf_min_participant)
    
    }else{
      
      output$bf <- NULL
      
      # Create a notification icon if the sample size is not enough for BF analysis
      
      callModule(notification_module, "bf_warning")

    }
    
    
    values$inference_BF_current <- if(values$bf_data$BF[nrow(values$bf_data)] < 0.33){"H0"} else if(values$bf_data$BF[nrow(values$bf_data)] > 3){"H1"} else {"Inconclusive"}
    values$evidence_BF_current <- if(values$bf_data$BF[nrow(values$bf_data)] < 0.33 | values$bf_data$BF[nrow(values$bf_data)] > 3){"moderate"} else if(values$bf_data$BF[nrow(values$bf_data)] < 0.1 | values$bf_data$BF[nrow(values$bf_data)] > 10){"strong"} else if(values$bf_data$BF[nrow(values$bf_data)] < 0.033 | values$bf_data$BF[nrow(values$bf_data)] > 30){"very strong"} else {"anekdotal"}
    
    
    
    general_text = if(values$inference_BF_current == "H1"){
      paste(" Based on the observed data it is <b> ", round(values$bf_data$BF[nrow(values$bf_data)],0),
            " times more likely </b> that the data came from a population where <b> H1 is true </b> compared to a population where H0 is true",
            sep = "")
    } else if(values$inference_BF_current == "H0"){
      paste(" Based on the observed data it is <b> ", round(1/(values$bf_data$BF[nrow(values$bf_data)]),0),
            " times more likely </b> that the data came from a population where <b> H0 is true </b> compared to a population where H1 is true",
            sep = "")
    } else if(values$inference_BF_current == "Inconclusive" & values$bf_data$BF[nrow(values$bf_data)] < 1){
      paste(" Based on the observed data it is <b> ", round(1/(values$bf_data$BF[nrow(values$bf_data)]),0),
            " times more likely that the data came from a population where <b> H0 is true </b> compared to a population where H1 is true. However, this study outcome did not reach the pre-specified criteria of strong support for either model.",
            sep = "")
    } else if(values$inference_BF_current == "Inconclusive" & values$bf_data$BF[nrow(values$bf_data)] > 1){
      paste(" Based on the observed data it is <b> ", round(values$bf_data$BF[nrow(values$bf_data)],0),
            " times more likely that the data came from a population where <b> H1 is true </b> compared to a population where H0 is true. However, this study outcome did not reach the pre-specified criteria of strong support for either model.",
            sep = "")
    } else {paste("There is no conclusive result yet.",
                  sep = "")}
    
    if(values$bf_data$BF[nrow(values$bf_data)] > 1){
    evidence_level_text = paste0("<b> The level of Bayesian evidence supporting H1 is ", values$evidence_BF_current, " </b> .")
    } else if(values$bf_data$BF[nrow(values$bf_data)] < 1) {
    evidence_level_text = paste0("<b> The level of Bayesian evidence supporting H0 is ", values$evidence_BF_current, " </b> .")
    } else {evidence_level_text = " "}
    
    output$final_text <- renderText({
      
      paste0(general_text, ". ", evidence_level_text)
      
    })

  })
  
  # Generate and download report
  
})



