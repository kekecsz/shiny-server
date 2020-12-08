# Loading packages

library(shiny)
library(shinydashboard)


shinyUI(
  dashboardPage(
      dashboardHeader(title = "Real-time Research Report",
                              notification_module_output("bf_warning")),
    
    ## Define sidebar
  
  
    dashboardSidebar(id = "", sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("th"))
      )),
    
    ## Define app body
  
   dashboardBody(
    tabItems(
      
      # First tab content
      
      tabItem(tabName = "dashboard",
              
              fluidRow(
                textOutput("refresh_time_text")
              ),
              
              fluidRow(
                htmlOutput("final_text"),
                tags$head(tags$style("#final_text{color: black;
                                   font-size: 20px;
                                   font-style: bold;
                                   }"
                )
                )
              ),
              
              
              fluidRow(
                       bayes_plot_module_output("bayes"),
              
                violin_module_output("violin")
              ),
              
              fluidRow(
                       scatter_module_output("scatter"),
               
                       cor_module_output("cor")
                )
              )
      )
    )
  )
)

