library(shiny)
library(shinydashboard)

refresh_time = 2000

shinyUI(
  dashboardPage(
    dashboardHeader(title = "TPP Pilot"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome", tabName = "tab_welcome"),
        menuItem("Results summary" , tabName = "res_summary"),
        menuSubItem("Main Confirmatory Analysis", tabName = "conf_analysis"),
        menuSubItem("Time machine", tabName = "test"),
        menuSubItem("Robustness Analysis", tabName = "robust_analysis"),
        menuSubItem("Exploratory analysis", tabName = "explor_analysis"),
        menuItem("Preprint", tabName = "rmarkdown_manuscript")
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "tab_welcome",
                
                fluidRow(
                  h1("Transparent Psi Project - Pilot data results")
                ),
                fluidRow(
                  p("You can follow the results of the Transparent Psi Project here in real time")
                ),
                fluidRow(
                  p("Use the menu items on the left to get the results of the different analyses")
                ),
                fluidRow(
                  textOutput("text_refresh_rate")
                ),
                fluidRow(
                  textOutput("text_refresh1")
                )
        ),
        
        tabItem(tabName = "res_summary",
                
                fluidRow(
                  h1("Summary of the results")
                ),
                fluidRow(
                  textOutput("text_summary")
                ),
                fluidRow(
                  box(plotOutput("plot1a"))
                ),
                fluidRow(
                  p("Note: To support any model, all three Bayes Factor values need to pass the threshold."),
                  textOutput("warning_about_finality_1"),
                  tags$head(tags$style("#warning_about_finality_1{color: red}")
                  )
                ),
                fluidRow(
                  textOutput("text_refresh2")
                )
                
        ),
        
        
        tabItem(tabName = "test",
                
                fluidRow(
                  p("Here you can see how the evidence evolved with the accumulation of data.")
                ),
                
                fluidRow(
                  sliderInput("loop_slider", "Select trial number (may take a few seconds to refresh chart)", min=40, max=150, value=40, step = 10,
                              animate = animationOptions(interval = refresh_time, loop = TRUE))
                ),
                
                fluidRow(
                  h1("analysis")
                ),
                
                fluidRow(
                  box(plotOutput("plot1_loop"))
                ),
                fluidRow(
                  textOutput("text_loop")
                ),
                fluidRow(
                  p("Note: To support any model, all three Bayes Factor values need to pass the threshold"),
                  textOutput("warning_about_finality_2"),
                  tags$head(tags$style("#warning_about_finality_2{color: red}"))
                ),
                fluidRow(
                  textOutput("text_refresh_test")
                )
        ),
        
        
        tabItem(tabName = "conf_analysis",
                
                fluidRow(
                  h1("Main Confirmatory Analysis results")
                ),
                fluidRow(
                  box(plotOutput("plot1b"))
                ),
                fluidRow(
                  p("Note: To support any model, all three Bayes Factor values need to pass the threshold"),
                  textOutput("warning_about_finality_3"),
                  tags$head(tags$style("#warning_about_finality_3{color: red}"))
                ),
                fluidRow(
                  textOutput("text_refresh3")
                )
        ),
        
        tabItem(tabName = "robust_analysis",
                
                fluidRow(
                  h1("Result of the Bayesian Parameter Estimation Robustness Analysis")
                ),
                fluidRow(
                  box(plotOutput("plot2"))
                ),
                fluidRow(
                  p("Note: Results of the Bayesian Parameter Estimation Robustness Analysis"),
                  textOutput("warning_about_finality_4"),
                  tags$head(tags$style("#warning_about_finality_4{color: red}"))
                ),
                fluidRow(
                  textOutput("text_refresh4")
                )
        ),
        
        tabItem(tabName = "explor_analysis",
                
                fluidRow(
                  p("Loading the contents of this page may take a few seconds. Please, wait.")
                ),
                fluidRow(
                  h1("Histogram overlay of the expected and observed distribution of successful guess rate")
                ),
                fluidRow(
                  box(plotOutput("plot3"))
                ),
                fluidRow(
                  textOutput("warning_about_finality_5"),
                  tags$head(tags$style("#warning_about_finality_5{color: red}"))
                ),
                fluidRow(
                  textOutput("text_refresh5")
                )
                
        ),
        
        tabItem(tabName = "rmarkdown_manuscript",
                
                helpText( a("Click here to view the Rmarkdown preprint", href="http://178.128.174.226:3838/Shiny_TPP_Rmarkdown/", target="_blank")
                )
                
        )
      )
      
      
      
    )
  )
)
