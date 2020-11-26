# Module UI function

violin_module_output <- function(id){
  
  ns <- NS(id)
  
  box(title = "Relationship between gender and height",
      solidHeader = T,
      plotOutput(ns("plot"),
                 height = 250))
  
}

# Module server function

violin_module <- function(input, output, session, data){
  
  output$plot <- renderPlot({
    
    violin_plot(data = data())
    
  })
}

# Module util functions

violin_plot <- function(data){
  
  temp %>%
    ggplot() +
    aes(y = Height, x = Gender) +
    geom_violin(aes(fill = Gender)) +
    geom_boxplot(width = 0.2)+
    geom_jitter(width = 0.2)+
    labs(x = "Gender",
         y = "Height") +
    theme_minimal()
}