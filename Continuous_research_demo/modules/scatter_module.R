# Module UI function

scatter_module_output <- function(id){
  
  ns <- NS(id)
  
  box(title = "relationship between Height and shoe size",
      status = "primary",
      solidHeader = T,
      plotOutput(ns("plot"),
                 height = 250))
  
}

# Module server function

scatter_module <- function(input, output, session, data){
  
    output$plot <- renderPlot({
      
      scatter_plot(data = data())
      
      })
    }

# Module util functions

scatter_plot <- function(data){
  
  data %>%
    ggplot() +
    aes(x = Shoesize,
        y = Height) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(y = "Height",
         x = "Shoe size") +
    theme_minimal()
  
}