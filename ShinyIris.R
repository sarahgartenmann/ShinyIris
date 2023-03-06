# loading the required packages 
library(shiny) # for the shiny App
library(ggplot2) # for plotting
library(gridExtra) # for arranging the plots generatedby ggplot2 neatly

# code for ui.R: controls how the App looks 
ui <- fluidPage(
shinyUI(pageWithSidebar(
  headerPanel('Visualising the Differences in Length and Width of the Petal and Sepal of Three Iris Species'),
  sidebarPanel(
    selectInput('Species', 'Select Species', as.character(unique(iris$Species)))
  ),
  mainPanel(
    plotOutput('irisplot',width = 400, height = 600)
  )
))
)

# code for server.R: controls what the app does
server <- function(input, output) {
    data <- reactive({iris[iris$Species == input$Species,]})
    output$irisplot <- renderPlot({
    # Make four plots for the four parameters
      # Plot 1: Sepal Length 
      plot_SL <- ggplot(data(), aes(Sepal.Length)) +
        geom_histogram(bins=20, fill="blue", color = "black",alpha = .2) +
        geom_vline( aes(xintercept = mean(data()$Sepal.Length)), colour="yellow", size=1, alpha=1) +
        labs(x = "Sepal Length", y = "Frequency", title = paste("Sepal Length, mean =", round(mean(data()$Sepal.Length),2),"cm")) +
        theme(panel.background = element_rect(fill='transparent'), plot.background = element_rect(fill='transparent', color=NA))
      
      # Plot 2: Sepal Width
      plot_SW <- ggplot(data(), aes(Sepal.Width)) +
        geom_histogram(bins=20, fill="blue", color = "black",alpha = .2) +
        geom_vline( aes(xintercept = mean(data()$Sepal.Width)), colour="yellow", size=1, alpha=1) +
        labs(x = "Sepal Width", y = "Frequency", title = paste("Sepal Width, mean =", round(mean(data()$Sepal.Width),2),"cm"))+
        theme(panel.background = element_rect(fill='transparent'), plot.background = element_rect(fill='transparent', color=NA))
      
      # Plot 3: Petal Length
      plot_PL <- ggplot(data(), aes(Petal.Length)) +
        geom_histogram(bins=20, fill="blue", color = "black",alpha = .2) +
        geom_vline( aes(xintercept = mean(data()$Petal.Length)), colour="yellow", size=1, alpha=1) +
        labs(x = "Petal Length", y = "Frequency", title = paste("Petal Length, mean =", round(mean(data()$Petal.Length),2),"cm"))+
        theme(panel.background = element_rect(fill='transparent'), plot.background = element_rect(fill='transparent', color=NA))
      
      # Plot 4: Petal Width
      plot_PW <- ggplot(data(), aes(Petal.Width)) +
        geom_histogram(bins=20, fill="blue", color = "black",alpha = .2) +
        geom_vline( aes(xintercept = mean(data()$Petal.Width)), colour="yellow", size=1, alpha=1) +
        labs(x = "Petal Width", y = "Frequency", title = paste("Petal Width, mean =", round(mean(data()$Petal.Width),2),"cm"))+
        theme(panel.background = element_rect(fill='transparent'), plot.background = element_rect(fill='transparent', color=NA))
      
      # Plotting 4 graphs together
      grid.arrange(plot_SL,plot_SW, plot_PL, plot_PW,nrow=4, ncol=1)
    })
  }
 
shinyApp(ui = ui, server = server)
