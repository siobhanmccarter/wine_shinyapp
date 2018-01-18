#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(gridExtra)

wine <- read_csv("data/winemag-data-130k-v2.csv")
wine <- subset(wine, select = -X1 )
names(wine)[4] <- "quality"

list <- sort(unique(wine$country))
list2 <- sort(unique(wine$variety))

ui <- fluidPage(theme = "bootstrap.css",
  
  # Application title
  titlePanel("Wine App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel("User Inputs",
                 
                 sliderInput("priceInput",
                             "Price",
                             min = 0,
                             max = 3500,
                             value = c(20,100)),
                 
                 selectInput("countryInput", "Country",
                                    choices = list,
                                    selected = "Argentina"),
                 
                 selectInput("varietyInput", "Variety",
                             choices = list2,
                             selected = "Chardonnay")
                 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("coolplot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$coolplot <- renderPlot({
    
    filtered <-
      wine %>%
      filter(country == input$countryInput &
             price >= input$priceInput[1] &
             price <= input$priceInput[2] &
             variety == input$varietyInput) 
    
    #ggplot(filtered, aes_string(x = "year",input$yInput)) +
      #geom_histogram(aes_string(colour = "country")) + 
      #ggtitle(input$titleInput) + 
      #xlab("Year") 
    ggplot(filtered, aes(quality,price)) +
      geom_point(colour = "violetred4") +
      xlab("Quality") + 
      ylab("Price") + 
      ggtitle("How much might you spend for quality?") +
      theme_minimal()

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)