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
library(leaflet)
library(DT)

wine <- read_csv("data/winemag-data-130k-v2.csv")
wine <- subset(wine, select = -X1 )
world <- map_data("world")
names(wine)[4] <- "quality"

w_count <- wine %>%
  group_by(country) %>% 
  mutate(count = n()) %>% # count the # of entries in each country
  select(country, count)

w_count <- unique(w_count)
names(w_count)[1] <- "region"

w_geo <- left_join(world,w_count)

list <- sort(unique(wine$country))
list2 <- sort(unique(wine$variety))
#theme = "bootstrap.css",
ui <- fluidPage(
  
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
                 
                 sliderInput("qualityInput",
                             "Quality",
                             min = 0,
                             max = 100,
                             value = c(70,100)),
                 
                 selectInput("countryInput", "Country",
                                    choices = list,
                                    selected = "Argentina"),
                 
                 selectInput("varietyInput", "Variety",
                             choices = list2,
                             selected = "Chardonnay")
                 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("mymap"),plotOutput("coolplot"), DT::dataTableOutput("wineList"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  output$mymap <- renderPlot({
    
    highlighted <- subset(world, region == input$countryInput)
    
    ggplot(w_geo) +
       geom_polygon(aes(x=long, y = lat, group = group, fill = count)) + 
       guides(fill = FALSE) + 
       theme_minimal() +
       scale_fill_gradient(low = "misty rose",high = "violetred4", name= "Numbers of \nEntries") + 
       xlab("Longitude") + 
       ylab("Latitude") +
        geom_polygon(data = highlighted, aes(x = long, y = lat,group = group), fill = NA, colour = "black")
      
      
      
    
  })
  
  output$coolplot <- renderPlot({
    
    filtered <-
      wine %>%
      filter(country == input$countryInput &
            quality >= input$qualityInput[1] &
              quality <= input$qualityInput[2] &
             price >= input$priceInput[1] &
             price <= input$priceInput[2] &
             variety == input$varietyInput ) 
    
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
  
  output$test <- renderPlot({
    
    filtered <-
      wine %>%
      filter(country == input$countryInput &
               quality >= input$qualityInput[1] &
               quality <= input$qualityInput[2] &
               price >= input$priceInput[1] &
               price <= input$priceInput[2] &
               variety == input$varietyInput ) 
  
    ggplot(filtered, aes(quality,price)) +
      geom_point(colour = "violetred4") +
      xlab("Quality") + 
      ylab("Price") + 
      ggtitle("How much might you spend for quality?") +
      theme_minimal()
})
  
  output$wineList <- DT::renderDataTable({
    wine %>% filter(country == input$countryInput &
                      quality >= input$qualityInput[1] &
                      quality <= input$qualityInput[2] &
                      price >= input$priceInput[1] &
                      price <= input$priceInput[2] &
                      variety == input$varietyInput ) 
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)