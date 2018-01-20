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
library(DT)

wine <- read_csv("data/winemag-data-130k-v2.csv")
wine <- subset(wine, select = -X1 ) %>% 
  filter(is.na(country) == FALSE) %>% 
  filter(is.na(variety) == FALSE) %>% 
  filter(is.na(price) == FALSE)

wine$country[wine$country == "US"] <- "USA"
  
world <- map_data("world")
names(wine)[4] <- "quality"


w_count <- wine %>% 
  group_by(country) %>% 
  mutate(count = n()) %>% # count the # of entries in each country
  select(country, count)

w_count <- unique(w_count)
names(w_count)[1] <- "region"
w_count$region[w_count$region == "US"] <- "USA"
w_geo <- left_join(world,w_count)

list <- sort(unique(wine$country))
list2 <- sort(unique(wine$variety))
#theme = "bootstrap.css",
ui <- fluidPage(theme = "bootstrap.css",
  
  # Application title
  headerPanel("Making Pour Decisions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    wellPanel("Your Selections",
                 selectInput("countryInput", "Country",
                             choices = list,
                             selected = "Argentina"),
                 
                 selectInput("varietyInput", "Variety",
                             choices = list2,
                             selected = "Chardonnay"),
                 
                 sliderInput("priceInput",
                             "Price",
                             min = 0,
                             max = 3500,
                             value = c(0,100)),
                 
                 sliderInput("qualityInput",
                             "Quality",
                             min = 80,
                             max = 100,
                             value = c(80,100))
                 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Location",plotOutput("mymap"), dataTableOutput("wineList")),
        tabPanel("Price vs. Quality",plotOutput("coolplot")))
  )
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    country <- wine %>% filter(country == input$countryInput)
    
    updateSelectInput(session, "varietyInput", choices = unique(country$variety), selected = "Chardonnay")
    
  })
  
  output$mymap <- renderPlot({
    
    highlighted <- subset(world, region == input$countryInput)
    
    ggplot(w_geo) +
       geom_polygon(aes(x=long, y = lat, group = group, fill = count)) + 
       guides(fill = FALSE) + 
       theme_light() +
      theme(axis.title.x=element_blank(),
            axis.title.y = element_blank()) +
       scale_fill_gradient(low = "misty rose",high = "violetred4", name= "Numbers of \nEntries") + 
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
             variety == input$varietyInput) 
    
    ggplot(filtered, aes(quality,price)) +
      geom_point(colour = "violetred4") +
      xlab("Quality") + 
      ylab("Price") + 
      ggtitle("How much might you spend for quality?") +
      theme_minimal()

  })
  
  output$wineList <- renderDataTable({
    DT::datatable(wine %>% filter(country == input$countryInput &
                      quality >= input$qualityInput[1] &
                      quality <= input$qualityInput[2] &
                      price >= input$priceInput[1] &
                      price <= input$priceInput[2] &
                      variety == input$varietyInput) %>% 
              select(designation,winery,quality, price, province),
    options = list(
      pageLength = 5, autoWidth = TRUE
    ), rownames = FALSE)
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)