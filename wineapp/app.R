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
library(maps)
library(shinythemes)
library(plotly)
library(devtools)

wine <- read_csv("data/winemag-data-130k-v2.csv")
wine <- subset(wine, select = -X1 ) %>% 
  filter(is.na(country) == FALSE) %>% 
  filter(is.na(variety) == FALSE) %>% 
  filter(is.na(price) == FALSE)

wine$country[wine$country == "US"] <- "USA"

wine$country[wine$country == "England"] <- "UK"
  
world <- map_data("world")
names(wine)[4] <- "quality"

list <- sort(unique(wine$country))
list2 <- sort(unique(wine$variety))
ui <- fluidPage(theme = shinytheme("cosmo"),
  
  # Application title
 titlePanel("Making Pour Decisions"),
  
  # Sidebar with 4 inputs: 2 sliders and 2 dropdown menus 
  sidebarLayout(
    sidebarPanel("Choose your selections!",
                 selectInput("countryInput", "Country",
                             choices = list,
                             selected = "Argentina"),
                 
                 selectInput("varietyInput", "Variety",
                             choices = list2,
                             selected = "Chardonnay"),
                 
                 sliderInput("priceInput",
                             "Price",
                             pre = "$",
                             min = min(wine$price),
                             max = max(wine$price),
                             value = c(0,100))
                 
    ),
    
    # Show the main panel with two tabs - first with a graph and 
    mainPanel(
      tabsetPanel(
        tabPanel("Location",plotOutput("mymap"), dataTableOutput("wineList")),
        tabPanel("Price vs. Quality",plotlyOutput("coolplot"),
                 tableOutput("averageprice"), dataTableOutput("wineList2")))
  )
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # filter the dropdown selections based on the country
  observe({
    country <- wine %>% 
      filter(country == input$countryInput)
    
    updateSelectInput(session, "varietyInput", choices = unique(country$variety), selected = "Chardonnay")
    
  })
  
  # filter the price options based on the quality and variety
  observe({
    quality <- wine %>% 
      filter(variety == input$varietyInput,
             country == input$countryInput)
    
    updateSliderInput(session, "priceInput", 
                      min = min(quality$price), max = max(quality$price), value = c(min,max))
  })
  
  output$mymap <- renderPlot({
    
    highlighted <- subset(world, region == input$countryInput)
    
    w_count <- wine %>% 
      filter(variety == input$varietyInput) %>% 
      group_by(country) %>% 
      mutate(count = n()) %>% # count the # of entries in each country
      select(country, count)
    
    w_count <- unique(w_count)
    names(w_count)[1] <- "region"
    w_count$region[w_count$region == "US"] <- "USA"
    w_geo <- left_join(world,w_count)
    
    ggplot(w_geo) +
       geom_polygon(aes(x=long, y = lat, group = group, fill = count)) + 
       theme_light() +
      theme(axis.title.x=element_blank(),
            axis.title.y = element_blank()) +
      scale_fill_gradient(low = "misty rose",high = "violetred4",
                          guide = "colourbar", name = "Number of \nWines") + 
      geom_polygon(data = highlighted, aes(x = long, y = lat,group = group), fill = NA, colour = "black")

  })
  
  output$coolplot <- renderPlotly({
    
    filtered <-
      wine %>%
      filter(country == input$countryInput &
             price >= input$priceInput[1] &
             price <= input$priceInput[2] &
             variety == input$varietyInput) 
    
    p <- ggplot(filtered, aes(quality,price)) +
      geom_jitter(colour = "violetred4", aes(text = sprintf("Quality: %s<br>Price: $%s<br>Winery: %s",
                                            quality, price, winery))) +
      xlab("Quality") + 
      ylab("Price") + 
      ggtitle("How much might you spend for quality?") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")

  })
  
  output$wineList <- renderDataTable({
    DT::datatable(wine %>% filter(country == input$countryInput &
                      price >= input$priceInput[1] &
                      price <= input$priceInput[2] &
                      variety == input$varietyInput) %>% 
              select(designation,winery,quality, price) %>% 
                arrange(desc(quality)),
              colnames = c("Designation","Winery","Quality","Price ($)"),
    options = list(
      pageLength = 5, autoWidth = TRUE
    ), rownames = FALSE)
  })
  
  output$averageprice <- renderTable({
    sumtable <- wine %>% filter(country == input$countryInput &
                                    price >= input$priceInput[1] &
                                    price <= input$priceInput[2] &
                                    variety == input$varietyInput)
    
    sumdf <- data_frame()
    sumdf <- cbind(round(mean(sumtable$price),2),round(mean(sumtable$quality),2)) 
    colnames(sumdf) <- c("Average Price","Average Quality")
    
    sumdf
  })
  
  output$wineList2 <- renderDataTable({
    DT::datatable(wine %>% filter(country == input$countryInput &
                                    price >= input$priceInput[1] &
                                    price <= input$priceInput[2] &
                                    variety == input$varietyInput) %>% 
                    select(designation,winery,quality, price) %>% 
                    arrange(desc(quality)),
                  colnames = c("Designation","Winery","Quality","Price ($)"),
                  options = list(
                    pageLength = 5, autoWidth = TRUE
                  ), rownames = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)