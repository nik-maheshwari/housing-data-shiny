#-------------------------------------------------------------------------------
# Shiny application protoype
#-------------------------------------------------------------------------------

# load required libraries
library(pxR)
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)

# read .px file of average rent between 2008 and 2017
dublin <- read.px("Avg_rent_monthly_2007-2018Q Dublin.px")
# convert px to R dataframe
data2 <- as.data.frame(dublin)
data2 <- data2[complete.cases(data2), ] # remove rows containing NA
temp <- str_split_fixed(data2$Quarter, "Q", 2) # split quarter column
data2$Year <- as.numeric(temp[, 1]) # create new column with year

# group by location and year, and get average rent price
year_rent2 <- data2 %>%
  group_by(Location, Year) %>%
  summarise(avg_price = mean(value, na.rm = T))

## Reading file "sale_price_cleaned.csv"

sale <- read.csv("sale_price_cleaned.csv", header = TRUE)
sale_dub <- sale %>% filter(str_detect(Postal.Code, "Dublin")) # get only Dublin sale data
temp2 <- str_split_fixed(sale_dub$Date.of.Sale..dd.mm.yyyy., "/", 3) # split date of sale column
sale_dub$Year <- as.numeric(temp2[, 3])# create new column with year

# group by Postal Code and year, and get average sale price
year_sale <- sale_dub %>% 
  group_by(Postal.Code, Year) %>%
  summarise(avg_price = mean(Price, na.rm = T))

year_rent2$Location <- as.character(year_rent2$Location)
year_sale$Postal.Code <- as.character(year_sale$Postal.Code)
# merging sale and rent prices, by comparing Location and Postal.Code, and Year
combine <- left_join(year_rent2, year_sale, by = c("Location" = "Postal.Code", "Year"))
combine <- as.data.frame(combine[complete.cases(combine),]) # remove rows containing NA
colnames(combine) <- c("Postcode", "Year", "Rent", "Sale") # rename column names
# str(combine) # make sure the data is like what we want
meltDF <- melt(combine, id = c("Year", "Postcode")) # melt data in the long format, required for making plot
meltDF$Postcode <- as.factor(meltDF$Postcode)

#--------------------------------time series example
# dub1 <- combine[combine$Postcode == "Dublin 1",]
# mts1 = MTS::VAR(dub1, 2)
#--------------------------------
#------------------------------------------------------------------------------
# Application's User Interface (UI) layout
#------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Application"),
  
  sidebarLayout(
    sidebarPanel(
      # each of this allows for user interaction. Add anything that you think 
      # will help user to filter data or run models. Basically, anything that
      # you want from the user should go here.
      
      selectInput("postcode", "Select Postcode", multiple = TRUE, choices = sort(unique(meltDF$Postcode)), selected = "Dublin 1"),
      selectInput("type", "Select Property type", choices = sort(unique(data2$Property.Type)), selected = data2[1, c("Property.Type")]),
      selectInput("bedroom", "Select number of bedrooms", choices = sort(unique(data2$Number.of.Bedrooms)), selected = data2[1, c("Number.of.Bedrooms")])
      
    ),
    #  This is what the user will see. 3 tabs, one for average rent plot, 
    #  one for data table, one for comparing sale and rent
    #  You can rearrange this in whichever way you want to show the data.
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Average Rent", plotlyOutput("plot")),
                  tabPanel("Data table", dataTableOutput("table")),
                  tabPanel("Comparison", plotlyOutput("saleRentCompare"))
      )
    )
  )
)

#------------------------------------------------------------------------------
# Application's server logic
#------------------------------------------------------------------------------

server <- function(input, output, session){
  
  #  Anything that depends on user's input becomes reactive/dependent.
  #  here, df is a reactive dataframe variable which depends on input location,
  #  proprty type and number of bedrooms.
  df <- reactive(data2 %>% 
                   filter(Location %in% input$postcode) %>%
                   filter(Property.Type %in% input$type) %>%
                   filter(Number.of.Bedrooms %in% input$bedroom))
  
  #  Reactive variables can only be called in reactive/render context, 
  #  by enclosing parentheses (). This shows a data table output on our
  #  filtered reactive dataframe.
  
  output$table <- renderDataTable({
    df()
  })
  
  # turn meltDF into reactive, depends on user-selected postal code
  meltDFR <- reactive(meltDF %>% filter(Postcode %in% input$postcode))
  
  #  A Plotly plot output, built on an initial ggplot object which is
  #  converted to a plotly output.
  
  output$plot <- renderPlotly({
    p <- df() %>%
      group_by(Year) %>% # group by Year first
      summarise(avg_price = mean(value, na.rm = T)) %>% # get average price
      ggplot(aes(x = as.factor(Year), y = avg_price)) + geom_point() +
      labs(x = "Year", y = "Average rent price (Euro)", title = "Dublin") +
      theme_bw()
    ggplotly(p)
  })
  
  #  A Plotly plot output, comparing sale and rent price for user-selected
  #  postcodes.
  
  output$saleRentCompare <- renderPlotly({
    
    p2 <- ggplot(data = meltDFR(), aes(x = Year, y = value, colour = Postcode)) +
      geom_line() +
      facet_wrap(variable ~ ., nrow = 1, ncol = 2, scales = "free") +
      labs(y = "Cost (Euro)") +
      theme_bw()
    ggplotly(p2)
  })
  
}

# run shinyApp using ui and server part.
shinyApp(ui, server)
