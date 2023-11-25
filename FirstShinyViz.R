library(shiny)
library(ggplot2)
library(dplyr)

# Load the dataset
world_data <- read.csv('/Users/USER/Downloads/archive-3/world_development_data_imputed.csv')

# Define UI
ui <- fluidPage(
  titlePanel("World Development Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Choose a Country:", choices = unique(world_data$Country)),
      selectInput("indicatorInput", "Choose an Indicator:", choices = colnames(world_data)),
    ),
    # In your UI
    
    mainPanel(
      tabsetPanel(
        tabPanel("Country Profile", plotOutput("countryPlot")),
        tabPanel("Global Trends", plotOutput("trendsPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Country Profile Plot
  output$countryPlot <- renderPlot({
    # Data processing and plotting logic for the country profile
    # Filter data by selected country and plot selected indicator
    country_data <- world_data %>% 
      filter(Country == input$countryInput) %>%
      select(Year, input$indicatorInput)
    
    ggplot(country_data, aes_string(x = "Year", y = input$indicatorInput)) +
      geom_line() +
      labs(title = paste("Indicator for", input$countryInput))
  })
  
  # Global Trends Plot
  output$trendsPlot <- renderPlot({
    # Get user-selected country and indicator
    selected_country <- input$countryInput
    selected_indicator <- input$indicatorInput
    
    # Prepare the data for plotting
    trends_data <- world_data %>%
      select(Country, Year, all_of(selected_indicator)) %>%
      na.omit()
    
    # Create the plot
    ggplot(trends_data, aes_string(x = "Year", y = selected_indicator, group = "Country")) +
      geom_line(aes(color = Country == selected_country), size = 1) +
      geom_line(data = filter(trends_data, Country == selected_country), 
                aes_string(x = "Year", y = selected_indicator), 
                color = "red", size = 1.5) +
      geom_text(data = filter(trends_data, Country == selected_country & Year == max(Year)), 
                aes_string(label = "Country", x = "Year", y = selected_indicator), 
                hjust = 1.5, vjust = 0, color = "red") +
      scale_color_manual(values = c("gray", "red")) +
      labs(title = paste("Global Trends of", selected_indicator),
           y = selected_indicator)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

