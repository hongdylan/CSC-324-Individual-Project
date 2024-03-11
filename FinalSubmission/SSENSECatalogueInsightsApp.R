library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(shinydashboard)
library(shinythemes)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Load the dataset
ssense_data <- read.csv("ssense_dataset.csv")

# Define UI with shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "SSENSE Catalogue Insights"),
  dashboardSidebar(
    selectInput("brand", "Brand", choices = unique(ssense_data$brand), selected = "Rick Owens", multiple = TRUE),
    sliderInput("priceRange", "Price Range", min = min(ssense_data$price_usd), max = max(ssense_data$price_usd), value = c(min(ssense_data$price_usd), max(ssense_data$price_usd))),
    radioButtons("type", "Type", choices = c("All", "mens", "womens")),
    textInput("search", "Search Description", "")
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Summary", tableOutput("summary")),
      tabPanel("Price Distribution", plotOutput("pricePlot")),
      tabPanel("Item Search", dataTableOutput("searchResults")),
      tabPanel("Type Distribution", plotOutput("typeDistPlot", height = "250px")),
      tabPanel("Description Word Cloud", plotOutput("descriptionWordcloud"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  # filtered_data <- reactive({
  #   temp <- ssense_data %>%
  #     filter(brand %in% input$brand) %>%
  #     filter(price_usd >= input$priceRange[1] & price_usd <= input$priceRange[2]) %>%
  #     filter(ifelse(input$type == "All", TRUE, type == input$type)) %>% # Ensure this logic correctly handles 'All'
  #     filter(grepl(input$search, description, ignore.case = TRUE))
  #   temp
  filtered_data <- reactive({
    temp <- ssense_data %>%
      filter(brand %in% input$brand) %>%
      filter(price_usd >= input$priceRange[1] & price_usd <= input$priceRange[2]) %>%
      filter((input$type == "All") | (type == input$type)) %>%
      filter(grepl(input$search, description, ignore.case = TRUE))
    temp
  })
  
  output$summary <- renderTable({
    filtered_data() %>%
      group_by(brand, type) %>%
      summarise(Count = n(), Avg_Price = mean(price_usd), .groups = 'drop')
  })
  
  output$pricePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = price_usd, fill = type)) +
      geom_histogram(binwidth = 100, position = "dodge") +
      labs(title = "Price Distribution by Type", x = "Price (USD)", y = "Count") +
      theme_minimal()
  })
  
  output$searchResults <- renderDataTable({
    filtered_data()
  })
  
  
  output$typeDistPlot <- renderPlot({
    data <- filtered_data()
    type_counts <- data %>% group_by(type) %>% summarise(count = n(), .groups = 'drop')
    ggplot(type_counts, aes(x = "", y = count, fill = type)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      labs(title = "Item Type Distribution", x = "", y = "", fill = "Type") +
      theme_void() +
      scale_fill_manual(values = c("mens" = "blue", "womens" = "pink"))
  })
  output$descriptionWordcloud <- renderPlot({
    # Assuming 'description' is a column in your 'ssense_data' dataframe
    # Create a text corpus
    descriptions <- Corpus(VectorSource(ssense_data$description))
    
    # Clean up the text
    descriptions <- tm_map(descriptions, content_transformer(tolower))
    descriptions <- tm_map(descriptions, removePunctuation)
    descriptions <- tm_map(descriptions, removeNumbers)
    descriptions <- tm_map(descriptions, removeWords, stopwords("en"))
    descriptions <- tm_map(descriptions, stripWhitespace)
    
    # Convert the text corpus into a matrix of word counts
    tdm <- TermDocumentMatrix(descriptions)
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE) 
    dm <- data.frame(word = names(word_freqs), freq = word_freqs)
    
    # Plot the word cloud
    set.seed(1234)  # Set seed for reproducibility
    wordcloud(words = dm$word, freq = dm$freq, min.freq = 1,
              max.words = 200, random.order = FALSE, rot.per = 0.25, 
              colors = brewer.pal(8, "Dark2"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
