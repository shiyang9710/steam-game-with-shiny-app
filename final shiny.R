library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)
library(tidyr)

# Clean and Prepare Data
games <- read.csv('C:\\Users\\user\\Desktop\\steam_games.csv', sep = ';') %>%
  separate_rows(Genre, sep = ', ') %>%
  mutate(Year = as.numeric(str_sub(Release.Date, 1, 4)),
         Release.Date = ymd(Release.Date),
         Owners = str_remove_all(Owners, ','),
         Price = Price / 100,
         Initial.Price = Initial.Price / 100)

# Define UI
ui <- fluidPage(
  titlePanel("Steam Games Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      sliderInput("yearRange", "Select Year Range:",
                  min = min(games$Year, na.rm = TRUE),
                  max = max(games$Year, na.rm = TRUE),
                  value = c(min(games$Year, na.rm = TRUE), max(games$Year, na.rm = TRUE)),
                  step = 1,
                  sep = ""),
      numericInput("topN", "Select Top N Genres (Set to 0 for all):", value = 0, min = 0, step = 1),
      selectInput("review_type", "Select Review Type:",
                  choices = c("Both", "Positive.Reviews", "Negative.Reviews")),
      checkboxInput("show_facet", "Facet by Review Type", value = TRUE),
      sliderInput("price_range", "Select Price Range (USD):",
                  min = 0, max = 1100, value = c(0, 1100), step = 1),
      selectInput("selected_game", "Select Game:",
                  choices = c("All", unique(games$Name))),
      selectInput("selected_publisher", "Select Publisher:",
                  choices = c("All", unique(games$Publisher)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Top Genres by Frequency', plotOutput("genrePlot")),
        tabPanel('Top 10 Games by Reviews', plotOutput("reviewPlot")),
        tabPanel('Game by Price', plotOutput("expensiveGames")),
        tabPanel('Publishers', plotOutput("topPublishers"))
      )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  # Filtered data based on year range
  filtered_data <- reactive({
    games %>%
      filter(!is.na(Year) & Year >= input$yearRange[1] & Year <= input$yearRange[2])
  })
  
  # Render Genre Plot
  output$genrePlot <- renderPlot({
    genre_counts <- filtered_data() %>%
      count(Genre, sort = TRUE)
    
    if (input$topN > 0) {
      genre_counts <- genre_counts %>%
        top_n(input$topN, n)
    }
    
    ggplot(genre_counts, aes(x = reorder(Genre, n), y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = paste("Top", ifelse(input$topN == 0, "All", input$topN), "Genres by Frequency"),
        x = "Genre",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  # Render Review Plot
  output$reviewPlot <- renderPlot({
    games_long <- games %>%
      filter(!is.na(Positive.Reviews) & !is.na(Negative.Reviews)) %>%
      arrange(desc(Positive.Reviews)) %>%
      slice_max(Positive.Reviews, n = 10) %>%
      select(Name, Positive.Reviews, Negative.Reviews) %>%
      pivot_longer(cols = c(Positive.Reviews, Negative.Reviews),
                   names_to = "ReviewType", values_to = "ReviewCount")
    
    filtered_reviews <- if (input$review_type == "Both") {
      games_long
    } else {
      games_long %>% filter(ReviewType == input$review_type)
    }
    
    p <- ggplot(filtered_reviews, aes(x = reorder(Name, ReviewCount), y = ReviewCount, fill = ReviewType)) +
      geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      ggtitle("Top 10 Games by Positive and Negative Reviews") +
      labs(x = "Game Name", y = "Number of Reviews") +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = c("Positive.Reviews" = "#00AFBB", "Negative.Reviews" = "#FC4E07")) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom"
      )
    
    if (input$show_facet) {
      p <- p + facet_wrap(~ReviewType, scales = "free")
    }
    
    print(p)
  })
  
  # Render Expensive Games Plot
  output$expensiveGames <- renderPlot({
    filtered_games <- games %>%
      filter(grepl("English", Languages, ignore.case = TRUE)) %>%
      filter(!is.na(iconv(Name, "UTF-8", "ASCII", sub = NA))) %>%
      filter(Price >= input$price_range[1], Price <= input$price_range[2])
    
    if (input$selected_game != "All") {
      filtered_games <- filtered_games %>% filter(Name == input$selected_game)
    }
    
    filtered_games %>%
      arrange(desc(Price)) %>%
      slice_max(Price, n = 10) %>%
      ggplot(aes(x = reorder(Name, Price), y = Price)) +
      geom_bar(stat = "identity", fill = "#fc8d62", color = "black", width = 0.7) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      ggtitle("Games by Price") +
      labs(x = "Game Name", y = "Price (USD)") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10)
      )
  })
  
  # Render Top Publishers Plot
  output$topPublishers <- renderPlot({
    filtered_publishers <- games %>%
      mutate(Publisher = str_trim(Publisher),
             Publisher = ifelse(Publisher == "" | is.na(Publisher), "Unknown Publisher", Publisher))
    
    if (input$selected_publisher != "All") {
      filtered_publishers <- filtered_publishers %>% filter(Publisher == input$selected_publisher)
    }
    
    filtered_publishers %>%
      count(Publisher) %>%
      arrange(desc(n)) %>%
      top_n(10, n) %>%
      ggplot(aes(x = reorder(Publisher, n), y = n)) +
      geom_bar(stat = "identity", fill = "#66c2a5", color = "black", width = 0.7) +  
      coord_flip() +
      theme_minimal(base_size = 14) +
      ggtitle("Publishers by Number of Games") +
      labs(x = "Publisher", y = "Number of Games") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)