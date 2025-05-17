# Himalayan Expeditions Dashboard - Interactive Shiny App

# Load required libraries
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(tmap)
library(shinydashboard)
library(bslib)
library(DT)  # Add DT package for better tables

# Set working directory to dataset location
setwd("/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/himalyan_expedition/dataset")

# Load and prepare data
peaks <- read.csv("peaks.csv")
exped <- read.csv("exped.csv")
members <- read.csv("members.csv")
refer <- read.csv("refer.csv")

# Data cleaning and preparation
peaks_clean <- peaks %>%
  filter(!is.na(peakid), !is.na(heightm))

# Join peaks with expeditions to get climb counts
peak_climbs <- exped %>%
  group_by(peakid) %>%
  summarise(climb_count = n()) %>%
  left_join(peaks_clean, by = "peakid") %>%
  arrange(desc(climb_count))

# Get top 10 peaks with their details
top_10_peaks <- head(peak_climbs, 10)

# Create the Shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # Application title
  titlePanel("🏔️ Himalayan Expeditions Dashboard"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(style = "background: #f8f9fa; border-radius: 8px; padding: 18px 16px 12px 16px; margin-bottom: 18px; box-shadow: 0 1px 3px rgba(0,0,0,0.04); border: 1px solid #e3e3e3;",
        h4("Peak Details", style = "margin-top:0;"),
        HTML('<span style="font-weight:600;">Height:</span> <span id="peak_height"></span><br>'),
        HTML('<span style="font-weight:600;">Region:</span> <span id="peak_region"></span><br>'),
        HTML('<span style="font-weight:600;">Status:</span> <span id="peak_status"></span><br>'),
        HTML('<span style="font-weight:600;">Total Expeditions:</span> <span id="total_expeditions"></span>'),
        tags$hr(),
        h4("Expedition Statistics"),
        htmlOutput("expedition_stats")
      ),
      selectInput("selected_peak", 
                 "Select a Peak:", 
                 choices = top_10_peaks$pkname,
                 selected = top_10_peaks$pkname[1])
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Map View",
                 leafletOutput("peak_map", height = "600px")),
        tabPanel("Peak Insights",
                 fluidRow(
                   column(6, plotOutput("height_dist")),
                   column(6, plotOutput("status_plot"))
                 ),
                 fluidRow(
                   column(12, plotOutput("first_ascents"))
                 )),
        tabPanel("Expedition Details",
                 fluidRow(
                   column(12, plotOutput("expeditions_ts", height = "250px"))
                 ),
                 DTOutput("expedition_table"))  # Changed to DTOutput
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for selected peak data
  selected_peak_data <- reactive({
    top_10_peaks %>%
      filter(pkname == input$selected_peak)
  })
  
  # Map output
  output$peak_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 85.3240, lat = 27.7172, zoom = 7) %>%  # Center on Nepal
      addMarkers(
        data = top_10_peaks,
        lng = ~as.numeric(gsub(".*([0-9]{2}\\.[0-9]+).*", "\\1", location)),
        lat = ~as.numeric(gsub(".*([0-9]{2}\\.[0-9]+).*", "\\1", location)),
        popup = ~paste("<b>", pkname, "</b><br>",
                       "Height: ", heightm, "m<br>",
                       "Expeditions: ", climb_count),
        layerId = ~pkname
      )
  })
  
  # Render Peak Details in JS for inline HTML
  observe({
    peak <- selected_peak_data()
    session$sendCustomMessage(type = 'updateText',
      list(id = 'peak_height', value = paste(peak$heightm, 'meters')))
    session$sendCustomMessage(type = 'updateText',
      list(id = 'peak_region', value = peak$region))
    session$sendCustomMessage(type = 'updateText',
      list(id = 'peak_status', value = peak$pstatus))
    session$sendCustomMessage(type = 'updateText',
      list(id = 'total_expeditions', value = peak$climb_count))
  })

  # Expedition statistics with line breaks and bold labels
  output$expedition_stats <- renderUI({
    peak <- selected_peak_data()
    peak_expeditions <- exped %>% filter(peakid == peak$peakid)
    success_metrics <- peak_expeditions %>%
      summarise(
        total_expeditions = n(),
        successful_expeditions = sum(success1 == TRUE | success2 == TRUE | 
                                   success3 == TRUE | success4 == TRUE, na.rm = TRUE),
        disputed_expeditions = sum(disputed == TRUE, na.rm = TRUE),
        claimed_expeditions = sum(claimed == TRUE, na.rm = TRUE)
      )
    success_rate <- (success_metrics$successful_expeditions / 
                    success_metrics$total_expeditions) * 100
    avg <- peak_expeditions %>% summarise(avg = mean(totmembers, na.rm = TRUE))
    o2_used <- peak_expeditions %>% summarise(rate = mean(o2used == TRUE, na.rm = TRUE) * 100)
    HTML(paste0(
      '<span style="font-weight:600;">Success Rate:</span> ', round(success_rate, 1), ' %<br>',
      '<span style="font-weight:600;">Total Expeditions:</span> ', success_metrics$total_expeditions, '<br>',
      '<span style="font-weight:600;">Successful Attempts:</span> ', success_metrics$successful_expeditions, '<br>',
      '<span style="font-weight:600;">Disputed Claims:</span> ', success_metrics$disputed_expeditions, '<br>',
      '<span style="font-weight:600;">Claimed Summits:</span> ', success_metrics$claimed_expeditions, '<br>',
      '<span style="font-weight:600;">Average Team Size:</span> ', round(avg$avg, 1), '<br>',
      '<span style="font-weight:600;">Oxygen Usage Rate:</span> ', round(o2_used$rate, 1), ' %'
    ))
  })
  
  # Visualization outputs
  output$height_dist <- renderPlot({
    ggplot(peaks_clean, aes(x = heightm)) +
      geom_histogram(bins = 30, fill = "lightblue", color = "black") +
      labs(title = "Distribution of Peak Heights",
           x = "Height (meters)",
           y = "Count") +
      theme_minimal()
  })
  
  output$status_plot <- renderPlot({
    status_summary <- peaks_clean %>%
      group_by(pstatus) %>%
      summarise(count = n()) %>%
      mutate(pstatus = factor(pstatus, 
                              levels = c("open", "restricted", "trekking", "unlisted")))
    
    ggplot(status_summary, aes(x = pstatus, y = count)) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(title = "Peak Status Distribution",
           x = "Status",
           y = "Number of Peaks") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$first_ascents <- renderPlot({
    first_ascents <- exped %>%
      filter(success1 == TRUE) %>%
      group_by(year) %>%
      summarise(first_ascents = n()) %>%
      arrange(year)
    
    ggplot(first_ascents, aes(x = year, y = first_ascents)) +
      geom_line(color = "darkgreen") +
      geom_point(color = "darkgreen") +
      labs(title = "Timeline of First Ascents",
           x = "Year",
           y = "Number of First Ascents") +
      theme_minimal()
  })
  
  # Time series chart for expeditions per year for selected peak
  output$expeditions_ts <- renderPlot({
    peak <- selected_peak_data()
    exped %>%
      filter(peakid == peak$peakid) %>%
      group_by(year) %>%
      summarise(Expeditions = n()) %>%
      ggplot(aes(x = year, y = Expeditions)) +
      geom_line(color = "#0072B2", size = 1.2) +
      geom_point(color = "#0072B2", size = 2) +
      labs(title = paste("Yearly Expedition Trend for", peak$pkname),
           x = "Year", y = "Number of Expeditions") +
      theme_minimal()
  })
  
  # Expedition details table using DT
  output$expedition_table <- renderDT({
    peak <- selected_peak_data()
    exped %>%
      filter(peakid == peak$peakid) %>%
      mutate(Peak = peak$pkname) %>%
      select(Peak,
             Year = year,
             Season = season,
             Host = host,
             Nation = nation,
             "Total Members" = totmembers,
             "Summit Members" = smtmembers,
             Success = success1) %>%
      arrange(desc(Year)) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        )
      )
  })
}

# Add JS handler for updating inline text
jsCode <- "
Shiny.addCustomMessageHandler('updateText', function(message) {
  document.getElementById(message.id).innerText = message.value;
});
"

# Run the application
shinyApp(ui = tagList(
  tags$head(tags$script(HTML(jsCode))),
  ui
), server = server) 