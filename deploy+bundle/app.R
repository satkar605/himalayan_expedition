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
#setwd("/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/himalyan_expedition/dataset")

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
                 leafletOutput("peak_map", height = "600px"),
                 div(style = "background: #f8f9fa; border-radius: 8px; padding: 16px; margin-top: 16px; border: 1px solid #e3e3e3;",
                     p("This demo dashboard maps expedition activity across Himalayan peaks, helping visualize trends in climbing history, geography, and expedition outcomes. It's a work in progress; feedback is welcome at karkisatkarhere@gmail.com.",
                       style = "margin: 0; color: #555; font-size: 14px; line-height: 1.5;")
                 )),
        tabPanel("Peak Insights",
                 fluidRow(
                   column(6, plotOutput("outcomes_prop_bar", height = "120px")),
                   column(6, htmlOutput("top_nations_flags"))
                 ),
                 fluidRow(
                   column(12, plotOutput("success_rate_ts", height = "300px"))
                 )
        ),
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
  
  # Improved horizontal stacked bar for expedition outcomes
  output$outcomes_prop_bar <- renderPlot({
    peak <- selected_peak_data()
    exped_peak <- exped %>% filter(peakid == peak$peakid)
    outcomes <- exped_peak %>%
      mutate(
        Outcome = case_when(
          success1 == TRUE | success2 == TRUE | success3 == TRUE | success4 == TRUE ~ "Success",
          claimed == TRUE ~ "Claimed",
          disputed == TRUE ~ "Disputed",
          TRUE ~ "Failure"
        )
      ) %>%
      count(Outcome) %>%
      complete(Outcome = c("Success", "Claimed", "Disputed", "Failure"), fill = list(n = 0)) %>%
      mutate(Prop = n / sum(n))
    outcomes$Outcome <- factor(outcomes$Outcome, levels = c("Success", "Claimed", "Disputed", "Failure"))
    ggplot(outcomes, aes(x = 1, y = Prop, fill = Outcome)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = c(
        Success = "#4CAF50",
        Claimed = "#2196F3",
        Disputed = "#FF9800",
        Failure = "#F44336"
      )) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(
        aes(label = ifelse(Prop > 0, paste0(round(Prop * 100), "%"), "")),
        position = position_stack(vjust = 0.5),
        color = c("white", "white", "black", "white"),
        size = 6, fontface = "bold"
      ) +
      labs(
        title = paste("Expedition Outcomes for", peak$pkname),
        subtitle = "Proportion of expeditions by outcome (Success, Claimed, Disputed, Failure)",
        x = NULL, y = NULL, fill = "Outcome"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 13, color = "#666", margin = margin(b = 10)),
        plot.margin = margin(0, 0, 0, 0)
      ) +
      coord_flip()
  })
  
  # Smoothed line chart for yearly success rate
  output$success_rate_ts <- renderPlot({
    peak <- selected_peak_data()
    exped_peak <- exped %>% filter(peakid == peak$peakid)
    yearly <- exped_peak %>%
      group_by(year) %>%
      summarise(
        total = n(),
        success = sum(success1 == TRUE | success2 == TRUE | success3 == TRUE | success4 == TRUE, na.rm = TRUE)
      ) %>%
      mutate(success_rate = ifelse(total > 0, 100 * success / total, NA))
    ggplot(yearly, aes(x = year, y = success_rate)) +
      geom_line(color = "#2196F3", size = 1.2, alpha = 0.7) +
      geom_point(color = "#2196F3", size = 2, alpha = 0.7) +
      geom_smooth(method = "loess", se = FALSE, color = "#F44336", size = 1.2, linetype = "dashed") +
      labs(title = paste("Yearly Success Rate for", peak$pkname),
           x = "Year", y = "Success Rate (%)") +
      theme_minimal(base_size = 14) +
      ylim(0, 100)
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
  
  # Top 5 Nations flags
  output$top_nations_flags <- renderUI({
    peak <- selected_peak_data()
    exped_peak <- exped %>% filter(peakid == peak$peakid)
    top_nations <- exped_peak %>%
      count(nation, sort = TRUE) %>%
      slice_max(n, n = 5)
    # Use countrycode to get ISO2 codes for flag images
    if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
    library(countrycode)
    top_nations <- top_nations %>%
      mutate(
        iso2 = countrycode(nation, "country.name", "iso2c", warn = FALSE),
        flag_url = paste0("https://flagcdn.com/32x24/", tolower(iso2), ".png")
      )
    # Build HTML for flags and names
    htmltools::tagList(
      tags$div(style = "background:#f8f9fa;padding:18px 12px 12px 12px;border-radius:8px;box-shadow:0 1px 3px rgba(0,0,0,0.04);border:1px solid #e3e3e3;",
        tags$h4("Top 5 Nations", style = "margin-top:0;"),
        lapply(seq_len(nrow(top_nations)), function(i) {
          tags$div(style = "display:flex;align-items:center;margin-bottom:10px;",
            tags$img(src = top_nations$flag_url[i], height = 24, width = 32, style = "margin-right:10px;border-radius:3px;border:1px solid #ddd;"),
            tags$span(style = "font-weight:600;font-size:16px;", top_nations$nation[i]),
            tags$span(style = "margin-left:auto;font-size:15px;color:#555;", paste0("(", top_nations$n[i], ")"))
          )
        })
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
shinyApp(
  ui = tagList(
    tags$head(tags$script(HTML(jsCode))),
    ui,
    tags$footer(
      style = "text-align:center; padding: 16px 0; color: #888; font-size: 15px; background: #f8f9fa; border-top: 1px solid #e3e3e3; margin-top: 32px;",
      "Dashboard created by Satkar Karki"
    )
  ),
  server = server
) 