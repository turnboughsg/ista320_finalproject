#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidyverse)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Global Oceanic Climate Change Indicators"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h3("Global Oceanic Climate Indicators"),
        p("Click on each tab to see the three respsective oceanic climate indicators."),
        HTML('Data from the <a href="https://www.epa.gov/climate-indicators/oceans">EPA</a>')
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Sea Surface Temperatures",
                               sliderInput(inputId = "range", 
                                           label = "Choose a start and end year:",
                                           min = min(1880), 
                                           max = max(2020), 
                                           value = c(2000, 2020),
                                           sep = ""),
                               plotOutput("line_sea")),
                      tabPanel("Ocean Level Temperatures", 
                               sliderInput(inputId = "range", 
                                           label = "Choose a start and end year:",
                                           min = min(1960), 
                                           max = max(2020), 
                                           value = c(2000, 2020),
                                           sep = ""),
                               plotOutput("line_ocean")),
                      tabPanel("Sea Level Measurements", plotOutput("scatter_plot")))
            )
        )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_sea <- renderPlot({
      
      sea = read.csv("./sea-surface-temp_fig-1.csv", 
                     header = TRUE)
      data = subset(sea, 
                    Year >= input$range[1] & Year <= input$range[2])
      data %>% 
      ggplot(aes(x = Year)) + 
        geom_line(aes(y = AnnualAnomaly, 
                      color = "Average Annual Anomaly")) + 
        geom_point(aes(y = AnnualAnomaly, 
                       color = "Average Annual Anomaly")) +
        geom_line(aes(y = Lower.95..confidence.interval, 
                      color = "95% Lower Confidence Bound")) + 
        geom_point(aes(y = Lower.95..confidence.interval, 
                       color = "95% Lower Confidence Bound")) +
        geom_line(aes(y = Upper.95..confidence.interval, 
                      color = "95% Upper Confidence Bound")) +
        geom_point(aes(y = Upper.95..confidence.interval, 
                       color = "95% Upper Confidence Bound")) +
        ggtitle("Global Annual Average Sea Surface Temperature Anomaly") +
        theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10)),
              plot.background = element_rect(fill = "white", color = "black", size = 2),
              legend.background = element_blank(),
              legend.title = element_text(),
              legend.key = element_rect(fill = NA)) +
        labs(x = "Year", 
             y = expression(paste("Temperature ( ", degree ~ F, " )")), 
             color = "Average Temperature Variations") +
        theme_classic() +
        scale_color_colorblind() + 
        guides(colour = guide_legend(override.aes = list(fill = NA), 
                                     reverse = TRUE)) 
    })
    
    
    output$line_ocean <- renderPlot({
      
      ocean_joules = read.csv("./ocean-heat-fig-2.csv", 
                     header = TRUE)
      data = subset(ocean_joules, 
                    Year >= input$range[1] & Year <= input$range[2])
      data %>% 
        ggplot(aes(x = Year)) +
        
        geom_line(aes(y = CSIRO, 
                      color = "CSIRO")) + 
        geom_point(aes(y = CSIRO, 
                       color = "CSIRO")) +
        geom_line(aes(y = IAP, 
                      color = "IAP")) + 
        geom_point(aes(y = IAP, 
                       color = "IAP")) +
        geom_line(aes(y = MRI_JMA, 
                      color = "MRI_JMA")) + 
        geom_point(aes(y = MRI_JMA, 
                       color = "MRI_JMA")) +
        geom_line(aes(y = NOAA, 
                      color = "NOAA")) + 
        geom_point(aes(y = NOAA, 
                       color = "NOAA")) +

        ggtitle("Global Ocean Level Heat Content - 700 Meters") +
        theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10)),
              plot.background = element_rect(fill = "white", color = "black", size = 2),
              legend.background = element_blank(),
              legend.title = element_text(),
              legend.key = element_rect(fill = NA)) +
        labs(x = "Year", 
             y = expression(paste("Heat Content - 10^22 Joules")), 
             color = "Locations") +
        theme_classic() +
        scale_color_colorblind() + 
        guides(colour = guide_legend(override.aes = list(fill = NA), 
                                     reverse = TRUE)) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
