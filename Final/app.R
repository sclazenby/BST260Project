#
# This is a Shiny web application. You can run the application by clicking
#LAST UPDATED 12/20/2020
# MICHELLE
# 11:49 PM PST
#
# The dreaded shiny app.
library(tidyverse)
library(rvest)
library(dplyr)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(shiny)
library(tidyverse)
library(forcats)
library(dslabs)


# Do we need to do what we did with the midterm?

data(global5050)
data(covidsex)

ui = fluidPage( 
    #theme = shinythemes::shinytheme("darkly"),
    titlePanel("A Case for Sex-disaggregated COVID-19 Deaths"),
    tabsetPanel(
        # First tab: Create two rows, bottom row with static time series
        tabPanel("Time Series",
                 sidebarLayout(
                     sidebarPanel(
                         p("The plots generated here were created using data collected from the", a("GenderSci Lab.", href="https://www.genderscilab.org/")),
                         
                         selectInput("country", label = "Select a State",
                                     choices = as.list(levels(covidsex$State)))
                     ),
                     mainPanel(
                         plotOutput("line")
                     )
                 )),
        
        # Second tab
        # Include dropdown menu of years to choose from 
        tabPanel("Case Study: The United States",
                 sidebarLayout(
                     sidebarPanel(
                         p("The plots generated here were created using data collected and created from the", a("GenderSci Lab.", href="https://www.genderscilab.org/")), 
                         
                         br(),
                         
                         sliderInput(inputId = "date", label = "Date:",
                                     min = as.Date("2020-04-13", "%Y-%m-%d"), 
                                     max = as.Date("2020-11-30", "%Y-%m-%d"),
                                     value = as.Date("2020-04-13", "%Y-%m-%d"), 
                                     step = 1,
                                     sep = "",       # keep years in year format and not 2,020 format
                                     ticks = FALSE,  # don't show tick marks on slider bar
                                     animate = TRUE) # add play button to animate
                     ),
                     mainPanel(
                         plotOutput("scatterPlot")
                     )
                 ))))

# third tab to be made: US and global heat maps.


server = function(input, output){
    data(global5050)
    data(covidsex)
    # Time series for ratios to first tab. Have US as comparison?
    output$line = renderPlot({
        covidsex %>% filter(State %in% input$State) %>%
            ggplot(aes_string(x = "Date", y = "case_ratio")) +
            geom_line(data = covidsex, mapping = aes_string(x = "Date", y = "case_ratio"), color = "blue") +
            xlab("Date") +
            ylab("Ratio f:m") +
            scale_y_continuous(breaks = seq(0, 3, 0.5), limits = c(0, 3))
    })
    
    # Create scatterplot of ratios by time. 
    output$scatterPlot = renderPlot ({        
        covidsex %>% filter(death_ratio %in% input$death_ratio & !is.na(death_ratio)) %>%
            ggplot(aes(death_ratio, Date, color = State)) + 
            geom_point(alepha=0.5) +
            xlab("Date") +
            ylab("Ratio m:f") +
            ggtitle(sprintf("Ratio f:m overtime in %d", input$Date))+
            scale_color_discrete(name="State") +
            theme_bw() +
            scale_y_continuous(breaks = seq(0, 3, 0.5), limits=c(0,3)) +
            scale_x_date(date_breaks = "1 week", date_labels = "%b %d") 
    })
}

shinyApp(ui = ui, server = server)




