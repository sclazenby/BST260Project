library(shiny)
library(tidyverse)
library(maps)

# Which datawrangling do I add here?
#us_map <- map_data("state")

#ussexd <- covidex %>%
    #mutate(region = str_to_lower(rownames(covidsex)))

# merge
#ussexd<- right_join(covidsex, us_map, by = "region")


# Define UI
ui <- fluidPage(
    # Application title
    titlePanel("A Case for Sex-disaggregated COVID-19 Deaths"),
    
    # first row: interactive elements
    fluidRow(
        # first column: explanatory text
        column(3,
               p("The plots generated here were created using data collected from the", a("GenderSci Lab.", href="https://www.genderscilab.org/")),
               br()
        ), # end of first column
        
        # second column: radio button to choose the crime
        column(3,
               radioButtons(inputId = "category", label = "Select a category",
                            choices = c("Cases", "Deaths"),
                            selected = "Cases")
        ), # end of second column
        
        
        # fourth column: radio button to select scatterplot or heatmap
        column(3,
               radioButtons(inputId = "plot_type", label = "What kind of plot?",
                            choices = c("Scatterplot", "Heatmap"))
        ), # end of fourth column
        
    ), # end of fluidRow
    
    # plot output and text output
    fluidRow(
        column(9,
               plotOutput("plot")
        ), # end of column 1
    ) # end of fluidRow
) # fluidPage

# Define server logic
server <- function(input, output) {
    covidsex <- reactive({
        covidsex %>%
            select("region", input$crime) %>%
            right_join(us_map, by = "region")
    })
    
    output$plot <- renderPlot({
        if (input$plot_type == "Scatterplot") {
            # make a scatter plot of murder vs urbanpop
            ylabel <- paste(input$category)
            ggplot(USArrests, aes_string(x = "Date", y = input$category)) +
                geom_point(color = black) +
                xlab("Date") +
                ylab(ylabel) 
        } else if (input$plot_type == "Heatmap") {
            #print(input$crime)
            globalsexd %>% filter(!is.na(deaths_mfratio)) %>%
                ggplot() + 
                geom_polygon(data=map, aes(x = long, y=lat, group = group), fill = "light grey") +
                geom_polygon(data=globalsexd, aes(x = long, y = lat, group = group, fill = `deaths_mfratio`)) +
                scale_color_brewer("Diamond\nclarity") +
                theme(panel.grid.major = element_blank(), 
                      panel.background = element_blank(),
                      axis.title = element_blank(), 
                      axis.text = element_blank(),
                      axis.ticks = element_blank()) +
                coord_fixed(1.75) +
                labs(x= "Longitude", y = "Latitude", title = "Global Ratio of male to female COVID deaths")
        }
    }) # end of renderPlot
}


# Run the application
shinyApp(ui = ui, server = server)

