#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(crayon)
library(ggplot2)
library(magrittr)
library(data.table)
library(reshape2)
library(viridis)
library(DT)
library(dplyr)
library(tidyr)
library(scales)
library(readr)


df <-   read.csv('New_covid.csv')

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19Lab", titleWidth = 400),
    dashboardSidebar(
        width = 400,
        sidebarMenu(
            selectInput(
                inputId = "continent",
                label = "Select Continent:",
                choices = unique(df$continent)
            ),
            
            selectInput(
                inputId = "location",
                label = "Select the Location:",
                choices = NULL
            ),
            
            checkboxGroupInput(
                inputId = "country",
                label = "Country Code",
                choices = NULL
            ),
            
            selectInput(
                inputId = "DATE1",
                label = "From Date:",
                choices = NULL
            ),
            
            selectInput(
                inputId = "DATE2",
                label = "To Date:",
                choices = NULL
            ),
            
            actionButton(inputId = "button", label = "Display")
        )
        
    ),
    
    dashboardBody(fluidRow(
        tabBox(
            width = 12,
            height = 500,
            tabPanel("Insights",
                     fluidRow(
                         box(
                             title = "Results",
                             height = 400,
                             width = 12,
                             status = "primary",
                             solidHeader = T,
                             div(DT::dataTableOutput("table"), style = "font-size: 100%; width: 100%")
                         )
                     ),
                     
                     fluidRow(
                         box(
                             title = "Description",
                             height = 100,
                             width = 12,
                             # to add color to terminal output, create styles for notes, warnings, errors; and combine styles
                             
                         )
                     )),
            tabPanel("Visualization",
                     fluidRow(
                         box(
                             title = "Chart1",
                             height = 500,
                             width = 6,
                             status = "primary",
                             solidHeader = T,
                             plotOutput("ggplot1")
                         ),
                         box(
                             title = "Chart2",
                             height = 500,
                             width = 6,
                             status = "primary",
                             solidHeader = T,
                             plotOutput("ggplot2")
                         )
                     ),),
            #Display title for tab
            tabPanel(
                "Forecasting",
                fluidRow(
                    box(
                        title = "Forecast for Contient1",
                        height = 500,
                        width = 12,
                        status = "primary",
                        solidHeader = T,
                        plotOutput("Asia")
                    )
                ),
                fluidRow(
                    box(
                        title = "Forecast for Contient2",
                        height = 500,
                        width = 12,
                        status = "primary",
                        solidHeader = T,
                        plotOutput("Europe")
                    )
                ),
                fluidRow(
                    box(
                        title = "Forecast for Contient3",
                        height = 500,
                        width = 12,
                        status = "primary",
                        solidHeader = T,
                        plotOutput("North")
                    )
                ),
                fluidRow(
                    box(
                        title = "Forecast for Contient4",
                        height = 500,
                        width = 12,
                        status = "primary",
                        solidHeader = T,
                        plotOutput("South")
                    )
                ),
                fluidRow(
                    box(
                        title = "Forecast for Contient5",
                        height = 500,
                        width = 12,
                        status = "primary",
                        solidHeader = T,
                        plotOutput("Afirica")
                    )
                ),
                fluidRow(
                    box(
                        title = "Forecast for Contient6",
                        height = 500,
                        width = 12,
                        status = "primary",
                        solidHeader = T,
                        plotOutput("Oicean")
                    )
                )
            )
        )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Shiny's reactive programming framework is primarily designed
    #for calculated values (reactive expressions) and side-effect-causing actions (observers)
    #that respond to any of their inputs changing
    # expression that represents the event
    observeEvent(input$continent, {
        # Change the value of a select input on the client
        # input$continent is the dependency for the change of
        # location
        updateSelectInput(session, 'location',
                          choices = unique(df$location[df$continent == input$continent]))
    })
    
    observeEvent(input$location, {
        updateCheckboxGroupInput(session, 'country',
                                 choices = unique(df$iso_code[df$continent ==
                                                                  input$continent &
                                                                  df$location == input$location]))
    })
    
    observeEvent(input$country, {
        dateList = unique(df$date[df$iso_code == input$country])
        updateSelectInput(session, 'DATE1', choices = dateList)
        updateSelectInput(session, 'DATE2', choices = dateList)
        
    })
    
    
    observeEvent(input$button, {
        continent <- input$continent
        location <- input$location
        country <- input$country
        date1 <- input$DATE1
        date2 <- input$DATE2
        df <-   read.csv('New_covid.csv')
        df[1] <- as.factor(df[1])
        df[2] <- as.factor(df[2])
        df[3] <- as.factor(df[3])
        count <-
            df %>% group_by_if(is.factor) %>% summarize_if(is.numeric, sum, na.rm = TRUE)
        
        output$table <-
            DT::renderDataTable(# to  create an HTML widget to display rectangular data (a matrix or data frame) using the JavaScript library DataTables.
                datatable(
                    count,
                    rownames = FALSE,
                    options = list(
                        searching = FALSE,
                        bPaginate = FALSE,
                        lengthChange = FALSE,
                        info = FALSE,
                        pageLength = 20,
                        scrollX = T
                    )
                ))
        
        Prepared_df <-
            melt(df[4:8] ,  id.vars = 'date', variable.name = 'CasesOrDeaths')
        Prepared_df$date <-
            as.Date(Prepared_df$date, format = '%Y-%m-%d')
        
        output$ggplot1 <- renderPlot({
            plot1 <-
                ggplot(Prepared_df, aes(date, value)) + geom_line(aes(colour = CasesOrDeaths)) +
                scale_x_date(labels = date_format("%d-%m-%Y")) + theme_bw() + labs(y = "the number",
                                                                                   title = "the Trend of the four status  by Date",
                                                                                   subtitle = "Date from 2020 to 2021")
            plot1
            
        })
        
        output$ggplot2 <- renderPlot({
            Prepared_df2 <- df[17:18]
            
            Plot2 <-
                ggplot(Prepared_df2,aes(x = positive_rate, y = hospital_beds_per_thousand))+geom_point()+labs(x=" the positive rate",y="the number of bes per 1000") + theme_bw()
            Plot2
            
        })
        
        
    })
    
}



# Run the application
shinyApp(ui = ui, server = server)
