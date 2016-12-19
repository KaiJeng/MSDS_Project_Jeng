# Thanks to shiny.rstudio.com and Prof. Buyske's code for the templates. 

library(shiny)
library(tidyverse)
library(ggplot2)
# The DT package allows for the feature of selectable observations in the data table, giving a better level of interaction to the user.
library(DT)

# Initialize reading data set. If for some reason it doesn't read it properly, please download the actual csvs and redirect to where they are in your directory)
Score_Sales_All <- read.csv("https://raw.githubusercontent.com/KaiJeng/MSDS_Project_Jeng/master/data/Score_Sales_All", stringsAsFactors=FALSE)
Score_Sales_All$X <- NULL
Score_Sales_All$Week.Ending <- as.Date(Score_Sales_All$Week.Ending)
Score_Sales_Week_One <- read.csv("https://raw.githubusercontent.com/KaiJeng/MSDS_Project_Jeng/master/data/Score_Sales_Week_One", stringsAsFactors=FALSE)
Score_Sales_Week_One$X <- NULL
Score_Sales_Week_One$Week.Ending <- as.Date(Score_Sales_Week_One$Week.Ending)

# Data set for interactive plot
Score_Sales_Interactive <- Score_Sales_Week_One %>% filter(Complicated == "Sales data below:", Weekly != 0)

# Building the UI
# For the text input and select platform input
ui <- fluidPage(
  titlePanel("Video Game Software Analysis 2016"),
  sidebarLayout(
    sidebarPanel(
      textInput("gameInput", "Game"),
      selectInput("platformInput", "Platform",
                  c("PlayStation 4" = "optionPS4",
                    "Xbox One" = "optionXONE",
                    "Wii U" = "optionWIIU",
                    "Nintendo 3DS" = "option3DS",
                    "PlayStation Vita" = "optionVITA",
                    "PC" = "optionPC")),
      "Note: Sales numbers unavailable for some titles"
    ),
    
    # Any of those "dum" tables are used to break two lines into separate lines. What I mean is if you take those tables out, the line above will be appended to the next line as a single line of text.
    mainPanel(
      tabsetPanel(type = "tabs",
                  # Outputs table with general description of game
                  tabPanel("Description", tableOutput("tableDesc"),
                           "Information provided by GameRankings.com"),
                  # The intial textOutput "whoops" is for if a game does not have any sales data; a message will appear.
                  tabPanel("Sales Numbers", textOutput("whoops"), plotOutput("plotNum"),
                           dataTableOutput("tableNum"), "*Weekly sales numbers with value zero signify that data is unavailable",
                           tableOutput("dumdum"),
                           "Data provided by VGChartz.com"),
                  # fluidRow allows for the set of different filter options on the same row in the UI
                  tabPanel("Comparison: Sales and Reviews", 
                           fluidRow(column(4,selectInput("genes2", "Genre", c("All", unique(as.character(Score_Sales_Week_One$Genre))))),
                            column(4,selectInput("ploop2", "Platform", c("All", unique(as.character(Score_Sales_Week_One$Platform))))),
                            column(4,sliderInput("goalie", "Score Range", min = 0, max = 100, value = c(0, 100)))),
                           dataTableOutput("table3"),
                           "*First week sales", tableOutput("dummbu"),
                           "Data provided by VGChartz.com and GameRankings.com"),
                  # The interactive plot is on this tab
                  tabPanel("Comparison: Sales vs. Reviews",
                           "Based on available data",
                           plotOutput("plot4"), "Click on any observation to highlight points on the graph", dataTableOutput("table4"),
                           "*First week sales", tableOutput("dummbuu"),
                           "Data provided by VGChartz.com and GameRankings.com")
                  )
    )
  )
  
)

server <- function(input, output, session) {
  # Description tab
  info_df <- reactive({
    plat <- switch(input$platformInput,
                   "optionPS4" = "PS4",
                   "optionXONE" = "XONE",
                   "optionWIIU" = "WIIU",
                   "option3DS" = "3DS",
                   "optionVITA" = "VITA",
                   "optionPC" = "PC"
    )
    
    filter(Score_Sales_Week_One, Title == toupper(input$gameInput), Platform %in% plat)
  })
  output$tableDesc <- renderTable({ 
    select(info_df(), -Pos, -Weekly, -Total, -Week.Number, -Week.Ending, -Complicated, -Title)
  })
  
  # Sales Numbers tab
  sales_df <- reactive({
    plat <- switch(input$platformInput,
                  "optionPS4" = "PS4",
                  "optionXONE" = "XONE",
                  "optionWIIU" = "WIIU",
                  "option3DS" = "3DS",
                  "optionVITA" = "VITA",
                  "optionPC" = "PC"
    )
      filter(Score_Sales_All, Title == toupper(input$gameInput), Platform %in% plat)
  })
  output$whoops <- renderText ({
       ies <- filter(Score_Sales_Week_One, Title == toupper(input$gameInput))
       ies$Complicated[1] 
  })
  output$plotNum <- renderPlot({
    ggplot(data = sales_df(), aes(Week.Ending, Weekly)) + geom_point() + scale_x_date() + xlab("Date") + ylab("Sales Number")
  })
  output$tableNum <- renderDataTable({ 
    select(sales_df(),Pos,Weekly,Week.Number,Week.Ending)
  })
  
  # Comparison tab
  output$table3 <- renderDataTable({
    data2 <- select(Score_Sales_Week_One,Platform,Game,Publisher,Genre,Score,Weekly)
    if (input$genes2 != "All") {
      data2 <- data2[data2$Genre == input$genes2,]
    }
    if (input$ploop2 != "All") {
      data2 <- data2[data2$Platform == input$ploop2,]
    }
    filter(data2, Score >= input$goalie[1] & Score <= input$goalie[2])
  })
  
  # Comparison vs. tab; interactive plot is based on code in shiny.rstudio.com as I couldn't figure out how to personally tweak it
  ssint <- select(Score_Sales_Interactive,Score,Weekly)
  output$table4 <- DT::renderDataTable(select(Score_Sales_Interactive,Platform,Game,Publisher,Genre,Score,Weekly,Week.Ending), server = FALSE)
  output$plot4 <- renderPlot({
    s = input$table4_rows_selected
    par(mar = c(4, 4, 1, .1))
    plot(ssint)
    if (length(s)) points(ssint[s, , drop = FALSE], pch = 19, cex = 2)
  })

  
}
shinyApp(ui = ui, server = server)
