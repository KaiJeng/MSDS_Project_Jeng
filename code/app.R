# Thanks to shiny.rstudio.com and Prof. Buyske's code for the templates. 

library(shiny)
library(tidyverse)
library(ggplot2)

# Initialize and some extra tidyings
Score_Sales_All <- read.csv("~/Documents/MSDS597/MSDS Project/data/Score_Sales_All", stringsAsFactors=FALSE)
Score_Sales_All$X <- NULL
Score_Sales_All$Week.Ending <- as.Date(Score_Sales_All$Week.Ending)
Score_Sales_Week_One <- read.csv("~/Documents/MSDS597/MSDS Project/data/Score_Sales_Week_One", stringsAsFactors=FALSE)
Score_Sales_Week_One$X <- NULL
Score_Sales_Week_One$Week.Ending <- as.Date(Score_Sales_Week_One$Week.Ending)
Score_Sales_Week_One$Genre[is.na(Score_Sales_Week_One$Genre)] <- "Unlisted"
# Quick fix to be patched later
for (i in 1:nrow(Score_Sales_Week_One)) {
  if (is.na(Score_Sales_Week_One$Total[i] == FALSE)) {Score_Sales_Week_One$Complicated[i] <- "Sales data below:"}
  if (is.na(Score_Sales_Week_One$Total[i] == TRUE)) {Score_Sales_Week_One$Complicated[i] <- "Sales data unavailable for this title."}
}



# Building the UI
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
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Description", tableOutput("tableDesc"),
                           "Information provided by GameRankings.com"),
                  tabPanel("Sales Numbers", textOutput("whoops"), plotOutput("plotNum"),
                           dataTableOutput("tableNum"), "*Weekly sales numbers with value zero signify that data is unavailable",
                           tableOutput("dumdum"),
                           "Data provided by VGChartz.com"),
                  tabPanel("Comparison: Sales", 
                           fluidRow(column(4,selectInput("genes", "Genre", c("All", unique(as.character(Score_Sales_Week_One$Genre))))),
                           column(4,selectInput("ploop", "Platform", c("All", unique(as.character(Score_Sales_Week_One$Platform)))))),
                           dataTableOutput("table2"), "*First week sales",
                           tableOutput("dummy"),
                           "Data provided by VGChartz.com"),
                  tabPanel("Comparison: Reviews", 
                           fluidRow(column(4,selectInput("genes2", "Genre", c("All", unique(as.character(Score_Sales_Week_One$Genre))))),
                            column(4,selectInput("ploop2", "Platform", c("All", unique(as.character(Score_Sales_Week_One$Platform))))),
                            column(4,sliderInput("goalie", "Score Range", min = 0, max = 100, value = c(0, 100)))),
                           dataTableOutput("table3"),
                           "Data provided by GameRankings.com")
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
    filter(Score_Sales_Week_One, Game == input$gameInput, Platform %in% plat)
  })
  output$tableDesc <- renderTable({ 
    select(info_df(), -Pos, -Weekly, -Total, -Week.Number, -Week.Ending, -Complicated)
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
      filter(Score_Sales_All, Game == input$gameInput, Platform %in% plat)
  })
  output$whoops <- renderText ({
       ies <- filter(Score_Sales_Week_One, Game == input$gameInput)
       ies[[13]][1] 
  })
  output$plotNum <- renderPlot({
    ggplot(data = sales_df(), aes(Week.Ending, Weekly)) + geom_point() + scale_x_date() + xlab("Date") + ylab("Sales Number")
  })
  output$tableNum <- renderDataTable({ 
    select(sales_df(),Pos,Weekly,Week.Number,Week.Ending)
  })

  # Comparison: Sales tab 
  output$table2 <- renderDataTable({
    data <- select(Score_Sales_Week_One,Platform,Game,Publisher,Genre,Pos,Weekly,Week.Ending)
    if (input$genes != "All") {
      data <- data[data$Genre == input$genes,]
    }
    if (input$ploop != "All") {
      data <- data[data$Platform == input$ploop,]
    }
    data
  })
  
  # Comparison: Reviews tab
  output$table3 <- renderDataTable({
    data2 <- select(Score_Sales_Week_One,Platform,Game,Publisher,Genre,Score,Number.of.Reviews,Week.Ending)
    if (input$genes2 != "All") {
      data2 <- data2[data2$Genre == input$genes2,]
    }
    if (input$ploop2 != "All") {
      data2 <- data2[data2$Platform == input$ploop2,]
    }
    filter(data2, Score >= input$goalie[1] & Score <= input$goalie[2])
  })
  
}
shinyApp(ui = ui, server = server)
