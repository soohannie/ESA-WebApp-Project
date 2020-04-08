rm(list=ls()) 
library(shiny)
source("social_distancing.R")

#==============================================================================================
#Shiny App Functionality Design

#We will use a R Environment to store bindings of variables to values
resources_dict <- new.env(hash=TRUE) 
for (i in 1:4){
  player_num <- sprintf("Player%s",i)
  resources_dict[[player_num]] = 0
}

#resources_dict[["Player1"]]

#Nic u can edit this for your counting function?
counterButton <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    actionButton(ns("button"), label = label),
    verbatimTextOutput(ns("out"))
  )
}

counter <- function(input, output, session) {
  count <- reactiveVal(0)
  observeEvent(input$button, {
    count(count() + 1)
  })
  output$out <- renderText({
    count()
  })
  
  count
}



#==============================================================================================
# Main Dashboard UI and Server Backend Design

ui <- fluidPage(
  titlePanel("Team 04 : Outbreak Game Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      counterButton("counter1", "Counter #1"),
      counterButton("counter2", "Counter #2"),
      counterButton("counter3", "Counter #3"),
      counterButton("counter4", "Counter #4"),
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput(inputId = "degree",
                  label = "Degree of Social Distancing:",
                  min = 0.4,
                  max = 1,
                  value = 0.8)
    ),
    mainPanel(
      #Insert Main graphs here
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "infectionPlot")
    )
  )
  
)

server <- function(input, output, session) {
  callModule(counter, "counter1")
  callModule(counter, "counter2")
  callModule(counter, "counter3")
  callModule(counter, "counter4")
  
  #just example plot - TO BE DELETED
  output$distPlot <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
  output$infectionPlot <- renderPlot({
    degr <- input$degree
    simulate_model(degr)
  })
}

shinyApp(ui, server)

