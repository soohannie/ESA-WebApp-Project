ui <- fluidPage(
  titlePanel("Outbreak enhancement"),
  fluidRow(
    h3("Resources"),
    h4("Resources in deck"),
    column(2, wellPanel(sliderInput(inputId = "Deck_food", label = "Food", min = 1, max = 20, value =20, step =1 ),
           sliderInput("Deck_water",label = "Water", min = 0, max = 20, value =20, step = 1),
           sliderInput("Deck_entertainment",label = "Entertainment",min = 0, max = 20, value =20, step = 1),
           sliderInput("Deck_sanitation",label = "Sanitation", min = 0, max = 20, value =20, step = 1),
           sliderInput("Deck_infection",label = "Infection",min = 0, max = 20, value =20, step = 1))
  ),
  mainPanel(
    fluidRow(
    column(8,plotOutput(outputId = "drawplot")) #Resources left in draw pile
    )
           
  )
  )
)
server <- function(input,output,session){
  output$drawplot <- renderPlot({
    resource_value <- c(input$Deck_food, input$Deck_water, input$Deck_entertainment, input$Deck_sanitation, input$Deck_infection)
    sum_resource <- sum(resource_value)
    barplot(height= resource_value,
            names.arg = c("Food", "Water", "Entertainment","Sanitation","Infection"),
            ylab= "Number of resources left in deck",
            col = c("red","blue","orange","lightblue","darkgreen"),
            main = "Resources in deck",
            ylim = c(0,25)
            ) 
    text(x = c(1,2,3,4,5), y= resource_value +4,labels = as.character(resource_value))
    text(x = c(1,2,3,4,5), y= resource_value +2, labels = as.character(100*round(resource_value/sum_resource,3)))
  })
  
}

shinyApp(ui, server)