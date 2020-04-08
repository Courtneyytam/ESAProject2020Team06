library(shiny)
library(shinydashboard)
library(lubridate)

cards_data <- read.csv(file="cards.csv")
actions <- cards_data$Action
effects <- cards_data$Effect
quantities <- cards_data$Quantity

ui <- dashboardPage(skin="black",
  dashboardHeader(title = span("Input Actions", style = "font-size: 18px; font-weight: bold; font-family: monospace")),
  dashboardSidebar(
    tags$head( 
      tags$style(HTML(".main-sidebar {font-family: monospace}"))
    ),
    selectizeInput(
      'action_id', '1. Action', choices = actions,
      options = list(
        placeholder = 'select action',
        onInitialize = I('function() { this.setValue(""); }')
      ),
    ),
    selectizeInput(
      'effect_id', '2. Type of Effect', choices = effect,
      options = list(
        placeholder = 'select effect',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    hr(),
    fluidRow(column(4,
        actionButton('start','Start Game'),
        actionButton('stop','End Game'),
      )
    )
  ),
  dashboardBody(
    fluidRow(
      titlePanel(
        h1("Going Viral", align = "center", style='font-size: 42px; font-weight: bold; font-family: monospace')
      ),
    ),
    fluidRow(
      column(12,align="center", style='font-size: 42px; font-weight: bold; font-family: monospace',
        valueBoxOutput("Infected",),
        valueBoxOutput("Recovered"),
        valueBoxOutput("Time"),
      ),
    ), 
    fluidRow(
      img(src='placeholder.png',width='1200',style="display: block; margin-left: auto; margin-right: auto;"),
    )
  )
)

server <- function(input, output, session) {
  timer <- reactiveVal(7200)
  active <- reactiveVal(FALSE)
  score <- 0
  infected <- 0
  recovered <- 0
  
  counters_df <- shiny::reactiveValues()
  counters_df$df <- data.frame("infected" = numeric(), "recovered" = numeric())
  
  output$Infected <- renderValueBox({
    valueBox("###000","Total Infected", color = "red")
  })
  output$Recovered <- renderValueBox({
    valueBox("###000", "Total Recovered", color = "green")
  })
  
  output$Time <- renderValueBox({
    valueBox(renderText({paste(seconds_to_period(timer()))}), "Time Remaining", color = "yellow")
  })
  
  observe({
    invalidateLater(7200, session)
    isolate({
      if(active()) {
        timer(timer()-1)
        if(timer()<1) {
          active(FALSE)
          showModal(modalDialog(title =  span("Game Over", style = "font-size: 24px; font-weight: bold; font-family: monospace"), 
                                renderText({paste("You were able to save ", recovered, " people.")}),
                                renderText({paste("You allowed ", infected, " people to be infected.")}),
                                renderText({paste("Your final score is ", score, ".")}),
                                style='font-size: 16px; font-weight: bold; font-family: monospace',))
        }
      }
    })
  })
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
}

shinyApp(ui = ui, server = server)
