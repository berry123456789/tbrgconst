library(shiny)
library(scales)
library(magrittr)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: lightblue;
        }
      ")
    )
  ),

  titlePanel("Political Party Support Analysis"),

  # Add image in the top left corner
  #########tags$img(src = "", style = "position:absolute; top:10px; left:10px; height:50px; width:50px;"),

  # Tab layout with 4 tabs
  tabsetPanel(
    tabPanel("Data Input",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   # Party support sliders
                   numericInput("dup_support", "DUP Support",
                                min = 0, max = 100, value = 50),
                   numericInput("uup_support", "UUP Support",
                                min = 0, max = 100, value = 50),
                   numericInput("alliance_support", "Alliance Support",
                                min = 0, max = 100, value = 50),
                   numericInput("tuv_support", "TUV Support",
                                min = 0, max = 100, value = 50),
                   numericInput("sinn_fein_support", "Sinn Fein Support",
                                min = 0, max = 100, value = 50),
                   numericInput("sdlp_support", "SDLP Support",
                                min = 0, max = 100, value = 50),
                   numericInput("other_support", "Other Support",
                                min = 0, max = 100, value = 50),
                   # Sliding scale inputs for C2DE and ABC1 for each party
                   numericInput("dup_c2de", "DUP C2DE Support",
                                min = 0, max = 100, value = 50),
                   numericInput("dup_abc1", "DUP ABC1 Support",
                                min = 0, max = 100, value = 50),
                   numericInput("uup_c2de", "UUP C2DE Support",
                                min = 0, max = 100, value = 50),
                   numericInput("uup_abc1", "UUP ABC1 Support",
                                min = 0, max = 100, value = 50),
                   numericInput("alliance_c2de", "Alliance C2DE Support",
                                min = 0, max = 100, value = 50),
                   numericInput("alliance_abc1", "Alliance ABC1 Support",
                                min = 0, max = 100, value = 50),
                   numericInput("tuv_c2de", "TUV C2DE Support",
                                min = 0, max = 100, value = 50),
                   numericInput("tuv_abc1", "TUV ABC1 Support",
                                min = 0, max = 100, value = 50),
                   numericInput("sinn_fein_c2de", "Sinn Fein C2DE Support",
                                min = 0, max = 100, value = 50),
                   numericInput("sinn_fein_abc1", "Sinn Fein ABC1 Support",
                                min = 0, max = 100, value = 50),
                   numericInput("sdlp_c2de", "SDLP C2DE Support",
                                min = 0, max = 100, value = 50),
                   numericInput("sdlp_abc1", "SDLP ABC1 Support",
                                min = 0, max = 100, value = 50),
                   numericInput("other_c2de", "Other C2DE Support",
                                min = 0, max = 100, value = 50),
                   numericInput("other_abc1", "Other ABC1 Support",
                                min = 0, max = 100, value = 50)
                 ),
                 mainPanel(
                   # Submit button
                   actionButton("submit", "Submit")
                 )
               )
             )
    ),
    tabPanel("East Belfast",
             plotOutput("east_belfast_pie", width = "100%", height = "600px")
    ),
    tabPanel("North Down",
             plotOutput("north_down_pie", width = "100%", height = "600px")
    ),
    tabPanel("Belfast South & Mid Down",
             plotOutput("belfast_south_mid_down_pie", width = "100%", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive expression to process data based on user inputs
  data <- reactive({
    data <- data.frame(
      Party = c("DUP", "UUP", "Alliance", "TUV", "Sinn Fein", "SDLP", "Other"),
      Support = c(input$dup_support, input$uup_support, input$alliance_support,
                  input$tuv_support, input$sinn_fein_support, input$sdlp_support,
                  input$other_support),
      C2DE_Support = c(input$dup_c2de, input$uup_c2de, input$alliance_c2de,
                       input$tuv_c2de, input$sinn_fein_c2de, input$sdlp_c2de,
                       input$other_c2de),
      ABC1_Support = c(input$dup_abc1, input$uup_abc1, input$alliance_abc1,
                       input$tuv_abc1, input$sinn_fein_abc1, input$sdlp_abc1,
                       input$other_abc1)
    )
    data
  })

  # Render pie chart for East Belfast
  output$east_belfast_pie <- renderPlot({
    data_east <- data()
    data_east <- data_east[c("Party", "Support")]
    pie(data_east$Support, labels = paste0(data_east$Party, " (", percent(data_east$Support / sum(data_east$Support)), ")"), main = "East Down Party Support", col = rainbow(length(data_east$Support)))
  })

  # Render pie chart for North Down
  output$north_down_pie <- renderPlot({
    data_north <- data()
    data_north <- data_north[c("Party", "Support")]
    pie(data_north$Support, labels = paste0(data_north$Party, " (", percent(data_north$Support / sum(data_north$Support)), ")"), main = "North Down Party Support", col = rainbow(length(data_north$Support)))
  })

  # Render pie chart for Belfast South & Mid Down
  output$belfast_south_mid_down_pie <- renderPlot({
    data_belfast <- data()
    data_belfast <- data_belfast[c("Party", "Support")]
    pie(data_belfast$Support, labels = paste0(data_belfast$Party, " (", percent(data_belfast$Support / sum(data_belfast$Support)), ")"), main = "Belfast South & Mid Down Party Support", col = rainbow(length(data_belfast$Support)))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
