library(shiny)

# Define UI
ui <- fluidPage(
  
  titlePanel("Political Party Support Analysis"),
  
  # Tab layout with 4 tabs
  tabsetPanel(
    tabPanel("Data Input", 
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   # Party support sliders
                   sliderInput("dup_support", "DUP Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("uup_support", "UUP Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("alliance_support", "Alliance Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("tuv_support", "TUV Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("sinn_fein_support", "Sinn Fein Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("sdlp_support", "SDLP Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("other_support", "Other Support",
                               min = 0, max = 100, value = 50),
                   # Sliding scale inputs for C2DE and ABC1 for each party
                   sliderInput("dup_c2de", "DUP C2DE Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("dup_abc1", "DUP ABC1 Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("uup_c2de", "UUP C2DE Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("uup_abc1", "UUP ABC1 Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("alliance_c2de", "Alliance C2DE Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("alliance_abc1", "Alliance ABC1 Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("tuv_c2de", "TUV C2DE Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("tuv_abc1", "TUV ABC1 Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("sinn_fein_c2de", "Sinn Fein C2DE Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("sinn_fein_abc1", "Sinn Fein ABC1 Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("sdlp_c2de", "SDLP C2DE Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("sdlp_abc1", "SDLP ABC1 Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("other_c2de", "Other C2DE Support",
                               min = 0, max = 100, value = 50),
                   sliderInput("other_abc1", "Other ABC1 Support",
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
             dataTableOutput("east_belfast_table")
    ),
    tabPanel("North Down",
             dataTableOutput("north_down_table")
    ),
    tabPanel("Belfast South & Mid Down",
             dataTableOutput("belfast_south_mid_down_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Render tables for each region
  output$east_belfast_table <- renderDataTable({
    # Sample table, replace with your actual data processing and table creation
    head(data(), 10)
  })
  
  output$north_down_table <- renderDataTable({
    # Sample table, replace with your actual data processing and table creation
    head(data(), 10)
  })
  
  output$belfast_south_mid_down_table <- renderDataTable({
    # Sample table, replace with your actual data processing and table creation
    head(data(), 10)
  })
  
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
}

# Run the application
shinyApp(ui = ui, server = server)