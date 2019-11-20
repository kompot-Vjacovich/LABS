library(shiny)

ui <- fluidPage(
  
  titlePanel("Наивный байесовский классификатор"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6, sliderInput("n", "Количество элементов 1-го класса", 10, 100, 30, 1)),
        column(6, sliderInput("m", "Количество элементов 2-го класса", 10, 100, 40, 1)),
        
        column(6, sliderInput("mu11", "Мю11", 0, 6, 2, 0.1)), 
        column(6, sliderInput("mu12", "Мю12", 0, 6, 4, 0.1)), 
        column(6, sliderInput("mu21", "Мю21", 0, 4, 1, 0.1)),
        column(6, sliderInput("mu22", "Мю22", 0, 4, 3, 0.1)),
        
        column(6, sliderInput("sigma11", "Сигма11", 0.1, 1, 0.7, 0.1)), 
        column(6, sliderInput("sigma12", "Сигма12", 0.1, 1, 0.6, 0.1)), 
        column(6, sliderInput("sigma21", "Сигма21", 0.1, 1, 1, 0.1)),
        column(6, sliderInput("sigma22", "Сигма22", 0.1, 1, 0.7, 0.1)),
        
        column(12, sliderInput("P", "Вероятность появления Класса1|Класса2", 0.01, 0.99, 0.5, 0.01))
      )
    ),
    
    mainPanel(
      plotOutput("plot", width = "800px", height = "600px")
    )
  )
)

plug_in <- function(xl, len1, len2, Py) {
  
  calc_mu <- function(xl) sum(xl) / length(xl)
  
  
}

server <- function(input, output, session) {
  
}

shinyApp(ui, server)