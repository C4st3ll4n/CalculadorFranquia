library(shiny)

dados = read.csv("slr12.csv", sep = ";")
modelo = lm(CusInic ~FrqAnual, data = dados)


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Previsao de custo inicial para montar uma franquia"),
  
  fluidRow(column(4, h2("Dados"), tableOutput("dados")),
           column(8, plotOutput("graph"))),
  
  fluidRow(column(6, h2("Valor anual da franquia"), numericInput("NovoValor", "Insira um valor (R$)", value = 1500, min=1, max = 9999999),
                  actionButton("Processar","Processar")),
           column(6), h1(textOutput("Resultado")))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$graph = renderPlot({
    plot(CusInic ~FrqAnual, data=dados)
    abline(modelo)
  })
  
  output$dados = renderTable({head(dados,10)})
  
  observeEvent(input$Processar,{
    valr = input$NovoValor
    prev = predict(modelo, data.frame(FrqAnual = eval(parse(text = valr))))
    prev = paste0("Custo inicial R$: ",round(prev,2))
    output$Resultado = renderText({prev})
  })
}

# Run the application
shinyApp(ui = ui, server = server)
