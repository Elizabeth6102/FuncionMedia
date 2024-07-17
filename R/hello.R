# Cargar librerías necesarias
library(shiny)
library(readxl)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Cálculo de Media desde Excel"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Cargar archivo Excel", accept = ".xlsx"),
      selectInput("columna", "Seleccionar columna:", choices = NULL),
      actionButton("calcular", "Calcular")
    ),
    mainPanel(
      verbatimTextOutput("media_resultado"),
      plotOutput("media_plot")
    )
  )
)

# Server
server <- function(input, output, session) {

  datos <- reactive({
    req(input$file1)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath)
  })

  observe({
    req(datos())
    updateSelectInput(session, "columna", choices = names(datos()), selected = NULL)
  })

  mediaR <- function() {
    req(input$calcular)
    req(input$columna)
    if (is.null(datos()))
      return(NULL)

    columna_seleccionada <- datos()[[input$columna]]
    media <- mean(columna_seleccionada, na.rm = TRUE)
    print(paste("La media de la columna", input$columna, "es:", media))

    # Gráfico de cajas
    boxplot_data <- data.frame(Valor = columna_seleccionada)
    ggplot(boxplot_data, aes(x = "", y = Valor)) +
      geom_boxplot(fill = "pink") +
      labs(title = "Diagrama de Cajas (Media)", y = "Valor") +
      theme_minimal()
  }

  output$media_resultado <- renderPrint({
    mediaR()
  })

  output$media_plot <- renderPlot({
    mediaR()
  })
}

# Combinar UI y server en un solo objeto Shiny
shinyApp(ui = ui, server = server)



