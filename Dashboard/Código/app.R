# Cargando librerías necesarias:
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# Cargando datos limpios:
file_path <- "~/Jimena Rode Aguilar/Maestría - Ciencia de Datos del Comportamiento/Modulo 8 - Reto 2/datos_clean.csv"
datos_clean <- read_csv(file_path)

# Renombrando las columnas:
datos_clean <- datos_clean %>%
  rename(
    Nombre = name,
    Pais = cntry,
    Genero = Gndr,
    FrecuenciaFelicidad = W4q50,
    PesoMuestral = C2weight,
    AnosEducacion = Eduyrs,
    UsoInternet = Netusoft
  )

# Definiendo la interfaz de usuario:
ui <- fluidPage(
  titlePanel("Dashboard de Datos de Encuesta CRONOS-2"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Selecciona una variable para visualizar:",
                  choices = c("Frecuencia de Felicidad" = "FrecuenciaFelicidad", 
                              "Peso Muestral" = "PesoMuestral", 
                              "Años de Educación" = "AnosEducacion", 
                              "Uso de Internet" = "UsoInternet")),
      selectInput("country", "Selecciona un país:",
                  choices = unique(datos_clean$Pais)),
      selectInput("gender", "Selecciona un género:",
                  choices = c("Masculino", "Femenino")),
      selectInput("ageRange", "Selecciona un rango de edad:",
                  choices = c("18-30", "31-45", "46-60", "61+"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico de Barras", plotlyOutput("barPlot")),
        tabPanel("Gráfico de Dispersión", plotlyOutput("scatterPlot")),
        tabPanel("Boxplot", plotlyOutput("boxPlot")),
        tabPanel("Gráfico de Líneas", plotlyOutput("linePlot")),
        tabPanel("Mapa de Calor", plotlyOutput("heatMap")),
        tabPanel("Gráfico Circular", plotlyOutput("pieChart")),
        tabPanel("Histogramas", plotlyOutput("histogram")),
        tabPanel("Boxplot de Ingresos", plotlyOutput("incomeBoxPlot"))
      )
    )
  )
)

# Definiendo la lógica del servidor:
server <- function(input, output) {
  output$barPlot <- renderPlotly({
    data <- datos_clean %>%
      filter(Pais == input$country)
    
    p <- ggplot(data, aes_string(x = input$variable, fill = input$variable)) +
      geom_bar() +
      labs(title = paste("Distribución de", input$variable, "en", input$country),
           x = input$variable,
           y = "Frecuencia") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$scatterPlot <- renderPlotly({
    p <- ggplot(datos_clean, aes(x = AnosEducacion, y = PesoMuestral, color = Pais)) +
      geom_point(alpha = 0.7) +
      labs(title = "Relación entre Años de Educación y Peso Muestral",
           x = "Años de Educación",
           y = "Peso Muestral") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$boxPlot <- renderPlotly({
    p <- ggplot(datos_clean, aes(x = Pais, y = FrecuenciaFelicidad, fill = Pais)) +
      geom_boxplot() +
      labs(title = "Distribución de la Frecuencia de Felicidad por País",
           x = "País",
           y = "Frecuencia de Felicidad") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$linePlot <- renderPlotly({
    p <- ggplot(datos_clean, aes(x = AnosEducacion, y = PesoMuestral, color = Pais)) +
      geom_line() +
      labs(title = "Tendencias de Años de Educación en Relación con el Peso Muestral",
           x = "Años de Educación",
           y = "Peso Muestral") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$heatMap <- renderPlotly({
    heatmap_data <- datos_clean %>%
      group_by(Pais, UsoInternet) %>%
      summarize(frequency = n())
    
    p <- ggplot(heatmap_data, aes(x = Pais, y = UsoInternet, fill = frequency)) +
      geom_tile() +
      labs(title = "Mapa de Calor del Uso de Internet por País",
           x = "País",
           y = "Frecuencia de Uso de Internet") +
      scale_fill_gradient(low = "white", high = "blue") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$pieChart <- renderPlotly({
    pie_data <- datos_clean %>%
      filter(Pais == input$country) %>%
      count(Genero)
    
    p <- plot_ly(pie_data, labels = ~Genero, values = ~n, type = 'pie') %>%
      layout(title = paste("Distribución de Género en", input$country))
    
    p
  })
  
  output$histogram <- renderPlotly({
    p <- ggplot(datos_clean, aes(x = AnosEducacion, fill = Genero)) +
      geom_histogram(binwidth = 1, position = "dodge") +
      labs(title = "Distribución de Años de Educación por Género",
           x = "Años de Educación",
           y = "Frecuencia") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$incomeBoxPlot <- renderPlotly({
    p <- ggplot(datos_clean, aes(x = Genero, y = PesoMuestral, fill = Genero)) +
      geom_boxplot() +
      labs(title = "Distribución del Peso Muestral por Género",
           x = "Género",
           y = "Peso Muestral") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Ejecutando la aplicación shiny:
shinyApp(ui = ui, server = server)
