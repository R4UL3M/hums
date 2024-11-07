library(shiny)
library(bslib)
library(tidyverse)
library(janitor)

# Cleaning ----

# if (interactive()){
#   if(getwd() != "D:/OneDrive/R/aqhums") setwd("D:/OneDrive/R/aqhums")
#    }

## Consultas ---

df_consultas_realizadas <- read_csv("data/consultas_realizadas.csv") %>%
  clean_names("snake") %>%
  mutate(
    fecha = as.Date(fecha),
    codigo = as.factor(codigo)
  )

df_agendas_por_servicio <- df_consultas_realizadas %>% 
  group_by(servicio) %>% 
  summarise(agendas = c(agenda))

# servicios <- c(
#   "ACV", "CCA", "CGAD",
#   "COM", "CPE", "CPL", "CTO",
#   "DER", "GIN", "NRC", "OFT",
#   "ORL", "TRA", "URO"
#   )

servicios <- c(
  "Vascular", "Cardíaca", "General", "Maxilofacial", "Pediátrica",
  "Plástica", "Torácica",   "Dermatología", "Ginecología", "Neurocirugía", 
  "Oftalmología", "ORL", "Traumatología", "Urología"
)

## RDQ ----

## Intervenciones ----


# UI ----



ui <- page_navbar(
  id = "navigation",
  # bg = "#78c2ad",
  theme = bs_theme(
    preset = "zephyr", 
    version = 5),
  
    
  title = tags$span(icon("hospital"),"Área Quirúrgica"),

  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  # ),
  
  ## Main sidebar ----
  
  sidebar = sidebar(gap = 20,
    
    dateRangeInput(
      inputId = "periodo",
      label = "Periodo",
      format = "yyyy-mm-dd", 
      start = "2021-01-01", end = Sys.Date(),
      max = Sys.Date(),
      startview = "month"
    ),
    
    selectInput(
      inputId = "servicio",
      label = "Servicio",
      choices =  c("Todos", servicios)
      ),
    

    ### Paneles condicionales ----
    
      #### Consultas ----

    conditionalPanel(
      condition = "input.navigation == 'consultas'",
      
      selectInput(
        "agenda", "Agenda", 
        choices = df_consultas_realizadas %>% pull(agenda) %>% unique(), 
        multiple = TRUE)
    ),
    
      #### RDQ
    
    conditionalPanel(
      condition = "input.navigation == 'rdq'",
      
      checkboxGroupInput(
        "garantia", "Garantía",
        choices = c("Sí", "No"),
        selected = c("Sí", "No")
      ),
      
      checkboxGroupInput(
        "plazos", "Plazos",
        choices = c("<31 días", "31-60 días", "61-90 días", "91-180 días", 
                    "181-365 días", ">365 días"),
        selected = c("<31 días", "31-60 días", "61-90 días", "91-180 días", 
                     "181-365 días", ">365 días")
      )
    ),
      
      #### Intervenciones ----

    conditionalPanel(
      condition = "input.navigation == 'intervenciones'",
      
      selectInput(
        "unidad", "Unidad",
        choices = NULL
      ),
      
      selectInput(
        "quirofano", "Quirófano",
        choices = c("Todos",1:29,paste0("MI",2:6)),
        selected = "Todos",
      ),
      checkboxGroupInput(
        "jornada", "Tipo de Jornada",
        choices = c("Mañanas", "Prolongación", "Continuada", 
                    "Exceso Jornada", "Autoconcertación", "Urgencia"),
        selected = c("Mañanas", "Prolongación", "Continuada", 
                    "Exceso Jornada", "Autoconcertación", "Urgencia")
      )
      
    ),
    
    conditionalPanel(
      condition = "input.navigation == 'patologias'",
      
      selectInput(
        "patologia", "Patología",
        choices = NULL,
        selected = NULL
      )
      
    )
    
    
    
    ),
  
  ## Nav panels ----
  
  
  ### Concultas ----
  
  nav_panel(
    title = "Consultas", 
    value = "consultas",
    icon = icon("user-doctor"),

    layout_columns(

        col_widths = c(3,3,3,3,6,6,4,4,4),
        row_heights = c(1,2,2),
    

          #### Boxes ----
          
          value_box(
            title = tags$span(
              "Demora",
              tooltip(
                icon("circle-info"),
                "Demora media en días a fecha del último día del periodo 
                de tiempo seleccionado"
              )),
            value = p("100", icon("calendar-days")),
            showcase_layout = "top right",
            theme = "info",
            showcase = icon("clock", style="font-size:2rem")
          ),
          
          value_box(
            title = tags$span(
              "Pendientes",
              tooltip(
                icon("circle-info"),
                "Número de primeras visitas pendientes de ser valoradas a 
                fecha del último día del periodo de tiempo seleccionado"
              )),
            value = p("120", icon("person")),
            showcase_layout = "top right",
            theme = "info",
            showcase = icon("envelope", style="font-size:2rem"),
          ),
          
          value_box(
            title = tags$span(
              "Primeras",
              tooltip(
                icon("circle-info"), 
                "Número de primeras visitas realizadas durante todo 
                el periodo de tiempo seleccionado"
              )),
            value = p("80", icon("percent")),
            showcase_layout = "top right",
            theme = "info",
            showcase = icon("door-open", style="font-size:2rem"),
          ),
          
          value_box(
            title = tags$span(
              "Demora",
              tooltip(
                icon("circle-info"),
                "Rendimiento de consultas externas para el servicio/agenda-s,  
                periodo de tiempo y filtros seleccionados"
              )),
            value = p("67", icon("percent")),
            showcase_layout = "top right",
            theme = "info",
            showcase = icon("battery-three-quarters", style="font-size:2rem"),
          ),
        
        #### Plots ----
        
        card(
          full_screen = TRUE,
          card_header(
             "Actividad Realizada",
             popover(
               icon("gear"),
               selectInput(
                 "de_actividad_agrupacion", "Agrupar por",
                 choices = c("Ámbito", "Agenda", "Tipo de Visita", "Tipo de Jornada"),
                 selected = NULL
                 )
             ),
             class = "d-flex justify-content-between"
             ),
    
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            "Rendimiento",
            popover(
              icon("gear"),
              selectInput(
                "ce_rendimiento_agrupacion", "Agrupar por",
                choices = c("Agenda", "Tipo de Jornada"),
                selected = NULL
              )
            ),
            class = "d-flex justify-content-between"
          ),
          
        ),
        
        
        card(
          full_screen = TRUE,
          card_header(
            "Demora",
            popover(
              icon("gear"),
              selectInput(
                "ce_demora_agrupacion", "Agrupar por",
                choices = c("Agenda", "Prioridad"),
                selected = NULL
              )
            ),
            class = "d-flex justify-content-between"
          ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            "Espera",
            popover(
              icon("gear"),
              selectInput(
                "ce_espera_agrupacion", "Agrupar por",
                choices = c("Agenda", "Prioridad", "Tipo de Jornada"),
                selected = NULL
              )
            ),
            class = "d-flex justify-content-between"
          ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            "Primeras Pendientes",
            popover(
              icon("gear"),
              selectInput(
                "ce_primeras_pendientes_agrupacion", "Agrupar por",
                choices = c("Ámbito", "Servicio Peticionario", "Prioridad"),
                selected = NULL
              )
            ),
            class = "d-flex justify-content-between"
          ),
        )
      )
  ),
  
  ### RDQ ----
  
  nav_panel(
    title = "Demanda", 
    value = "rdq",
    icon = icon("clipboard-user"),
            
      layout_columns(
        
        col_widths = c(3,3,3,3,6,6,6,6),
        row_heights = c(1,1.5,1.5),
        
        #### Boxes ----
        
        value_box(
          title = tags$span(
            "Esperando",
            tooltip(
              icon("circle-info"),
              "Número de paciente en lista de espera a fecha del último
              día del periodo de tiempo seleccionado"
            )),
          value = p("1345", icon("person")),
          showcase_layout = "top right",
          theme = "warning",
          showcase = icon("users-line", style="font-size:2rem"),
        ),
        
        value_box(
          title = tags$span(
            "Demora",
            tooltip(
              icon("circle-info"),
              "Demora media en días a fecha del último día del periodo de 
              tiempo seleccionado"
            )),
          value = p("100", icon("calendar-days")),
          showcase_layout = "top right",
          theme = "warning",
          showcase = icon("clock", style="font-size:2rem"),
        ),
        
        value_box(
          title = tags$span(
            "> 180 días",
            tooltip(
              icon("circle-info"),
              "Número de paciente en lista de espera con demora superior a 
              180 días a fecha del último día del periodo de tiempo seleccionado"
            )),
          value = p("1234", icon("person")),
          showcase_layout = "top right",
          theme = "warning",
          showcase = icon("cloud-sun", style="font-size:2rem"),
        ),
        
        value_box(
          title = tags$span(
            "Garantía",
            tooltip(
              icon("circle-info"),
              "Número de paciente en lista de espera fuera de garantía 
              a fecha del último día del periodo de tiempo seleccionado"
            )),
          value = p("120", icon("person")),
          showcase_layout = "top right",
          theme = "warning",
          showcase = icon("ban", style="font-size:2rem"),
        ),
        
        #### Plots ----
        
        card(
          full_screen = TRUE,
          card_header(
            "Activos en Espera",
            popover(
              icon("gear"),
              selectInput(
                "le_activos_agrupacion", "Agrupar por",
                choices = c("Ámbito", "Prioridad", "Garantía", "Derivable"),
                selected = NULL
              ),
              
              "Filtrar por",
              
              numericInput(
                "le_activos_plazoinf", "Espera mínima (días)",
                min = 0,
                max = 99999999999999999,
                value = 0
              ),
              numericInput(
                "le_activos_plazosup", "Espera Máxima (días)",
                min = 0,
                max = 99999999999999999,
                value = 1000
              ) 
            ),
            class = "d-flex justify-content-between"
          ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            "Salidas"
          ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            "Demora",
            popover(
              icon("gear"),
              selectInput(
                "le_demora_agrupacion", "Agrupar por",
                choices = c("Ámbito", "Prioridad", "Garantía"),
                selected = NULL
              )
            ),
            class = "d-flex justify-content-between"
          ),
        ),
        

        card(
          full_screen = TRUE,
          card_header(
            "Espera",
            popover(
              icon("gear"),
              selectInput(
                "le_espera_agrupacion", "Agrupar por",
                choices = c("Ámbito", "Prioridad", "Garantía"),
                selected = NULL
              )
            ),
            class = "d-flex justify-content-between"
          ),
        ),
        
      )
            
            
  ),
  
  
  ### Quirófanos ----
  
  nav_panel(
    title = "Intervenciones", 
    value = "intervenciones",
    icon = icon("head-side-mask"),
            
      layout_columns(
        
        col_widths = c(4,4,4,6,6,6,6),
        row_heights = c(1,1.5,1.5),
        
        #### Boxes ----
        
        value_box(
          title = tags$span(
            "Procedimientos",
            tooltip(
              icon("circle-info"),
              "Número de procedimientos realizados durante el periodo de tiempo 
              y filtros seleccionado"
            )),
          value = p("12300", icon("person")),
          showcase_layout = "top right",
          theme = "primary",
          showcase = icon("bed-pulse", style="font-size:2rem"),
        ),
        
        value_box(
          title = tags$span(
            "Espera",
            tooltip(
              icon("circle-info"),
              "Tiempo de espera medio de los pacientes intervenidos durante 
              el periodo de tiempo seleccionado"
            )),
          value = p("1234", icon("calendar-days")),
          showcase_layout = "top right",
          theme = "primary",
          showcase = icon("cloud-sun", style="font-size:2rem"),
        ),
        
        value_box(
          title = tags$span(
            "Rendimiento",
            tooltip(
              icon("circle-info"),
              "Rendimiento quirúrgico durante el periodo de tiempo 
              seleccionado"
            )),
          value = p("67", icon("percent")),
          showcase_layout = "top right",
          theme = "primary",
          showcase = icon("battery-three-quarters", style="font-size:2rem"),
        ),
        
        
        #### Plots ----
        
        card(
          full_screen = TRUE,
          card_header(
            "Total Realizadas"
            ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            "Extraordinaria Realizadas"
            ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            "Rendimiento"
            ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            "Cancelaciones"
            ),
        )
      )
            
  ),
  
  
  ### Patologías ----
  
  nav_panel(
    title = "Patologías", 
    value = "patologias",
    icon = icon("file-medical"),
            
      layout_columns(
        col_widths = c(6,6,6,6),
        row_heights = c(1,1),
        
        #### Plots ----
        
        card(
          full_screen = TRUE,
          card_header(
            "Entradas"
            ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
           "Espera"
           ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
           "Total Realizadas"
          ),
        ),
        
        card(
          full_screen = TRUE,
          card_header(
           "Extraordinaria Realizadas"
           ),
        )
        
      )
            
  ),
  
  ### Cuadro Mandos ----
  
  nav_panel(
    title = "Cuadro de Mandos",
    value = "cuadro_mandos",
    icon = icon("gamepad"),
    
  ),
  
  
  ### Modelización ----
  
  nav_panel(
    title = "Modelización",
    value = "modelizacion",
    icon = icon("arrow-trend-up"),
    
    
  )
  
  
  
)




# Server ----

server <- function(input, output, session){
  
  bs_themer()
  ## Data consultas ----
  
  ### Main sidebar ----
  
  observeEvent(input$servicio,{
    opciones <- switch(
      input$servicio,
      "Todos" = "Todos",
      "Traumatología" = c("Todas", "Cadera", "Columna", "Fracturas", "Rodilla", "..."),
      "General" = c("Todas", "Colorectal", "Hepatobiliar", "Endocrinología", "Esofagogástrica", "...")
    )
    updateSelectInput(session, "unidad", choices = opciones)
  })
  
  
  df_consultas_realizadas_filter <- reactive({
    
    if(input$servicio == "Todos"){
      df_consultas_realizadas %>% 
        filter(
          fecha >= input$periodo[1] & fecha <= input$periodo[2]
          )
      
    }else{
      df_consultas_realizadas %>% 
        filter(
          servicio == input$servicio & 
          fecha >= input$periodo[1] & fecha <= input$periodo[2]
      ) 
    }
  })
  
  tiempo <- reactiveValues()
  
  observe({
    tiempo$mes_inicio <- month(input$periodo[1])
    tiempo$mes_final <- month(input$periodo[2])
    tiempo$anio_inicio <- year(input$periodo[1])
    tiempo$anio_final <- year(input$periodo[2])
  })
  
  ts_total <- reactive({
    ts(
      data = df_consultas_realizadas_filter() %>% 
        group_by(year(fecha), month(fecha)) %>% count() %>% pull(n),
      start = c(tiempo$anio_inicio, tiempo$mes_inicio),
      frequency = 12
    )
  })
  

    
  
  

  
  ## Consultas ----
  
  output$consultas_total <- renderPlot({
    plot(ts_total())
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
