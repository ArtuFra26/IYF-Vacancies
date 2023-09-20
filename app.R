#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Leer el código y las librerías necesarias
library(shiny)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, plyr, stringr, ggplot2, ggthemes, stringi, ggpubr, tm, lubridate, rgdal, broom,
               shinyWidgets, shinydashboard, knitr, skimr, arules, arulesViz, reshape2, tidytext, showtext)
source("funciones_vacantes.R")

#Leer los datos requeridos para el análisis
#df_mx <- read_excel("data/Vacantes_App_130923.xlsx", guess_max = 50000)

#Lista de directorios
direcs <- c(
        #Directorio de Arturo    
        "C:/Users/MEX-1ZNLJL3/OneDrive - International Youth Foundation/Documentos/Vacantes_App_130923.xlsx")

#Función para la lectura de la base
df_mx <- lapply(direcs, readVacs)

#Transformar lista a dataframe
df_mx <- data.frame(df_mx[!is.na(df_mx)])

#Definir lista de areas y de estados
ar_ls <- unique(df_mx$Area.Amplia)

#Modificar vector de educacion
df_mx <- edu_rd(df_mx)

#Definir lista de áreas profesionales
ax <- data.frame(unique(df_mx$Area.Amplia))
ax <- ax[order(ax$unique.df_mx.Area.Amplia.),]

#Definir lista de programas y trayectos
ex <- read_excel("data/ProgramasTrayectos_IYFMex_060923.xlsx")
ex <- ex %>%  arrange(Programa)

#Definir lista de claves de estado
ux_mx <- read_excel("data/ListaEstados_App_120923.xlsx", sheet = "México")
ux_cl <- read_excel("data/ListaEstados_App_120923.xlsx", sheet = "Colombia")

#Importar datos espaciales
shape_mx <- readOGR(dsn ="data/Capas México", layer = "Entidades_Federativas", use_iconv = TRUE)
shape_col <- readOGR(dsn ="data/Capas Colombia", layer = "MGN_DPTO_POLITICO", use_iconv = TRUE)

#Ordenar datos espaciales
{
  #Generar dataframes de datos espaciales
  capas_mx <- broom::tidy(shape_mx)
  capas_col <- broom::tidy(shape_col)
  
  #Unificar con identificadores estatales
  shape_mx@data$id <- rownames(shape_mx@data)
  shape_col@data$id <- rownames(shape_col@data)
  
  #Unificar dataframes de datos espaciales
  capas_mx <- inner_join(capas_mx, shape_mx@data, by = "id")
  capas_col <- inner_join(capas_col, shape_col@data, by = "id")
}

#Cargar fuentes de gráficos
font_add_google("Lora", "lora")
showtext_auto()



# Configuración de la interfaz de usuarios 
ui <- navbarPage(title = "Visualizador de datos de vacantes",
  
  #Página cero: presentación de la herramienta
  
  tabPanel(title = "Presentación", 
    
    fluidPage(
      
      #Generar cuadro contenedor de texto
      HTML('<style>
       .box {
       border: 1px solid #76D7C4;
       padding: 5px;
       fig-align: center;
       text-align: justify;
       width: 100%;
       margin: 0 auto; 
       }
       </style>'),
    
      br(),
      
      #Incorporar texto de presentación
      fluidRow(
                box(
                  uiOutput("iyf_pres", align = "center"),
                  hr(),
                  br(),
                  uiOutput('txt_pres'),
                  br(),
                  width = 4
        )
      )
    )
  ),
      
  #Primera página: distribución espacial
  
  tabPanel(title = "Dinámicas espaciales",
    
    fluidPage(
      
      #Título de la página
      titlePanel(tags$b("Dinámicas espaciales")),
      
      #Menú para la selección de características
      sidebarLayout(
        sidebarPanel(
          h3(strong("Vacantes por estado")),
          p("Dinámicas espaciales de la demanda de trabajo"),
          br(),
          
          #Input de área profesional deseada
          selectInput("pais_est", h5(strong("País")),
                      choices = c("Brasil", "Colombia", "El Salvador", "Mexico", "Panama")), 
          
          #Input de área profesional deseada
          selectizeInput(inputId = "estado", 
                         label = "Entidad",
                         choices = NA, 
                         multiple = TRUE,
                         options = list(placeholder = "Seleccione la entidad que desea visualizar")),
          
          #Input de cantidad de posiciones
          numericInput("est_posi", h5(strong("Cantidad de resultados")), 3),
          p(helpText("Seleccione la cantidad de posiciones que desea visualizar en la gráfica.", style  = "font-si8pt")),
          
          #Input de nivel de escolaridad deseado
          selectInput("sec_est", h5(strong("Área profesional")),
                      choices = ax),
          
          #Input de nivel de escolaridad deseado
          selectInput("pro_est", h5(strong("Programa de IYF")),
                      choices = unique(ex$Programa)),
          
          #Botón para actualizar búsqueda
          actionButton("button_es", "Actualizar"),
          
          #Logo decorativo de IYF
          img(src = "iyf_logo.png", height = 90, width = 200, align = "right"),
          br(),
          br(),
          br()
        ),
        
        #Panel principal para la impresión de resultados 
        mainPanel(width = 8,
          tabsetPanel( 
            
            #Sección de competencias técnicas
            tabPanel("Panorama estatal",
                     br(),
                     p("El siguiente gráfico presenta las áreas de actividad profesional más demandadas en la entidad seleccionada.", style = "text-align:justify;"),
                     br(),
                     plotOutput("pan_est", width = "100%")),
            
            #Sección de competencias técnicas
            tabPanel("Demanda sectorial",
                     br(),
                     p("El siguiente gráfico presenta la distribución espacial, al nivel de la entidad, de la demanda de trabajo del área profesional seleccionada."),
                     br(),
                     p(strong("Nota 1:"), "los mapas únicamente están disponibles para México y para Colombia."),
                     p(strong("Nota 2:"), "antes de intentar visualizar un mapa, es importante seleccionar a México o Colombia en el menú de país y pulsar en el botón de 'Actualizar'; posteriormente
                       puede dar paso a la selección del área profesional de interés."),
                     br(),
                     plotOutput("est_sect", width = "100%")),
            
            #Sección de habilidades suaves
            tabPanel("Demanda de programas de IYF",
                     br(),
                     p("El siguiente gráfico presenta la distribución espacial, al nivel de la entidad, de la demanda de trabajadores calificados en las comptencias profesionales instruidas a través
                       de los programas de IYF México."),
                     br(),
                     p(strong("Nota:"), "antes de intentar visualizar un mapa, es importante seleccionar a México en el menú de país y pulsar en el botón de 'Actualizar'; posteriormente
                       puede dar paso a la selección del programa de interés."),
                     br(),
                     plotOutput("est_pro", width = "100%"))
        )
      )
     )
    )
  ),

  #Segunda página: área profesional
  
  tabPanel(title = "Área profesional",
    
    fluidPage(
      #Modificar el estilo de la barra lateral
      tags$head(
        tags$style(HTML("
          * {font-size = 14px; }"
        ))),
      
      #Configurar widgets de Shiny
      useShinydashboard(),
      
      #Título de la página
      titlePanel(tags$b("Área profesional")),
      
      #Menú para la selección de características
      sidebarLayout(
        sidebarPanel(
          h3(strong("Características de vacantes")),
          p("Aplicación para la visualización de datos de vacantes."),
          br(),
          
          #Input de área profesional deseada
          selectInput("pais_ar", h4(strong("País")),
                      choices = c("Brasil", "Colombia", "El Salvador", "Mexico", "Panama")),
          
          #Input de área profesional deseada
          selectInput("area_amplia", h4(strong("Área profesional")),
                      choices = ax), 
                      
          #Input de nivel de escolaridad deseado
          selectInput("nvl_esc", h4(strong("Nivel de escolaridad")),
                      choices = c("Primaria", "Secundaria", "Bachillerato", "Tecnica", "Todos", "Practicas", "Licenciatura", "Ingenieria","Maestria"),
                      selected = "Todos"),
          
          #Seleccionar cantidad de posiciones a graficar
          sliderInput("top_menciones", h4(strong("Cantidad de resultados")), 
                      min = 3, max = 15, value = 3),
          p(helpText("Seleccione la cantidad de competencias que desea visualizar en la gráfica.", style  = "font-si8pt")),
          
          #Botón para actualizar búsqueda
          actionButton("button", "Actualizar"),
          
          #Logo decorativo de IYF
          img(src = "iyf_logo.png", height = 90, width = 200, align = "right"),
          br(),
          br(),
          br()
        ),
        
        #Panel principal para la impresión de resultados 
        mainPanel(
          tabsetPanel( 
          
          #Sección de numeralia
          tabPanel("Numeralia",
          br(),
          br(),
          p("La siguiente lista presenta datos básicos del sector dados el país y el nivel de escolaridad seleccionados."),
          br(),
          box(title = "Numeralia por área profesional", color = "teal", 
              style = "font-size:16px; padding-top:10px;",
            tags$div(align = "left", 
              tags$ul(
                tags$li(textOutput("num_ar1")),
                tags$li(textOutput("num_ar2")),
                tags$li(textOutput("num_ar3")),
                tags$li(textOutput("num_ar4"))
              )
            )
          )),
          
          #Sección de competencias técnicas
          tabPanel("Tendencia del empleo",
          br(),
          p("El siguiente gráfico representa la tendencia y estacionalidad temporales de la demanda de trabajadores en el área profesional seleccionada."),
          br(),
          p(strong("Nota:"), "la periodicidad base de la serie de tiempo es mensual, con enero de 2023 como primer periodo."),
          br(),
          plotOutput("ten_job", width = "100%")),
          
          #Sección de competencias técnicas
          tabPanel("Competencias técnicas",
          br(),
          p("El siguiente gráfico representa las competencias técnicas más demandadas por empleadores en el área profesional seleccionada."),
          br(),
          p(strong("Nota:"), "el universo de comptencias representado en los gráficos está limitado a las competencias prioritarias identificadas por IYF."),
          br(),
          plotOutput("sel_comp", width = "100%"),
          ),
          
          #Sección de habilidades suaves
          tabPanel("Habilidades suaves",
          br(),
          p("El siguiente gráfico representa las habilidades suaves más demandadas por empleadores en el área profesional seleccionada."),
          br(),
          p(strong("Nota:"), "el universo de habilidades representado en los gráficos está limitado a las habilidades prioritarias identificadas por IYF."),
          br(),
          plotOutput("sk_comp", width = "100%"))
          )
        )
      )
    )
  ),
  
  #Tercera página: trayectos de programas
  
  tabPanel(title = "Programas de IYF",
         
         #Título de la página
         titlePanel(tags$b("Programas de IYF")),
         
         #Menú para la selección de características
         sidebarLayout(
            sidebarPanel(
              h3(strong("Programas y trayectos")),
              p("Demanda de habilidades asociadas a programas de IYF"),
              br(),
               
          #Input de área profesional deseada
          selectInput("programa", h5(strong("Programa")),
                           choices = unique(ex$Programa)), 
          
          #Input de cantidad de posiciones
          numericInput("top_posi", h5(strong("Cantidad de resultados")), 3),
          p(helpText("Seleccione la cantidad de posiciones que desea visualizar en la gráfica.", style  = "font-si8pt")),
          br(),
          
               
          #Input de competencia técnica a analizar
          selectizeInput(inputId = "hsk_ls", 
                         label = "Competencias técnicas",
                         choices = NA, 
                         multiple = TRUE,
                         options = list(placeholder = 
                                          "Seleccione la competencia técnica que desea visualizar")),
          br(),
          
          #Input de competencia técnica a analizar
          selectizeInput(inputId = "ssk_ls", 
                         label = "Habilidades suaves",
                         choices = NA, 
                         multiple = TRUE,
                         options = list(placeholder = 
                                          "Seleccione la habilidad suave que desea visualizar")),
        
              
          #Botón para actualizar búsqueda
          actionButton("button_pro", "Actualizar"),
          
          #Logo decorativo de IYF
          img(src = "iyf_logo.png", height = 90, width = 200, align = "right"),
          br(),
          br(),
          br()
        ),       
           
          #Panel principal para la impresión de resultados 
          mainPanel(
            tabsetPanel( 
              
              #Sección de demanda sectorial
              tabPanel("Demanda sectorial",
                       br(),
                       p("El siguiente gráfico representa la demanda, por área profesional, de los trayectos currículares del programa seleccionado."),
                       br(),
                       plotOutput("tra_de_sec", width = "100%")),
              
              #Sección de habilidades duras
              tabPanel("Oferta de posiciones",
                       br(),
                       p("El siguiente gráfico presenta los puestos de trabajo que, en virtud de la concordancia de competencias técnicas demandadas, son accesibles para los egresados de los
                         trayectos currículares del programa seleccionado."), 
                       br(),
                       p(strong("Nota:"), "la construcción de la lista de posiciones toma como insumo el campo de 'Título' de la publicación de empleo, de ahí que la lista pueda contener elementos
                         no equiparables a una posición laboral (Ejemplo: 'bonos'; '100% nómina')."),
                       br(),
                       plotOutput("ofr_pos", width = "100%")),
              
              #Sección de habilidades duras
              tabPanel("Demanda de competencias técnicas",
                       br(),
                       p("El siguiente gráfico permite visualizar cuáles son las competencias técnicas más estrechamente relacionadas con la competencia seleccionada en el menú desplegable de cada programa."),
                       br(),
                       p("Es necesario tener en cuenta cuatro elementos a fin de interpretar la gráfica:"),
                       tags$ul(
                         tags$li(p("La competencia seleccionada se encontrará en la última posición del eje vertical del gráfico.")),
                         tags$li(p("Las competencias asociadas al elemento seleccionado serán representadas en el eje vertical como una lista ordenada; de manera que la competencia ubicada en la primera posición de la lista
                                   puede ser interpretada como la competencia que aparece de manera más recurrente en las publicaciones de empleo junto con la competencia seleccionada.")),
                         tags$li(p("La intensidad del color de la flecha es indicativa de la significancia estadística de la asociación entre las dos competencias. Si la flecha cuenta con un color rojo intenso, es posible postular
                                   que la asociación observada entre las competencias no fue un producto del azar.")),
                         tags$li(p("Es posible seleccionar más de una competencia para representar gráficamente. En ese caso, el gráfico se dividirá en un número de partes equivalente a la cantidad seleccionada de competencias. Seleccionar más
                                   de una competencia puede ser de utilidad en caso de que se desee comparar los patrones de asociación de dos competencias que podrían guardar algún vínculo teórico o conceptual.")), 
                         style = "text-align: justify;"
                       ),
                       br(),
                       p(strong("Nota:", "también es posible posible manipular la cantidad de asociaciones graficadas usando el botón de 'Cantidad de resultados'.")),
                       plotOutput("tec_pro", width = "100%")),
              
              #Sección de habilidades suaves
              tabPanel("Demanda de habilidades suaves",
                       br(),
                       p("El siguiente gráfico permite visualizar cuáles son las habilidades suaves más estrechamente relacionadas con la habilidad seleccionada en el menú desplegable de cada programa."),
                       br(),
                       p(strong("Nota:"), "el uso e interpretación de este gráfico sigue la misma lógica que la observada para el gráfico de la sección de 'Demanda de competencias técnicas'."),
                       plotOutput("sft_pro", width = "100%"))
            )
          )   
    )       
  )           
)

# Definición de la lógica del servidor 
server <- function(input, output, session){
  
  #Tab 0: Presentación
  
    #Imagen de IYF
    output$iyf_pres <- renderUI({
      tags$img(src = "iyf_logo.png", width = '250px', align = "center")
    })
    
    #Texto de presentación
    output$txt_pres <- renderUI({
      HTML(paste0("<p>", "<b> Introducción: </b>", "esta aplicación web fue diseñada con el objetivo de facilitar la visualización y análisis de los datos de demanda de trabajo que son publicados a través de
                  un conjunto de portales web de empleo en México, Panamá, El Salvador, Colombia y Brasil.", "</p>", "<p>", "Esta aplicación constituye el eslabón final de un flujo de trabajo cuyo inicio radica
                  en la adquisición de los datos mediante un procedimiento de scrapping, su posterior limpieza y documentación a través de un algoritmo fijo y, finalmente, su validación por el personal de IYF.", "</p>",
                  "<p>", "En este sentido, es necesario que la incorporación de nuevos datos a la aplicación tenga lugar en el contexto del flujo de trabajo; en tanto que el adecuado funcionamiento de la herramienta depende de
                  que sus insumos se conformen a los requerimientos fijados como parte del protocolo de limpieza de datos.", "</p>", "<p>", "La aplicación consta de tres secciones de visualización y análisis:
                  <b> dinámicas espaciales; área profesional; programas de IYF </b>. Cada una de las secciones busca explotar una dimensión distinta del conjunto de datos, tomando en consideración las necesidades de información
                  expresadas por el personal de la fundación.", "</p>",  "<p>", "<b> Nota sobre el uso de la herramienta: </b> en cada sección encontrará un menú lateral poblado por un conjunto de sub-menús desplegables y, al fondo,
                  por un botón para <b> 'Actualizar' </b>. La función de los sub-menús desplegables consiste en especificar la selección de datos que el usuario desea visualizar. La introducción de un cambio en estos sub-menús requerirá 
                  que el usuario presione el botón de 'Actualizar' a fin de que se ejecuten los cambios solicitados en la información o figuras presentadas en el panel principal de la aplicación."))
    })
  
  
  #Tab 1: Dinámica espacial
  
    #Generar dataframes de trabajo para la sesión
    df_mx2 <- df_mx
    ex2 <- ex
    ux2 <- ux_mx
    ux3 <- ux_cl
    capas_mx2 <- capas_mx
    capas_col2 <- capas_col
    
    #Generar objeto de tipo reactive
    makeReactiveBinding("df_mx2")
    
    #Generar objeto de tipo reactive
    makeReactiveBinding("ex2")
  
    #Generar objeto reactivo con claves de estados
    makeReactiveBinding("ux2")
    
    #Generar objeto reactivo con claves de departamentos
    makeReactiveBinding("ux3")
    
    #Generar objeto reactivo con shape de estados
    makeReactiveBinding("capas_mx2")
    
    #Generar objeto reactivo con shape de departamentos
    makeReactiveBinding("capas_col2")
    
    #Especificar argumentos de función reactiva
    df_mp <- reactive({
      input$button_es
      isolate({
        #Criterios de subsetting de la base original
        df_mx2 <- pais_sbs(df_mx2, input$pais_est)
      })
    })
    
    #Generar lista de nombres de competencia técnica
    est_tab <- reactive({
      #Llamar objeto reactivo
      df_mx2 <- df_mp()
      #Definir lista de estados
      ix <-  df_mx2 %>% 
             dplyr::select(Estado) %>%
             dplyr::distinct(Estado) %>%
             arrange(Estado)
    })
    observe({updateSelectInput(session, inputId = "estado", label = NULL, 
                               choices = est_tab()$Estado, selected = NULL)})
  
    #Gráfico de panorama estatal 
    output$pan_est <- renderPlot({
      df_mx2 <- df_mp()
      sec_est(df_mx2, input$estado, input$est_posi, input$pais_est) 
    },  height = 550, width = 800)
  
    
    #Gráfico de demanda estatal por sector
    output$est_sect <- renderPlot({
      df_mx2 <- df_mp()
      map_sec(input$pais_est, ux2, ux3, df_mx2, capas_mx2, capas_col2, input$sec_est)
    },  height = 550, width = 800)
    
    
    #Gráfico de demanda estatal por sector
    output$est_pro <- renderPlot({
      df_mx2 <- df_mp()
      map_pro(ux2, ex2, df_mx2, capas_mx2, input$pro_est)
    },  height = 550, width = 800)
    
    
    
  #Tab 2: áreas profesionales 
    
    #Especificar argumentos de función reactive
    df_sb <- reactive({
    input$button
    isolate({
    #Criterios de subsetting de la base original
    df_mx2 <- esco_sbs(pais_sbs(df_mx2, input$pais_ar), input$area_amplia, input$nvl_esc) 
      })
    })
        
    #Lista con la numeralia del área
    {
      #Número de empleos
      output$num_ar1 <- renderText({
        paste("Cantidad de vacantes en el sector:", " ", 
              df_mx %>% filter(Area.Amplia == input$area_amplia) %>% nrow())
      })
      #Escolaridad demandada
      output$num_ar2 <- renderText({
        df_mx2 <- df_sb()
        paste("Cantidad de vacantes en el sector para", " ", input$nvl_esc, ":"," ",
                  scl_cnt(df_mx2))
    
      })
      #Salario promedio
      output$num_ar3 <- renderText({
        df_mx2 <- df_sb()
        paste("Salario promedio en el sector para", " ", input$nvl_esc, ":"," ",
              "$", scl_wg(df_mx2))
      })
      #Experiencia promedio
      output$num_ar4 <- renderText({
        df_mx2 <- df_sb()
        paste("Demanda promedio de años de experiencia en el sector para"," ", input$nvl_esc, ":"," ",
              scl_exp(df_mx2),"años")
      })
    }
    
    #Grafico de tendencia del empleo
    output$ten_job <- renderPlot({
      df_mx2 <- df_sb()
      ten_ar(df_mx2, input$nvl_esc, input$area_amplia, input$pais_ar)
    }, height = 550, width = 800)
    
    #Grafico de competencias técnicas 
    output$sel_comp <- renderPlot({
      df_mx2 <- df_sb()
      com_plt(df_mx2, input$top_menciones, input$area_amplia, input$nvl_esc, input$pais_ar)
    }, height = 550, width = 800)
    
    #Gráfico de habilidades suaves
    output$sk_comp <- renderPlot({
      df_mx2 <- df_sb()
      sk_plt(df_mx2, input$top_menciones, input$area_amplia, input$nvl_esc, input$pais_ar)
    }, height = 550, width = 800)
    
    
  
  #Tab 3: programas de IYF

    #Especificar argumentos de función reactiva
    df_pb <- reactive({
      input$button_pro
      isolate({
        #Criterios de subsetting de la base original
        df_mx2 <- pro_sbs(ex2, df_mx2, input$programa) 
      })
    })
    
    #Generar lista de nombres de competencia técnica
    hsk_tab <- reactive({
      df_mx2 <- df_pb()
      hsk <- data.frame(Competencias = hks_gen(1, df_mx2))
    })
    observe({updateSelectInput(session, inputId = "hsk_ls", label = NULL, 
                               choices = hsk_tab()$Competencias, selected = NULL)})
    
    #Generar lista de nombres de habilidades suaves
    ssk_tab <- reactive({
      df_mx2 <- df_pb()
      ssk <- data.frame(Habilidades = hks_gen(0, df_mx2))
    })
    observe({updateSelectInput(session, inputId = "ssk_ls", label = NULL, 
                               choices = ssk_tab()$Habilidades, selected = NULL)})
    
    
    #Gráfico de demanda de trayectos por sector
    output$tra_de_sec <- renderPlot({
      df_mx2 <- df_pb()
        tra_sec(df_mx2, input$programa) 
    }, height = 550, width = 800)
  
    #Grafico de oferta de posiciones por trayecto
    output$ofr_pos <- renderPlot({
      df_mx2 <- df_pb()
      pro_cld(df_mx2, input$programa, input$top_posi)
    }, height = 550, width = 800)
    
    #Gráfico de demanda de competencias técnicas
    output$tec_pro <- renderPlot({
      df_mx2 <- df_pb()
      com_gra(1, df_mx2, input$top_posi, input$hsk_ls)
    }, height = 550, width = 800)
    
    #Gráfico de demanda de habilidades suaves
    output$sft_pro <- renderPlot({
      df_mx2 <- df_pb()
      com_gra(0, df_mx2, input$top_posi, input$ssk_ls)
    }, height = 550, width = 800)
}   

# Run the application 
shinyApp(ui = ui, server = server)
