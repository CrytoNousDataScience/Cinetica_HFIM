library(shiny)
library(ggplot2)
library(pracma)
library(rmarkdown)
library(knitr)


# Definir la interfaz de usuario
ui <- fluidPage(
  titlePanel(tags$h1("Simulador de Cinética de Primer Orden y Setup de HFIM. v.2.5.beta.", 
                     style = "color: blue; text-align: center; font-family: Arial, sans-serif;")),
  sidebarLayout(
    sidebarPanel(
      h3(tags$b("FUNCIONES:")),
      br(),
      h4("Simulación de la Curva de Concentración teórica (ug/mL) en función del tiempo."),
      br(),
      h4("Cálculo de parámetros PK/PD."),
      br(),
      h4("Cálculo de parámetros de programación de Hollow Fiber Infection Model (HFIM)."),
      br(),
      h3(tags$b("REGLAS DE USO:")), 
      br(),
      h5("1.- Seleccione el modelo farmacocinético y ajuste los parámetros correspondientes."),
      h5("2.- Es obligatorio colocar el nombre del antibiótico para que se generen la gráfica y los parámetros."),
      h5("3.- Si simulas el mismo antibiótico con distintos parámetros, debes diferenciar el nombre del antibiótico."),
      h5("4.- Se recomienda una revisión bibliográfica de los parámetros farmacocinéticos del antibiótico seleccionado que se quiera optimizar."),
      h5("5.- El programa permite simular varios antibióticos manteniendo las gráficas y los parámetros en la interfaz del usuario."),
      br(),
      br(),
      h5( "ADVERTENCIA: los valores de PK pueden variar con respecto a modelos experimentales, debido a que sólo es una simulación basada en un modelo matemático. Sin embargo se han incluido métodos iterativos y de integración numérica de gran precisión."),
      br(),
      br(),
      img(src = "logo1", height = 150, width = 150),
      br(),
      span("CrytoNous Data Science.", style = "color:blue"),
      h5("Actio pro libero scientia."),
      br(),
      br(),
      span(tags$b("Powered by:", style= "Color:blue")),
      br(),
      img(src = "logo2.jpeg", height = 75, width = 75),
      br(),
      br(),
      h6("Para recomendaciones o detección de errores se dispone del siguiente e-mail: mario.ampuero.morisaki@gmail.com"),
      br(),
      br(),
      downloadButton("downloadReport", "Descargar Reporte")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Perfusión intermitente en Betalactámicos", 
                 textInput("nombre_antibiotico_intermitente", "Nombre del Antibiótico", value = ""),
                 helpText("Obligatorio rellenar la casilla con el nombre del antibiótico."),
                 numericInput("dose1", "Dosis inicial (ug/mL)", 183.2),
                 helpText("Se elige una dosis inicial para alcanzar un valor de Cmax buscado en el literatura."),
                 numericInput("intervalo_dosis", "Intervalo de dosis (horas)", 8),
                 numericInput("tiempo_infusion", "Tiempo de la infusión (horas)", 4),
                 numericInput("vida_media1", "Vida media (horas)", 1),
                 helpText("Se elige este valor según la literatura."),
                 numericInput("Con_Stock", "Concentración Stock de antibiótico (ug/mL)", 100000),
                 helpText("Por ejemplo un vial de 1g reconstituido en 10mL dH2O, son 100000 ug/mL."),
                 numericInput("flow_rate", "Tasa de flujo (mL/min)", 1.25),
                 helpText("Tasa de flujo programada en la primera bomba peristáltica de la botella de medio de cultivo al Compartimento Central del HFIM."),
                 numericInput("Vol_ATB_8_24", "Volumen de antibiótico para 24 horas (mL)", 53),
                 helpText("Para esta metodología de HFIM se usan bolsas de 50 mL de SSF. donde se perfundirá el antibiótico. Se hacen los cáculos considerando el volumen adicional del antibiótico."),
                 numericInput("CMI", "CMI (ug/mL)", 6),
                 actionButton("btn1", "Generar Modelo"),
                 plotOutput("grafico_intermitente"),
                 verbatimTextOutput("parametros_intermitentes"),

        ),
        tabPanel("Perfusión continua en Betalactámicos",
                 textInput("nombre_antibiotico_continuo", "Nombre del Antibiótico", value = ""),
                 helpText("Obligatorio rellenar la casilla con el nombre del antibiótico."),
                 numericInput("dose2", "Dosis inicial (ug/mL)", 238),
                 helpText("Se elige una dosis inicial para alcanzar un valor de Cmax buscado en el literatura."),
                 numericInput("vida_media", "Vida media (horas)", 3),
                 helpText("Se elige este valor según la literatura."),
                 numericInput("Con_Stock2", "Concentración Stock de antibiótico (ug/mL)", 100000),
                 helpText("Por ejemplo un vial de 1g reconstituido en 10mL dH2O, son 100 0000 ug/mL"),
                 numericInput("flow_rate2", "Tasa de flujo (mL/min)", 1.25),
                 helpText("Tasa de flujo programada en la primera bomba peristáltica de la botella de medio de cultivo al Compartimento Central del HFIM."),
                 numericInput("Vol_ATB_8_24_2", "Volumen de antibiótico para 24 horas (mL)", 53),
                 helpText("Para esta metodología de HFIM se usan bolsas de 50 mL de SSF. donde se perfundirá el antibiótico. Se hacen los cáculos considerando el volumen adicional del antibiótico."),
                 numericInput("CMI", "CMI (ug/mL)", 6),
                 actionButton("btn2", "Generar Modelo"),
                 plotOutput("grafico_continuo"),
                 verbatimTextOutput("parametros_continuo"),
        ),
        tabPanel("Aminoglucósidos",
                 textInput("nombre_aminoglucosido", "Nombre del Antibiótico", value= ""),
                 helpText("Obligatorio rellenar la casilla con el nombre del antibiótico."),
                 numericInput("dose3", "Dosis inicial (ug/mL)", 100),
                 helpText("Se elige una dosis inicial para alcanzar un valor de Cmax buscado en la literatura."),
                 numericInput("vida_media3", "vida media (horas)", 3),
                 helpText("Se elige este valor según la literatura."),
                 numericInput("tiempo_infusion3", "Tiempo de la infusión (horas)", 1),
                 numericInput("Con_Stock3", "Concentración Stock de antibiótico (ug/mL)", 100000),
                 helpText("Por ejemplo un vial de 1g reconstituido en 10mL dH2O, son 100 0000 ug/mL"),
                 numericInput("flow_rate3", "Tasa de flujo (mL/min)", 1.25),
                 helpText("Tasa de flujo programada en la primera bomba peristáltica de la botella de medio de cultivo al Compartimento Central del HFIM."),
                 numericInput("Vol_ATB_8_24_3", "Volumen de antibiótico para 24 horas (mL)", 53),
                 helpText("Para esta metodología de HFIM se usan bolsas de 50 mL de SSF. donde se perfundirá el antibiótico. Se hacen los cáculos considerando el volumen adicional del antibiótico."),
                 numericInput("CMI", "CMI (ug/mL)", 16),
                 actionButton("btn3", "Generar Modelo"),
                 plotOutput("grafico_aminoglucosido"),
                 verbatimTextOutput("parametros_aminoglucosido"),
              )
        ),
      uiOutput("resultados_antibioticos_ui"),
      )
    )
  )


# Definir el servidor
server <- function(input, output, session){
  
  resultados_antibioticos <- reactiveValues(datos = list())
  
  # Función para agregar resultados a la lista
  agregar_resultado <- function(nombre, grafico, parametros) {
    resultados_antibioticos$datos[[nombre]] <- list(
      grafico = grafico,
      parametros = parametros
    )
  }
  
  observeEvent(input$btn1, {
    # Lógica para el Modelo 1
    dose1 <- input$dose1
    tiempo_infusion<-input$tiempo_infusion
    intervalo_dosis<- input$intervalo_dosis
    vida_media1<- input$vida_media1
    flow_rate<-input$flow_rate
    Con_Stock<-input$Con_Stock
    Vol_ATB_8_24<-input$Vol_ATB_8_24
    CMI<- input$CMI
    
    k <- log(2) / vida_media1
    
    # Función para la fase de infusión
    concentracion_infusion <- function(minuto, C_inicial) {
      concentracion <- C_inicial
      for (i in 1:minuto) {
        concentracion <- (concentracion + (dose1 / (tiempo_infusion * 60))) * exp(-k / 60)
      }
      return(concentracion)
    }
    
    # Función para la fase de postinfusión
    concentracion_postinfusion <- function(minuto, Cmax, tiempo_infusion) {
      hora_postinfusion <- (minuto - tiempo_infusion*60) / 60
      return(Cmax * exp(-k * hora_postinfusion))
    }
    
    # Creación de datos para el gráfico
    tiempos <- seq(0, 1440)  # Minutos totales en 24 horas
    concentraciones <- numeric(length(tiempos))
    C_inicial <- 0
    Cmax <- 0
    
    for (minuto in tiempos) {
      ciclo <- floor(minuto / (intervalo_dosis * 60))
      minuto_ciclo <- minuto - ciclo * (intervalo_dosis * 60)
      
      if (minuto_ciclo == 0 && minuto != 0) {
        # Establecer la concentración inicial para el siguiente ciclo
        C_inicial <- concentraciones[minuto]
      }
      
      if (minuto_ciclo <= tiempo_infusion*60) {
        # Fase de infusión
        concentraciones[minuto + 1] <- concentracion_infusion(minuto_ciclo, C_inicial)
        if (minuto_ciclo == tiempo_infusion*60) {
          Cmax <- concentraciones[minuto + 1]
        }
      } else {
        # Fase de postinfusión
        concentraciones[minuto+1] <- concentracion_postinfusion(minuto_ciclo, Cmax, tiempo_infusion)
      }
    }
    
    data <-data.frame(Tiempo = tiempos, Concentracion = concentraciones)
    
    #Calculo de Cmax y dosis de mantenimiento (new_dose). Cálculo de cmax-Cmin
    Cmax <- max(concentraciones[1:(tiempo_infusion * 60 + 1)]) # Asumiendo que 'tiempo_infusion' está en horas y 'concentraciones' se calcula por minuto.
    Cmin <- min(concentraciones[(intervalo_dosis * 60):(intervalo_dosis * 60 + tiempo_infusion * 60)]) # Asumiendo que 'intervalo_dosis' y 'tiempo_infusion' están en horas y 'concentraciones' se calcula por minuto.
    Cmax_Cmin<- Cmax - Cmin
    Cmax2<- Cmax_Cmin
  
    ### Método Iterativo para Calcular dose_new. 
    #Luego, utilizamos un método iterativo para encontrar la nueva dosis (dose_new) que resulte en una Cmax igual a Cmax2. 
    #Este método ajusta gradualmente la dosis hasta que la Cmax calculada con esa dosis se acerca a Cmax2.
    #Este método modifica la dose_new en un 1% en cada iteración, aumentando o disminuyendo según sea necesario para acercarse a Cmax2. 
    #La tolerancia determina qué tan cerca debe estar la Cmax calculada de Cmax2 para considerar que la dosis es adecuada.
    
    dose_new <- dose1  # Iniciar con la dosis actual como primera estimación
    tolerancia <- 1  # Definir una tolerancia para la diferencia aceptable entre Cmax calculada y Cmax2
    max_iteraciones <- 20  # Limitar el número de iteraciones para evitar un bucle infinito
    iteracion <- 0
    
    while (iteracion < max_iteraciones) {
      iteracion <- iteracion + 1
      
      # Calcular Cmax con la dosis actual
      Cmax_calculada <- 0
      for (minuto in seq(0, tiempo_infusion * 60, by = 1)) {
        Cmax_calculada <- max(Cmax_calculada, concentracion_infusion(minuto, 0))
      }
      
      # Comparar Cmax_calculada con Cmax2 y ajustar dose_new
      if (abs(Cmax_calculada - Cmax2) < tolerancia) {
        break  # La Cmax calculada está suficientemente cerca de Cmax2
      } else if (Cmax_calculada < Cmax2) {
        dose_new <- dose_new * (1 + 0.01)  # Aumentar la dosis si Cmax_calculada es menor que Cmax2
      } else {
        dose_new <- dose_new * (1 - 0.01)  # Disminuir la dosis si Cmax_calculada es mayor que Cmax2
      }
    }
    dose_new
    
    
    # AUC24h: Método de Simpson.
    # Este método es más preciso que el trapezoidal, especialmente cuando la curva no es lineal. 
    #El método de Simpson utiliza un polinomio de segundo grado para aproximar el área bajo la curva en cada intervalo.
    
    calcular_auc_simpson <- function(tiempos, concentraciones) {
      n <- length(tiempos)
      if (n <= 1) {
        return(0)
      }
      h <- (tiempos[n] - tiempos[1]) / (n - 1)
      suma <- concentraciones[1] + concentraciones[n]
      for (i in 2:(n - 1)) {
        w <- 2 + 2 * (i %% 2)  # Peso: 4 para índices impares, 2 para pares
        suma <- suma + w * concentraciones[i]
      }
      return(h * suma / 3)
    }
    
    AUC24h_1 <- calcular_auc_simpson(tiempos, concentraciones)/60
    
    # AUC24h: Método ordinario. 
    
    AUC24h_2<- ((dose1/k)-(Cmin/k))*3
    
    
    # Calcula el número total de minutos en los que la concentración es mayor que la CMI
    minutos_sobre_CMI <- sum(concentraciones > CMI)
    
    # Calcula el porcentaje de tiempo que la concentración es mayor que la CMI
    porcentaje_sobre_CMI <- (minutos_sobre_CMI / 1440) * 100  # 1440 minutos en 24 horas
    
    #Calculo del volumen central y Flow Rate en 
    Volumen_Central<- (flow_rate*60)/k
    Flow_Rate_dia<- (flow_rate*60*24)/1000
    
    #Concentración de la jeringa
    Volumen_dosis_inicial<-4
    Concentración_jeringa<-(dose1* Volumen_Central)/Volumen_dosis_inicial
    
    # Volumen de antibiótico
    Vol_ATB<-Concentración_jeringa*Vol_ATB_8_24/Con_Stock
    
    #Volumen de agua 
    agua<-Vol_ATB_8_24-Vol_ATB
    
    #Volumen de dosis de mantenimiento
    Vol_dosis_mante<- dose_new*Volumen_Central/Concentración_jeringa
    
    parametros_intermitente <- list(
      AUC24h_2=AUC24h_2,
      AUC24h_1=AUC24h_1,
      Cmax=Cmax,
      Cmin=Cmin,
      Cmax_Cmin=Cmax_Cmin,
      porcentaje_sobre_CMI=porcentaje_sobre_CMI,
      dose_new=dose_new,
      Vol_dosis_mante=Vol_dosis_mante,
      Concentración_jeringa=Concentración_jeringa,
      Vol_ATB=Vol_ATB,
      agua=agua, 
      Volumen_Central=Volumen_Central,
      Flow_Rate_dia=Flow_Rate_dia
    )
    
    
    # Código para mostrar los valores PK/PD del Modelo 1
    # Convertir los parámetros en texto
    texto_parametros_intermitente <- paste("PARÁMETROS PK/PD:","\nAUC 24h=", format(AUC24h_2), "ug*h/mL", "\nAUC 24h=", format(AUC24h_1), "ug*h/mL (Método por Integración Numérica de Simpson)",  "\nCmax=", format(Cmax),"ug/mL", "\nCmin=", format(Cmin), "ug/mL", "\nCmax-Cmin=", format(Cmax_Cmin), "ug/mL", "\nVida Media=", format(vida_media1), "horas",  "\nT>CMI=", format(porcentaje_sobre_CMI), "%",
                                           "\nPROGRAMACIÓN DE HFIM:", "\nDosis inicial=", format(dose1), "ug/mL", "\nVolumen de Dosis inicial= 4mL", "\nDosis de mantenimiento=", format(dose_new),"ug/mL (Estimado por modelo matemático iterativo a partir de la Cmax-Cmin)","\nVolumen de Dosis de mantenimiento=", format(Vol_dosis_mante), "mL", "\nConcentración de jeringa=", format(Concentración_jeringa),"ug/mL (considera el volumen del Compartimento Central).", "\nVolumen Total para la bomba de perfusión=", format(Vol_ATB_8_24), "mL" , "\nVolumen de antibiótico=", format(Vol_ATB),"mL (Volumen a incluir en la bolsa de 50mL SSF).", "\nVolumen de agua=", format(agua), "mL","\nVolumen compartimento Central=", format(Volumen_Central), "mL", "\nTasa de Flujo=", format(flow_rate), "mL/min (A programar en la primera bomba peristáltica).", "\nTasa de Flujo por día=", format(Flow_Rate_dia), "L/día")
    
    
    grafico_intermitente<-ggplot(data, aes(x = Tiempo, y = Concentracion)) +
      geom_line(color = "blue", linewidth = 1) +
      labs(title = "Curva de Concentración Teórica del Antibiótico (24 horas)", x = "Tiempo (minutos)", y = "Concentración (ug/mL)") +
      scale_x_continuous(breaks = seq(0, max(data$Tiempo), by = 100)) +
      scale_y_continuous(breaks = seq(0, max(data$Concentracion), by = 10)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    nombre_antibiotico <- input$nombre_antibiotico_intermitente
    agregar_resultado(nombre_antibiotico, grafico_intermitente, texto_parametros_intermitente)
  }) 
  
  
  ##################################################################################################
  ##################################################################################################
  
  observeEvent(input$btn2, {
    # Lógica para el Modelo 2
    dose2<- input$dose2
    vida_media<- input$vida_media
    Con_Stock2<-input$Con_Stock2
    flow_rate2<-input$flow_rate2
    Vol_ATB_8_24_2<-input$Vol_ATB_8_24_2
    CMI<- input$CMI
    
    # Calculando la constante de eliminación (k) 
    k <- log(2) / vida_media
    
    # Función para calcular la concentración de manera iterativa
    calcular_concentracion <- function(minutos_total) {
      concentraciones <- numeric(minutos_total + 1)
      concentraciones[1] <- 0  # Inicializamos la concentración en el tiempo 0
      
      for (minuto in 1:minutos_total) {
        concentraciones[minuto + 1] <- (concentraciones[minuto] + (dose2 * (1 / 720))) * exp(-k * 1 / 60)
      }
      
      return(concentraciones)
    }
    
    # Creación de datos para el gráfico
    tiempos <- 0:(24 * 60)  # Minutos totales en 24 horas
    concentraciones <- calcular_concentracion(24 * 60)
    
    data <-data.frame(Tiempo = tiempos, Concentracion = concentraciones)
    
    # Parámetros PK/PD
    #Cmax
    Cmax<-concentraciones[ 721]
    
    #AUC24h
    AUC24h<-dose2/k
    
    #Css
    Css<-AUC24h/24
    
    #T>CMI
    # Calcular T>CMI directamente de los datos
    minutos_sobre_CMI <- sum(concentraciones > CMI)
    T_CMI <- (minutos_sobre_CMI / (24 * 60)) * 100
    
    #Calculo del volumen central y Flow Rate en 
    Volumen_Central2<- (flow_rate2*60)/k
    Flow_Rate_dia<- (flow_rate2*60*24)/1000
    
    #Concentración de la jeringa
    Volumen_dosis_inicial<-12
    Concentración_jeringa2<-(dose2* Volumen_Central2)/Volumen_dosis_inicial
    
    # Volumen de antibiótico
    Vol_ATB2<-Concentración_jeringa2*Vol_ATB_8_24_2/Con_Stock2 
    
    #Volumen de agua 
    agua2<-Vol_ATB_8_24_2-Vol_ATB2
    
    parametros_continuo<-list(
      AUC24h=AUC24h,
      Cmax=Cmax, 
      T_CMI=T_CMI,
      Css=Css,
      Concentración_jeringa2=Concentración_jeringa2,
      Vol_ATB2=Vol_ATB2,
      Vol_ATB_8_24_2=Vol_ATB_8_24_2,
      agua2=agua2,
      Volumen_Central2=Volumen_Central2,
      Flow_Rate_dia=Flow_Rate_dia
    )
    
    
    # Código para mostrar los valores PK/PD del Modelo 2
    # Convertir los parámetros en texto
    texto_parametros_continuo <-paste("PARÁMETROS PK/PD:", "\nAUC 24h=", format(AUC24h), "ug*h/mL", "\nCmax=", format(Cmax),"ug/mL", "\nConcentración estacionaria=", format(Css), "ug/mL","\nVida Media=", format(vida_media), "horas",  "\nT>CMI=", format(T_CMI), "%", "\nPROGRAMACIÓN DE HFIM:", "\nDosis inicial=", format(dose2), "ug/mL","\nVolumen de Dosis inicial= 12mL", "\nConcentración de jeringa=", format(Concentración_jeringa2), "ug/mL (Considerando el volumen del Compartimento Central).",
                                       "\nVolumen Total para la bomba de perfusión=", format(Vol_ATB_8_24_2), "mL",  "\nVolumen del antibiótico=", format(Vol_ATB2), "mL (Volumen a incluir en la bolsa de 50mL SSF).","\nVolumen de agua=", format(agua2), "mL", "\nVolumen de Compartimento Central=", format(Volumen_Central2), "mL","\nTasa de Flujo=", format(flow_rate2), "mL/min (A programar en la primera bomba peristáltica).","\nTasa de Flujo por día=", format(Flow_Rate_dia), "L/día")
    
    
    grafico_continuo<- ggplot(data, aes(x = Tiempo, y = Concentracion)) +
      geom_line(color= "blue", linewidth = 1) +
      labs(title = "Curva de Concentración Teórica del Antibiótico (24 horas)", 
           x = "Tiempo (minutos)", 
           y = "Concentración (ug/mL)")+
      scale_x_continuous(breaks = seq(0, max(data$Tiempo), by = 100)) +
      scale_y_continuous(breaks =seq(0,max(data$Concentracion), by=10)) +
      theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
    
    nombre_antibiotico <- input$nombre_antibiotico_continuo
    agregar_resultado(nombre_antibiotico, grafico_continuo, texto_parametros_continuo)
  })
  
  
################################################################################################################
################################################################################################################
     observeEvent(input$btn3, {
      # Lógica para el Modelo 3
        dose3<- input$dose3
        vida_media3<- input$vida_media3
        tiempo_infusion3<-input$tiempo_infusion3
        Con_Stock3<-input$Con_Stock3
        flow_rate3<-input$flow_rate3
        Vol_ATB_8_24_3<-input$Vol_ATB_8_24_3
        CMI<-input$CMI
         
        k <- log(2) / vida_media3
        
        # Función para la fase de infusión
        concentracion_infusion <- function(minuto, C_inicial) {
          concentracion <- C_inicial
          for (i in 1:minuto) {
            concentracion <- (concentracion + (dose3 / (tiempo_infusion3 * 60))) * exp(-k / 60)
          }
          return(concentracion)
        }
        
        # Función para la fase de postinfusión
        concentracion_postinfusion <- function(minuto, Cmax, tiempo_infusion3) {
          hora_postinfusion <- (minuto - tiempo_infusion3*60) / 60
          return(Cmax * exp(-k * hora_postinfusion))
        }
        
        # Creación de datos para el gráfico
        tiempos <- seq(0, 1439)  # Minutos totales en 24 horas
        concentraciones <- numeric(length(tiempos))
        C_inicial <- 0
        Cmax <- 0
        
        for (minuto in tiempos) {
          ciclo <- floor(minuto / (24 * 60))
          minuto_ciclo <- minuto - ciclo * (24 * 60)
          
          if (minuto_ciclo == 0 && minuto != 0) {
            # Establecer la concentración inicial para el siguiente ciclo
            C_inicial <- 0
          }
          
          if (minuto_ciclo <= tiempo_infusion3*60) {
            # Fase de infusión
            concentraciones[minuto + 1] <- concentracion_infusion(minuto_ciclo, C_inicial)
            if (minuto_ciclo == tiempo_infusion3*60) {
              Cmax <- concentraciones[minuto + 1]
            }
          } else {
            # Fase de postinfusión
            concentraciones[minuto+1] <- concentracion_postinfusion(minuto_ciclo, Cmax, tiempo_infusion3)
          }
        }
        
        data <-data.frame(Tiempo = tiempos, Concentracion = concentraciones)
                  
        #Cmax
        Cmax
        
        #¢min
        Cmin<-min(data$Concentracion)
        
        # Calculo de AUC24h
        AUC24h <- (Cmax/k)-(Cmin/k)
       
        #Cmax/CMI
        Cmax_CMI<- Cmax/CMI
                      
      #Calculo del volumen central y Flow Rate en 
      Volumen_Central3<- (flow_rate3*60)/k
      Flow_Rate_dia<- (flow_rate3*60*24)/1000
                       
      #Concentración de la jeringa
      Volumen_dosis_inicial<-12
      Concentración_jeringa3<-(dose3* Volumen_Central3)/Volumen_dosis_inicial
                            
    # Volumen de antibiótico
      Vol_ATB3<-Concentración_jeringa3*Vol_ATB_8_24_3/Con_Stock3
                           
    #Volumen de agua 
    agua3<-Vol_ATB_8_24_3-Vol_ATB3
      
              
      parametros_aminoglucosido<-list(
              Cmax=Cmax,
              Cmin=Cmin,
              AUC24h=AUC24h,
              Cmax_CMI=Cmax_CMI,
              Concentración_jeringa3=Concentración_jeringa3,
              Vol_ATB3=Vol_ATB3,
              agua3=agua3,
              Volumen_Central3=Volumen_Central3,
              Flow_Rate_dia=Flow_Rate_dia
              )
                               
      # Convertir los parámetros en texto
    texto_parametros_aminoglucosido <- paste("PARÁMETROS PK/PD:", "\nCmax=", format(Cmax),"ug/mL", "\nCmin=", format(Cmin), "ug/mL", "\nAUC 24h=", format(AUC24h), "ug*h/mL", "\nCmax/CMI=", format(Cmax_CMI),  "\nVida Media=", format(vida_media3), "horas", "\nPROGRAMACIÓN DE HFIM:", "\nDosis inicial=", format(dose3), "Ug/mL", "\nVolumen de Dosis inicial= 12mL","\nConcentración de Jeringa=", format(Concentración_jeringa3), "ug/mL(considera el volumen del Compartimento Central).", "\nVolumen de Compartimento Central=", format(Volumen_Central3),"mL", "\nVolumen Total para la bomba de perfusión=", format(Vol_ATB_8_24_3), "mL (Volumen a incluir en la bolsa de 50mL SSF).", "\nVolumen de Antibiótico=", format(Vol_ATB3), "mL", "\nVolumen de agua=", format(agua3), "mL","\nTasa de Flujo=", format(flow_rate3), "mL/min (A programar en la primera bomba peristáltica).", "\nTasa de Flujo por día=", format(Flow_Rate_dia), "L/día")
                       
           
      grafico_aminoglucosido<- ggplot(data, aes(x = Tiempo, y = Concentracion)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(title = "Curva de Concentración Teórica del Antibiótico (24 horas)", x = "Tiempo (minutos)", y = "Concentración (ug/mL)") +
        scale_x_continuous(breaks = seq(0, max(data$Tiempo), by = 100)) +
        scale_y_continuous(breaks = seq(0, max(data$Concentracion), by = 10)) +
        theme(plot.title = element_text(hjust = 0.5))
                                  
       nombre_antibiotico <- input$nombre_aminoglucosido
      agregar_resultado(nombre_antibiotico, grafico_aminoglucosido, texto_parametros_aminoglucosido)
                                      
    }) 
    
  output$resultados_antibioticos_ui <- renderUI({
    ui_elements <- lapply(names(resultados_antibioticos$datos), function(nombre_antibiotico) {
      tagList(
        h4(nombre_antibiotico),
        plotOutput(outputId = paste0("grafico_", nombre_antibiotico)),
        verbatimTextOutput(outputId = paste0("parametros_", nombre_antibiotico))
      )
    })
    do.call(tagList, ui_elements)
  })
  
  observe({
    lapply(names(resultados_antibioticos$datos), function(nombre_antibiotico) {
      output[[paste0("grafico_", nombre_antibiotico)]] <- renderPlot({
        resultados_antibioticos$datos[[nombre_antibiotico]]$grafico
      })
      output[[paste0("parametros_", nombre_antibiotico)]] <- renderText({
        resultados_antibioticos$datos[[nombre_antibiotico]]$parametros
      })
    })
  })
  
}

# Crear la aplicación Shiny
shinyApp(ui = ui, server = server)