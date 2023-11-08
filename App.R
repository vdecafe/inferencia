library(shiny)
library(ggplot2)
library(EnvStats)
library(bslib)
library(waiter)

# ESTÉTICA ------------

theme_AGR <- bs_theme(
  version = 5, 
  bg = "#FFFFFF",
  fg = "#212529",
  primary = "#9CAF88",
  secondary = "#667b68",
  success = "#38FF12",
  info = "#00F5FB",
  warning = "#FFF100",
  danger = "#FF00E3",
  base_font = font_collection(font_google("Roboto", local = FALSE), "Roboto", "sans-serif"),
  heading_font = "Roboto",
  code_font = "Chalkduster",
 )


# Define UI for application that draws a histogram
ui <- shiny::tagList(
  withMathJax(), 
  includeCSS(path = "www/css/styles.css"), 
  
  tags$head(
    tags$link(
      rel = "shortcut icon", 
      href = "https://cdn-icons-png.flaticon.com/512/11083/11083341.png"
    )
  ), 
  
  tags$div(
    tags$div(
      class = "app_title", 
      
      titlePanel(
        title = "Inferencia estadística", 
        windowTitle = "Inferencia"
      ),
     
     tags$h5(
     tags$a(
     href = "https://gestion2.urjc.es/pdi/ver/alejandro.garcia", 
     target = "_blank", 
     "Alejandro García Romero (creador de la versión actual)"
      )
      )
     
    ),
    
   
# INPUTS -----------------   
    ## SELECTOR DE CONTRASTES---------

  fluidPage(
      autoWaiter(),
      theme = theme_AGR,
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "inference",
            label = "Inferencia para:",
            choices = c("Una media", "Dos medias (independientes)",
                        "Dos medias (relacionadas)", 
                        "Una proporción", 
                        "Dos proporciones", 
                        "Una varianza", 
                        "Dos varianzas"),
            multiple = FALSE,
            selected = "Una media"
          ),
          
          conditionalPanel(
            condition = "input.inference == 'Una media' | 
              input.inference == 'Dos medias (independientes)'| 
              input.inference == 'Dos medias (relacionadas)' |
              input.inference == 'Una varianza'|
              input.inference == 'Dos varianzas'
            ",
            selectInput(
            inputId = "inference2",
            label = "Introducir datos usando:",
            choices = c("Valores poblaciones y muestrales", "Un conjunto de datos de forma manual"),
            multiple = FALSE,
            selected = "Valores poblaciones y muestrales"
          ),
          hr()
          ),
          
          ### Opciones sobre contrastes ---------
          
          #### Una muestra y valores específicos sobre la muestra y la población ---------
          conditionalPanel(
            condition = "input.inference == 'Una media' & input.inference2 == 'Valores poblaciones y muestrales'",
            numericInput("smean_onemean", "\\(\\bar{X} = \\)",
                         value = 1, step = 0.1
            ),
            
            numericInput("sd_onemean", "\\(S_x = \\)",
                         value = 1, min = 0, step = 0.1
            ),
            
            numericInput("n_onemean", "\\(n = \\)",
                         value = 10, min = 0, step = 1
            ),
            
            checkboxInput("popsd_onemean2", "La varianza de la población es conocida", FALSE),
            conditionalPanel(
              condition = "input.popsd_onemean2 == 1",
              numericInput("sigma2_onemean", "\\(\\sigma^2 = \\)",
                           value = 1, min = 0, step = 1
              )
            )
          ),
          
          
          #### Una muestra y datos de forma manual ---------
          
          conditionalPanel(
            condition = "input.inference == 'Una media' & input.inference2 == 'Un conjunto de datos de forma manual'",
            textInput("muestra_onemean", "Muestra", value = "0.9, -0.8, 1.3, -0.3, 1.7", 
                      placeholder = "Introduzca valores separados por una coma con decimales como puntos, por ejemplo 4.2, 4.4, 5, 5.03, etc."),
            hr(),
            checkboxInput("popsd_onemean", "La varianza de la población es conocida", FALSE),
            conditionalPanel(
              condition = "input.popsd_onemean == 1",
              numericInput("sigma2_onemean", "\\(\\sigma^2 = \\)",
                           value = 1, min = 0, step = 1
              )
            )
          ),
          
          #### Dos medias independientes y valores ---------
          
          conditionalPanel(
            condition = "input.inference == 'Dos medias (independientes)' & input.inference2 == 'Valores poblaciones y muestrales'",
            tags$b("Muestra 1"),
            tags$br(),
            numericInput("mean1_2indemean", "\\(\\bar{X}_{1} = \\)",
                         value = 0.02, step = 0.1
            ),
            numericInput("sd1_2indemean", "\\(\\ S_{1} = \\)",
                         value = 0.63, step = 0.1
            ),
            numericInput("n1_2indemean", "\\(\\ n_{1} = \\)",
                         value = 5, step = 1
            ),
            tags$b("Muestra 2"),
            numericInput("mean2_2indemean", "\\(\\bar{X}_{2} = \\)",
                         value = 0.06, step = 0.1
            ),
            numericInput("sd2_2indemean", "\\(\\ S_{2} = \\)",
                         value = 0.634, step = 0.1
            ),
            numericInput("n2_2indemean", "\\(\\ n_{2} = \\)",
                         value = 5, step = 1
            ),
            conditionalPanel(
              condition = "input.popsd_twomeans2 == 0",
              radioButtons(
                inputId = "var.equal2",
                label = "Asumiendo",
                choices = c(
                  "\\( \\sigma^2_1 = \\sigma^2_2 \\)" = TRUE,
                  "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = FALSE
                )
              )
            ),
            
            
            checkboxInput("popsd_twomeans2", "Las varianzas poblacionales son conocidas", FALSE),
            conditionalPanel(
              condition = "input.popsd_twomeans2 == 1",
              numericInput("sigma21_twomeans", "\\(\\sigma^2_1 = \\)",
                           value = 1, min = 0, step = 1
              ),
              numericInput("sigma22_twomeans", "\\(\\sigma^2_2 = \\)",
                           value = 1, min = 0, step = 1
              )
            )),
          
          #### Dos medias independientes y datos manuales ---------
          conditionalPanel(
            condition = "input.inference == 'Dos medias (independientes)' & input.inference2 == 'Un conjunto de datos de forma manual'",
            textInput("muestra1_twomeans", "Muestra 1", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Introduzca valores separados por una coma con decimales como puntos, p.ej. 4.2, 4.4, 5, 5.03, etc."),
            textInput("muestra2_twomeans", "Muestra 2", value = "0.8, -0.9, -0.1, 0.4, 0.1", placeholder = "Introduzca valores separados por una coma con decimales como puntos, p.ej. 4.2, 4.4, 5, 5.03, etc."),
            hr(),
            conditionalPanel(
              condition = "input.popsd_twomeans == 0",
              radioButtons(
                inputId = "var.equal",
                label = "Asumiendo",
                choices = c(
                  "\\( \\sigma^2_1 = \\sigma^2_2 \\)" = TRUE,
                  "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = FALSE
                )
              )
            ),
            
            checkboxInput("popsd_twomeans", "La varianza de las poblaciones es conocida", FALSE),
            conditionalPanel(
              condition = "input.popsd_twomeans == 1",
              numericInput("sigma21_twomeans", "\\(\\sigma^2_1 = \\)",
                           value = 1, min = 0, step = 1
              ),
              numericInput("sigma22_twomeans", "\\(\\sigma^2_2 = \\)",
                           value = 1, min = 0, step = 1
              )
            )
          ),
          
          #### Dos medias relacionadas y valores ---------
          conditionalPanel(
            condition = "input.inference == 'Dos medias (relacionadas)' & input.inference2 == 'Valores poblaciones y muestrales'",
            hr(),
            numericInput("dif_twomeanspaired", "\\( \\bar{Y_1} - \\bar{Y}_{2} = \\bar{D} = \\)",
                         value = 0.04, min = 0, step = 0.1),
            numericInput("sd_difs_twomeanspaired", "\\({S_{D}} = \\)",
                         value = 0.371, min = 0, step = 0.1), 
            numericInput("n_twomeanspaired", "\\(n = \\)",
                         value = 5, min = 0, step = 1),
            checkboxInput("popsd_twomeanspaired2", "\\( \\sigma^2_D \\) es conocida", FALSE),
            conditionalPanel(
              condition = "input.popsd_twomeanspaired2 == 1",
              numericInput("sigma2_twomeanspaired2", "\\(\\sigma^2_D = \\)",
                           value = 1, min = 0, step = 1
              )
            )
          ),
          
          #### Dos medias relacionadas y datos manuales ---------
          
          conditionalPanel(
            condition = "input.inference == 'Dos medias (relacionadas)' & input.inference2 == 'Un conjunto de datos de forma manual'",
            textInput("muestra1_twomeanspaired", "Muestra 1", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Introduzca valores separados por una coma con decimales como puntos (p. ej.,4.2, 4.4, 5, 5.03, etc.)"),
            textInput("muestra2_twomeanspaired", "Muestra 2", value = "0.8, -0.9, -0.1, 0.4, 0.1", placeholder = "Introduzca valores separados por una coma con decimales como puntos (p. ej.,4.2, 4.4, 5, 5.03, etc.)"),
            hr(),
            checkboxInput("popsd_twomeanspaired", "Se conoce la \\( \\sigma^2_D \\)", FALSE),
            conditionalPanel(
              condition = "input.popsd_twomeanspaired == 1",
              numericInput("sigma2_twomeanspaired", "\\(\\sigma^2_D = \\)",
                           value = 1, min = 0, step = 1
              )
            )
          ),
          
          #### Una proporción  ---------
          
          conditionalPanel(
            condition = "input.inference == 'Una proporción'",
            tags$b("Tamaño muestral"),
            numericInput("n_oneprop", "\\(n = \\)",
                         value = 30, min = 0, step = 1
            ),
            hr(),
            radioButtons(
              inputId = "propx_oneprop",
              label = NULL,
              choices = c(
                "Proporción de éxitos \\(P_1\\)" = "prop_true",
                "Número de éxitos \\(n_1\\)" = "prop_false"
              )
            ),
            conditionalPanel(
              condition = "input.propx_oneprop == 'prop_true'",
              tags$b("Proporción de éxitos"),
              numericInput("p_oneprop", "\\(P_1 = \\)",
                           value = 0.2, min = 0, max = 1, step = 0.01
              )
            ),
            conditionalPanel(
              condition = "input.propx_oneprop == 'prop_false'",
              tags$b("Número de éxitos"),
              numericInput("x_oneprop", "\\(n_1\\)",
                           value = 10, min = 0, step = 1
              )
            )
          ),
          
          #### Dos proporciones ---------
          
          conditionalPanel(
            condition = "input.inference == 'Dos proporciones'",
            tags$b("Tamaño muestral de la muestra 1"),
            numericInput("n1_twoprop", "\\(n_1 = \\)",
                         value = 30, min = 0, step = 1
            ),
            tags$b("Tamaño muestral de la muestra 2"),
            numericInput("n2_twoprop", "\\(n_2 = \\)",
                         value = 30, min = 0, step = 1
            ),
            hr(),
            radioButtons(
              inputId = "propx_twoprop",
              label = NULL,
              choices = c(
                "Proporción de éxitos \\(\\hat{p}\\)" = "prop_true",
                "Número de éxitos \\(x\\)" = "prop_false"
              )
            ),
            conditionalPanel(
              condition = "input.propx_twoprop == 'prop_true'",
              tags$b("Proporción de éxitos"),
              numericInput("p1_twoprop", "\\(P_1 = \\)",
                           value = 0.2, min = 0, max = 1, step = 0.01
              ),
              numericInput("p2_twoprop", "\\(P_2 = \\)",
                           value = 0.3, min = 0, max = 1, step = 0.01
              )
            ),
            conditionalPanel(
              condition = "input.propx_twoprop == 'prop_false'",
              tags$b("Número de éxitos"),
              numericInput("x1_twoprop", "\\(x_1 = \\)",
                           value = 10, min = 0, step = 1
              ),
              numericInput("x2_twoprop", "\\(x_2 = \\)",
                           value = 12, min = 0, step = 1
              )
            )
          ),
          
          #### Una varianza ---------

          conditionalPanel(
            condition = "input.inference == 'Una varianza' & input.inference2 == 'Un conjunto de datos de forma manual'",
            textInput("muestra_onevar", "Muestra", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Introduzca valores separados por una coma con decimales como puntos (p. ej., 0.9, -0.8, 0.1, -0.3, 0.2, etc.)")
          ),
          
          conditionalPanel(
            condition = "input.inference == 'Una varianza' & input.inference2 == 'Valores poblaciones y muestrales'",
            tags$b("Datos muestrales"),
            numericInput("unavarianza_S2", "\\(S^2 = \\)",
                         value = 0.397, step = 0.01, min = 0),
            numericInput("unavarianza_n", "\\(n = \\)",
                         value = 5, step = 1, min = 0)
            
            
            
          ),
            
          #### Dos varianzas ---------
          
          
          conditionalPanel(
            condition = "input.inference == 'Dos varianzas' & input.inference2 == 'Un conjunto de datos de forma manual'",
            textInput("muestra1_twovar", "Muestra 1", value = "0.9, -0.8, 0.1, -0.3, 0.2, 0.7, -0.8, 0.1, -0.3, 0.2", placeholder = "Introduzca valores separados por una coma con decimales como puntos (p. ej., 0.9, -0.8, 0.1, -0.3, 0.2, etc.)"),
            textInput("muestra2_twovar", "Muestra 2", value = "0.4, -0.3, -0.1, 0.4, 0.1, 0.2, -0.1, -0.1, 0.4, 0.1", placeholder = "Introduzca valores separados por una coma con decimales como puntos (p. ej., 0.9, -0.8, 0.1, -0.3, 0.2, etc.)")
          ),
          
          
          conditionalPanel(
            condition = "input.inference == 'Dos varianzas' & input.inference2 == 'Valores poblaciones y muestrales'",
            tags$b("Datos muestrales"),
            numericInput("dosvarianzas_n1", "\\(n_1 = \\)",
                         value = 10, step = 1, min=2),
            numericInput("dosvarianzas_s21", "\\(S^2_1 = \\)",
                         value = 0.318, step = 0.1, min=0),
            numericInput("dosvarianzas_n2", "\\(n_2 = \\)",
                         value = 10, step = 1, min=2),
            numericInput("dosvarianzas_s22", "\\(S^2_2 = \\)",
                         value = 0.062, step = 0.1, min=0)
            
            
            ),
          
          
          hr(),
          tags$b("Hipótesis nula"),
          conditionalPanel(
            condition = "input.inference == 'Una media'",
            sprintf("\\( H_0 : \\mu = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos medias (independientes)'",
            sprintf("\\( H_0 : \\mu_1 - \\mu_2 = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos medias (relacionadas)'",
            sprintf("\\( H_0 : \\mu_D = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'Una proporción'",
            sprintf("\\( H_0 : \\pi_1 = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos proporciones'",
            sprintf("\\( H_0 : P_1 - P_2 = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'Una varianza'",
            sprintf("\\( H_0 : \\sigma^2 = \\)")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos varianzas'",
            sprintf("\\( H_0 : \\sigma^2_1 = \\sigma^2_2 \\)")
          ),
          conditionalPanel(
            condition = "input.inference != 'Dos varianzas'",
            numericInput("h0",
                         label = NULL,
                         value = 0.3, step = 0.1
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos varianzas'",
            br()
          ),
          conditionalPanel(
            condition = "input.inference != 'Dos varianzas'",
            radioButtons(
              inputId = "Alternativa",
              label = "Alternativa",
              choices = c(
                "\\( \\neq \\)" = "two.sided",
                "\\( > \\)" = "greater",
                "\\( < \\)" = "less"
              )
            )
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos varianzas'",
            radioButtons(
              inputId = "Alternativa_twovar",
              label = "Alternativa",
              choices = c(
                "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = "two.sided",
                "\\( \\sigma^2_1 > \\sigma^2_2 \\)" = "greater",
                "\\( \\sigma^2_1 < \\sigma^2_2 \\)" = "less"
              )
            )
          ),
          hr(),
          sliderInput("alpha",
                      "Nivel de significación \\(\\alpha = \\)",
                      min = 0.01,
                      max = 0.20,
                      value = 0.05
          ),
          width = 4),
        
        mainPanel(
          br(),
          conditionalPanel(
            condition = "input.inference == 'Una media' && input.inference2 == 'Un conjunto de datos de forma manual'",
            uiOutput("results_onemean")
          ),
          conditionalPanel(
            condition = "input.inference == 'Una media' && input.inference2 == 'Valores poblaciones y muestrales'",
            uiOutput("results_onemean2")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos medias (independientes)' && input.inference2 == 'Un conjunto de datos de forma manual'",
            uiOutput("results_twomeans")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos medias (independientes)' && input.inference2 == 'Valores poblaciones y muestrales'",
            uiOutput("results_twomeans2")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos medias (relacionadas)' && input.inference2 == 'Un conjunto de datos de forma manual'",
            uiOutput("results_twomeanspaired")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos medias (relacionadas)' && input.inference2 == 'Valores poblaciones y muestrales'",
            uiOutput("results_twomeanspaired2")
          ),
          conditionalPanel(
            condition = "input.inference == 'Una proporción'",
            uiOutput("results_oneprop")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos proporciones'",
            uiOutput("results_twoprop")
          ),
          conditionalPanel(
            condition = "input.inference == 'Una varianza'",
            uiOutput("results_onevar")
          ),
          conditionalPanel(
            condition = "input.inference == 'Dos varianzas'",
            uiOutput("results_twovar")
          ),
          br(),
          br(),
          plotOutput("plot"),
          br(),
          br()
        )
      ),
    )      
  ), 
  
  tags$footer(
    tags$div(
      class = "footer_container", 
      
      includeHTML(path = "www/html/footer.html")
    )
  )
  
)
  


server <- function(input, output) {
  
  
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  
  ###FUNCIONES###
  ## NUEVO ###
  #IC c. una media
  t.testIC <- function(mean, sd, n, alfa, varprob = NULL){
    IC <- c() 
    ICInf <- c()
    ICSup <- c()
    if (is.null(varprob)) {
      ICInf <- mean - qt(alfa/2, df = n -1 , lower.tail = FALSE)*sd/sqrt(n)
      ICSup <- mean + qt(alfa/2, df = n -1 , lower.tail = FALSE)*sd/sqrt(n)
      
    } else {
      
      ICInf <- mean - qnorm(alfa/2, lower.tail = FALSE)*sqrt(varprob)/sqrt(n)
      ICSup <- mean + qnorm(alfa/2, lower.tail = FALSE)*sqrt(varprob)/sqrt(n)
      
    }
    
    IC <- c(ICInf,ICSup)
    
    return(IC)
    print(IC)
  }
  
   
  ## NUEVO ###
  #t test c. una media
  
  t.testv2 <- function(mean, sd, n, alfa, mu, varprob = NULL, alternative){
    estadistico <- c()
    pvalor <- c()
    
    if(is.null(varprob)){
      
      estadistico <- (mean-mu)/(sd/sqrt(n))
      
      if(alternative == "two.sided"){
        
        if(estadistico > 0) {pvalor <- 2*pt(estadistico, df = n-1, lower.tail = FALSE)}
        
        else {pvalor <- 2*pt(estadistico, df = n-1, lower.tail = TRUE)}
        
      } else if (alternative == "less") {
        
        pvalor <- pt(estadistico, df = n-1, lower.tail = TRUE)
        
      } else { pvalor <- pt(estadistico, df = n-1, lower.tail = FALSE)}
      
    } else {
      
      estadistico <- (mean-mu)/(sqrt(varprob)/sqrt(n))
      
      if(alternative == "two.sided"){
        
        if(estadistico > 0) {pvalor <- 2*pnorm(estadistico, lower.tail = FALSE)}
        
        else {pvalor <- 2*pnorm(estadistico, lower.tail = TRUE)}
        
      } else if (alternative == "less") {
        
        pvalor <- pnorm(estadistico, lower.tail = TRUE)
        
      } else { pvalor <- pnorm(estadistico, lower.tail = FALSE)}
      
    }
    
    datos <- list(Estadistico = estadistico, pvalor = pvalor)
    
    return(datos)
  }
  
  t.test2 <- function(x, V, m0 = 0, alpha = 0.05, Alternativa = "two.sided") {
    M <- mean(x)
    n <- length(x)
    sigma <- sqrt(V)
    S <- sqrt(V / n)
    statistic <- (M - m0) / S
    p <- if (Alternativa == "two.sided") {
      2 * pnorm(abs(statistic), lower.tail = FALSE)
    } else if (Alternativa == "less") {
      pnorm(statistic, lower.tail = TRUE)
    } else {
      pnorm(statistic, lower.tail = FALSE)
    }
    # p <- (1 - pnorm((M-m0)/S))
    LCL <- (M - S * qnorm(1 - alpha / 2))
    UCL <- (M + S * qnorm(1 - alpha / 2))
    value <- list(mean = M, m0 = m0, sigma = sigma, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, Alternativa = Alternativa)
    # print(sprintf("P-value = %g",p))
    # print(sprintf("Lower %.2f%% Confidence Limit = %g",
    #               alpha, LCL))
    # print(sprintf("Upper %.2f%% Confidence Limit = %g",
    #               alpha, UCL))
    return(value)
  }
  
  ### NUEVO ### 
  
  #t.test2_muestrameans
  
  
  
  t.test3 <- function(x, y, V1, V2, m0 = 0, alpha = 0.05, Alternativa = "two.sided") {
    M1 <- mean(x)
    M2 <- mean(y)
    n1 <- length(x)
    n2 <- length(y)
    sigma1 <- sqrt(V1)
    sigma2 <- sqrt(V2)
    S <- sqrt((V1 / n1) + (V2 / n2))
    statistic <- (M1 - M2 - m0) / S
    p <- if (Alternativa == "two.sided") {
      2 * pnorm(abs(statistic), lower.tail = FALSE)
    } else if (Alternativa == "less") {
      pnorm(statistic, lower.tail = TRUE)
    } else {
      pnorm(statistic, lower.tail = FALSE)
    }
    # p <- (1 - pnorm((M-m0)/S))
    LCL <- (M1 - M2 - S * qnorm(1 - alpha / 2))
    UCL <- (M1 - M2 + S * qnorm(1 - alpha / 2))
    value <- list(mean1 = M1, mean2 = M2, m0 = m0, sigma1 = sigma1, sigma2 = sigma2, S = S, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, Alternativa = Alternativa)
    # print(sprintf("P-value = %g",p))
    # print(sprintf("Lower %.2f%% Confidence Limit = %g",
    #               alpha, LCL))
    # print(sprintf("Upper %.2f%% Confidence Limit = %g",
    #               alpha, UCL))
    return(value)
  }
  prop.z.test <- function(x, n, p0 = 0.5, conf.level = 0.95, Alternativa = "two.sided") {
    ts.z <- NULL
    cint <- NULL
    p.val <- NULL
    phat <- x / n
    qhat <- 1 - phat
    SE.phat <- sqrt((phat * qhat) / n)
    
    if (x<(n*p0)) {
      ts.z <- ((phat+0.5/n) - p0) / SE.phat
    } else if (x>(n*p0)) {
      ts.z <- ((phat-0.5/n) - p0) / SE.phat
    } else {
      ts.z <- (phat - p0) / SE.phat }
    
    p.val <- if (Alternativa == "two.sided") {
      2 * pnorm(abs(ts.z), lower.tail = FALSE)
    } else if (Alternativa == "less") {
      pnorm(ts.z, lower.tail = TRUE)
    } else {
      pnorm(ts.z, lower.tail = FALSE)
    }
    
    cint <- c(phat - (abs((qnorm(p = (1-conf.level)/ 2))) * SE.phat),
              phat + (abs((qnorm(p = (1-conf.level)/ 2))) * SE.phat))
              
      
    return(list(x = x, n = n, estimate = phat, null.value = p0, stderr = SE.phat, statistic = ts.z, p.value = p.val, conf.int = cint))
  }
  prop.z.test2 <- function(x1, x2, n1, n2, p0 = 0, pooled.stderr = TRUE, conf.level = 0.95, Alternativa = "two.sided") {
    ts.z <- NULL
    cint <- NULL
    p.val <- NULL
    phat1 <- x1 / n1
    qhat1 <- 1 - phat1
    phat2 <- x2 / n2
    qhat2 <- 1 - phat2
    pooled.phat <- ((n1 * phat1) + (n2 * phat2)) / (n1 + n2)
    pooled.qhat <- 1 - pooled.phat
    if (pooled.stderr == FALSE) {
      SE.phat <- sqrt((phat1 * qhat1) / n1 + (phat2 * qhat2) / n2)
    } else {
      SE.phat <- sqrt(pooled.phat * pooled.qhat * (1 / n1 + 1 / n2))
    }
    ts.z <- (phat1 - phat2 - p0) / SE.phat
    p.val <- if (Alternativa == "two.sided") {
      2 * pnorm(abs(ts.z), lower.tail = FALSE)
    } else if (Alternativa == "less") {
      pnorm(ts.z, lower.tail = TRUE)
    } else {
      pnorm(ts.z, lower.tail = FALSE)
    }
    cint <- (phat1 - phat2) + c(
      -1 * ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat),
      ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat)
    )
    return(list(x1 = x1, x2 = x2, n1 = n1, n2 = n2, estimate1 = phat1, estimate2 = phat2, null.value = p0, stderr = SE.phat, pooled.phat = pooled.phat, statistic = ts.z, p.value = p.val, conf.int = cint))
  }
  prop.z.test3 <- function(x, n, p0 = 0.5, conf.level = 0.95, Alternativa = "two.sided") {
    ts.z <- NULL
    cint <- NULL
    p.val <- NULL
    phat <- x / n
    qhat <- 1 - phat
    SE.phat <- sqrt((p0 * (1-p0)) / n)
   
    
    if (x<(n*p0)) {
      ts.z <- ((phat+0.5/n) - p0) / SE.phat
    } else if (x>(n*p0)) {
      ts.z <- ((phat-0.5/n) - p0) / SE.phat
    } else {
      ts.z <- (phat - p0) / SE.phat }
    
    
    p.val <- if (Alternativa == "two.sided") {
      2 * pnorm(abs(ts.z), lower.tail = FALSE)
    } else if (Alternativa == "less") {
      pnorm(ts.z, lower.tail = TRUE)
    } else {
      pnorm(ts.z, lower.tail = FALSE)
    }
    return(list(x = x, n = n, estimate = phat, null.value = p0, stderr = SE.phat, statistic = ts.z, p.value = p.val))
  }
  
  
  # ANALISIS UNA MEDIA CON DATOS MANUALES  ---------------------------------
  

  output$results_onemean <- renderUI({
    dat <- extract(input$muestra_onemean)
    if (anyNA(dat) | length(dat) < 2) {
      "Entrada no válida o número insuficiente de observaciones"
    } else if (input$inference == "Una media" & input$popsd_onemean == FALSE) {
      test_confint <- t.test(x = dat, mu = input$h0, Alternativa = "two.sided", conf.level = 1 - input$alpha)
      test <- t.test(x = dat, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste(c(paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(\\bar{X} =\\) ", round(mean(dat), 3)),
        br(),
        paste0("\\(S_x =\\) ", round(sd(dat), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "IC al ", (1 - input$alpha) * 100, "% para \\(\\mu = \\bar{X} \\pm t_{\\alpha/2, n - 1} \\dfrac{S_x}{\\sqrt{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr * sqrt(length(dat)), 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", test$null.value, " y \\(H_1 : \\mu \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(T = \\dfrac{\\bar{X} - \\mu_0}{S_x / \\sqrt{n}} = \\) ",
          "(", round(test$estimate, 3), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$Alternativa == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intepretación"),
        br(),
        paste0("Considerando un nivel de significación del ", input$alpha * 100, "%",
               ifelse(test$p.value < input$alpha, " debemos rechazar la hipótesis nula que afirma que la verdadera media es ", 
                      " NO debemos rechazar la hipótesis nula que afirma que la verdadera media es  "), 
               input$h0, " \\((p\\)-value ", 
               ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference
               == "Una media" & input$popsd_onemean == TRUE) {
      test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, Alternativa = input$Alternativa)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste(c(paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(\\bar{x} =\\) ", round(mean(dat), 3)),
        br(),
        paste0("\\(\\sigma =\\) ", round(sqrt(input$sigma2_onemean), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "IC al ", (1 - input$alpha) * 100, "% para \\(\\mu = \\bar{x} \\pm z_{\\alpha/2} \\dfrac{\\sigma}{\\sqrt{n}} = \\) ",
          round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", input$h0, " y \\(H_1 : \\mu \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(z_{obs} = \\dfrac{\\bar{x} - \\mu_0}{\\sigma / \\sqrt{n}} = \\) ",
          "(", round(test$mean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$sigma / sqrt(length(dat)), 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("Considerando un nivel de significación del ", input$alpha * 100, "%",
               ifelse(test$p.value < input$alpha, " debemos rechazar la hipótesis nula que afirma que la verdadera media es ", 
                      " NO debemos rechazar la hipótesis nula que afirma que la verdadera media es  "), 
               input$h0, " \\((p\\)-value ", 
               ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
        )
    } else {
      print("loading...")
    }
  })
  
  # ANALISIS UNA MEDIA CON VALORES  ---------------------------------
  output$results_onemean2 <- renderUI({
    if (input$n_onemean < 2) {
      "Entrada no válida o número insuficiente de observaciones"
    } 
    
    #Sin varianza poblacional
    else if (input$inference == "Una media" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_onemean2 == FALSE) {
      IC <- t.testIC(mean = input$smean_onemean, sd = input$sd_onemean, n = input$n_onemean, alfa = input$alpha)
      test <- t.testv2(mean = input$smean_onemean, n = input$n_onemean, sd = input$sd_onemean, alternative = input$Alternativa, mu = input$h0)
        #test_confint <- t.test(x = dat, mu = input$h0, Alternativa = "two.sided", conf.level = 1 - input$alpha)
      #test <- t.test(x = dat, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha)
      withMathJax(
        tags$b("Datos:"),
        br(),
        br(),
        paste0("\\(n =\\) ", input$n_onemean),
               br(),
               paste0("\\(\\bar{X} =\\) ", input$smean_onemean),
        br(),
        paste0("\\(S_x =\\) ", input$sd_onemean),
      br(),
      br(),
      tags$b("Intervalo de confianza"),
      tags$em("(Bilateral)"),
      br(),
      paste0(
      "IC al ", (1 - input$alpha) * 100, "% para \\(\\mu = \\bar{X} \\pm t_{\\alpha/2, n - 1} \\dfrac{S_x}{\\sqrt{n}} = \\) ",
      input$smean_onemean, "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = input$n_onemean - 1, lower.tail = FALSE), 3), " * ", round(input$sd_onemean,3), 
      " / ", round((sqrt(input$n_onemean)), 3), "\\( ) \\) ", "\\( = \\) ",
       "[", round(IC[1], 3), "; ", round(IC[2], 3), "]"),
      br(),
      br(),
      tags$b("Contraste de la hipótesis:"),
      br(),
      paste0("1. \\(H_0 : \\mu = \\) ", input$h0, " y \\(H_1 : \\mu \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
      br(),
      paste0(
        "2. Estadístico de contraste : \\(T = \\dfrac{\\bar{X} - \\mu_x}{S_x / \\sqrt{n}} = \\) ",
        "(", round(input$smean_onemean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round((input$sd_onemean/sqrt(input$n_onemean)), 3), 
        " \\( = \\) ",
        round((input$smean_onemean-(input$h0))/(input$sd_onemean/sqrt(input$n_onemean)), 3)),
      br(),
      paste0(
        "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", 
                                     ifelse(input$Alternativa == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
        ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), ", ", input$n_onemean-1, "\\()\\)", " \\( = \\) ",
        ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
        ifelse(input$Alternativa == "two.sided", round(qt(input$alpha / 2, df = input$n_onemean-1, lower.tail = FALSE), 3), round(qt(input$alpha, df = input$n_onemean-1, lower.tail = FALSE), 3))
      ),
      br(),
      paste0("4. Conclusión/Decisión : ", ifelse(test$pvalor < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
      br(),
      br(),
      tags$b("Interpretación"),
      br(),
      paste0("Considerando un nivel de significación del ", input$alpha * 100, "%",
             ifelse(test$pvalor < input$alpha, " debemos rechazar la hipótesis nula que afirma que la verdadera media es ", 
                    " NO debemos rechazar la hipótesis nula que afirma que la verdadera media es  "), 
             input$h0, " \\((p\\)-value ", 
             ifelse(test$pvalor < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$pvalor, 3))), ")", ".")
      )
      #Con la varianza poblacional
      } else if (input$inference == "Una media" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_onemean2 == TRUE) {
        IC <- t.testIC(mean = input$smean_onemean, sd = input$sd_onemean, n = input$n_onemean, alfa = input$alpha, varprob = input$sigma2_onemean)
        test <- t.testv2(mean = input$smean_onemean, n = input$n_onemean, sd = input$sd_onemean, alternative = input$Alternativa, mu = input$h0, varprob = input$sigma2_onemean)
        #test_confint <- t.test(x = dat, mu = input$h0, Alternativa = "two.sided", conf.level = 1 - input$alpha)
        #test <- t.test(x = dat, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha)
        withMathJax(
          tags$b("Datos:"),
          br(),
          br(),
          paste0("\\(n =\\) ", input$n_onemean),
          br(),
          paste0("\\(\\bar{X} =\\) ", input$smean_onemean),
          br(),
          paste0("\\(\\sigma =\\) ", round(sqrt(input$sigma2_onemean), 3)),
          br(),
          br(),
          tags$b("Intervalo de confianza"),
          tags$em("(Bilateral)"),
          br(),
          paste0(
            "IC al ", (1 - input$alpha) * 100, " % para \\(\\mu = \\bar{X} \\pm z_{\\alpha/2} \\dfrac{\\sigma}{\\sqrt{n}} = \\) ",
            input$smean_onemean, "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, mean = 0, sd=1, lower.tail = FALSE), 3), 
            " * ", round(sqrt(input$sigma2_onemean),3), 
            " / ", round((sqrt(input$n_onemean)), 3), "\\( ) \\) ", "\\( = \\) ",
            "[", round(IC[1], 3), "; ", round(IC[2], 3), "]"),
          br(),
          br(),
          tags$b("Contraste de la hipótesis:"),
          br(),
          paste0("1. \\(H_0 : \\mu = \\) ", input$h0, " y \\(H_1 : \\mu \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
          br(),
          paste0(
            "2. Estadístico de contraste : \\(Z = \\dfrac{\\bar{X} - \\mu}{\\sigma / \\sqrt{n}} = \\) ",
            "(", round(input$smean_onemean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", 
            round((sqrt(input$sigma2_onemean)/sqrt(input$n_onemean)), 3), 
            " \\( = \\) ",
            round((input$smean_onemean-(input$h0))/(sqrt(input$sigma2_onemean)/sqrt(input$n_onemean)), 3)),
          br(),
          paste0(
            "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
            ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
            ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
            ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
          ),
          br(),
          paste0("4. Conclusión/Decisión : ", ifelse(test$pvalor < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
          br(),
          br(),
          tags$b("Interpretación"),
          br(),
          paste0("Considerando un nivel de significación del ", input$alpha * 100, "%",
                 ifelse(test$pvalor < input$alpha, " debemos rechazar la hipótesis nula que afirma que la verdadera media es ", 
                        " NO debemos rechazar la hipótesis nula que afirma que la verdadera media es  "), 
                 input$h0, " \\((p\\)-value ", 
                 ifelse(test$pvalor < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$pvalor, 3))), ")", ".")
        )}
  })
  

# ANALISIS DOS MEDIAS INDEPENDIENTES CON VALORES  ---------------------------------

  ## Función ---------------------------------
  
  t.2indemeans <- function(M1, M2, s1, s2, n1, n2, m0 = 0, alpha = 0.05, Alternativa = "two.sided", Varpob1, Varpob2, IgualVar = NULL, Varpob = "FALSE") {
  varmuestral1 <- s1^2
  varmuestral2 <- s2^2
  ICInf <- c()
  ICSup <- c()
  estadistico <- c()
  pvalor <- c()
  v <- c()
  ## ASUMIENDO VARIANZAS NO IGUALES
  if (IgualVar == "TRUE" & Varpob == "FALSE") {
    sp <- sqrt((((n1 - 1) * varmuestral1 + (n2 - 1) * varmuestral2) / (n1 + n2 - 2)) * ((1/n1)+(1/n2)))
    estadistico <- (M1 - M2 - m0) / sp

    if (Alternativa == "two.sided") {
      if (estadistico > 0) {
        pvalor <- 2 * pt(estadistico, df = n1 + n2 - 2, lower.tail = FALSE)
      } else {
        pvalor <- 2 * pt(estadistico, df = n1 + n2 - 2, lower.tail = TRUE)
      }
    } else if (Alternativa == "less") {
      pvalor <- pt(estadistico, df = n1 + n2 - 2, lower.tail = TRUE)
    } else {
      pvalor <- pt(estadistico, df = n1 + n2 - 2, lower.tail = FALSE)
    }

    Emax <- (qt(alpha / 2, df = n1 + n2 - 2, lower.tail = FALSE) * (sp))
    ICInf <- abs(M1 - M2) - Emax
    ICSup <- abs(M1 - M2) + Emax

    resultados <- list(Estadistico = estadistico, sp = sp, pvalor = pvalor, ICInf = ICInf, ICSup = ICSup, Emax = Emax)
  }

  ## ASUMIENDO VARIANZAS NO IGUALES
  else if (IgualVar == "FALSE" & Varpob == "FALSE") {
    vnum <- ((varmuestral1 / n1) + (varmuestral2 / n2))^2
    vden <- ((varmuestral1 / n1)^2 / (n1 - 1)) + ((varmuestral2 / n2)^2 / (n2 - 1))
    v <- vnum / vden
    estadistico <- (M1 - M2 - m0) / (sqrt((varmuestral1 / n1) + (varmuestral2 / n2)))

    if (Alternativa == "two.sided") {
      if (estadistico > 0) {
        pvalor <- 2 * pt(estadistico, df = v, lower.tail = FALSE)
      } else {
        pvalor <- 2 * pt(estadistico, df = v, lower.tail = TRUE)
      }
    } else if (Alternativa == "less") {
      pvalor <- pt(estadistico, df = v, lower.tail = TRUE)
    } else {
      pvalor <- pt(estadistico, df = v, lower.tail = FALSE)
    }

    Emax <- (qt(alpha / 2, df = v, lower.tail = FALSE)) * (sqrt((varmuestral1 / n1) + (varmuestral2 / n2)))
    ICInf <- (M1 - M2) - Emax
    ICSup <- (M1 - M2) + Emax

    resultados <- list(Estadistico = estadistico, v = v, pvalor = pvalor, ICInf = ICInf, ICSup = ICSup, Emax = Emax)
  }

  ## VARIANZAS POBLACIONALES CONOCIDAS
  else if (Varpob == "TRUE") {
    s <- sqrt((Varpob1 / n1) + (Varpob2 / n2))
    estadistico <- (M1 - M2 - m0) / s

    if (Alternativa == "two.sided") {
      if (estadistico > 0) {
        pvalor <- 2 * pnorm(estadistico, mean = 0, sd = 1, lower.tail = FALSE)
      } else {
        pvalor <- 2 * pnorm(estadistico, mean = 0, sd = 1, lower.tail = TRUE)
      }
    } else if (Alternativa == "less") {
      pvalor <- pnorm(estadistico, mean = 0, sd = 1, lower.tail = TRUE)
    } else {
      pvalor <- pnorm(estadistico, mean = 0, sd = 1, lower.tail = FALSE)
    }

    Emax <- (qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE)) * (sqrt((Varpob1 / n1) + (Varpob2 / n2)))
    ICInf <- (M1 - M2) - Emax
    ICSup <- (M1 - M2) + Emax

    resultados <- list(Estadistico = estadistico, pvalor = pvalor, s = s, ICInf = ICInf, ICSup = ICSup, Emax = Emax)

    return(resultados)
  }
}
  
  ## Output de resultados ---------------------------------
  
  ### Usando valores ---------------------------------
  
  output$results_twomeans2 <- renderUI({
    if (input$n1_2indemean < 2 | input$n2_2indemean < 2) {
      "Entrada no válida o número insuficiente de observaciones"
      #### Asumiendo varianzas iguales ---------------------------------
    } else if (input$inference == "Dos medias (independientes)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeans2 == FALSE & input$var.equal2 == TRUE) {
      inde2means <- t.2indemeans(
        n1 = input$n1_2indemean, n2 = input$n2_2indemean,
        M1 = input$mean1_2indemean, M2 = input$mean2_2indemean,
        s1 = input$sd1_2indemean, s2 = input$sd2_2indemean,
        IgualVar = TRUE, m0 = input$h0, alpha = input$alpha, 
        Alternativa = input$Alternativa
      )
    
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste0("\\(n_1 =\\) ", input$n1_2indemean),
        br(),
        paste0("\\(n_2 =\\) ", input$n2_2indemean),
        br(),
        paste0("\\(\\bar{X}_1 =\\) ", round(input$mean1_2indemean, 3)),
        br(),
        paste0("\\(\\bar{X}_2 =\\) ", round(input$mean2_2indemean, 3)),
        br(),
        paste0("\\(S^2_1 =\\) ", round(input$sd1_2indemean^2, 3)),
        br(),
        paste0("\\(S^2_2 =\\) ", round(input$sd2_2indemean^2, 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ",(1 - input$alpha) * 100, "% para \\(\\mu_1 - \\mu_2 = \\left | \\bar{X}_1 - \\bar{X}_2 \\right | \\pm t_{\\ 1 - \\alpha/2, n_1 + n_2 - 2} \\cdot \\widehat{\\sigma}_{\\bar{X}_1-\\bar{X}_2} \\)"),
        br(),
        paste0("donde ", "\\( \\widehat{\\sigma}_{\\bar{X}_1-\\bar{X}_2} = \\sqrt{\\dfrac{(n_1 - 1)S^2_1 + (n_2 - 1)S^2_2}{n_1 + n_2 - 2} \\left ( \\dfrac{1}{n_1} +\\dfrac{1}{n_2} \\right)} = \\) ", round(inde2means$sp, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% IC para \\(\\mu_1 - \\mu_2 =  |\\) ", round(input$mean1_2indemean, 3), ifelse(input$mean2_2indemean >= 0, paste0(" - ", round(input$mean2_2indemean, 3)), paste0(" + ", round(abs(input$mean2_2indemean, 3)))), " \\( | \\,\\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = input$n1_2indemean + input$n2_2indemean - 2, lower.tail = FALSE), 3), " * ", round(inde2means$sp, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(inde2means$ICInf, 3), "; ", round(inde2means$ICSup, 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", input$h0, " y \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Estadístico de contraste: \\( T = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\widehat{\\sigma}_{\\bar{X}_1-\\bar{X}_2}} = \\) ",
          "(", round(input$mean1_2indemean, 3), ifelse(input$mean2_2indemean >= 0, paste0(" - ", round(input$mean2_2indemean, 3)), paste0(" + ", round(abs(input$mean2_2indemean), 3))), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / (", round(inde2means$sp, 3), ") \\( = \\) ",
          round(inde2means$Estadistico, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico:", ifelse(input$Alternativa == "two.sided", " \\( \\pm t_{\\alpha/2, n_1 + n_2 - 2} = \\pm t(\\)", ifelse(input$Alternativa == "greater", " \\( t_{\\alpha, n_1 + n_2 - 2} = t(\\)", " \\( -t_{\\alpha, n_1 + n_2 - 2} = -t(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), ", ", input$n1_2indemean + input$n2_2indemean - 2, "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qt(input$alpha / 2, df = input$n1_2indemean + input$n2_2indemean - 2, lower.tail = FALSE), 3), round(qt(input$alpha, df = input$n1_2indemean + input$n2_2indemean - 2, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(inde2means$pvalor < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretación:"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%", ifelse(inde2means$pvalor < input$alpha,
          " RECHAZAMOS la Hipótesis nula que afirma que la verdadera diferencia de las medias es ",
          " NO rechazamos la Hipótesis nula que afirma que la verdadera diferencia de las medias es "
        ), input$h0, " \\((p\\)-value ", ifelse(inde2means$pvalor < 0.001, "< 0.001", paste0("\\(=\\) ", round(inde2means$pvalor, 3))), ")", ".")
      )
    #### No asumiendo varianzas iguales ---------------------------------
    } else if (input$inference == "Dos medias (independientes)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeans2 == FALSE & input$var.equal2 == FALSE) {
      inde2means <- t.2indemeans(
        n1 = input$n1_2indemean, n2 = input$n2_2indemean,
        M1 = input$mean1_2indemean, M2 = input$mean2_2indemean,
        s1 = input$sd1_2indemean, s2 = input$sd2_2indemean,
        IgualVar = FALSE, m0 = input$h0, alpha = input$alpha,
        Alternativa = input$Alternativa
      )
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste0("\\(n_1 =\\) ", input$n1_2indemean),
        br(),
        paste0("\\(n_2 =\\) ", input$n2_2indemean),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(input$mean1_2indemean, 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(input$mean2_2indemean, 3)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(input$sd1_2indemean^2, 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(input$sd2_2indemean^2, 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral):"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\mu_1 - \\mu_2 = \\left | \\bar{x}_1 - \\bar{x}_2 \\right | \\pm t_{\\alpha/2, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}} \\)"),
        br(),
        paste0("donde ", "\\( \\nu = \\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}\\Bigg)^2}{\\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1}\\Bigg)^2}{n_1-1} + \\dfrac{\\Bigg(\\dfrac{s^2_2}{n_2}\\Bigg)^2}{n_2-1}} = \\) ", round(inde2means$v, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% IC para \\(\\mu_1 - \\mu_2 = \\) ", round(input$mean1_2indemean, 3), ifelse(input$mean2_2indemean >= 0, paste0(" - ", round(input$mean2_2indemean, 3)), paste0(" + ", round(abs(input$mean2_2indemean), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = inde2means$v, lower.tail = FALSE), 3), " * ", round((sqrt((input$sd1_2indemean^2 / input$n1_2indemean) + (input$sd2_2indemean^2 / input$n2_2indemean))), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(inde2means$ICInf, 3), "; ", round(inde2means$ICSup, 3), "]"
        ),
        br(),
        br(),
        tags$em(paste0("Nota: para simplificar, el número de grados de libertad se aproxima a veces como gl = \\(min(n_1 - 1, n_2 - 1) \\), así que en este caso gl = ", min(input$n1_2indemean - 1, input$n2_2indemean - 1), ".")),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", input$h0, " y \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
          "(", round(input$mean1_2indemean, 3), ifelse(input$mean2_2indemean >= 0, paste0(" - ", round(input$mean2_2indemean, 3)), paste0(" + ", round(abs(input$mean2_2indemean), 3))), ifelse(input$h0 >= 0,
            paste0(" - ", input$h0), paste0(" + ", abs(input$h0))
          ), ") / ", round(sqrt((input$sd1_2indemean^2 / input$n1_2indemean) + (input$sd2_2indemean^2 / input$n2_2indemean)), 3), " \\( = \\) ",
          round(inde2means$Estadistico, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm t_{\\alpha/2, \\nu} = \\pm t(\\)", ifelse(input$Alternativa == "greater", " \\( t_{\\alpha, \\nu} = t(\\)", " \\( -t_{\\alpha, \\nu} = -t(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), ", ", round(inde2means$v, 3), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qt(input$alpha / 2, df = inde2means$v, lower.tail = FALSE), 3), round(qt(input$alpha, df = inde2means$v, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(inde2means$pvalor < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%", ifelse(inde2means$pvalor < input$alpha,
          " RECHAZAMOS la Hipótesis nula que afirma que la verdadera diferencia de las medias es ",
          " NO rechazamos la Hipótesis nula que afirma que la verdadera diferencia de las medias es "
        ), input$h0, " \\((p\\)-value ", ifelse(inde2means$pvalor < 0.001, "< 0.001", paste0("\\(=\\) ", round(inde2means$pvalor, 3))), ")", ".")
      )
      #### Varianzas poblacionales conocidas  ------------
      } else if (input$inference == "Dos medias (independientes)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeans2 == TRUE) {
      inde2means <- t.2indemeans(
        n1 = input$n1_2indemean, n2 = input$n2_2indemean,
        s1 = input$sd1_2indemean, s2 = input$sd2_2indemean,
        M1 = input$mean1_2indemean, M2 = input$mean2_2indemean,
        Varpob1 = input$sigma21_twomeans, Varpob2 = input$sigma22_twomeans,
        IgualVar = FALSE, m0 = input$h0, alpha = input$alpha, Varpob = TRUE,
        Alternativa = input$Alternativa
      )
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste0("\\(n_1 =\\) ", input$n1_2indemean),
        br(),
        paste0("\\(n_2 =\\) ", input$n2_2indemean),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(input$mean1_2indemean, 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(input$mean2_2indemean, 3)),
        br(),
        paste0("\\(\\sigma^2_1 =\\) ", round(input$sigma21_twomeans, 3)),
        br(),
        paste0("\\(\\sigma^2_2 =\\) ", round(input$sigma22_twomeans, 3)),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}} = \\) ",
          round(input$mean1_2indemean, 3), ifelse(input$mean2_2indemean >= 0, paste0(" - ", round(input$mean2_2indemean, 3)), paste0(" + ", round(abs(input$mean2_2indemean), 3))), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(inde2means$s, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(inde2means$ICInf, 3), "; ", round(inde2means$ICSup, 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", input$h0, " y \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(z_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}}} = \\) ",
          "(", round(input$mean1_2indemean, 3), ifelse(input$mean2_2indemean >= 0, paste0(" - ", round(input$mean2_2indemean, 3)), paste0(" + ", round(abs(input$mean2_2indemean), 3))), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(inde2means$s, 3), " \\( = \\) ",
          round(inde2means$Estadistico, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(inde2means$pvalor < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%", ifelse(inde2means$pvalor < input$alpha,
                                                                                    " RECHAZAMOS la Hipótesis nula que afirma que la verdadera diferencia de las medias es ",
                                                                                    " NO rechazamos la Hipótesis nula que afirma que la verdadera diferencia de las medias es "
        ), input$h0, " \\((p\\)-value ", ifelse(inde2means$pvalor < 0.001, "< 0.001", paste0("\\(=\\) ", round(inde2means$pvalor, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  ## Valores manuales ----------
  
  output$results_twomeans <- renderUI({
    dat1 <- extract(input$muestra1_twomeans)
    dat2 <- extract(input$muestra2_twomeans)
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Entrada no válida o número insuficiente de observaciones"
      #### Asumiendo varianzas iguales ------------
    } else if (input$inference == "Dos medias (independientes)" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
      test_confint <- t.test(x = dat1, y = dat2, mu = input$h0, Alternativa = "two.sided", conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      s_p <- sqrt(((length(dat1) - 1) * var(dat1) + (length(dat2) - 1) * var(dat2)) / test_confint$parameter)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste(c("\\(muestra_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(muestra_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(\\bar{X}_1 =\\) ", round(mean(dat1), 3)),
        br(),
        paste0("\\(\\bar{X}_2 =\\) ", round(mean(dat2), 3)),
        br(),
        paste0("\\(S^2_1 =\\) ", round(var(dat1), 3)),
        br(),
        paste0("\\(S^2_2 =\\) ", round(var(dat2), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\mu_1 - \\mu_2 = \\bar{X}_1 - \\bar{X}_2 \\pm t_{\\alpha/2, n_1 + n_2 - 2} (s_p) \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}} \\)"),
        br(),
        paste0("where ", "\\( s_p = \\sqrt{\\dfrac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2}{n_1 + n_2 - 2}} = \\) ", round(s_p, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ", round(test_confint$estimate[1], 3), ifelse(test_confint$estimate[2] >= 0, paste0(" - ", round(test_confint$estimate[2], 3)), paste0(" + ", round(abs(test_confint$estimate[2]), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(s_p, 3), " * ", round(sqrt(1 / length(dat1) + 1 / length(dat2)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " y \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{s_p \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}}} = \\) ",
          "(", round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / (", round(s_p, 3), " * ", round(sqrt((1 / length(dat1)) + (1 / length(dat2))), 3), ") \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm t_{\\alpha/2, n_1 + n_2 - 2} = \\pm t(\\)", ifelse(input$Alternativa == "greater", " \\( t_{\\alpha, n_1 + n_2 - 2} = t(\\)", " \\( -t_{\\alpha, n_1 + n_2 - 2} = -t(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "RECHAZAR \\(H_0\\)", "NO RECHAZAR \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretación"),
        br(),
        paste0("At the ", input$alpha * 100, "% Nivel de significación, ", ifelse(test$p.value < input$alpha, "we reject the Hipótesis nula that the true diferencia in means is ", "we do not reject the Hipótesis nula that the true diferencia in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
      #### No asumiendo varianzas iguales ------------
    } else if (input$inference == "Dos medias (independientes)" & input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
      test_confint <- t.test(x = dat1, y = dat2, mu = input$h0, Alternativa = "two.sided", conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste(c("\\(muestra_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(muestra_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(mean(dat1), 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(mean(dat2), 3)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}} \\)"),
        br(),
        paste0("where ", "\\( \\nu = \\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}\\Bigg)^2}{\\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1}\\Bigg)^2}{n_1-1} + \\dfrac{\\Bigg(\\dfrac{s^2_2}{n_2}\\Bigg)^2}{n_2-1}} = \\) ", round(test$parameter, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ", round(test_confint$estimate[1], 3), ifelse(test_confint$estimate[2] >= 0, paste0(" - ", round(test_confint$estimate[2], 3)), paste0(" + ", round(abs(test_confint$estimate[2]), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$em(paste0("Note: for simplicity, the number of degrees of freedom is sometimes approximated as df = \\(min(n_1 - 1, n_2 - 1) \\), so in this case df = ", min(length(dat1) - 1, length(dat2) - 1), ".")),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " y \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
          "(", round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm t_{\\alpha/2, \\nu} = \\pm t(\\)", ifelse(input$Alternativa == "greater", " \\( t_{\\alpha, \\nu} = t(\\)", " \\( -t_{\\alpha, \\nu} = -t(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), ", ", round(test$parameter, 3), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% Nivel de significación, ", ifelse(test$p.value < input$alpha, "we reject the Hipótesis nula that the true diferencia in means is ", "we do not reject the Hipótesis nula that the true diferencia in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
      #### Varianzas poblaciones conocidas --------------
    } else if (input$inference == "Dos medias (independientes)" & input$popsd_twomeans == TRUE) {
      test <- t.test3(x = dat1, y = dat2, V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, Alternativa = input$Alternativa)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste(c("\\(muestra_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(muestra_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(mean(dat1), 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(mean(dat2), 3)),
        br(),
        paste0("\\(\\sigma^2_1 =\\) ", round(input$sigma21_twomeans, 3)),
        br(),
        paste0("\\(\\sigma^2_2 =\\) ", round(input$sigma22_twomeans, 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}} = \\) ",
          round(test$mean1, 3), ifelse(test$mean2 >= 0, paste0(" - ", round(test$mean2, 3)), paste0(" + ", round(abs(test$mean2), 3))), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$S, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", input$h0, " y \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(z_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}}} = \\) ",
          "(", round(test$mean1, 3), ifelse(test$mean2 >= 0, paste0(" - ", round(test$mean2, 3)), paste0(" + ", round(abs(test$mean2), 3))), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$S, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% Nivel de significación, ", ifelse(test$p.value < input$alpha, "we reject the Hipótesis nula that the true diferencia in means is ", "we do not reject the Hipótesis nula that the true diferencia in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  
# ANÁLISIS DOS MEDIAS RELACIONADAS --------------
  
  ## Función -------------
  
  t.2relameans <- function(dif, n, var_pobdif, hipodif, sd_muestradif, Alternativa = "two.sided", Varpob = "FALSE", alpha){
    
    if (Varpob == "FALSE"){
      
      estadistico <- (dif-hipodif)/(sd_muestradif/sqrt(n))
      Emax <- qt(alpha / 2, df = n-1, lower.tail = FALSE) * (sd_muestradif/sqrt(n))
      ICInf <- dif - Emax
      ICSup <- dif + Emax
      
      if (Alternativa == "two.sided") {
        if (estadistico > 0) {
          pvalor <- 2 * pt(estadistico, df = n-1, lower.tail = FALSE)
        } else {
          pvalor <- 2 * pt(estadistico, df = n-1, lower.tail = TRUE)
        }
      } else if (Alternativa == "less") {
        pvalor <- pt(estadistico, df = n-1, lower.tail = TRUE)
      } else {
        pvalor <- pt(estadistico, df = n-1, lower.tail = FALSE)
      }
      
    } else if (Varpob == "TRUE"){
      sd_pobdif <- sqrt(var_pobdif)
      estadistico <- (dif-hipodif)/(sd_pobdif/sqrt(n))
      Emax <- qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) * (sd_pobdif/sqrt(n))
      ICInf <- dif - Emax
      ICSup <- dif + Emax
      
      if (Alternativa == "two.sided") {
        if (estadistico > 0) {
          pvalor <- 2 * pnorm(estadistico, mean = 0, sd = 1, lower.tail = FALSE)
        } else {
          pvalor <- 2 * pnorm(estadistico, mean = 0, sd = 1, lower.tail = TRUE)
        }
      } else if (Alternativa == "less") {
        pvalor <- pnorm(estadistico, mean = 0, sd = 1, lower.tail = TRUE)
      } else {
        pvalor <- pnorm(estadistico, mean = 0, sd = 1, lower.tail = FALSE)
      }
    }
    
    resultados <- list(Estadistico = estadistico, pvalor = pvalor, ICInf = ICInf, ICSup = ICSup, Emax = Emax)
    
    return(resultados)
  }
  
  ## Output de resultados -------------
  
  ### Usando valores ------------
  
  output$results_twomeanspaired2 <- renderUI({
      if (input$n_twomeanspaired < 2) {
      "Entrada no válida o número insuficiente de observaciones"
      #### Varianza de las diferencias desconocida ------------
    } else if (input$inference == "Dos medias (relacionadas)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeanspaired2 == FALSE) {
      rela2means <- t.2relameans (dif = input$dif_twomeanspaired, n = input$n_twomeanspaired,
                                  Varpob = "FALSE", sd_muestradif = input$sd_difs_twomeanspaired, 
                                  hipodif = input$h0, alpha = input$alpha, 
                                  Alternativa = input$Alternativa)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste0("Número de pares \\(n =\\) ", input$n_twomeanspaired),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(input$dif_twomeanspaired, 3)),
        br(),
        paste0("\\(s^2_D =\\) ", round(input$sd_difs_twomeanspaired^2, 3)),
        br(),
        paste0("\\(s_D =\\) ", round(input$sd_difs_twomeanspaired, 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\mu_D = \\bar{D} \\pm t_{\\alpha/2, n - 1} \\dfrac{s_D}{\\sqrt{n}} = \\) ",
          round(input$h0, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = input$n_twomeanspaired-1, lower.tail = FALSE), 3), " * ", 
          round(input$sd_difs_twomeanspaired, 3), " / ", 
          round(input$n_twomeanspaired, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(rela2means$ICInf, 3), "; ", round(rela2means$ICSup, 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", input$h0, " y \\(H_1 : \\mu_D \\) ", 
               ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", 
                      ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(t_{obs} = \\dfrac{\\bar{D} - \\mu_0}{s_D / \\sqrt{n}} = \\) ",
          "(", round(input$dif_twomeanspaired, 3), 
          ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), 
          ") / ", round(input$sd_difs_twomeanspaired/input$n_twomeanspaired, 3), " \\( = \\) ",
          round(rela2means$Estadistico, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", 
                                       ifelse(input$Alternativa == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), ", ", input$n_twomeanspaired-1, "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qt(input$alpha / 2, df = input$n_twomeanspaired-1, lower.tail = FALSE), 3), round(qt(input$alpha, df = input$n_twomeanspaired-1, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(rela2means$pvalor < input$alpha, "RECHZAR \\(H_0\\)", "NO RECHAZAR \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretación"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%", ifelse(rela2means$pvalor < input$alpha,
                                                                                    " RECHAZAMOS la Hipótesis nula que afirma que la verdadera diferencia de las medias es ",
                                                                                    " NO rechazamos la Hipótesis nula que afirma que la verdadera diferencia de las medias es "
        ), input$h0, " \\((p\\)-value ", ifelse(rela2means$pvalor < 0.001, "< 0.001", paste0("\\(=\\) ", round(rela2means$pvalor, 3))), ")", ".")
      )
      
      #### Varianza de las diferencias conocida ------------
    } else if (input$inference == "Dos medias (relacionadas)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeanspaired2 == TRUE) {
      rela2means <- t.2relameans (dif = input$dif_twomeanspaired, n = input$n_twomeanspaired,
                                  Varpob = "TRUE", sd_muestradif = input$sd_difs_twomeanspaired, 
                                  hipodif = input$h0, alpha = input$alpha, var_pobdif = input$sigma2_twomeanspaired2,
                                  Alternativa = input$Alternativa)
        withMathJax(
        tags$b("Datos:"),
        br(),
        paste0("Número de pares \\(n =\\) ", input$n_twomeanspaired),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(input$dif_twomeanspaired, 3)),
        br(),
        paste0("\\(\\sigma^2_D =\\) ", round(input$sigma2_twomeanspaired2, 3)),
        br(),
        paste0("\\(\\sigma_D =\\) ", round(sqrt(input$sigma2_twomeanspaired2), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\mu_D = \\bar{D} \\pm z_{\\alpha/2} \\dfrac{\\sigma_D}{\\sqrt{n}} = \\) ",
          round(input$dif_twomeanspaired, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(input$sigma2_twomeanspaired2, 3), " / ", round(sqrt(input$n_twomeanspaired), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(rela2means$ICInf, 3), "; ", round(rela2means$ICSup, 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", input$h0, " y \\(H_1 : \\mu_D \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(z_{obs} = \\dfrac{\\bar{D} - \\mu_0}{\\sigma_D / \\sqrt{n}} = \\) ",
          "(", round(input$dif_twomeanspaired, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(input$sigma2_twomeanspaired2 / sqrt(input$n_twomeanspaired), 3), " \\( = \\) ",
          round(rela2means$Estadistico, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(rela2means$pvalor < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretación"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%", ifelse(rela2means$pvalor < input$alpha,
                                                                                    " RECHAZAMOS la Hipótesis nula que afirma que la verdadera diferencia de las medias es ",
                                                                                    " NO rechazamos la Hipótesis nula que afirma que la verdadera diferencia de las medias es "
        ), input$h0, " \\((p\\)-value ", ifelse(rela2means$pvalor < 0.001, "< 0.001", paste0("\\(=\\) ", round(rela2means$pvalor, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })

  ### Datos manuales ------------
  
  output$results_twomeanspaired <- renderUI({
    dat1 <- extract(input$muestra1_twomeanspaired)
    dat2 <- extract(input$muestra2_twomeanspaired)
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Entrada no válida o número insuficiente de observaciones"
    } else if (length(dat1) != length(dat2)) {
      "Number of observations must be equal in the two muestras"
      #### Varianza de las diferencias desconocida ------------
    } else if (input$inference == "Dos medias (relacionadas)" & input$inference2 == "Un conjunto de datos de forma manual" & input$popsd_twomeanspaired == FALSE) {
      test_confint <- t.test(x = dat2, y = dat1, mu = input$h0, Alternativa = "two.sided", conf.level = 1 - input$alpha, paired = TRUE)
      test <- t.test(x = dat2, y = dat1, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha, paired = TRUE)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste(c("\\(muestra_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(muestra_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste(c("diferencia \\((D) = muestra_2 - muestra_1=\\)", paste(dat2 - dat1, collapse = ", ")), collapse = " "),
        br(),
        paste0("Number of pairs \\(n =\\) ", length(dat1)),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(mean(dat2 - dat1), 3)),
        br(),
        paste0("\\(s^2_D =\\) ", round(var(dat2 - dat1), 3)),
        br(),
        paste0("\\(s_D =\\) ", round(sd(dat2 - dat1), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\mu_D = \\bar{D} \\pm t_{\\alpha/2, n - 1} \\dfrac{s_D}{\\sqrt{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr * sqrt(length(dat1)), 3), " / ", round(sqrt(length(dat1)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", test$null.value, " y \\(H_1 : \\mu_D \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(t_{obs} = \\dfrac{\\bar{D} - \\mu_0}{s_D / \\sqrt{n}} = \\) ",
          "(", round(test$estimate, 3), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$Alternativa == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% Nivel de significación, ", ifelse(test$p.value < input$alpha, "we reject the Hipótesis nula that the true mean of the diferencia is equal to ", "we do not reject the Hipótesis nula that the true mean of the diferencia is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
      
      #### Varianza de las diferencias conocida ------------
    } else if (input$inference == "Dos medias (relacionadas)" & input$inference2 == "Un conjunto de datos de forma manual" & input$popsd_twomeanspaired == TRUE) {
      test <- t.test2(x = dat2 - dat1, V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, Alternativa = input$Alternativa)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste(c("\\(muestra_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(muestra_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste(c("diferencia \\((D) = muestra_2 - muestra_1=\\)", paste(dat2 - dat1, collapse = ", ")), collapse = " "),
        br(),
        paste0("Number of pairs \\(n =\\) ", length(dat1)),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(mean(dat2 - dat1), 3)),
        br(),
        paste0("\\(\\sigma^2_D =\\) ", round(input$sigma2_twomeanspaired, 3)),
        br(),
        paste0("\\(\\sigma_D =\\) ", round(sqrt(input$sigma2_twomeanspaired), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\mu_D = \\bar{D} \\pm z_{\\alpha/2} \\dfrac{\\sigma_D}{\\sqrt{n}} = \\) ",
          round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(length(dat1)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", input$h0, " y \\(H_1 : \\mu_D \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(z_{obs} = \\dfrac{\\bar{D} - \\mu_0}{\\sigma_D / \\sqrt{n}} = \\) ",
          "(", round(test$mean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$sigma / sqrt(length(dat1)), 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% Nivel de significación, ", ifelse(test$p.value < input$alpha, "we reject the Hipótesis nula that the true mean of the diferencia is equal to ", "we do not reject the Hipótesis nula that the true mean of the diferencia is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  
  # ANÁLISIS UNA PROPORCIÓN  ----------------
  
  output$results_oneprop <- renderUI({
    if (input$inference == "Una proporción" & input$propx_oneprop == "prop_true") {
      test <- prop.z.test(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa)
      test2 <- prop.z.test3(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa)
      test_confint <- prop.z.test(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = "two.sided")
      withMathJax(
        tags$h6(paste0("De acuerdo con el Teorema del Límite Central, a medida que el tamaño muestral aumenta, las distribuciones de \\((P_1)\\) y \\((n_1)\\) se aproximan a la normal 
                       siempre que se cumpla que \\( nP_1 \\geq 5\\) y \\( n(1-P_1) \\geq 5\\).")),
        tags$h6(paste0("Se ilustran, por tanto, los resultados para los estadísticos \\((P_1)\\) y \\((n_1)\\), y también para su aproximación a la normal (aplicando, por defecto, la corrección por continuidad), 
                       la cual solamente debe usarse en el caso de que se cumplan los supuestos.")),
        tags$b("Datos:"),
        br(),
        paste0("\\(n =\\) ", test$n),
        br(),
        paste0("Proporción de éxitos \\((P_1) =\\) ", round(test$estimate, 3)),
        br(),
        paste0("Proporción de fracasos \\( (1- P_1) =\\) ", round(1 - test$estimate, 3)),
        br(),
        helpText(paste0("\\( n_1 = nP_1 \\)")),
        br(),
        paste0("Número de éxitos \\((n_1) =\\) ", round(test$estimate*test$n, 0)),
        br(),
        paste0("Número de fracasos \\((1-n_1) =\\) ", round(test$n-test$estimate*test$n,0)),
        br(),
        br(),
        tags$b("Supuestos:"),
        paste0("Considerando que \\( nP_1 = \\) ", round(test$n * test$estimate, 3), " y \\( n(1-P_1) = \\) ", round(test$n * (1 - test$estimate), 3)),
        paste0("se puede afirmar que los supuestos de que \\( nP_1 \\geq 5\\) y \\( n(1-P_1) \\geq 5\\)", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, " se cumplen", " no se cumplen.")),
        tags$h6(paste0("Por tanto, ", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, "puede utilizarse la aproximación a la normal.", "no es recomendable utilizar la aproximación a la normal."))),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral):"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\pi_1 = P_1 \\pm \\left |  z_{\\alpha/2} \\right | \\sqrt{\\dfrac{P_1(1-P_1)}{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\pi_1 = \\) ", test$null.value, " y \\(H_1 : \\pi_1 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Estadísticos de contraste:"),
        br(),
        tags$li(paste0("Estadístico 1: \\((P_1) =\\) ", round(test$estimate, 3), " y \\((n_1)=\\) ", round(test$estimate*test$n, 0))),
        tags$h6(paste0("\\( P_1\\) y \\( n_1\\) son, en realidad, el mismo estadístico y ambos siguen una distribución Binomial.")),
        
        tags$li("Estadístico 2: \\( Z = \\)",
                ifelse((input$n_oneprop * input$p_oneprop) < (input$n_oneprop * input$h0), " \\( \\dfrac{(P_1 + 0,5/n) - \\pi_1}{\\sqrt{\\dfrac{\\pi_1(1-\\pi_1)}{n}}} = \\)", 
                       ifelse((input$n_oneprop * input$p_oneprop) > (input$n_oneprop * input$h0), " \\( \\dfrac{(P_1 - 0,5/n) - \\pi_1}{\\sqrt{\\dfrac{\\pi_1(1-\\pi_1)}{n}}} = \\)", 
                              "\\( \\dfrac{P_1 - \\pi_1}{\\sqrt{\\dfrac{\\pi_1(1-\\pi_1)}{n}}} = \\)")),
                "(", round(test2$estimate, 3),
                ifelse((input$n_oneprop * input$p_oneprop) < (input$n_oneprop * input$h0), "+", 
                      ifelse((input$n_oneprop * input$p_oneprop) > (input$n_oneprop * input$h0), "-", 
                      "")),
                round((0.5/test$n),3), "-",test2$null.value,")/",round(test2$stderr, 3), " \\( = \\) ",
                ifelse(test2$null.value >= 0 & test2$null.value <= 1, round(test2$statistic, 3), "Error: \\( p_0 \\) must be \\( 0 \\leq p_0 \\leq 1\\)")
                ),
                
        tags$h6(paste0("El estadístico \\( Z \\) se distribuye según la normal tipificada. En este caso", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, 
                                                                                                               " se cumplen los supuestos y, por tanto, está recomendado su uso, especialmente si \\( n>30 \\).",
                                                                                                                " no se cumplen los supuestos y, por tanto, no se recomienda su uso."))),
        br(),
        paste0(
          "3. Valor crítico:"),
        tags$li("Valor crítico (para el estadístico \\( Z \\) ):",
          ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( z_{\\alpha} = z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, ifelse(input$Alternativa == "greater",  1-input$alpha,  input$alpha)), 
          "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test2$p.value < input$alpha, "RECHAZAR \\(H_0\\)", "NO RECHAZAR \\(H_0\\)")),
        
        tags$li("p-valor (para los estadístico \\((P_1)\\) o \\((n_1) = \\) ",
                round(binom.test(x = round(input$n_oneprop * input$p_oneprop,0), alternative = input$Alternativa, n = input$n_oneprop, p = input$h0)$p.value,3)),
        tags$li("p-valor (para el estadístico \\( Z )= \\)  ", round(test2$p.value, 3)),
        br(),
        br(),
          tags$b("Interpretación"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test2$p.value < input$alpha, 
                " RECHAZAMOS la hipótesis nula que afirma que la verdadera proporción es ", 
                " NO RECHAZAMOS la hipótesis nula que afirma que la verdadera proporción es "), 
               test2$null.value, " \\((p\\)-value ", ifelse(test2$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test2$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "Una proporción" & input$propx_oneprop == "prop_false") {
      test <- prop.z.test(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa)
      test2 <- prop.z.test3(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa)
      test_confint <- prop.z.test(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = "two.sided")
      withMathJax(
        tags$h6(paste0("De acuerdo con el Teorema del Límite Central, a medida que el tamaño muestral aumenta, las distribuciones de \\((P_1)\\) y \\((n_1)\\) se aproximan a la normal 
                       siempre que se cumpla que \\( nP_1 \\geq 5\\) y \\( n(1-P_1) \\geq 5\\).")),
        tags$h6(paste0("Se ilustran, por tanto, los resultados para los estadísticos \\((P_1)\\) y \\((n_1)\\), y también para su aproximación a la normal (aplicando, por defecto, la corrección por continuidad),
                       la cual solamente debe usarse en el caso de que se cumplan los supuestos.")),
        tags$b("Datos:"),
        br(),
        paste0("\\(n =\\) ", test$n),
        br(),
        paste0("Proporción de éxitos \\((P_1) =\\) ", round(test$estimate, 3)),
        br(),
        paste0("Proporción de fracasos \\( (1- P_1) =\\) ", round(1 - test$estimate, 3)),
        br(),
        helpText(paste0("\\( n_1 = nP_1 \\)")),
        br(),
        paste0("Número de éxitos \\((n_1) =\\) ", input$x_oneprop),
        br(),
        paste0("Número de fracasos \\((1-n_1) =\\) ", input$n_oneprop-input$x_oneprop),
        br(),
        br(),
        tags$b("Supuestos:"),
        paste0("Considerando que \\( nP_1 = \\) ", round(test$n * test$estimate, 3), " y \\( n(1-P_1) = \\) ", round(test$n * (1 - test$estimate), 3)),
        paste0("se puede afirmar que los supuestos de que \\( nP_1 \\geq 5\\) y \\( n(1-P_1) \\geq 5\\)", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, " se cumplen", " no se cumplen.")),
        tags$h6(paste0("Por tanto, ", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, "puede utilizarse la aproximación a la normal.", "no es recomendable utilizar la aproximación a la normal."))),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral):"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(\\pi_1 = P_1 \\pm \\left |  z_{\\alpha/2} \\right | \\sqrt{\\dfrac{P_1(1-P_1)}{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\pi_1 = \\) ", test$null.value, " y \\(H_1 : \\pi_1 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Estadísticos de contraste:"),
        br(),
        tags$li(paste0("Estadístico 1: \\((P_1) =\\) ", round(test$estimate, 3), " y \\((n_1)=\\) ", round(test$estimate*test$n, 0))),
        tags$h6(paste0("\\( P_1\\) y \\( n_1\\) son, en realidad, el mismo estadístico y ambos siguen una distribución Binomial.")),
        
        tags$li("Estadístico 2: \\( Z = \\)",
                ifelse((input$x_oneprop) < (input$n_oneprop * input$h0), " \\( \\dfrac{(n_1 + 0,5) - n\\pi_1}{\\sqrt{n\\pi_1(1-\\pi_1)}} = \\)", 
                       ifelse((input$x_oneprop) > (input$n_oneprop * input$h0), " \\( \\dfrac{(P_1 - 0,5/n) - \\pi_1}{\\sqrt{\\dfrac{\\pi_1(1-\\pi_1)}{n}}} = \\)", 
                              "\\( \\dfrac{n_1 - n\\pi_1}{\\sqrt{n\\pi_1(1-\\pi_1)}} = \\)")),
                "(", round(test2$estimate, 3),
                ifelse((input$x_oneprop) < (input$n_oneprop * input$h0), "+", 
                       ifelse((input$x_oneprop) > (input$n_oneprop * input$h0), "-", 
                              "")),
                round((0.5/test$n),3), "-",test2$null.value,")/",round(test2$stderr, 3), " \\( = \\) ",
                ifelse(test2$null.value >= 0 & test2$null.value <= 1, round(test2$statistic, 3), "Error: \\( p_0 \\) must be \\( 0 \\leq p_0 \\leq 1\\)")
        ),
        
        tags$h6(paste0("El estadístico \\( Z \\) se distribuye según la normal tipificada. En este caso", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, 
                                                                                                                 " se cumplen los supuestos y, por tanto, está recomendado su uso, especialmente si \\( n>30 \\).",
                                                                                                                 " no se cumplen los supuestos y, por tanto, no se recomienda su uso."))),
        br(),
        paste0(
          "3. Valor crítico:"),
        tags$li("Valor crítico (para el estadístico \\( Z \\) ):",
                ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( z_{\\alpha} = z(\\)")),
                ifelse(input$Alternativa == "two.sided", input$alpha / 2, ifelse(input$Alternativa == "greater",  1-input$alpha,  input$alpha)), 
                "\\()\\)", " \\( = \\) ",
                ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
                ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test2$p.value < input$alpha, "RECHAZAR \\(H_0\\)", "NO RECHAZAR \\(H_0\\)")),
        
        tags$li("p-valor (para los estadístico \\((P_1)\\) o \\((n_1) = \\) ",
                round(binom.test(x = input$x_oneprop, alternative = input$Alternativa, n = input$n_oneprop, p = input$h0)$p.value,3)),
        tags$li("p-valor (para el estadístico \\( Z )= \\)  ", round(test2$p.value, 3)),
        br(),
        br(),
        tags$b("Interpretación"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test2$p.value < input$alpha, 
                                                                                     " RECHAZAMOS la hipótesis nula que afirma que la verdadera proporción es ", 
                                                                                     " NO RECHAZAMOS la hipótesis nula que afirma que la verdadera proporción es "), 
               test2$null.value, " \\((p\\)-value ", ifelse(test2$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test2$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  # ANÁLISIS DOS PROPORCIONES  ----------------
  
  output$results_twoprop <- renderUI({
    if (input$inference == "Dos proporciones" & input$propx_twoprop == "prop_true" & input$h0 != 0) {
      test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa, pooled.stderr = FALSE)
      test_confint <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        tags$b("Datos:"),
        br(),
        tags$h5("Tamaños muestrales:"),
        tags$li(paste0("\\(n_1 =\\) ", round(test$n1, 3), " y \\(n_2 =\\) ", round(test$n2, 3))),
        tags$h5("Proporciones de éxito:"),
        tags$li(paste0("\\(\\hat{p}_1 =\\) ", round(test$estimate1, 3), " y \\(\\hat{p}_2 =\\) ", round(test$estimate2, 3))),
        tags$h5(paste0("Número de éxitos (\\(\\hat{x} = n\\hat{p} \\)): ")),
        tags$li(paste0("\\(x_1 =\\) ", round(test$n1 * test$estimate1, 0), " y \\(x_2 =\\) ", round(test$n2 * test$estimate2, 0))),
        tags$h5("Proporciones de fracasos:"),
        tags$li(paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)," y \\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\)", round(1 - test$estimate2, 3))),
        br(),
        tags$b("Supuestos:"),
        br(),
        tags$li(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " y \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        tags$li(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " y \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        tags$h6(paste0("Por tanto, los supuestos de que \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) y \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " se cumplen.", " no se cumplen."))),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(P_1 - P_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " y \\(H_1 : p_1 - p_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(Z = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intepretación"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test$p.value < input$alpha, 
                                                                                     " RECHAZAMOS la hipótesis nula que afirma que la diferencia de proporciones es ", 
                                                                                     " NO RECHAZAMOS la hipótesis nula que afirma que la diferencia de proporciones es "), 
               input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ").")
    
      )
      
      } else if (input$inference == "Dos proporciones" & input$propx_twoprop == "prop_false" & input$h0 != 0 ) {
      test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa, pooled.stderr = FALSE)
      test_confint <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        tags$b("Datos:"),
        br(),
        tags$h5("Tamaños muestrales:"),
        tags$li(paste0("\\(n_1 =\\) ", round(test$n1, 3), " y \\(n_2 =\\) ", round(test$n2, 3))),
        tags$h5(paste0("Número de éxitos: ")),
        tags$li(paste0("\\(x_1 =\\) ", input$x1_twoprop, " y \\(x_2 =\\) ", input$x2_twoprop)),
        tags$h5("Proporciones de éxito (\\(\\hat{p}_i = \\dfrac{x_i}{n_i} \\)):"),
        tags$li(paste0("\\(\\hat{p}_1 = \\dfrac{x_1}{n_1} = \\)", round(test$estimate1, 3), " y \\(\\hat{p}_2 = \\dfrac{x_2}{n_2} = \\)", round(test$estimate2, 3))),
        tags$h5("Proporciones de fracasos:"),
        tags$li(paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)," y \\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\)", round(1 - test$estimate2, 3))),
        br(),
        tags$b("Supuestos:"),
        br(),
        tags$li(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " y \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        tags$li(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " y \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        tags$h6(paste0("Por tanto, los supuestos de que \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) y \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " se cumplen.", " no se cumplen."))),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " y \\(H_1 : p_1 - p_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(Z = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intepretación"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test$p.value< input$alpha, 
                                                                                     " RECHAZAMOS la hipótesis nula que afirma que la diferencia de proporciones es ", 
                                                                                     " NO RECHAZAMOS la hipótesis nula que afirma que la diferencia de proporciones es "), 
               input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ").")
      )
    } else if (input$inference == "Dos proporciones" & input$propx_twoprop == "prop_true" & input$h0 == 0) {
      test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa, pooled.stderr = TRUE)
      test_confint <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        tags$b("Datos:"),
        br(),
        tags$h5("Tamaños muestrales:"),
        tags$li(paste0("\\(n_1 =\\) ", round(test$n1, 3), " y \\(n_2 =\\) ", round(test$n2, 3))),
        tags$h5(paste0("Número de éxitos: ")),
        tags$li(paste0("\\(x_1 =\\) ", input$x1_twoprop, " y \\(x_2 =\\) ", input$x2_twoprop)),
        tags$h5("Proporciones de éxito (\\(\\hat{p}_i = \\dfrac{x_i}{n_i} \\)):"),
        tags$li(paste0("\\(\\hat{p}_1 = \\dfrac{x_1}{n_1} = \\)", round(test$estimate1, 3), " y \\(\\hat{p}_2 = \\dfrac{x_2}{n_2} = \\)", round(test$estimate2, 3))),
        tags$h5("Proporciones de fracasos:"),
        tags$li(paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)," y \\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\)", round(1 - test$estimate2, 3))),
        br(),
        tags$b("Supuestos:"),
        br(),
        tags$li(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " y \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        tags$li(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " y \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        tags$h6(paste0("Por tanto, los supuestos de que \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) y \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " se cumplen.", " no se cumplen."))),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de confianza al ", (1 - input$alpha) * 100, "% para \\(P_1 - P_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : P_1 - P_2 = \\) ", test$null.value, " y \\(H_1 : P_1 - P_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Estadístico de contraste : \\(Z = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (P_1 - P_2)}{\\sqrt{P(1-\\hat{P})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} \\) "),
        br(),
        paste0("donde ", "\\(P = \\dfrac{n_1 \\hat{p}_1 + n_2 \\hat{p}_2}{n_1 + n_2} = \\) ", "(", test$n1, " * ", round(test$estimate1, 3), " + ", test$n2, " * ", round(test$estimate2, 3), ") / (", test$n1, " + ", test$n2, ") = ", round(test$pooled.phat, 3)),
        br(),
        paste0(
          "\\( \\Rightarrow Z = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{P(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretación:"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test$p.value < input$alpha, 
                                                                                     " RECHAZAMOS la hipótesis nula que afirma que la diferencia de proporciones es ", 
                                                                                     " NO RECHAZAMOS la hipótesis nula que afirma que la diferencia de proporciones es "), 
               input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ").")
      )
    } else if (input$inference == "Dos proporciones" & input$propx_twoprop == "prop_false" & input$h0 == 0) {
      test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa, pooled.stderr = TRUE)
      test_confint <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        tags$b("Datos:"),
        br(),
        tags$h5("Tamaños muestrales:"),
        tags$li(paste0("\\(n_1 =\\) ", round(test$n1, 3), " y \\(n_2 =\\) ", round(test$n2, 3))),
        tags$h5(paste0("Número de éxitos: ")),
        tags$li(paste0("\\(x_1 =\\) ", input$x1_twoprop, " y \\(x_2 =\\) ", input$x2_twoprop)),
        tags$h5("Proporciones de éxito (\\(\\hat{p}_i = \\dfrac{x_i}{n_i} \\)):"),
        tags$li(paste0("\\(\\hat{p}_1 = \\dfrac{x_1}{n_1} = \\)", round(test$estimate1, 3), " y \\(\\hat{p}_2 = \\dfrac{x_2}{n_2} = \\)", round(test$estimate2, 3))),
        tags$h5("Proporciones de fracasos:"),
        tags$li(paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)," y \\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\)", round(1 - test$estimate2, 3))),
        br(),
        tags$b("Supuestos:"),
        br(),
        tags$li(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " y \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        tags$li(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " y \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        tags$h6(paste0("Por tanto, los supuestos de que \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) y \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " se cumplen.", " no se cumplen."))),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " y \\(H_1 : p_1 - p_2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Estadístico de contraste : \\(Z = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (P_1 - P_2)}{\\sqrt{P(1-\\hat{P})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} \\) "),
        br(),
        paste0("donde ", "\\(P = \\dfrac{n_1 \\hat{p}_1 + n_2 \\hat{p}_2}{n_1 + n_2} = \\) ", "(", test$n1, " * ", round(test$estimate1, 3), " + ", test$n2, " * ", round(test$estimate2, 3), ") / (", test$n1, " + ", test$n2, ") = ", round(test$pooled.phat, 3)),
        br(),
        paste0(
          "\\( \\Rightarrow Z = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{P(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Valor crítico :", ifelse(input$Alternativa == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$Alternativa == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$Alternativa == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$Alternativa == "two.sided", "\\( \\pm \\)", ifelse(input$Alternativa == "greater", "", " -")),
          ifelse(input$Alternativa == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intepretación"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test$p.value < input$alpha, 
                                                                                     " RECHAZAMOS la hipótesis nula que afirma que la diferencia de proporciones es ", 
                                                                                     " NO RECHAZAMOS la hipótesis nula que afirma que la diferencia de proporciones es "), 
               input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ").")
      )
    } else {
      print("loading...")
    }
  })
  
  # ANÁLISIS UNA VARIANZA ----------------
   ##Función ----------------
      
      unavarianza <- function(s2, sigma2, n, alfa, alternativa = "two.sided") {
        estimate <- s2
        ICSup <- ((n-1)*s2)/(qchisq(p=alfa/2, df=n-1, lower.tail = TRUE))
        ICInf <- ((n-1)*s2)/(qchisq(p=1-(alfa/2), df=n-1, lower.tail = TRUE))
        
        
        statistic <- ((n-1)*s2)/sigma2
        
        if (alternativa == "two.sided") {
          
          p.value <- 2*pchisq(statistic, df = n-1, lower.tail = FALSE)
          
        } else if (alternativa == "less") {
          p.value <- pchisq(statistic, df = n-1, lower.tail = TRUE)
        } else {
          p.value <- pchisq(statistic, df = n-1, lower.tail = FALSE)
        }
        
        resultados <- list(ICInf = ICInf, ICSup = ICSup, estimate = estimate, statistic=statistic, p.value=p.value)
        return(resultados)
        
      }

  output$results_onevar <- renderUI({
    dat <- extract(input$muestra_onevar)
    if (anyNA(dat) | length(dat) < 2) {
      "Entrada no válida o número insuficiente de observaciones"
    } else if (input$h0 <= 0) {
      withMathJax(
        sprintf("\\( \\sigma^2_0 \\) debe ser > 0")
      )
      ## CON DATOS MANUALES ----------------
      
     } else if (input$inference == "Una varianza" & input$inference2 == "Un conjunto de datos de forma manual") {
      test_confint <- varTest(x = dat, sigma.squared = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha)
      test <- varTest(x = dat, sigma.squared = input$h0, alternative = input$Alternativa, conf.level = 1 - input$alpha)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste(c(paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(s^2 =\\) ", round(var(dat), 3)),
        br(),
        paste0("\\(s =\\) ", round(sd(dat), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "IC al ", (1 - input$alpha) * 100, " % para \\(\\sigma^2 = \\Bigg[ \\dfrac{(n-1)s^2}{\\chi^2_{\\alpha/2, n-1}} ; \\dfrac{(n-1)s^2}{\\chi^2_{1-\\alpha/2, n-1}} \\Bigg] = \\) ",
          "[(", round((length(dat) - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3), ") ; (", round((length(dat) - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = TRUE), 3), ")] = ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        paste0("1. \\(H_0 : \\sigma^2 = \\) ", test$null.value, " y \\(H_1 : \\sigma^2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Estadístico de contraste : \\(\\chi^2 = \\dfrac{(n-1)s^2}{\\sigma^2_0} = \\) ",
          "[(", length(dat), " - 1) * ", round(test$estimate, 3), "] / ", test$null.value, " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        if (input$Alternativa == "two.sided") {
          withMathJax(
            paste0("3. Valores críticos : \\( \\chi^2_{1-\\alpha/2, n - 1} \\) y \\( \\chi^2_{\\alpha/2, n - 1} =\\) "),
            paste0("\\( \\chi^2 \\)(", 1 - input$alpha / 2, ", ", test$parameters, ") y \\( \\chi^2 \\)(", input$alpha / 2, ", ", test$parameters, ") = "),
            paste0(round(qchisq(1 - input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3), " y ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3))
          )
        } else if (input$Alternativa == "greater") {
          withMathJax(
            paste0("3. Valor crítico : \\( \\chi^2_{\\alpha, n - 1} =\\) "),
            paste0("\\( \\chi^2 \\)(", input$alpha, ", ", test$parameters, ") = "),
            paste0(round(qchisq(input$alpha, df = test$parameters, lower.tail = FALSE), 3))
          )
        } else {
          withMathJax(
            paste0("3. Valor crítico : \\( \\chi^2_{1-\\alpha, n - 1} =\\) "),
            paste0("\\( \\chi^2 \\)(", 1 - input$alpha, ", ", test$parameters, ") = "),
            paste0(round(qchisq(1 - input$alpha, df = test$parameters, lower.tail = FALSE), 3))
          )
        },
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretación"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test$p.value < input$alpha, 
                                                                                     " RECHAZAMOS la hipótesis nula que afirma que la verdadera varianza es ", 
                                                                                     " NO RECHAZAMOS la hipótesis nula que afirma que la verdadera varianza es "), 
               input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ").")
      )
          ## CON VALORES ----------------
      
     } else if (input$inference == "Una varianza" & input$inference2 == "Valores poblaciones y muestrales") {
        test <- unavarianza (s2 = input$unavarianza_S2, n = input$unavarianza_n, sigma2 = input$h0, alternativa = input$Alternativa, alfa = input$alpha)
        withMathJax(
          tags$b("Datos:"),
          br(),
          paste0("\\(n =\\) ", input$unavarianza_n),
          br(),
          paste0("\\(s^2 =\\) ", round(input$unavarianza_S2, 3)),
          br(),
          paste0("\\(s =\\) ", round(sqrt(input$unavarianza_S2), 3)),
          br(),
          br(),
          tags$b("Intervalo de confianza"),
          tags$em("(Bilateral)"),
          br(),
          paste0(
            "IC al ", (1 - input$alpha) * 100, " % para \\(\\sigma^2 = \\Bigg[ \\dfrac{(n-1)s^2}{\\chi^2_{\\alpha/2, n-1}} ; \\dfrac{(n-1)s^2}{\\chi^2_{1-\\alpha/2, n-1}} \\Bigg] = \\) ",
            "[(", round((input$unavarianza_n - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = input$unavarianza_n - 1, lower.tail = FALSE), 3), ") ; (", round((input$unavarianza_n - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = input$unavarianza_n - 1, lower.tail = TRUE), 3), ")] = ",
            "[", round(test$ICInf, 3), "; ", round(test$ICSup, 3), "]"
          ),
          br(),
          br(),
          tags$b("Contraste de la hipótesis:"),
          br(),
          paste0("1. \\(H_0 : \\sigma^2 = \\) ", input$h0, " y \\(H_1 : \\sigma^2 \\) ", ifelse(input$Alternativa == "two.sided", "\\( \\neq \\) ", ifelse(input$Alternativa == "greater", "\\( > \\) ", "\\( < \\) ")),input$h0),
          br(),
          paste0(
            "2. Estadístico de contraste : \\(\\chi^2 = \\dfrac{(n-1)s^2}{\\sigma^2_0} = \\) ",
            "[(", input$unavarianza_n, " - 1) * ", round(test$estimate, 3), "] / ",input$h0, " \\( = \\) ",
            round(test$statistic, 3)
          ),
          br(),
          if (input$Alternativa == "two.sided") {
            withMathJax(
              paste0("3. Valores críticos : \\( \\chi^2_{1-\\alpha/2, n - 1} \\) y \\( \\chi^2_{\\alpha/2, n - 1} =\\) "),
              paste0("\\( \\chi^2 \\)(", 1 - input$alpha / 2, ", ", input$unavarianza_n - 1, ") y \\( \\chi^2 \\)(", input$alpha / 2, ", ", input$unavarianza_n - 1, ") = "),
              paste0(round(qchisq(1 - input$alpha / 2, df = input$unavarianza_n - 1, lower.tail = FALSE), 3), " y ", round(qchisq(input$alpha / 2, df = input$unavarianza_n - 1, lower.tail = FALSE), 3))
            )
          } else if (input$Alternativa == "greater") {
            withMathJax(
              paste0("3. Valor crítico : \\( \\chi^2_{\\alpha, n - 1} =\\) "),
              paste0("\\( \\chi^2 \\)(", input$alpha, ", ", input$unavarianza_n - 1, ") = "),
              paste0(round(qchisq(input$alpha, df = input$unavarianza_n - 1, lower.tail = FALSE), 3))
            )
          } else {
            withMathJax(
              paste0("3. Valor crítico : \\( \\chi^2_{1-\\alpha, n - 1} =\\) "),
              paste0("\\( \\chi^2 \\)(", 1 - input$alpha, ", ", input$unavarianza_n - 1, ") = "),
              paste0(round(qchisq(1 - input$alpha, df = input$unavarianza_n - 1, lower.tail = FALSE), 3))
            )
          },
          br(),
          paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
          br(),
          br(),
          tags$b("Interpretación"),
          br(),
          paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test$p.value < input$alpha, 
                                                                                       " RECHAZAMOS la hipótesis nula que afirma que la verdadera varianza es ", 
                                                                                       " NO RECHAZAMOS la hipótesis nula que afirma que la verdadera varianza es "), 
                 input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ").")
          
          )
    } else {
      print("loading...")
    }
  })
  
  # ANÁLISIS DOS VARIANZAS ----------------
  
  ### Función ---------------
  
  dosvarianzas <- function (s21, s22, n1, n2, alternativa, alfa) {
    
    F <- (s21/s22)
    
    
    ICInf <- F * qf(alfa / 2, df1 = n1-1, df2 = n2-1, lower.tail = TRUE)
    ICSup <- F * qf(alfa / 2, df1 = n1-1, df2 = n2-1, lower.tail = FALSE)
    
    if (alternativa == "two.sided") {
      if (F>=1) {
        p.value <- 2*pf(F, df1 = n1-1, df2 = n2-1, lower.tail = FALSE)}
      else {
        
        p.value <- 2*pf(F, df1 = n1-1, df2 = n2-1, lower.tail = TRUE)
      }
      
    } else if (alternativa == "less") {
      p.value <- pf(F, df1 = n1-1, df2 = n2-1, lower.tail = TRUE)
    } else {
      p.value <- pf(F, df1 = n1-1, df2 = n2-1, lower.tail = FALSE)
    }
    
    
    resultados <- list(statistic=F, ICInf=ICInf, ICSup=ICSup, p.value=p.value)
    return(resultados)
    
    }
  
  
  
  output$results_twovar <- renderUI({
    dat1 <- extract(input$muestra1_twovar)
    dat2 <- extract(input$muestra2_twovar)
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Entrada no válida o número insuficiente de observaciones"
    } else if (input$h0 <= 0) {
      withMathJax(
        sprintf("\\( \\sigma^2_1 - \\sigma^2_2 \\) must be > 0")
      )
      
      ## Con datos manuales  ----------------
    } else if (input$inference == "Dos varianzas" & input$inference2 == "Un conjunto de datos de forma manual") {
      test_confint <- var.test(x = dat1, y = dat2, ratio = 1, alternative = "two.sided", conf.level = 1 - input$alpha)
      test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$Alternativa_twovar, conf.level = 1 - input$alpha)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste(c("\\(Muestra_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Muestra_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
        br(),
        paste0("\\(s_1 =\\) ", round(sd(dat1), 3)),
        br(),
        paste0("\\(s_2 =\\) ", round(sd(dat2), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de Confianza al ",(1 - input$alpha) * 100, " % para \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = \\Bigg[ \\dfrac{s^2_1}{s^2_2}F_{1-\\alpha/2, n_1 - 1, n_2-1} \\) ; 
          \\( \\dfrac{s^2_1}{s^2_2} F_{\\alpha/2, n_1 - 1, n_2-1} \\Bigg] = \\) ",
          "\\( \\big[ \\)", round(test_confint$estimate, 3), " * ", round(qf(1-input$alpha / 2, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), "; ", 
          round(test_confint$estimate, 3), " * ", round(qf(input$alpha / 2, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), "\\( \\big] = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        if (input$Alternativa_twovar == "two.sided") {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) y \\(H_1 : \\sigma^2_1 \\neq \\sigma^2_2 \\) ")
          )
        } else if (input$Alternativa_twovar == "greater") {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) y \\(H_1 : \\sigma^2_1 > \\sigma^2_2 \\) ")
          )
        } else {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) y \\(H_1 : \\sigma^2_1 < \\sigma^2_2 \\) ")
          )
        },
        br(),
        paste0(
          "2. Estadístico de contraste : \\(F = \\dfrac{s^2_1}{s^2_2} = \\) ",
          "[", round(var(dat1), 3), " / ", round(var(dat2), 3), "]", " \\( = \\) ",
          round(test$statistic, 3)
      ),
      br(),
      if (input$Alternativa_twovar == "two.sided") {
        withMathJax(
          paste0("3. Valores críticos : \\( F_{\\alpha/2, n_1 - 1, n_2-1} \\) y \\( F_{1-\\alpha/2, n_1 - 1, n_2-1} =\\) "),
          paste0("\\( F \\)(", input$alpha / 2, ", ", test$parameter[1], ", ", test$parameter[2], ") y \\( F \\)(", 1 - (input$alpha / 2), ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
          paste0(round(qf(input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE), 3), " y ", round(qf(input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE), 3))
        )
      } else if (input$Alternativa_twovar == "greater") {
        withMathJax(
          paste0("3. Valor crítico : \\( F_{1-\\alpha, n_1 - 1, n_2-1} =\\) "),
          paste0("\\( F \\)(", 1-input$alpha, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
          paste0(round(qf(input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE), 3))
        )
      } else {
        withMathJax(
          paste0("3. Valor críticos : \\( F_{\\alpha, n_1 - 1, n_2-1} = \\) "),
          paste0("\\( F \\)(", input$alpha, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
          paste0(round(qf(input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE), 3))
        )
      },
      br(),
      paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
      br(),
      br(),
      tags$b("Intepretación"),
      br(),
      paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test$p.value < input$alpha, 
                                                                                   " RECHAZAMOS la hipótesis nula que afirma que las varianzas poblacionales son iguales ", 
                                                                                   " NO RECHAZAMOS la hipótesis nula que afirma que las varianzas poblacionales son iguales "), 
             input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ").")
      )
      
      ## Con valores  ----------------
      
    } else if (input$inference == "Dos varianzas" & input$inference2 == "Valores poblaciones y muestrales") {
      test_confint <- dosvarianzas(s21=input$dosvarianzas_s21, s22=input$dosvarianzas_s22, 
                                   n1=input$dosvarianzas_n1, n2=input$dosvarianzas_n2, alfa=input$alpha, alternativa="two.sided")
      test <- dosvarianzas(s21=input$dosvarianzas_s21, s22=input$dosvarianzas_s22, 
                                   n1=input$dosvarianzas_n1, n2=input$dosvarianzas_n2, alfa=input$alpha, alternativa = input$Alternativa_twovar)
      withMathJax(
        tags$b("Datos:"),
        br(),
        paste0("\\(n_1 =\\) ", input$dosvarianzas_n1),
        br(),
        paste0("\\(n_2 =\\) ", input$dosvarianzas_n2),
        br(),
        paste0("\\(s^2_1 =\\) ", round(input$dosvarianzas_s21, 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(input$dosvarianzas_s22, 3)),
        br(),
        paste0("\\(s_1 =\\) ", round(sqrt(input$dosvarianzas_s21), 3)),
        br(),
        paste0("\\(s_2 =\\) ", round(sqrt(input$dosvarianzas_s22), 3)),
        br(),
        br(),
        tags$b("Intervalo de confianza"),
        tags$em("(Bilateral)"),
        br(),
        paste0(
          "Intervalo de Confianza al ",(1 - input$alpha) * 100, " % para \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = \\Bigg[ \\dfrac{s^2_1}{s^2_2}F_{1-\\alpha/2, n_1 - 1, n_2-1} \\) ; 
          \\( \\dfrac{s^2_1}{s^2_2} F_{\\alpha/2, n_1 - 1, n_2-1} \\Bigg] = \\) ",
          "\\( \\big[ \\)", round(test_confint$statistic, 3), " * ", round(qf(1-input$alpha / 2, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = FALSE), 3), "; ", 
          round(test_confint$statistic, 3), " * ", round(qf(input$alpha / 2, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = FALSE), 3), "\\( \\big] = \\) ",
          "[", round(test_confint$ICInf, 3), "; ", round(test_confint$ICSup, 3), "]"
        ),
        br(),
        br(),
        tags$b("Contraste de la hipótesis:"),
        br(),
        if (input$Alternativa_twovar == "two.sided") {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) y \\(H_1 : \\sigma^2_1 \\neq \\sigma^2_2 \\) ")
          )
        } else if (input$Alternativa_twovar == "greater") {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) y \\(H_1 : \\sigma^2_1 > \\sigma^2_2 \\) ")
          )
        } else {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) y \\(H_1 : \\sigma^2_1 < \\sigma^2_2 \\) ")
          )
        },
        br(),
        paste0(
          "2. Estadístico de contraste : \\(F = \\dfrac{s^2_1}{s^2_2} = \\) ",
          "[", round(input$dosvarianzas_s21, 3), " / ", round(input$dosvarianzas_s22, 3), "]", " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        if (input$Alternativa_twovar == "two.sided") {
          withMathJax(
            paste0("3. Valores críticos : \\( F_{\\alpha/2, n_1 - 1, n_2-1} \\) y \\( F_{1-\\alpha/2, n_1 - 1, n_2-1} =\\) "),
            paste0("\\( F \\)(", input$alpha / 2, ", ", input$dosvarianzas_n1-1, ", ", input$dosvarianzas_n2-1, ") y \\( F \\)(", 1 - (input$alpha / 2), ", ", input$dosvarianzas_n1-1, ", ", input$dosvarianzas_n2-1, ") = "),
            paste0(round(qf(input$alpha / 2, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = TRUE), 3), " y ", round(qf(input$alpha / 2, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = FALSE), 3))
          )
        } else if (input$Alternativa_twovar == "greater") {
          withMathJax(
            paste0("3. Valor crítico : \\( F_{1-\\alpha, n_1 - 1, n_2-1} =\\) "),
            paste0("\\( F \\)(", 1-input$alpha, ", ", input$dosvarianzas_n1-1, ", ", input$dosvarianzas_n2-1, ") = "),
            paste0(round(qf(input$alpha, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = FALSE), 3))
          )
        } else {
          withMathJax(
            paste0("3. Valor críticos : \\( F_{\\alpha, n_1 - 1, n_2-1} = \\) "),
            paste0("\\( F \\)(", input$alpha, ", ", input$dosvarianzas_n1-1, ", ", input$dosvarianzas_n2-1, ") = "),
            paste0(round(qf(input$alpha, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = TRUE), 3))
          )
        },
        br(),
        br(),
        paste0("4. Conclusión/Decisión : ", ifelse(test$p.value < input$alpha, "Rechazar \\(H_0\\)", "No rechazar \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intepretación"),
        br(),
        paste0("Con un nivel de significación del ", input$alpha * 100, "%,", ifelse(test$p.value < input$alpha, 
                                                                                     " RECHAZAMOS la hipótesis nula que afirma que las varianzas poblacionales son iguales ", 
                                                                                     " NO RECHAZAMOS la hipótesis nula que afirma que las varianzas poblacionales son iguales "), 
               input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ").")
        
     
  )
      
    } else {
      print("loading...")
    }
  })

  # GENERADOR DE GRÁFICOS ------
  
  output$plot <- renderPlot({
    ## Una media, datos manuales y varianza poblacional desconocida ------
    if (input$inference == "Una media" & input$inference2 == "Un conjunto de datos de forma manual" & input$popsd_onemean == FALSE) {
      dat <- extract(input$muestra_onemean)
      test <- t.test(x = dat, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.7, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      ## Una media, datos manuales y varianza poblacional conocida ---------------
    } else if (input$inference == "Una media" & input$popsd_onemean == TRUE) {
      dat <- extract(input$muestra_onemean)
      test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, Alternativa = input$Alternativa)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.7, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      ## Una media, con valores y varianza poblacional desconocida ------

  } else if (input$inference == "Una media" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_onemean2 == FALSE) {
        test <- t.testv2(mean = input$smean_onemean, n = input$n_onemean, sd = input$sd_onemean, alternative = input$Alternativa, mu = input$h0)
        if (input$Alternativa == "two.sided") {
          funcShaded <- function(x) {
            y <- dt(x, df = input$n_onemean-1)
            y[x < qt(input$alpha / 2, df = input$n_onemean-1, lower.tail = FALSE) & x > qt(input$alpha / 2, df = input$n_onemean-1) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "greater") {
          funcShaded <- function(x) {
            y <- dt(x, df = input$n_onemean-1)
            y[x < qt(input$alpha, df = input$n_onemean-1, lower.tail = FALSE) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "less") {
          funcShaded <- function(x) {
            y <- dt(x, df = input$n_onemean-1)
            y[x > qt(input$alpha, df = input$n_onemean-1, lower.tail = TRUE) ] <- NA
            return(y)
          }
        }
        p <- ggplot(data.frame(x = c(qt(0.999, df = input$n_onemean-1, lower.tail = FALSE), qt(0.999, df = input$n_onemean-1, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dt, args = list(df = input$n_onemean-1), linewidth = 0.8) +
          stat_function(fun = funcShaded, geom = "area", alpha = 0.7, fill = "red") +
          theme_minimal() +
          geom_vline(xintercept = test$Estadistico, color = "steelblue") +
          geom_text(aes(x = test$Estadistico, label = paste0("Estadístico de contraste = ", round(test$Estadistico, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
          ggtitle(paste0("Distribución t de Student", " t(", round(input$n_onemean-1, 3), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Densidad") +
          xlab("x")
        p
        
        
        ## Una media, con valores y varianza poblacional conocida ------
  } else if (input$inference == "Una media" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_onemean2 == TRUE) {
        test <- t.testv2(mean = input$smean_onemean, n = input$n_onemean, sd = input$sd_onemean, alternative = input$Alternativa, mu = input$h0, varprob = input$sigma2_onemean)
        if (input$Alternativa == "two.sided") {
          funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "greater") {
          funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "less") {
          funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
            return(y)
          }
        }
        
        p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 0.8) +
          stat_function(fun = funcShaded, geom = "area", alpha = 0.7, fill = "red") +
          theme_minimal() +
          geom_vline(xintercept = test$Estadistico, color = "steelblue") +
          geom_text(aes(x = test$Estadistico, label = paste0("Estadístico de contraste = ", round(test$Estadistico, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 40)) +
          ggtitle(paste0("Distribución Normal N(0,1)")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Densidad") +
          xlab("x")
        p
        
        ## Dos medias independientes, con valores y asumiendo varianzas iguales ------      
        } else if (input$inference == "Dos medias (independientes)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeans2 == FALSE & input$var.equal2 == TRUE) {
          inde2means <- t.2indemeans(n1 = input$n1_2indemean, n2 = input$n2_2indemean, 
                                     M1 = input$mean1_2indemean, M2 = input$mean2_2indemean, 
                                     s1 = input$sd1_2indemean, s2 = input$sd2_2indemean, 
                                     IgualVar = input$var.equal2, m0 = input$h0, alpha = input$alpha)
        if (input$Alternativa == "two.sided") {
          funcShaded <- function(x) {
            y <- dt(x, df = input$n1_2indemean + input$n2_2indemean - 2)
            y[x < qt(input$alpha / 2, df = input$n1_2indemean + input$n2_2indemean - 2, lower.tail = FALSE) & x > qt(input$alpha / 2, df = input$n1_2indemean + input$n2_2indemean - 2) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "greater") {
          funcShaded <- function(x) {
            y <- dt(x, df = input$n1_2indemean + input$n2_2indemean - 2)
            y[x < qt(input$alpha, df = input$n1_2indemean + input$n2_2indemean - 2, lower.tail = FALSE) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "less") {
          funcShaded <- function(x) {
            y <- dt(x, df = input$n1_2indemean + input$n2_2indemean - 2)
            y[x > qt(input$alpha, df =input$n1_2indemean + input$n2_2indemean - 2, lower.tail = TRUE) ] <- NA
            return(y)
          }
        }
        p <- ggplot(data.frame(x = c(qt(0.999, df = input$n1_2indemean + input$n2_2indemean - 2, lower.tail = FALSE), qt(0.999, df = input$n1_2indemean + input$n2_2indemean - 2, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dt, args = list(df = input$n1_2indemean + input$n2_2indemean - 2), linewidth = 0.8) +
          stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
          theme_minimal() +
          geom_vline(xintercept = inde2means$Estadistico, color = "steelblue") +
          geom_text(aes(x = inde2means$Estadistico, label = paste0(("Estadístico de contraste = "), round(inde2means$Estadistico, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
          ggtitle(paste0("Distribución t de Student", " t(", round(input$n1_2indemean + input$n2_2indemean - 2, 3), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Densidad") +
          xlab("x")
        p
        ## Dos medias independientes, con valores y asumiendo varianzas NO iguales ------      
      } else if (input$inference == "Dos medias (independientes)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeans2 == FALSE & input$var.equal2 == FALSE) {
        inde2means <- t.2indemeans(n1 = input$n1_2indemean, n2 = input$n2_2indemean, 
                                   M1 = input$mean1_2indemean, M2 = input$mean2_2indemean, 
                                   s1 = input$sd1_2indemean, s2 = input$sd2_2indemean, 
                                   IgualVar = input$var.equal2, m0 = input$h0, alpha = input$alpha)
        
        if (input$Alternativa == "two.sided") {
          funcShaded <- function(x) {
            y <- dt(x, df = inde2means$v)
            y[x < qt(input$alpha / 2, df = inde2means$v, lower.tail = FALSE) & x > qt(input$alpha / 2, df = inde2means$v) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "greater") {
          funcShaded <- function(x) {
            y <- dt(x, df = inde2means$v)
            y[x < qt(input$alpha, df = inde2means$v, lower.tail = FALSE) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "less") {
          funcShaded <- function(x) {
            y <- dt(x, df = inde2means$v)
            y[x > qt(input$alpha, df = inde2means$v, lower.tail = TRUE) ] <- NA
            return(y)
          }
        }
        p <- ggplot(data.frame(x = c(qt(0.999, df = inde2means$v, lower.tail = FALSE), qt(0.999, df = inde2means$v, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dt, args = list(df = inde2means$v),linewidth = 0.8) +
          stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
          theme_minimal() +
          geom_vline(xintercept = inde2means$Estadistico, color = "steelblue") +
          geom_text(aes(x = inde2means$Estadistico, label = paste0("Estadístico de contraste = ", round(inde2means$Estadistico, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
          ggtitle(paste0("Distribución t de Student", " t(", round(inde2means$v, 3), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Densidad") +
          xlab("x")
        p
        ## Dos medias independientes, con valores y asumiendo varianzas poblacionales conocidas ------      
        
      } else if (input$inference == "Dos medias (independientes)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeans2 == TRUE) {
        inde2means <- t.2indemeans(
          n1 = input$n1_2indemean, n2 = input$n2_2indemean,
          s1 = input$sd1_2indemean, s2 = input$sd2_2indemean,
          M1 = input$mean1_2indemean, M2 = input$mean2_2indemean,
          Varpob1 = input$sigma21_twomeans, Varpob2 = input$sigma22_twomeans,
          IgualVar = FALSE, m0 = input$h0, alpha = input$alpha, Varpob = TRUE,
          Alternativa = input$Alternativa
        )
        if (input$Alternativa == "two.sided") {
          funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "greater") {
          funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
            return(y)
          }
        } else if (input$Alternativa == "less") {
          funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
            return(y)
          }
        }
        p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 0.8) +
          stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
          theme_minimal() +
          geom_vline(xintercept = inde2means$Estadistico, color = "steelblue") +
          geom_text(aes(x = inde2means$Estadistico, label = paste0("Estadístico de contraste = ", round(inde2means$Estadistico, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
          ggtitle(paste0("Distribución Normal N(0,1)")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Densidad") +
          xlab("x")
        p
        
        ## Dos medias independientes, con datos manuales y  asumiendo varianzas iguales ------      
        
    } else if (input$inference == "Dos medias (independientes)" & input$inference2 == "Un conjunto de datos de forma manual" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
      dat1 <- extract(input$muestra1_twomeans)
      dat2 <- extract(input$muestra2_twomeans)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      ## Dos medias independientes, con datos manuales y no asumiendo varianzas iguales ------   
    } else if (input$inference == "Dos medias (independientes)" & input$inference2 == "Un conjunto de datos de forma manual"& input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
      dat1 <- extract(input$muestra1_twomeans)
      dat2 <- extract(input$muestra2_twomeans)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      ## Dos medias independientes, con datos manuales y asumiendo varianzas poblacionales conocidas ------      
      
    } else if (input$inference == "Dos medias (independientes)" & input$inference2 == "Un conjunto de datos de forma manual" & input$popsd_twomeans == TRUE) {
      dat1 <- extract(input$muestra1_twomeans)
      dat2 <- extract(input$muestra2_twomeans)
      test <- t.test3(x = dat1, y = dat2, V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, Alternativa = input$Alternativa)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      ## Dos medias relacionadas, con valores y con la varianza de las diferencias desconocida -------- 
    } else if (input$inference == "Dos medias (relacionadas)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeanspaired2 == FALSE) {
      rela2means <- t.2relameans (dif = input$dif_twomeanspaired, n = input$n_twomeanspaired,
                                  Varpob = "FALSE", sd_muestradif = input$sd_difs_twomeanspaired, 
                                  hipodif = input$h0, alpha = input$alpha, 
                                  Alternativa = input$Alternativa)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = input$n_twomeanspaired -1)
          y[x < qt(input$alpha / 2, df = input$n_twomeanspaired -1, lower.tail = FALSE) & x > qt(input$alpha / 2, df = input$n_twomeanspaired -1) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = input$n_twomeanspaired -1)
          y[x < qt(input$alpha, df = input$n_twomeanspaired -1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = input$n_twomeanspaired -1)
          y[x > qt(input$alpha, df = input$n_twomeanspaired -1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = input$n_twomeanspaired -1, lower.tail = FALSE), qt(0.999, df = input$n_twomeanspaired -1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$n_twomeanspaired -1), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = rela2means$Estadistico, color = "steelblue") +
        geom_text(aes(x = rela2means$Estadistico, label = paste0("Estadístico de contraste = ", round(rela2means$Estadistico, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Distribución t de Student", " t(", round(input$n_twomeanspaired -1, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      ## Dos medias relacionadas, con valores y con la varianza de las diferencias conocida -------- 
      
    } else if (input$inference == "Dos medias (relacionadas)" & input$inference2 == "Valores poblaciones y muestrales" & input$popsd_twomeanspaired2 == TRUE) {
      rela2means <- t.2relameans (dif = input$dif_twomeanspaired, n = input$n_twomeanspaired,
                                  Varpob = "TRUE", sd_muestradif = input$sd_difs_twomeanspaired, 
                                  hipodif = input$h0, alpha = input$alpha, var_pobdif = input$sigma2_twomeanspaired2,
                                  Alternativa = input$Alternativa)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = rela2means$Estadistico, color = "steelblue") +
        geom_text(aes(x = rela2means$Estadistico, label = paste0("Estadístico de contraste = ", round(rela2means$Estadistico, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Distribución Normal N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      ## Dos medias relacionadas, con datos manuales y con la varianza de las diferencias desconocida -------- 
      
    } else if (input$inference == "Dos medias (relacionadas)" & input$inference2 == "Un conjunto de datos de forma manual" & input$popsd_twomeanspaired == FALSE) {
      dat1 <- extract(input$muestra1_twomeanspaired)
      dat2 <- extract(input$muestra2_twomeanspaired)
      test <- t.test(x = dat2, y = dat1, mu = input$h0, Alternativa = input$Alternativa, conf.level = 1 - input$alpha, paired = TRUE)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      ## Dos medias relacionadas, con datos manuales y con la varianza de las diferencias conocida -------- 
      
    } else if (input$inference == "Dos medias (relacionadas)" & input$inference2 == "Un conjunto de datos de forma manual" & input$popsd_twomeanspaired == TRUE) {
      dat1 <- extract(input$muestra1_twomeanspaired)
      dat2 <- extract(input$muestra2_twomeanspaired)
      test <- t.test2(x = dat2 - dat1, V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, Alternativa = input$Alternativa)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      ## Una proporción -------- 
      
      
    } else if (input$inference == "Una proporción") {
      if (input$propx_oneprop == "prop_true") {
        test <- prop.z.test3(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa)
      } else {
        test <- prop.z.test3(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa)
      }
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Distribución Normal N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      ## Dos proporciones -------- 
      
      
    } else if (input$inference == "Dos proporciones") {
      if (input$propx_twoprop == "prop_true" & input$h0 != 0) {
        test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa, pooled.stderr = FALSE)
      } else if (input$propx_twoprop == "prop_false" & input$h0 != 0) {
        test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa, pooled.stderr = FALSE)
      } else if (input$propx_twoprop == "prop_true" & input$h0 == 0) {
        test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa, pooled.stderr = TRUE)
      } else if (input$propx_twoprop == "prop_false" & input$h0 == 0) {
        test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, Alternativa = input$Alternativa, pooled.stderr = TRUE)
      }
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      
      ## Una varianza con datos manuales-------- 
    } else if (input$inference == "Una varianza" & input$inference2 == "Un conjunto de datos de forma manual") {
      dat <- extract(input$muestra_onevar)
      test <- varTest(x = dat, sigma.squared = input$h0, alternative = input$Alternativa, conf.level = 1 - input$alpha)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = test$parameters)
          y[x > qchisq(1 - input$alpha / 2, df = test$parameters, lower.tail = FALSE) & x < qchisq(1 - input$alpha / 2, df = test$parameters, lower.tail = TRUE)] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = test$parameters)
          y[x < qchisq(input$alpha, df = test$parameters, lower.tail = FALSE)] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = test$parameters)
          y[x > qchisq(1 - input$alpha, df = test$parameters, lower.tail = FALSE)] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(0, qchisq(0.999, df = test$parameters, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = test$parameters), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.1), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(bquote(paste("Distribución ", chi^2 , " con ", .(test$parameters), " grados de libertad" ))) +
        # ggtitle(withMathJax(paste0("\\(\\chi ^{2} =\\)  Chi-square distribution (gl = ", test$parameters, ")"))) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      ## Una varianza con valores-------- 
      
    } else if (input$inference == "Una varianza" & input$inference2 == "Valores poblaciones y muestrales") {
      test <- unavarianza(s2 = input$unavarianza_S2, n = input$unavarianza_n, sigma2 = input$h0, alternativa = input$Alternativa, alfa = input$alpha)
      if (input$Alternativa == "two.sided") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = input$unavarianza_n - 1)
          y[x > qchisq(1 - input$alpha / 2, df =input$unavarianza_n - 1, lower.tail = FALSE) & x < qchisq(1 - input$alpha / 2, df = input$unavarianza_n - 1, lower.tail = TRUE)] <- NA
          return(y)
        }
      } else if (input$Alternativa == "greater") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = input$unavarianza_n - 1)
          y[x < qchisq(input$alpha, df = input$unavarianza_n - 1, lower.tail = FALSE)] <- NA
          return(y)
        }
      } else if (input$Alternativa == "less") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = input$unavarianza_n - 1)
          y[x > qchisq(1 - input$alpha, df = input$unavarianza_n - 1, lower.tail = FALSE)] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(0, qchisq(0.999, df = input$unavarianza_n - 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = input$unavarianza_n - 1), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red" ) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.1), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(bquote(paste("Distribución ", chi^2 , " con ", .(input$unavarianza_n - 1), " grados de libertad" ))) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      ## Dos varianzas con datos manuales-------- 
      
      } else if (input$inference == "Dos varianzas" & input$inference2 == "Un conjunto de datos de forma manual") {
      dat1 <- extract(input$muestra1_twovar)
      dat2 <- extract(input$muestra2_twovar)
      test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$Alternativa_twovar, conf.level = 1 - input$alpha)
      if (input$Alternativa_twovar == "two.sided") {
        funcShaded <- function(x) {
          y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
          y[x > qf(1 - input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE) & x < qf(1 - input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE)] <- NA
          return(y)
        }
      } else if (input$Alternativa_twovar == "greater") {
        funcShaded <- function(x) {
          y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
          y[x < qf(input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE)] <- NA
          return(y)
        }
      } else if (input$Alternativa_twovar == "less") {
        funcShaded <- function(x) {
          y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
          y[x > qf(1 - input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE)] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(0, qf(0.99, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = test$parameter[1], df2 = test$parameter[2]), linewidth = 0.8) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Distribución F(", test$parameter[1], ", ", test$parameter[2], ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Densidad") +
        xlab("x")
      p
      
      ## Dos varianzas con valores-------- 
      
      } else if (input$inference == "Dos varianzas" & input$inference2 == "Valores poblaciones y muestrales") {
        test <- dosvarianzas(s21=input$dosvarianzas_s21, s22=input$dosvarianzas_s22, 
                             n1=input$dosvarianzas_n1, n2=input$dosvarianzas_n2, alfa=input$alpha, alternativa = input$Alternativa_twovar)        
        if (input$Alternativa_twovar == "two.sided") {
          funcShaded <- function(x) {
            y <- df(x, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1)
            y[x > qf(1 - input$alpha / 2, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = FALSE) & x < qf(1 - input$alpha / 2, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = TRUE)] <- NA
            return(y)
          }
        } else if (input$Alternativa_twovar == "greater") {
          funcShaded <- function(x) {
            y <- df(x, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1)
            y[x < qf(input$alpha, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = FALSE)] <- NA
            return(y)
          }
        } else if (input$Alternativa_twovar == "less") {
          funcShaded <- function(x) {
            y <- df(x, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1)
            y[x > qf(1 - input$alpha, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = FALSE)] <- NA
            return(y)
          }
        }
        p <- ggplot(data.frame(x = c(0, qf(0.99, df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = df, args = list(df1 = input$dosvarianzas_n1-1, df2 = input$dosvarianzas_n2-1), linewidth = 0.8) +
          stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "red") +
          theme_minimal() +
          geom_vline(xintercept = test$statistic, color = "steelblue") +
          geom_text(aes(x = test$statistic, label = paste0("Estadístico de contraste = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
          ggtitle(paste0("Distribución F(", input$dosvarianzas_n1-1, ", ", input$dosvarianzas_n2-1, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Densidad") +
          xlab("x")
        p
      
    } else {
      print("loading...")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
