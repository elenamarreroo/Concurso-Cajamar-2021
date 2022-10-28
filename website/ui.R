
# Descargas de librerías 
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("png")
# install.packages("shinyjs")
# install.packages("DT")
# install.packages("visNetwork")
# install.packages("rintrojs")
# install.packages("lubridate")
# install.packages("tidyverse")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinydashboardPlus")
# install.packages("eurostat")
# install.packages("leaflet")
# install.packages("RColorBrewer")

# Librerías
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(lubridate)
library(tidyverse)
library(shiny)
library(shinydashboard) 
library(shinydashboardPlus)
library(eurostat)
library(leaflet)
library(RColorBrewer)

# Datos para hacer un select en la pregunta 1
# Dataset 1
Dataset1_DatosConsumoAlimentarioMAPAporCCAA <- read_delim("Dataset1.- DatosConsumoAlimentarioMAPAporCCAA.txt", "|", escape_double = FALSE, trim_ws = TRUE)
# Dataset 2
Dataset2_Precios_Semanales_Observatorio_de_Precios_Junta_de_Andalucia <- read_delim("Dataset2.- Precios Semanales Observatorio de Precios Junta de Andalucia.txt",  "|", escape_double = FALSE, trim_ws = TRUE)
# MercaMadrid
Dataset3a_Datos_MercaMadrid <- read_delim("Dataset3a_Datos_MercaMadrid.txt", "|", escape_double = FALSE, trim_ws = TRUE)
# MercaBarna
Dataset3b_Datos_MercaBarna <- read_delim("Dataset3b_Datos_MercaBarna.txt", "|", escape_double = FALSE, trim_ws = TRUE)
# Dataset 4
Dataset4_Comercio_Exterior_de_Espania <- read_delim("Dataset4.- Comercio Exterior de Espana.txt", "|", escape_double = FALSE, trim_ws = TRUE)
# Dataset 5
Dataset5_Coronavirus_cases <- read_delim("Dataset5_Coronavirus_cases.txt", "|", escape_double = FALSE, trim_ws = TRUE)
# Dataset 6

DATA3 <- Dataset1_DatosConsumoAlimentarioMAPAporCCAA %>% filter(CCAA == "Total Nacional") 
colnames(DATA3)[colnames(DATA3) == "Precio medio kg"] <- "Precio_medio_kg"
DATA3$Precio_medio_kg <- str_replace(DATA3$Precio_medio_kg, ",", ".")
DATA3$Precio_medio_kg <- as.numeric(DATA3$Precio_medio_kg)

DATA3$Mes[DATA3$Mes == "Enero"] <- "01-01"
DATA3$Mes[DATA3$Mes == "Febrero"] <- "01-02"
DATA3$Mes[DATA3$Mes == "Marzo"] <- "01-03"
DATA3$Mes[DATA3$Mes == "Abril"] <-"01-04"
DATA3$Mes[DATA3$Mes == "Mayo"] <- "01-05"
DATA3$Mes[DATA3$Mes == "Junio"] <- "01-06"
DATA3$Mes[DATA3$Mes == "Julio"] <- "01-07"
DATA3$Mes[DATA3$Mes == "Agosto"] <- "01-08"
DATA3$Mes[DATA3$Mes == "Septiembre"] <- "01-09"
DATA3$Mes[DATA3$Mes == "Octubre"] <- "01-10"
DATA3$Mes[DATA3$Mes == "Noviembre"] <- "01-11"
DATA3$Mes[DATA3$Mes == "Diciembre"] <- "01-12"

colnames(DATA3)[colnames(DATA3) == "Año"] <- "Anio"

a <- DATA3 %>% unite(fecha, Mes, Anio, sep = "-") 
a$fecha <- as.Date(a$fecha, format = "%d-%m-%Y")

DATA_AT <- Dataset1_DatosConsumoAlimentarioMAPAporCCAA %>% 
    filter(CCAA == "Total Nacional") %>% 
    select(c(1:6))%>% 
    unique()

DATA_AT2 <- Dataset2_Precios_Semanales_Observatorio_de_Precios_Junta_de_Andalucia %>% 
    select(SECTOR, PRODUCTO) %>% 
    unique()
names(DATA_AT)[4] <- "PRODUCTO"

DATA_AT3 <- Dataset3a_Datos_MercaMadrid %>% 
    select(product, familia) %>% 
    unique()
names(DATA_AT3)[1] <- "PRODUCTO"


DATA_AT4 <- Dataset3b_Datos_MercaBarna  %>% 
    select(product, familia) %>% 
    unique()  %>% 
    separate(familia, into = c("FAMILIA", "TIPO"))

names(DATA_AT4)[1] <- "PRODUCTO"


UNION <- left_join(DATA_AT,DATA_AT2)
UNION2 <- left_join(UNION,DATA_AT3)
UNION3 <- left_join(UNION2,DATA_AT4)

UNION3$SECTOR[UNION3$SECTOR == "Frutales"] <- "FRUTAS"
UNION3$SECTOR[UNION3$SECTOR == "Hortalizas"] <- "HORTALIZAS"


SEPARACION <- UNION3 %>% unite(SECTOR, familia, FAMILIA) %>% 
    separate(SECTOR, c("famila", "cd"), sep ="_")
colnames(SEPARACION)[colnames(SEPARACION) == "Año"] <- "Anio"

SEPARACION$famila[SEPARACION$famila == "NA"] <- NA 
SEPARACION$cd[SEPARACION$cd == "NA"] <- NA 

DATA.FRUTAS.V <- SEPARACION %>% filter(famila == "FRUTAS") #%>% select(Mes, Año, 'Volumen (miles de kg)') 
DATA.HORTALIZAS.V <- SEPARACION %>% filter(famila == "HORTALIZAS") #%>% select(Mes, Año, 'Volumen (miles de kg)') 

# Datos para hacer un select en la preguna 3

Dataset4 <- Dataset4_Comercio_Exterior_de_Espania

Dataset4$Value[Dataset4$Value == ":"] <- "0"
Dataset4 <- Dataset4 %>% mutate(Value = as.numeric(Value))
Dataset4 <- Dataset4 %>% filter(PERIOD != "Jan.-Dec. 2019",PERIOD != "Jan.-Dec. 2018")
Dataset4 <- Dataset4 %>% separate(PERIOD,c('Month','Year'))

Dataset4 <- Dataset4 %>% filter(Year == 2020)

Dataset4 <- Dataset4 %>% filter(INDICATORS == "QUANTITY_IN_100KG", Value != 0) %>% group_by(FLOW, Year,Month,REPORTER) %>% summarise(VALOR_MES = sum(Value))

Dataset4$Month[Dataset4$Month == "Jan"] <- "1"
Dataset4$Month[Dataset4$Month == "Feb"] <- "2"
Dataset4$Month[Dataset4$Month == "Mar"] <- "3"
Dataset4$Month[Dataset4$Month == "Apr"] <-"4"
Dataset4$Month[Dataset4$Month == "May"] <- "5"
Dataset4$Month[Dataset4$Month == "Jun"] <- "6"
Dataset4$Month[Dataset4$Month == "Jul"] <- "7"
Dataset4$Month[Dataset4$Month == "Aug"] <- "8"
Dataset4$Month[Dataset4$Month == "Sep"] <- "9"
Dataset4$Month[Dataset4$Month == "Oct"] <- "10"
Dataset4$Month[Dataset4$Month == "Nov"] <- "11"
Dataset4$Month[Dataset4$Month == "Dec"] <- "12"

Dataset4$REPORTER[Dataset4$REPORTER == "Belgium (incl. Luxembourg 'LU' -> 1998)"] <- "Belgium"
Dataset4$REPORTER[Dataset4$REPORTER == "France (incl. Saint Barthélemy 'BL' -> 2012| incl. French Guiana 'GF', Guadeloupe 'GP', Martinique 'MQ', Réunion 'RE' from 1997| incl. Mayotte 'YT' from 2014)"] <- "France"
Dataset4$REPORTER[Dataset4$REPORTER == "Germany (incl. German Democratic Republic 'DD' from 1991)"] <- "Germany"
Dataset4$REPORTER[Dataset4$REPORTER == "Ireland (Eire)"] <- "Ireland"

Dataset4$REPORTER[Dataset4$REPORTER == "Italy (incl. San Marino 'SM' -> 1993)"] <- "Italy"
Dataset4$REPORTER[Dataset4$REPORTER == "European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)"] <- "UnionEuropea"

Data <- Dataset5_Coronavirus_cases
Data$`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000` <- str_replace(Data$`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`, ",", ".")

Data$`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000` = as.numeric(Data$`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000` )

totales <- Data %>% group_by(year,month,countriesAndTerritories) %>% 
    mutate(totalcovid = sum(`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000` ,na.rm = T))

totales <- totales %>% group_by(year,month,countriesAndTerritories)%>% mutate(totalcases = sum(cases,na.rm = T))
totales <-totales %>% group_by(year,month,countriesAndTerritories)%>% mutate(totaldeath = sum(deaths,na.rm = T))

totales <- totales[,c(-1,-2,-5,-6,-12)]
totales <- unique(totales)

D4y5 <- unique(Dataset4$REPORTER)

# Interfaz de usuario de la página web
shinyUI(
    # Creamos un logo, le damos un tema a la página web, estilo, etc.
    navbarPage(title = img(src="HR.LOGOred3_cropped.png", height = "40px"), id = "navBar",
               theme = "paper.css",
               position = "fixed-top",
               header = tags$style(
                       ".navbar-right {
                       float: right !important;
                       }",
                       "body {padding-top: 70px;}"),
                   
                   # Panel inicial 
                   tabPanel("INICIO", value = "home",
                            shinyjs::useShinyjs(),
                            tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                            fluidRow(HTML("
                                     <section class='banner'>
                                     <h2 class='parallax'>DATATHON</h2>
                                     <p class='parallax_description'>UNIVERSITYHACK 2021</p>
                                     </section>")),
                
                            # Qué encontrarás aquí
                            fluidRow(column(3),column(6,
                                       shiny::HTML("<br><br><center><h1>¿Qué encontrarás aquí?</h1></center><br>"),
                                       shiny::HTML("<br><br><center><h5>Demdata es una herramienta interactiva para ayudar a explorar el análisis sobre el comportamiento
                                       del mercado español de frutas y hortalizas durante el periodo de la pandemia. Este periodo está definido entre 01/03/2020 
                                       y el 30/11/2020.</h5></center><br>")),
                                column(3)),fluidRow(style = "height:50px;"),
                        
                            # Salto de página
                            tags$hr(),
                            
                            # Como puede ayudar
                            fluidRow(column(3),column(6,
                                       shiny::HTML("<br><br><center><h1>¿Cómo puede ayudar?</h1></center><br>"),
                                       shiny::HTML("<br><br><center><h5> Realizar análisis y ampliar los conocimientos en cualquier ámbito permite realizar decisiones más precisas.
                                       DEMDATA permite analizar el comportamiento del mercado con el fin de obtener un conocimiento más preciso y acertado de las 
                                       dinámicas mercantiles.</h5></center><br>")),
                                column(3)),fluidRow(style = "height:50px;"),

                            # Salto de página
                            tags$hr(),
                            
                            # Botón de empezar
                            fluidRow(shiny::HTML("<br><br><center> <h1>¿Listos para empezar?</h1> </center><br>")),
                            fluidRow(column(3),column(6,tags$div(align = "center", tags$a("Start", 
                                    onclick="fakeClick('careerPF')", class="btn btn-primary btn-lg"))),
                                column(3)),
                            fluidRow(style = "height:25px;"),
                            
                            # Salto de página
                            tags$hr(),
                            
                            # Pie de página
                            shiny::HTML("<h5></br></h5>"),
                            shiny::HTML("<h5></br></h5>"),
                            shiny::HTML("<h5></br></h5>"),
                            shiny::HTML("<h5></br></h5>"),
                            shiny::HTML("<h5></br></h5>"),
                            shiny::HTML("<h5></br></h5>")),
               # Cerramos el panel principal 
               
               # Menu, apartado preguntas
               navbarMenu(title = "PREGUNTAS",
                          
                          # Pregunta 1
                          tabPanel("PREGUNTA 1", value = "careerPF", column(2),fluidRow(mainPanel(
                              shiny::HTML("<br><br><center><h3><b>¿De qué manera se ha visto afectado el consumo y la demanda de F&H durante la 
                                          pandemia con respecto a años anteriores?</b></h3></center><br>"),
                              shiny::HTML("<h5>El análisis del consumo y la demanda nos permite identificar las oportunidades que hay en el mercado, 
                                          entendiendo así la demanda de los consumidores hacia los productos. Existen algunos aspectos importantes 
                                          para la realización de un análisis de la demanda. Estas variables son el precio, el precio de otros 
                                          productos y los costes de producción:</b></h5>"),
                              
                              # Variable precio 
                              div(class="panel-body",  width = "600px",align = "center",div(tags$img(src = "precio.png",  width = "899px", height = "30px"))),
                              shiny::HTML("<h5>A medida que aumenta el precio de un producto, la oferta se incrementa, pues los productores están 
                                          dispuestos a fabricar más productos por su alta rentabilidad.</b></h5>"),
                             
                              # Gráficas del precio medio de frutas y hortalizas
                              mainPanel(fluidRow(column(width = 12,
                                                        plotOutput("FrutaMediaPrecio", height = 250),
                                                        # Salto de página
                                                        tags$hr(),
                                                        plotOutput("HortalizasMediaPrecio", height = 220)))),
                              shiny::HTML("<h5><justify>En el año 2018 el valor medio de las frutas mensualmente fue de 2.505.162.000 €, mientras que en el año 2019 el valor medio disminuyó a 2.493.797.000 €. Por último, cabe destacar que en el año 2020 el valor medio aumentó 20% respecto al 2019 y 19.5% respecto al 2018, llegando al valor de 2.996.151.000 €.</justify></h5>"),
                              shiny::HTML("<h5></br></h5>"),
                              # Salto de página
                              tags$hr(),
                              shiny::HTML("<h5>En el año 2018 el valor medio de las hortalizas mensualmente fue de 1.819.999.000 €, mientras que en el año 2019 el valor medio disminuyó a 1.687.704.000 € Por último, cabe destacar que en el año 2020 el valor medio aumentó 56.2% respecto al 2019 y 44.8% respecto al 2018, llegando al valor de 2.636.674.000 €.</h5>"),
                              # Salto de página
                              tags$hr(),
                              # Conclusión 
                              div(class="panel-body",align = "center",  width = "600px",div(tags$img(src = "conclusion.png",  width = "150px", height = "80px"))),
                              shiny::HTML("<h5>Basándonos en los resultados arrojados, es evidente que, en el año 2020 se aumenta el precio del producto, es decir, que la oferta se incrementa. Por lo que a la gráfica de las hortalizas respecta, podemos apreciar también un aumento del precio, aumentando así la oferta en las hortalizas. En este caso, el aumento comienza en el mes de junio, incrementando su valor hasta el mes de noviembre. </br></br>Podemos decir que en el ámbito de la demanda las consecuencias han sido positivas, con fuertes crecimientos del consumo en los hogares, tanto en las frutas como en las hortalizas.</h5>"),
                                
                              
                              tabsetPanel(
                                tabPanel("Frutas", sidebarLayout(sidebarPanel(
                                  
                                  
                                  # Seleccionar una fruta
                                  selectInput(inputId = "Fruta",label = "Selecciona una fruta ",
                                              choices = c(DATA.FRUTAS.V$PRODUCTO), selected = 1, multiple = FALSE)#,
                                  # Seleccionamos un rango 
                                  #  sliderInput(inputId = "Mes",label = "Rango ",choices = c(a$fecha),multiple = TRUE),
                                ),
                                
                                # Primeras gráficas de frutas
                                mainPanel(fluidRow(column(width = 12,
                                                          plotOutput("Fruta1", height = 200),
                                                          plotOutput("Fruta2", height = 200)))))),
                                tabPanel("Hortalizas", sidebarLayout(sidebarPanel(
                                  
                                  # Seleccionar una hortaliza
                                  selectInput(inputId = "Hortaliza",label = "Selecciona una hortaliza", 
                                              choices = c(DATA.HORTALIZAS.V$PRODUCTO),selected = 1,multiple = FALSE)
                                  # Seleccionamos un rango 
                                  #  sliderInput(inputId = "Mes",label = "Rango ",choices = c(a$fecha),multiple = TRUE),
                                ),
                                
                                # Primeras gráficas de hortalizas
                                mainPanel(fluidRow(column(width = 12,
                                                          plotOutput("Hortaliza1", height = 200),
                                                          plotOutput("Hortaliza2", height = 200))))))),
              
                              # Variable serie temporal
                              div(class="panel-body",  width = "600px",
                                  align = "center",
                                  div(tags$img(src = "serie.png", 
                                               width = "899px", height = "30px"))),
                              
                              shiny::HTML("<h5>Una serie temporal nos permite estudiar la relación entre diferentes variables que cambian con el tiempo y se influyen entre sí. Nos permite predecir valores futuros que tienen relación con los pasados.</b></h5>"),
                              shiny::HTML("<h5>Cabe destacar que se ha realizado una predicción de los valores para el 2020, sin usar los valores reales para dicho año. De esta manera se busca comprobar si el mercado de frutas y hortalizas siguió en 2020 la tendencia esperable, o fue afectado por la pandemia del COVID-19</h5>"),
                              # Gráficas series temporales
                              div(class="panel-body",  width = "600px",
                                  align = "center",
                                  div(tags$img(src = "series.png", 
                                               width = "900px", height = "550px"))),
                              
                              shiny::HTML("<h5>Como podemos observar con la línea roja, la predicción del valor medio del precio de hortalizas y frutas era similar a los años anteriores. Mientras que el valor es real que hemos obtenido es mucho mayor a la predicción. Por esta razón, pensamos que el COVID-19 sí que ha influenciado tanto en el consumo de frutas y hortalizas como en su precio medio mensual.</b></h5>"),
                              
                              # Salto de página
                              tags$hr(),
                              
                              # Botón de empezamos
                              fluidRow(shiny::HTML("<br><br><center> <h1>¿Seguimos?</h1> </center><br>")),
                              fluidRow(column(3),column(6,tags$div(
                                align = "center", tags$a("Siguiente", onclick="fakeClick('careerPF2')", class="btn btn-primary btn-lg"))),
                                       column(3)),
                              fluidRow(style = "height:25px;"),
                              
                              # Salto de página
                              tags$hr(),
                              
                              # Pie de página
                              shiny::HTML("<h5></br></h5>"),
                              shiny::HTML("<h5></br></h5>"),
                              shiny::HTML("<h5></br></h5>"),
                              shiny::HTML("<h5></br></h5>"),
                              
                              # Salto de página
                              tags$hr(),
                              
                              ))),
                          
                          # Pregunta 2
                          tabPanel("PREGUNTA 2", value = "careerPF2",column(2),fluidRow(mainPanel(
                            shiny::HTML("<br><br><center><h3><b>Qué efecto ha tenido sobre las importaciones/exportaciones de F&H?¿Ha tenido algún efecto especial el periodo de excepción (Marzo, abril y mayo)?</b></h3></center><br>"),
                            shiny::HTML("<h5> Se ha analizado el efecto que ha tenido el COVID-19 sobre las importaciones/ exportaciones de F&H a nivel nacional ya que a nivel Europeo se comenta en el siguiente apartado de pregunta 3.</h5>"),                              
                            
                            fluidRow(column(width = 12,plotOutput("impExp", height = 400))),
                            
                            shiny::HTML("<h5>El valor de las importaciones y exportaciones ha disminuido considerablemente en el año 2020. Disminuyendo 8.43% en exportaciones y 12.51% en las importaciones en comparación con el año 2019.</h5>"),                              
                            
                            # Linea Mapa casos covid
                            div(class="panel-body",  width = "600px",
                                align = "center",
                                div(tags$img(src = "casos.png",
                                             width = "899px", height = "55px"))),
                            
                            shiny::HTML("<h5>La primera ola, el estado de alarma y las medidas de confinamiento entraron en vigor el 15 de marzo de 2020, oficialmente llegamos a estar confinados en nuestros hogares 99 días salvo para realizar actividades permitidas y hasta que el 21 de junio de 2020 ya se inició la nueva normalidad.</h5>"),                              

                            # Mapa de España
                            sidebarLayout(sidebarPanel(
                              # Seleccionar una fecha
                              dateInput(inputId = "CCAA",label = "Selecciona una fecha ", value = "2020-03-01",
                                        format = "yyyy-mm-dd", min = "2020-01-01", max = "2020-12-31")),
                                       # Mapa
                                       mainPanel(fluidRow(column(width = 12,leafletOutput("MapaCCAA", height = 400)))),
                              ),
                            
                            # Efecto
                            div(class="panel-body",  width = "600px",
                                align = "center",
                                div(tags$img(src = "efecto.png",
                                             width = "899px", height = "55px"))),
                            
                            tabsetPanel(
                              # Importaciones
                              tabPanel("Importaciones",shiny::HTML("<h5></br></h5>"),
                                       # Primeras gráficas de importaciones
                                       mainPanel(fluidRow(column(width = 12,plotOutput("importEspana", height = 200))))),
                              
                              # Exportaciones
                              tabPanel("Exportaciones",shiny::HTML("<h5></br></h5>"),
                                       # Primeras gráficas de exportaciones
                                       mainPanel(fluidRow(column(width = 12,plotOutput("exportEspana", height = 200)))))),
                            
                            shiny::HTML("<h5>Se trata de un gráfico de barras que refleja la evolución de la importaciones y exportaciones a lo largo del año 2020. Hace referencia este año para ver lo efecto que ha tenido el COVID-19 en este sector, donde se denota un comportamiento muy diferenciado.</h5>"),                              
                            shiny::HTML("<h5></br></h5>"),
                            shiny::HTML("<h5>En marzo, abril y mayo  podemos contemplar que comienza el descenso en la exportaciones e importaciones, teniendo un gran impacto a lo largo de los siguientes meses.</h5>"),                              
                            
                            # Salto de página
                            tags$hr(),
                            
                            # Botón de empezamos
                            fluidRow(shiny::HTML("<br><br><center> <h1>¿Todavía quieres más?</h1> </center><br>")),
                            fluidRow(column(3),column(6,tags$div(
                              align = "center", tags$a("Siguiente", onclick="fakeClick('careerPF3')", class="btn btn-primary btn-lg"))),
                                                column(3)),
                                       fluidRow(style = "height:25px;"),
                            # Salto de página
                            tags$hr(),
                            
                            # Pie de página
                            shiny::HTML("<h5></br></h5>"),
                            shiny::HTML("<h5></br></h5>"),
                            shiny::HTML("<h5></br></h5>"),
                            shiny::HTML("<h5></br></h5>"),
                            
                            # Salto de página
                            tags$hr()))),
                          
                          
                          # Pregunta 3
                          tabPanel("PREGUNTA 3", value = "careerPF3", column(2),fluidRow(mainPanel(
                              shiny::HTML("<br><br><center><h3><b>¿Existe correlación entre los casos COVID-19 y las importaciones/exportaciones a nivel de la Unión Europea?</b></h3></center><br>"),
                              
                              # Calculo de la correlación
                              div(class="panel-body",  width = "600px",
                                  align = "center",
                                  div(tags$img(src = "correlacion.png", 
                                               width = "899px", height = "30px"))),
                              
                              shiny::HTML("<h5>Vamos a estudiar si existe correlación entre estas dos variables. Para comenzar a analizar la correlación se ha realizado un diagrama de dispersión entre las variables casos COVID-19 y las importaciones/exportaciones a nivel de la Unión Europea.</b></h5>"),
                              shiny::HTML("<h5></b></h5>"),
                              shiny::HTML("<h5></b></h5>"),
                              shiny::HTML("<h5>Dos variables están correlacionadas cuando una variable nos da información acerca de la otra. Por el contrario, cuando no existe asociación, el aumento o disminución de una variable no nos dice nada sobre el comportamiento de la otra variable.</b></h5>"),
                              
                              # Interpretación de la correlación
                              div(class="panel-body",  width = "600px",
                                  align = "center",
                                  div(tags$img(src = "interpretamos.png", 
                                               width = "899px", height = "50px"))),
                              shiny::HTML("<h5>En los diagramas de dispersión podemos identificar el signo, este nos indica la dirección de la relación.</b></h5>"),
                              
                              # Primer tabPanel de importaciones y exportaciones
                              tabsetPanel(
                                  # Imnportaciones
                                  tabPanel("Importaciones",shiny::HTML("<h5></br></h5>"),
                                  # Primeras gráficas de importaciones
                                  mainPanel(fluidRow(column(width = 12,plotOutput("import", height = 200))))),
                                  
                                  # Exportaciones
                                  tabPanel("Exportaciones",shiny::HTML("<h5></br></h5>"),
                                  # Primeras gráficas de exportaciones
                                  mainPanel(fluidRow(column(width = 12,plotOutput("export", height = 200)))))),
                                
                              shiny::HTML("<h5>La gráfica de <b>importaciones</b> muestra que el valor es negativo, indicandonos que se trata de una <b>relación indirecta</b>. En la gráfica de <b>exportaciones</b>, pasa lo contrario, tenemos valores positivos siendo esto una <b>relación directa</b>.</b></h5>"),
                              shiny::HTML("<h5></br></h5>"),
                              
                              # Medimos la correlación
                              div(class="panel-body",  width = "600px",
                                  align = "center",
                                  div(tags$img(src = "medimos.png", 
                                               width = "899px", height = "40px"))),
                              
                              shiny::HTML("<h5>También tenemos que tener en cuenta la magnitud, ya que esta nos indica la fuerza de la relación. Cuanto más cercano sea el valor a 1/-1, esto nos indicará que tiene una fuerte tendencia de las variables. Por el contrario, si el valor se acerca a 0 diremos que las variables no están correlacionadas.</h5>"),
                              
                              # Tabla magnitudes
                              div(class="panel-body",  width = "600px",
                                  align = "center",
                                  div(tags$img(src = "tabla.png", 
                                               width = "400px", height = "85px"))),
                              shiny::HTML("<h5>Para evaluar este coeficiente se ha utilizado el criterio de Cohen, en el cual los valores obtenidos nos indican que las variables exportaciones e importaciones presentan un efecto grande sobre los casos COVID-19. </h5>"),
                              
                              # Interpretación visual
                              div(class="panel-body",  width = "600px",
                                  align = "center",
                                  div(tags$img(src = "interpretacion.png", 
                                               width = "899px", height = "30px"))),
                              shiny::HTML("<h5> Una vez sabemos que sí que existe una correlación entre los casos COVID-19 y las importaciones/exportaciones a nivel de la unión Europea, podemos pasar a visualizar estos datos para ver como ha influido.</h5>"),
                              
                              tabsetPanel(
                                tabPanel("Importaciones",shiny::HTML("<h5></br></h5>"), sidebarLayout(sidebarPanel(
                                  #Seleccionar un país
                                  selectInput(inputId = "Pais", label = "Selecciona un país", choices = c(D4y5),   multiple = FALSE, selected = "UnionEuropea") 
                                ),
                                # Primeras gráficas de importaciones
                                mainPanel(fluidRow(column(width = 12,plotOutput("importCovid", height = 200)))))),
                                
                                # Exportaciones
                                tabPanel("Exportaciones",shiny::HTML("<h5></br></h5>"), sidebarLayout(sidebarPanel(
                                  #Seleccionar un país
                                  selectInput(inputId = "Pais2", label = "Selecciona un país", choices = c(D4y5),   multiple = FALSE, selected = "UnionEuropea") 
                                ),
                                # Primeras gráficas de importaciones
                                mainPanel(fluidRow(column(width = 12,plotOutput("exportCovid", height = 200))))))),
                              
                              shiny::HTML("<h5>Como podemos ver, en casi todos los paises, las importaciones crecieron en enero, febrero y marzo. 
                                          Cuando llegó la pandemia, empezó un fuerte decrecimiento durando hasta los meses de agosto y septiembre. 
                                          Obteniendo estos meses el menor número de importaciones en cada uno de los paises. 
                                          El aumento de casos de COVID-19, conllevó un decrecimiento en las importaciones. A partir del mes de septiembre, 
                                          se fueron retomando el número de importaciones positivamente, retomando de nuevo la media habitual.</b></h5>"),
                              
                              shiny::HTML("<h5>Hay que tener en cuenta que tenemos menor número de exportaciones en comparación con las importaciones.
                                          Podemos apreciar que en los meses de mayo y junio, en general hubo un menor movimiento, pero, a partir de 
                                          junio volvieron a crecer considerablemente las exportaciones, como por ejemplo, en el caso de Francia.</br></h5>"),
                              
                              shiny::HTML("<h5>Cabe destacar que Bélgica tiene muy pocas exportaciones en todo el año pero podemos ver que en el mes de julio aumenta notablemente.</br></h5>"),
                             
                              # # Linea Mapa casos covid
                              # div(class="panel-body",  width = "600px",
                              #     align = "center",
                              #     div(tags$img(src = "casos.png", 
                              #                  width = "899px", height = "55px"))),
                              #  
                              # 
                              # fluidRow(mainPanel(
                              #   
                              #   # Mapa de España
                              #   sidebarLayout(sidebarPanel(
                              #     # Seleccionar una fecha
                              #     dateInput(inputId = "EU",label = "Selecciona una fecha ", value = "2020-03-01",
                              #               format = "yyyy-mm-dd", min = "2020-01-01", max = "2020-12-31")),
                              #     # Mapa
                              #     mainPanel(fluidRow(column(width = 12,leafletOutput("mapaEU", height = 400, width = 650))))))),
                              # 
                              
                              # Salto de página
                              tags$hr(),
                              
                              # Pie de página
                              shiny::HTML("<h5></br></h5>"),
                              shiny::HTML("<h5></br></h5>"),
                              shiny::HTML("<h5></br></h5>"),
                              shiny::HTML("<h5></br></h5>"),
                              
                              # Salto de página
                              tags$hr())))),
               # Cerramos Menu, apartado preguntas
               
               # Información adicional
               tabPanel("INFORMACIÓN", value = "about",fluidRow(
               shiny::HTML("<br><br><center><h1>Acerca de DEMDATA</h1><h4>¿Qué hay detrás de los datos?</h4></center><br><br>"),style = "height:250px;"),
               
               fluidRow(column(2),column(8,
                                       # Panel sobre los datos 
                                       div(class="panel panel-default",
                                           div(class="panel-body",  
                                               tags$div( align = "center",
                                                         icon("bar-chart", class = "fa-4x"),
                                                         div( align = "center", h5("Los dataset"))),
                                               
                                               tags$p(h6("Se pusieron a nuestra disposición cinco datasets con información del mercado de frutas y hortalizas en España así como con información internacional de la evolución del COVID-19. Los ficheros contienen información desde Enero de 201 hasta 2020 para que puedas observar y comparar el comportamiento del mercado español de frutas y hortalizas de 2020.")),
                                               
                                               # Información sobre los dataset
                                               tags$ul(
                                                   
                                                   # Dataset 1
                                                   tags$li(shiny::HTML("<justify> <h6> El <b>primer dataset</b> incluye información mensual sobre el consumo alimentario en España, información proveniente del 
                                                              Ministerio de Agricultura, Pesca y Alimentación. Está basado en encuestas efectuadas entre los consumidores y los 
                                                              responsables de compras de dichos establecimientos. Se presentan datos mensuales de consumo de frutas y hortalizas.</h6> </justify>")),
                                                   
                                                   # Dataset 2
                                                   tags$li(shiny::HTML("<justify> <h6>El <b>segundo dataset</b> presenta los precios semanales de productos agroalimentarios que publica el Observatorio de 
                                                   Precios de los Alimentos de la Junta de Andalucía.</h6> </justify>")),
                                                   
                                                   shiny::HTML("<justify> <h6>Gracias a este Observatorio de Precios, se crea un marco de conocimiento sobre la formación de los precios a lo largo de 
                                                   la cadena agroalimentaria, mediante la publicación de datos objetivos y la 
                                                   realización de estudios e informes que permiten llevar a cabo un seguimiento sistemático de los precios. De esta forma, se 
                                                   pretende favorecer la transparencia y la eficiencia del proceso de comercialización, detectando posibles situaciones de 
                                                   desequilibrio en las cotizaciones de productos a lo largo de la cadena de comercialización. Contiene los precios medios 
                                                   semanales tanto en origen (agricultor) como en mercas y subastas de diferentes frutas y hortalizas. Incluye las siguientes 
                                                   variables para cada semana del año y producto</h6> </justify>"),
                                                   
                                                   # Dataset 3
                                                   tags$li(shiny::HTML("<justify> <h6>El <b>tercer dataset</b> ofrece información sobre los precios y kilogramos comercializados semanalmente de diferentes 
                                                              frutas y hortalizas en sus instalaciones.")),
                                                   
                                                   # Dataset 4
                                                   tags$li(shiny::HTML("<justify> <h6>El <b>cuarto dataset</b> contiene la serie histórica mensual de comercio exterior de España con el resto de países de 
                                                              Europa desde 2018 de diferentes frutas y hortalizas.")),
                                                   
                                                   # Dataset 5
                                                   tags$li(shiny::HTML("<justify> <h6>El <b>quinto dataset</b> supone estadísticas internacionales diarias de COVID-19 por país, desde enero de 2020 hasta noviembre de 2020.")))))), 
                        column(2)),
               fluidRow(column(2),column(8,
                                         # Panel sobre los datos 
                                         div(class="panel panel-default",
                                             div(class="panel-body",  
                                                 tags$div( align = "center",
                                                           icon("bar-chart", class = "fa-4x"),
                                                           div( align = "center", h5("Enriquecimiento de datos"))),
                                                 
                                                 tags$p(h6("Con la finalidad de enriquecer nuestros datos para aportar valor a nuestro análisis hemos añadido los siguientes datos:")),
                                                 
                                                 # Información sobre los dataset
                                                 tags$ul(
                                                   
                                                   # Dataset 1
                                                   tags$li(shiny::HTML("<justify> <h6> El <b>primer dataset</b> nos aporta los datos sobre el COVID-19 en España por Comunidades Autónomas. Lo hemos utilizado para realizar un mapa con los casos de COVID-19 por CCAA según la fecha introducida. </h6> </justify>")),
                                                  )))), 
                        column(2)),

               #Sobre el equipo
               fluidRow(column(3),column(6,
                                         shiny::HTML("<br><br><center> <h5>Sobre el equipo</h5> </center><br>"),
                                         shiny::HTML("<h6><center>Somos tres estudiantes de Ciencia de datos de la Universidad de Valencia. Muy interesados en el análisis de datos y en una buena visualización de los mismos. La originalidad, la innovación y la comunicación son algunas de las herramientas que nos identifican.</center></h6>")),
                        column(3)),fluidRow(style = "height:50px;"),
               
               fluidRow(column(3),
                        # Marina
                        column(2,div(class="panel panel-default", 
                                     # Foto Marina
                                     div(class="panel-body",  width = "600px",align = "center",
                                         div(tags$img(src = "woman.svg",  width = "50px", height = "50px")),
                                         # Estudios Marina
                                         div(tags$h5("Marina"),tags$h6( tags$i("Estudiante de Ciencia de datos"))),
                                         # Datos Marina
                                         div(" ")))),
                        # David
                        column(2, div(class="panel panel-default",
                                      # Foto David
                                      div(class="panel-body",  width = "600px", align = "center",
                                          div(tags$img(src = "man.svg", width = "50px", height = "50px")),
                                          # Estudios David
                                          div(tags$h5("David"),tags$h6( tags$i("Estudiante de Ciencia de datos"))),
                                          div(" ")))),
                        # Elena
                        column(2,div(class="panel panel-default",
                                     # Foto Elena
                                     div(class="panel-body",  width = "600px", align = "center",
                                         div(tags$img(src = "woman.svg", width = "50px", height = "50px")),
                                         # Estudios Elena
                                         div(tags$h5("Elena"), tags$h6( tags$i("Estudiante de Ciencia de datos"))),
                                         div(" ")))),
                        column(3),
                        
                        shiny::HTML("<h5></br></h5>"),
                        shiny::HTML("<h5></br></h5>"),
                        shiny::HTML("<h5></br></h5>"),
                        shiny::HTML("<h5></br></h5>"),
                        shiny::HTML("<h5></br></h5>"),
                        shiny::HTML("<h5></br></h5>"),
                        
                        # Salto de página
                        tags$hr(),
                          # Dataset 1
                          column(4,
                                 shiny::HTML("<br><center> <h5>Para una información más detallada mirar </h5> </center>"),
                                 a("pdf",target="_blank",href="DEMDATA.pdf",align = "center")),
               
                        ),fluidRow(style = "height:150px;")))
    # Cerramos Información adicional 
    )
# Cerramos interfaz de usuario 
