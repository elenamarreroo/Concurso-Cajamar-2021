

# Servidor de la página web
shinyServer(function(input, output, session) {
    
    # Menu  --------------------------------------------------------------------
    shinyjs::addClass(id = "navBar", class = "navbar-right")
    
    # Importación de los datos -------------------------------------------------

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
    CASOS_CCAA <- read_csv("casos_diagnostico_ccaa.csv",col_types = cols(fecha = col_date(format = "%Y-%m-%d")))
    # Dataset 7
    c <- c <- read_csv("c.csv")
    c <- c[,-1]
    
    # Limpieza de los datos ----------------------------------------------------
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
    
    DATA_AT <- Dataset1_DatosConsumoAlimentarioMAPAporCCAA %>% filter(CCAA == "Total Nacional") %>% select(c(1:6)) %>% unique()
    
    DATA_AT2 <- Dataset2_Precios_Semanales_Observatorio_de_Precios_Junta_de_Andalucia %>% select(SECTOR, PRODUCTO) %>% unique()
    names(DATA_AT)[4] <- "PRODUCTO"
    
    DATA_AT3 <- Dataset3a_Datos_MercaMadrid %>% select(product, familia) %>% unique()
    names(DATA_AT3)[1] <- "PRODUCTO"
    
    DATA_AT4 <- Dataset3b_Datos_MercaBarna  %>% select(product, familia) %>% unique()  %>% separate(familia, into = c("FAMILIA", "TIPO"))
    names(DATA_AT4)[1] <- "PRODUCTO"
    
    UNION <- left_join(DATA_AT,DATA_AT2)
    UNION2 <- left_join(UNION,DATA_AT3)
    UNION3 <- left_join(UNION2,DATA_AT4)
    
    UNION3$SECTOR[UNION3$SECTOR == "Frutales"] <- "FRUTAS"
    UNION3$SECTOR[UNION3$SECTOR == "Hortalizas"] <- "HORTALIZAS"
    
    SEPARACION <- UNION3 %>% unite(SECTOR, familia, FAMILIA) %>%  separate(SECTOR, c("famila", "cd"), sep ="_")
    
    SEPARACION$famila[SEPARACION$famila == "NA"] <- NA 
    SEPARACION$cd[SEPARACION$cd == "NA"] <- NA 
    
    colnames(SEPARACION)[colnames(SEPARACION) == "Año"] <- "Anio"
    
    DATA.FRUTAS.V <- SEPARACION %>% filter(famila == "FRUTAS") #%>% select(Mes, Anio, 'Volumen (miles de kg)') 
    
    DATA.HORTALIZAS.V <- SEPARACION %>% filter(famila == "HORTALIZAS") #%>% select(Mes, Anio, 'Volumen (miles de kg)')
    
    CASOS_CCAA <- read_csv("casos_diagnostico_ccaa.csv",col_types = cols(fecha = col_date(format = "%Y-%m-%d")))
    
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "AN"] <- "Andalucía"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "AR"] <- "Aragón"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "AS"] <- "Principado de Asturias"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CN"] <- "Canarias"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CB"] <- "Cantabria"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CM"] <- "Castilla-La Mancha"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CL"] <- "Castilla y León"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CT"] <- "Cataluña"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "EX"] <- "Extremadura"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "GA"] <- "Galicia"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "IB"] <- "Illes Balears"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "RI"] <- "La Rioja"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "MD"] <- "Comunidad de Madrid"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "MC"] <- "Región de Murcia"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "NC"] <- "Comunidad Foral de Navarra"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "PV"] <- "País Vasco"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "VC"] <- "Comunidad Valenciana"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CE"] <- "Ceuta"
    CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "ML"] <- "Melilla"
    
    CASOS_CCAA <- CASOS_CCAA %>%  filter(year(fecha) == 2020) %>% group_by(ccaa_iso, month(fecha)) %>% mutate(N_CASOS = sum(num_casos))
    
    # Pregunta 1 ---------------------------------------------------------------
    output$FrutaMediaPrecio <- renderPlot({
        
        DATA.HORTALIZA.P <- SEPARACION %>% filter(famila == "FRUTAS") %>% select(Mes, Anio, 'Valor (miles de €)') 
        
        colnames(DATA.HORTALIZA.P)[colnames(DATA.HORTALIZA.P) == "Valor (miles de €)"] <- "Valor"

        DATA.HORTALIZA.P$Valor <- as.numeric(DATA.HORTALIZA.P$Valor)
        
        DATA.HORTALIZA.P <- DATA.HORTALIZA.P %>% group_by(Mes, Anio) %>% mutate(Valor = mean(Valor)) %>% unique()

        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Enero"] <- "01-01"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Febrero"] <- "01-02"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Marzo"] <- "01-03"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Abril"] <-"01-04"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Mayo"] <- "01-05"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Junio"] <- "01-06"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Julio"] <- "01-07"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Agosto"] <- "01-08"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Septiembre"] <- "01-09"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Octubre"] <- "01-10"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Noviembre"] <- "01-11"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Diciembre"] <- "01-12"
        DATA.HORTALIZA.P$Mes
        
        a <- DATA.HORTALIZA.P %>% unite(fecha, Mes, Anio, sep = "-")

        a$fecha <- as.Date(a$fecha, format = "%d-%m-%Y")
        
        ggplot(a, aes(month(fecha), Valor)) + 
            geom_line(aes(col = as.factor(year(fecha)))) +
            geom_point(aes(col = as.factor(year(fecha)))) +
            scale_x_continuous(n.breaks = 12) + 
            scale_color_brewer("Anio", palette = "Paired") +
            labs(title = "Precio medio del kg de frutas mensualmente", y = "Precio medio(miles de €)", x = "Mes") +
            theme_minimal()
    })
    
    output$HortalizasMediaPrecio <- renderPlot({
        
        DATA.HORTALIZA.P <- SEPARACION %>% filter(famila == "HORTALIZAS") %>% select(Mes, Anio, 'Valor (miles de €)') 
        
        colnames(DATA.HORTALIZA.P)[colnames(DATA.HORTALIZA.P) == "Valor (miles de €)"] <- "Valor"

        DATA.HORTALIZA.P$Valor <- as.numeric(DATA.HORTALIZA.P$Valor)
        
        DATA.HORTALIZA.P <- DATA.HORTALIZA.P %>% group_by(Mes, Anio) %>% mutate(Valor = mean(Valor)) %>% unique()

        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Enero"] <- "01-01"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Febrero"] <- "01-02"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Marzo"] <- "01-03"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Abril"] <-"01-04"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Mayo"] <- "01-05"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Junio"] <- "01-06"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Julio"] <- "01-07"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Agosto"] <- "01-08"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Septiembre"] <- "01-09"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Octubre"] <- "01-10"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Noviembre"] <- "01-11"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Diciembre"] <- "01-12"
        DATA.HORTALIZA.P$Mes
        
        a <- DATA.HORTALIZA.P %>% unite(fecha, Mes, Anio, sep = "-")

        a$fecha <- as.Date(a$fecha, format = "%d-%m-%Y")
        
        ggplot(a, aes(month(fecha), Valor/1000)) + 
            geom_line(aes(col = as.factor(year(fecha)))) +
            geom_point(aes(col = as.factor(year(fecha)))) +
            scale_x_continuous(n.breaks = 12) + 
            scale_color_brewer("Anio", palette = "Paired") +
            labs(title = "Precio medio del kg de hortalizas mensualmente", y = "Precio medio(millones de €)", x = "Mes") +
            theme_minimal()
        
    })
    
    
    
    output$FrutaMediaPrecio1 <- renderPlot({
        
        colnames(DATA.HORTALIZA.P)[colnames(DATA.HORTALIZA.P) == "Año"] <- "Anio"
        
        DATA.HORTALIZA.P <- SEPARACION %>% filter(famila == "FRUTAS") %>% select(Mes, Anio, 'Valor (miles de €)') 
        
        colnames(DATA.HORTALIZA.P)[colnames(DATA.HORTALIZA.P) == "Valor (miles de €)"] <- "Valor"

        DATA.HORTALIZA.P$Valor <- as.numeric(DATA.HORTALIZA.P$Valor)
        
        DATA.HORTALIZA.P <- DATA.HORTALIZA.P %>% group_by(Mes, Anio) %>% mutate(Valor = mean(Valor)) %>% unique()

        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Enero"] <- "01-01"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Febrero"] <- "01-02"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Marzo"] <- "01-03"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Abril"] <-"01-04"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Mayo"] <- "01-05"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Junio"] <- "01-06"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Julio"] <- "01-07"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Agosto"] <- "01-08"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Septiembre"] <- "01-09"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Octubre"] <- "01-10"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Noviembre"] <- "01-11"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Diciembre"] <- "01-12"
        DATA.HORTALIZA.P$Mes
        
        a <- DATA.HORTALIZA.P %>% unite(fecha, Mes, Anio, sep = "-")

        a$fecha <- as.Date(a$fecha, format = "%d-%m-%Y")
        
        ggplot(a, aes(month(fecha), Valor)) + 
            geom_line(aes(col = as.factor(year(fecha)))) +
            geom_point(aes(col = as.factor(year(fecha)))) +
            scale_x_continuous(n.breaks = 12) + 
            scale_color_brewer("Anio", palette = "Paired") +
            labs(title = "Precio medio del kg de frutas mensualmente", y = "Precio medio(miles de €)", x = "Mes") +
            theme_minimal()
    })

    output$Fruta1<- renderPlot({
        
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
        
        SEPARACION$famila[SEPARACION$famila == "NA"] <- NA 
        SEPARACION$cd[SEPARACION$cd == "NA"] <- NA 
        
        DATA.FRUTAS.P <- SEPARACION %>% filter(famila == "FRUTAS") #%>% select(Mes, Anio, 'Volumen (miles de kg)') 
        
        colnames(DATA.FRUTAS.P)[colnames(DATA.FRUTAS.P) == "Volumen (miles de kg)"] <- "Volumen"
        colnames(DATA.FRUTAS.P)[colnames(DATA.FRUTAS.P) == "Valor (miles de €)"] <- "Valor"
        colnames(DATA.FRUTAS.P)[colnames(DATA.FRUTAS.P) == "Año"] <- "Anio"
        
        DATA.FRUTAS.P$Volumen <- str_replace(DATA.FRUTAS.P$Valor, ",", ".")
        
        DATA.FRUTAS.P$Volumen <- as.numeric(DATA.FRUTAS.P$Volumen)
        
        DATA.FRUTAS.P <- DATA.FRUTAS.P %>% filter(PRODUCTO == input$Fruta) %>%
            group_by(Mes, Anio) %>% 
            mutate(Valor = mean(Valor), Volumen = mean(Volumen)) %>% 
            unique()
        
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Enero"] <- "01-01"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Febrero"] <- "01-02"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Marzo"] <- "01-03"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Abril"] <-"01-04"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Mayo"] <- "01-05"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Junio"] <- "01-06"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Julio"] <- "01-07"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Agosto"] <- "01-08"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Septiembre"] <- "01-09"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Octubre"] <- "01-10"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Noviembre"] <- "01-11"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Diciembre"] <- "01-12"

        a2 <- DATA.FRUTAS.P %>% unite(fecha, Mes, Anio, sep = "-") 

        a2$fecha <- as.Date(a2$fecha, format = "%d-%m-%Y")
        
        ggplot(a2, aes(month(fecha), Valor)) + 
            geom_line(aes(col = as.factor(year(fecha)))) +
            geom_point(aes(col = as.factor(year(fecha)))) +
            scale_x_continuous(n.breaks = 12) + 
            scale_color_brewer("Anio", palette = "Paired") +
            labs(title = "Precio medio del kg de frutas mensualmente", y = "Precio medio(miles de €)", x =     "Mes") +
            theme_minimal()
        
    })
    
    output$Fruta2 <- renderPlot({
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
        
        SEPARACION$famila[SEPARACION$famila == "NA"] <- NA 
        SEPARACION$cd[SEPARACION$cd == "NA"] <- NA 
        
        DATA.FRUTAS.P <- SEPARACION %>% filter(famila == "FRUTAS") #%>% select(Mes, Anio, 'Volumen (miles de kg)') 
        colnames(DATA.FRUTAS.P)[colnames(DATA.FRUTAS.P) == "Año"] <- "Anio"
        
        colnames(DATA.FRUTAS.P)[colnames(DATA.FRUTAS.P) == "Volumen (miles de kg)"] <- "Volumen"
        colnames(DATA.FRUTAS.P)[colnames(DATA.FRUTAS.P) == "Valor (miles de €)"] <- "Valor"
        
        DATA.FRUTAS.P$Volumen <- str_replace(DATA.FRUTAS.P$Volumen, ",", ".")
        
        DATA.FRUTAS.P$Volumen <- as.numeric(DATA.FRUTAS.P$Volumen)
        
        DATA.FRUTAS.P <- DATA.FRUTAS.P %>% filter(PRODUCTO == input$Fruta) %>%
            group_by(Mes, Anio) %>% 
            mutate(Valor = mean(Valor), Volumen = mean(Volumen)) %>% 
            unique()
        
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Enero"] <- "01-01"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Febrero"] <- "01-02"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Marzo"] <- "01-03"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Abril"] <-"01-04"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Mayo"] <- "01-05"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Junio"] <- "01-06"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Julio"] <- "01-07"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Agosto"] <- "01-08"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Septiembre"] <- "01-09"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Octubre"] <- "01-10"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Noviembre"] <- "01-11"
        DATA.FRUTAS.P$Mes[DATA.FRUTAS.P$Mes == "Diciembre"] <- "01-12"

        a2 <- DATA.FRUTAS.P %>% unite(fecha, Mes, Anio, sep = "-") 

        a2$fecha <- as.Date(a2$fecha, format = "%d-%m-%Y")

        ggplot(a2, aes(month(fecha), Volumen)) + 
            geom_line(aes(col = as.factor(year(fecha)))) +
            geom_point(aes(col = as.factor(year(fecha)))) +
            scale_x_continuous(n.breaks = 12) + 
            scale_color_brewer("Anio", palette = "Paired") +
            labs(title = "Volumen medio del kg de frutas mensualmente", y = "Volumen medio(miles de kg)", x =     "Mes") +
            theme_minimal()
    })

    output$HortalizasMediaPrecio1 <- renderPlot({


        DATA.HORTALIZA.P <- SEPARACION %>% filter(famila == "HORTALIZAS") %>% select(Mes, Anio, 'Valor (miles de €)') 

        colnames(DATA.HORTALIZA.P)[colnames(DATA.HORTALIZA.P) == "Valor (miles de €)"] <- "Valor"

        DATA.HORTALIZA.P$Valor <- as.numeric(DATA.HORTALIZA.P$Valor)
        
        DATA.HORTALIZA.P <- DATA.HORTALIZA.P %>% group_by(Mes, Anio) %>% mutate(Valor = mean(Valor)) %>% unique()

        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Enero"] <- "01-01"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Febrero"] <- "01-02"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Marzo"] <- "01-03"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Abril"] <-"01-04"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Mayo"] <- "01-05"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Junio"] <- "01-06"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Julio"] <- "01-07"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Agosto"] <- "01-08"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Septiembre"] <- "01-09"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Octubre"] <- "01-10"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Noviembre"] <- "01-11"
        DATA.HORTALIZA.P$Mes[DATA.HORTALIZA.P$Mes == "Diciembre"] <- "01-12"
        DATA.HORTALIZA.P$Mes
        
        a <- DATA.HORTALIZA.P %>% unite(fecha, Mes, Anio, sep = "-")
        
        a$fecha <- as.Date(a$fecha, format = "%d-%m-%Y")
        
        ggplot(a, aes(month(fecha), Valor/1000)) + 
            geom_line(aes(col = as.factor(year(fecha)))) +
            geom_point(aes(col = as.factor(year(fecha)))) +
            scale_x_continuous(n.breaks = 12) + 
            scale_color_brewer("Anio", palette = "Paired") +
            labs(title = "Precio medio del kg de hortalizas mensualmente", y = "Precio medio(millones de €)", x = "Mes") +
            theme_minimal()
        
    })
    
    output$Hortaliza1 <- renderPlot({
        
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
        
        SEPARACION$famila[SEPARACION$famila == "NA"] <- NA 
        SEPARACION$cd[SEPARACION$cd == "NA"] <- NA 
        
        DATA.HORTALIZAS.P <- SEPARACION %>% filter(famila == "HORTALIZAS") #%>% select(Mes, Anio, 'Volumen (miles de kg)') 
        
        colnames(DATA.HORTALIZAS.P)[colnames(DATA.HORTALIZAS.P) == "Año"] <- "Anio"
        colnames(DATA.HORTALIZAS.P)[colnames(DATA.HORTALIZAS.P) == "Volumen (miles de kg)"] <- "Volumen"
        colnames(DATA.HORTALIZAS.P)[colnames(DATA.HORTALIZAS.P) == "Valor (miles de €)"] <- "Valor"
        
        DATA.HORTALIZAS.P$Volumen <- str_replace(DATA.HORTALIZAS.P$Valor, ",", ".")
        
        DATA.HORTALIZAS.P$Volumen <- as.numeric(DATA.HORTALIZAS.P$Valor)
        
        DATA.HORTALIZAS.P <- DATA.HORTALIZAS.P %>% filter(PRODUCTO == input$Hortaliza) %>%
            group_by(Mes, Anio) %>% 
            mutate(Valor = mean(Valor), Volumen = mean(Valor)) %>% 
            unique()
        
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Enero"] <- "01-01"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Febrero"] <- "01-02"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Marzo"] <- "01-03"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Abril"] <-"01-04"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Mayo"] <- "01-05"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Junio"] <- "01-06"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Julio"] <- "01-07"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Agosto"] <- "01-08"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Septiembre"] <- "01-09"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Octubre"] <- "01-10"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Noviembre"] <- "01-11"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Diciembre"] <- "01-12"

        a2 <- DATA.HORTALIZAS.P %>% unite(fecha, Mes, Anio, sep = "-") 

        a2$fecha <- as.Date(a2$fecha, format = "%d-%m-%Y")

        ggplot(a2, aes(month(fecha), Valor/1000)) + 
            geom_line(aes(col = as.factor(year(fecha)))) +
            geom_point(aes(col = as.factor(year(fecha)))) +
            scale_x_continuous(n.breaks = 12) + 
            scale_color_brewer("Anio", palette = "Paired") +
            labs(title = ("Precio medio del kg de mensualmente"), y = "Precio medio(millones de €)", x =     "Mes") +
            theme_minimal()        
   
    })
    
    output$Hortaliza2 <- renderPlot({
        
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
        
        SEPARACION$famila[SEPARACION$famila == "NA"] <- NA 
        SEPARACION$cd[SEPARACION$cd == "NA"] <- NA 
        
        DATA.HORTALIZAS.P <- SEPARACION %>% filter(famila == "HORTALIZAS") #%>% select(Mes, Anio, 'Volumen (miles de kg)') 
        
        colnames(DATA.HORTALIZAS.P)[colnames(DATA.HORTALIZAS.P) == "Año"] <- "Anio"
        colnames(DATA.HORTALIZAS.P)[colnames(DATA.HORTALIZAS.P) == "Volumen (miles de kg)"] <- "Volumen"
        colnames(DATA.HORTALIZAS.P)[colnames(DATA.HORTALIZAS.P) == "Valor (miles de €)"] <- "Valor"
        
        DATA.HORTALIZAS.P$Volumen <- str_replace(DATA.HORTALIZAS.P$Volumen, ",", ".")
        
        DATA.HORTALIZAS.P$Volumen <- as.numeric(DATA.HORTALIZAS.P$Volumen)
        
        DATA.HORTALIZAS.P <- DATA.HORTALIZAS.P %>% filter(PRODUCTO == input$Hortaliza) %>%
            group_by(Mes, Anio) %>% 
            mutate(Valor = mean(Valor), Volumen = mean(Volumen)) %>% 
            unique()
        
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Enero"] <- "01-01"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Febrero"] <- "01-02"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Marzo"] <- "01-03"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Abril"] <-"01-04"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Mayo"] <- "01-05"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Junio"] <- "01-06"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Julio"] <- "01-07"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Agosto"] <- "01-08"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Septiembre"] <- "01-09"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Octubre"] <- "01-10"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Noviembre"] <- "01-11"
        DATA.HORTALIZAS.P$Mes[DATA.HORTALIZAS.P$Mes == "Diciembre"] <- "01-12"

        a2 <- DATA.HORTALIZAS.P %>% unite(fecha, Mes, Anio, sep = "-") 

        a2$fecha <- as.Date(a2$fecha, format = "%d-%m-%Y")

        ggplot(a2, aes(month(fecha), Volumen)) + 
            geom_line(aes(col = as.factor(year(fecha)))) +
            geom_point(aes(col = as.factor(year(fecha)))) +
            scale_x_continuous(n.breaks = 12) + 
            scale_color_brewer("Anio", palette = "Paired") +
            labs(title = "Volumen medio del kg de frutas mensualmente", y = "Volumen medio(miles de kg)", x =     "Mes") +
            theme_minimal()    
        
        
    })
    # Pregunta 2 ---------------------------------------------------------------

    output$MapaCCAA <- renderLeaflet({

        CASOS_CCAA <- read_csv("casos_diagnostico_ccaa.csv",col_types = cols(fecha = col_date(format = "%Y-%m-%d")))
        
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "AN"] <- "Andalucía"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "AR"] <- "Aragón"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "AS"] <- "Principado de Asturias"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CN"] <- "Canarias"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CB"] <- "Cantabria"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CM"] <- "Castilla-La Mancha"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CL"] <- "Castilla y León"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CT"] <- "Cataluña"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "EX"] <- "Extremadura"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "GA"] <- "Galicia"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "IB"] <- "Illes Balears"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "RI"] <- "La Rioja"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "MD"] <- "Comunidad de Madrid"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "MC"] <- "Región de Murcia"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "NC"] <- "Comunidad Foral de Navarra"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "PV"] <- "País Vasco"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "VC"] <- "Comunidad Valenciana"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "CE"] <- "Ceuta"
        CASOS_CCAA$ccaa_iso[CASOS_CCAA$ccaa_iso == "ML"] <- "Melilla"

        CASOS_CCAA <- CASOS_CCAA %>% filter(year(fecha) == 2020) %>% 
            group_by(ccaa_iso, month(fecha)) %>% 
            mutate(N_CASOS = sum(num_casos)) 

        fecha <- input$CCAA
        fecha <- as.Date(fecha,format = "%y-%b-%d")
        mes <- month(fecha)
        CASOS_CCAA <- CASOS_CCAA %>% filter(month(fecha) == mes) 
        
        # Descarga de las geometrias desde eurostat con nuts_level = 2 (CCAA)
        geometrias <- get_eurostat_geospatial(resolution = "20", nuts_level = "2")
        geometrias_ES <- geometrias %>% filter(CNTR_CODE == "ES")
        
        MAPDATA <- left_join(geometrias_ES, CASOS_CCAA, by = c("NUTS_NAME"= "ccaa_iso"))
        mypalette <- colorBin(palette="Greens", domain=MAPDATA$N_CASOS, na.color="transparent")
        mytext <- paste(
            "CCAA: ", MAPDATA$NAME_LATN,"<br/>", 
            "Casos Covid-19: ", MAPDATA$N_CASOS, "<br/>", 
            sep="") %>%
            lapply(htmltools::HTML)
        
        leaflet(MAPDATA, options = leafletOptions(draggind = FALSE, minZoom = 4, maxZoom = 6)) %>%
            setView(lat=40.463667 , lng= -3.74922 , zoom= 5) %>% 
            addPolygons(color = ~mypalette(N_CASOS), smoothFactor = 0.5, opacity = 0.5,stroke=FALSE, label = mytext,
                        labelOptions = labelOptions( 
                            style = list("font-weight" = "normal", padding = "3px 8px"), 
                            textsize = "13px", 
                            direction = "auto")) %>%
            addLegend( pal=mypalette, values=~N_CASOS, opacity=0.8, title = "Casos Covid-19", position = "bottomleft" )%>%
            addProviderTiles("CartoDB.Positron")
        
        
    })
    
    
    
    
    output$impExp <- renderPlot({
        
        cuatro <- Dataset4_Comercio_Exterior_de_Espania
        cuatro$Value[cuatro$Value == ":"] <- "0"
        cuatro <- cuatro %>% mutate(Value = as.numeric(Value))
        
        cuatro <- cuatro %>% filter(PERIOD != "Jan.-Dec. 2019",PERIOD != "Jan.-Dec. 2018")
        
        cuatro <- cuatro %>% separate(PERIOD,c('Month','Year'))
        
        #cuatro <- cuatro %>% filter(Year == 2020)
        
        cuatro <- cuatro %>% filter(INDICATORS == "QUANTITY_IN_100KG",Value != 0) %>% group_by(FLOW, Year) %>% summarise(VALOR_MES = sum(Value))
        
        #Grafico
        
        ggplot(cuatro, aes(Year, VALOR_MES/1000, fill = FLOW)) +
            geom_bar(stat="identity", position=position_dodge()) +
            scale_fill_manual(values = c("seagreen3", "seagreen4"))+
            theme_minimal() +
            geom_text(aes(label=VALOR_MES),vjust=-1, hjust = 0.6,color="black",
                      position = position_dodge(1), size=2.75) + 
            labs(title = "Valor de la importaciones y exportaciones", y = "Precio (millones de €)", x =     "Año") + 
            theme_minimal() + 
            scale_fill_brewer("Año",palette = "Greens")  
        
        
    })
    
    output$exportEspana <- renderPlot({
        Dataset4 <- Dataset4_Comercio_Exterior_de_Espania
        
        cuatro <- Dataset4
        cuatro$Value[cuatro$Value == ":"] <- "0"
        cuatro <- cuatro %>% mutate(Value = as.numeric(Value))
        
        cuatro <- cuatro %>% filter(PERIOD != "Jan.-Dec. 2019",PERIOD != "Jan.-Dec. 2018")
        
        cuatro <- cuatro %>% separate(PERIOD,c('Month','Year'))
        
        cuatro$Month[cuatro$Month == "Jan"] <- "1"
        cuatro$Month[cuatro$Month == "Feb"] <- "2"
        cuatro$Month[cuatro$Month == "Mar"] <- "3"
        cuatro$Month[cuatro$Month == "Apr"] <-"4"
        cuatro$Month[cuatro$Month == "May"] <- "5"
        cuatro$Month[cuatro$Month == "Jun"] <- "6"
        cuatro$Month[cuatro$Month == "Jul"] <- "7"
        cuatro$Month[cuatro$Month == "Aug"] <- "8"
        cuatro$Month[cuatro$Month == "Sep"] <- "9"
        cuatro$Month[cuatro$Month == "Oct"] <- "10"
        cuatro$Month[cuatro$Month == "Nov"] <- "11"
        cuatro$Month[cuatro$Month == "Dec"] <- "12"
        cuatro$Month <- as.numeric(cuatro$Month)
        cuatro <- cuatro %>% filter(INDICATORS == "QUANTITY_IN_100KG",Value != 0) %>% group_by(FLOW, Year,Month) %>% summarise(VALOR_MES = sum(Value))
        
        #Grafico
        cuatro <- cuatro %>% filter( FLOW == "EXPORT",Year == '2020')
        #month != 12)#%>% arrange(Month)
        
        d <- c %>% filter(countriesAndTerritories == "Spain")
        
        ggplot(cuatro, aes(Month, VALOR_MES/100))+
            geom_bar(stat="identity", position = "dodge")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            scale_x_continuous(n.breaks = 10) +
            geom_col(aes(fill = VALOR_MES)) +
            scale_fill_gradient("Valor",low = "palegreen", high = "seagreen3") +
            geom_line (data = d,aes(month,totalcases), col =  "seagreen4" )+
            theme_minimal()+ 
            labs(title = "Exportaciones a lo largo del año", x = "Mes", y = "Cantidad (millones de kg)") 
        
        
        
        
    })
    
    output$importEspana <- renderPlot({
        Dataset4 <- Dataset4_Comercio_Exterior_de_Espania
        
        cuatro <- Dataset4
        cuatro$Value[cuatro$Value == ":"] <- "0"
        cuatro <- cuatro %>% mutate(Value = as.numeric(Value))
        
        cuatro <- cuatro %>% filter(PERIOD != "Jan.-Dec. 2019",PERIOD != "Jan.-Dec. 2018")
        
        cuatro <- cuatro %>% separate(PERIOD,c('Month','Year'))
        
        cuatro$Month[cuatro$Month == "Jan"] <- "1"
        cuatro$Month[cuatro$Month == "Feb"] <- "2"
        cuatro$Month[cuatro$Month == "Mar"] <- "3"
        cuatro$Month[cuatro$Month == "Apr"] <-"4"
        cuatro$Month[cuatro$Month == "May"] <- "5"
        cuatro$Month[cuatro$Month == "Jun"] <- "6"
        cuatro$Month[cuatro$Month == "Jul"] <- "7"
        cuatro$Month[cuatro$Month == "Aug"] <- "8"
        cuatro$Month[cuatro$Month == "Sep"] <- "9"
        cuatro$Month[cuatro$Month == "Oct"] <- "10"
        cuatro$Month[cuatro$Month == "Nov"] <- "11"
        cuatro$Month[cuatro$Month == "Dec"] <- "12"
        cuatro$Month <- as.numeric(cuatro$Month)
        cuatro <- cuatro %>% filter(INDICATORS == "QUANTITY_IN_100KG",Value != 0) %>% group_by(FLOW, Year,Month) %>% summarise(VALOR_MES = sum(Value))
        
        #Grafico
        cuatro <- cuatro %>% filter( FLOW == "IMPORT",Year == '2020')
        #month != 12)#%>% arrange(Month)
        d <- c %>% filter(countriesAndTerritories == "Spain")
        
        
        
        ggplot(cuatro, aes(Month, VALOR_MES/100))+
            geom_bar(stat="identity", position = "dodge")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            scale_x_continuous(n.breaks = 10) +
            geom_col(aes(fill = VALOR_MES)) +
            scale_fill_gradient("Valor",low = "palegreen", high = "seagreen3") +
            geom_line (data = d,aes(month,totalcases), col =  "seagreen4" )+
            theme_minimal()+ 
            labs(title = "Exportaciones a lo largo del año", x = "Mes", y = "Cantidad (millones de kg)") 
        
        
        
    })
    
    
    
    # Pregunta 3 ---------------------------------------------------------------
    
    output$export <- renderPlot({
        
        DATA5 <- Dataset4_Comercio_Exterior_de_Espania 
        
        DATA5$Value[DATA5$Value == ":"] <- "0"
        DATA5 <- DATA5 %>% mutate(Value = as.numeric(Value))
        a <- as.numeric(DATA5$Value)
        sum(a[0], a[1])
        
        DATA5 <- DATA5 %>% filter(INDICATORS == "QUANTITY_IN_100KG", Value != 0, PERIOD != "Jan.-Dec. 2019", PERIOD != "Jan.-Dec. 2018") %>% 
            group_by(FLOW, PERIOD) %>% 
            summarise(VALOR_MES = sum(Value))
        DATA_3.1 <- DATA5 %>% separate(PERIOD, c("MES", "ANIO")) 
        
        DATA_3.1$MES[DATA_3.1$MES == "Jan"] <- "01-01"
        DATA_3.1$MES[DATA_3.1$MES == "Feb"] <- "01-02"
        DATA_3.1$MES[DATA_3.1$MES == "Mar"] <- "01-03"
        DATA_3.1$MES[DATA_3.1$MES == "Apr"] <-"01-04"
        DATA_3.1$MES[DATA_3.1$MES == "May"] <- "01-05"
        DATA_3.1$MES[DATA_3.1$MES == "Jun"] <- "01-06"
        DATA_3.1$MES[DATA_3.1$MES == "Jul"] <- "01-07"
        DATA_3.1$MES[DATA_3.1$MES == "Aug"] <- "01-08"
        DATA_3.1$MES[DATA_3.1$MES == "Sep"] <- "01-09"
        DATA_3.1$MES[DATA_3.1$MES == "Oct"] <- "01-10"
        DATA_3.1$MES[DATA_3.1$MES == "Nov"] <- "01-11"
        DATA_3.1$MES[DATA_3.1$MES == "Dec"] <- "01-12"

        exp <- DATA_3.1 %>% filter(ANIO == "2020", FLOW == "EXPORT", MES != "01-12") %>% arrange(MES)
        imp <- DATA_3.1 %>% filter(ANIO == "2020", FLOW == "IMPORT", MES != "01-12") %>% arrange(MES)
        
        DATA_3.2 <- Dataset5_Coronavirus_cases %>% filter(geoId == "ES", year == 2020) %>%
            group_by(month) %>% 
            summarise(casos = sum(cases))
        export <- data.frame(x=exp$VALOR_MES, y=DATA_3.2$casos)
        
        ggplot(export, aes(x,y/1000)) + 
            geom_jitter(col = "springgreen4") + 
            geom_smooth(col = "springgreen3", fill = "darkseagreen1") +
            theme_minimal() +
            labs(title = "Correlación entre casos Covid-19 y exportaciónes", y = "Casos Covid-19 (miles)", x = "Número de exportaciones")
        
    })
    
    output$import<- renderPlot({
        
        DATA5 <- Dataset4_Comercio_Exterior_de_Espania 
        
        DATA5$Value[DATA5$Value == ":"] <- "0"
        DATA5 <- DATA5 %>% mutate(Value = as.numeric(Value))
        a <- as.numeric(DATA5$Value)
        sum(a[0], a[1])
        
        DATA5 <- DATA5 %>% filter(INDICATORS == "QUANTITY_IN_100KG", Value != 0, PERIOD != "Jan.-Dec. 2019", PERIOD != "Jan.-Dec. 2018") %>% 
            group_by(FLOW, PERIOD) %>% 
            summarise(VALOR_MES = sum(Value))
        DATA_3.1 <- DATA5 %>% separate(PERIOD, c("MES", "ANIO")) 
        
        DATA_3.1$MES[DATA_3.1$MES == "Jan"] <- "01-01"
        DATA_3.1$MES[DATA_3.1$MES == "Feb"] <- "01-02"
        DATA_3.1$MES[DATA_3.1$MES == "Mar"] <- "01-03"
        DATA_3.1$MES[DATA_3.1$MES == "Apr"] <-"01-04"
        DATA_3.1$MES[DATA_3.1$MES == "May"] <- "01-05"
        DATA_3.1$MES[DATA_3.1$MES == "Jun"] <- "01-06"
        DATA_3.1$MES[DATA_3.1$MES == "Jul"] <- "01-07"
        DATA_3.1$MES[DATA_3.1$MES == "Aug"] <- "01-08"
        DATA_3.1$MES[DATA_3.1$MES == "Sep"] <- "01-09"
        DATA_3.1$MES[DATA_3.1$MES == "Oct"] <- "01-10"
        DATA_3.1$MES[DATA_3.1$MES == "Nov"] <- "01-11"
        DATA_3.1$MES[DATA_3.1$MES == "Dec"] <- "01-12"
        
        exp <- DATA_3.1 %>% filter(ANIO == "2020", FLOW == "EXPORT", MES != "01-12") %>% arrange(MES)
        imp <- DATA_3.1 %>% filter(ANIO == "2020", FLOW == "IMPORT", MES != "01-12") %>% arrange(MES)
        
        DATA_3.2 <- Dataset5_Coronavirus_cases %>% filter(geoId == "ES", year == 2020) %>%group_by(month) %>% summarise(casos = sum(cases))
        
        import <- data.frame(x=imp$VALOR_MES, y=DATA_3.2$casos)
        
        ggplot(import, aes(x,y/1000)) + 
            geom_jitter(col = "springgreen4") + 
            geom_smooth(col = "springgreen3", fill = "darkseagreen1") +
            theme_minimal() +
            labs(title = "Correlación entre casos Covid-19 y importaciones", y = "Casos Covid-19 (miles)", x = "Número de importaciones")
        
    })
    
    output$exportCovid <- renderPlot({
        
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
        
        totales <- Data %>% group_by(month,year,countriesAndTerritories)%>% mutate(totalcases = sum(cases,na.rm = T))
        totales <-totales %>% group_by(year,month,countriesAndTerritories)%>% mutate(totaldeath = sum(deaths,na.rm = T))
        
        totales <- totales[,c(-1,-2,-5,-6,-12)]
        totales <- unique(totales)
        totales <- totales %>% filter(year == 2020, countriesAndTerritories == input$Pais2)
        
        D4y5 <- unique(Dataset4$REPORTER)
        Paises <- D4y5[-8]
        
        
        
        #Grafico  
        Dataset4$Month <- as.integer(Dataset4$Month)
        Dat4 <- Dataset4 %>% filter(REPORTER == input$Pais2,FLOW == 'EXPORT')
        Dat4 <- Dat4[with(Dat4,order(Dat4$Month)),]
        d <- c %>% filter(countriesAndTerritories == input$Pais2)
        
        ggplot(Dat4,aes(x = Month,y=VALOR_MES)) +
            geom_bar(stat = 'identity')+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
            geom_col(aes(fill = VALOR_MES)) +
            scale_fill_gradient("Valor",low = "palegreen", high = "seagreen3") +
            geom_line (data = d,aes(month,totalcases), col =  "seagreen4" )+
            scale_x_continuous(n.breaks = 11) +
            theme_minimal()+ 
            labs(title = "Casos de Covid-19 y números de exportaciones a lo largo del 2020", x = "Mes", y = "Valor") 
        
        
        
        
    })
    
    
    
    output$importCovid <- renderPlot({
        
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
        
        totales <- Data %>% group_by(month,year,countriesAndTerritories)%>% mutate(totalcases = sum(cases,na.rm = T))
        totales <-totales %>% group_by(year,month,countriesAndTerritories)%>% mutate(totaldeath = sum(deaths,na.rm = T))
        
        totales <- totales[,c(-1,-2,-5,-6,-12)]
        totales <- unique(totales)
        totales <- totales %>% filter(year == 2020, countriesAndTerritories == input$Pais)
        
        D4y5 <- unique(Dataset4$REPORTER)
        Paises <- D4y5[-8]
       
        
        #Grafico  
        Dataset4$Month <- as.integer(Dataset4$Month)
        Dat4 <- Dataset4 %>% filter(REPORTER == input$Pais,FLOW == 'IMPORT')
        Dat4 <- Dat4[with(Dat4,order(Dat4$Month)),]
        d <- c %>% filter(countriesAndTerritories == input$Pais)
        
        ggplot(Dat4,aes(x = Month,y=VALOR_MES)) +
            geom_bar(stat = 'identity')+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
            geom_col(aes(fill = VALOR_MES)) +
            scale_fill_gradient("Valor",low = "palegreen", high = "seagreen3") +
            geom_line (data = d,aes(month,totalcases), col =  "seagreen4" )+
            scale_x_continuous(n.breaks = 11) +
            theme_minimal()+ 
            labs(title = "Casos de Covid-19 y números de importaciones a lo largo del 2020", x = "Mes", y = "Valor") 
        
        
    })
    
    # 
    # 
    # output$mapaEU <- renderLeaflet({
    #     
    #     
    #     pr1 <- Dataset5_Coronavirus_cases %>% group_by(year,month,countriesAndTerritories)%>% mutate(avgcovid = mean(`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`,na.rm = T))
    #     
    #     pr1 <- pr1 %>% group_by(year,month,countriesAndTerritories)%>% mutate(avgcases = sum(cases,na.rm = T))
    #     pr1 <- pr1 %>% group_by(year,month,countriesAndTerritories)%>% mutate(avgdeath = sum(deaths,na.rm = T))
    #     
    #     pr1 <- pr1[,c(-1,-2,-5,-6,-12)]
    #     pr1 <- unique(pr1)
    #     
    #     pr3 <- pr1 %>% group_by(year,month,countriesAndTerritories)%>% mutate(avgcovidtotal = sum(avgcovid,na.rm = T))
    #     pr3 <- pr3 %>% group_by(year,month,countriesAndTerritories)%>% mutate(avgcasestotal = sum(avgcases,na.rm = T))
    #     pr3 <- pr3 %>% group_by(year,month,countriesAndTerritories)%>% mutate(avgdeathtotal = sum(avgdeath,na.rm = T))
    #     
    #     pr3 <- pr3[-7]
    #     pr3 <- pr3[-7]
    #     pr3 <- pr3[-7]
    #     pr3 <- unique(pr3)
    #     pr3 <- pr3 %>% filter(year==2020, month == month(input$EU))
    #     
    #     geometrias <- get_eurostat_geospatial(resolution = "10", nuts_level = "0")
    #     
    #     mapdata <- left_join(geometrias,pr3, by = c("geo" = "geoId" ))
    #     
    #     mypalette <- colorBin(palette="Greens", domain=mapdata$avgdeathtotal, na.color="transparent")
    #     mytext <- paste(
    #         "Pais: ", mapdata$NAME_LATN,"<br/>", 
    #         "Casos Covid-19: ", mapdata$avgcasestotal, "<br/>", 
    #         "Muertes Covid-19: ", mapdata$avgdeathtotal, "<br/>", 
    #         sep="") %>%
    #         lapply(htmltools::HTML)
    #     
    #     leaflet(mapdata, options = leafletOptions(draggind = FALSE, minZoom = 2, maxZoom = 4)) %>%
    #         setView(lat=54.526 , lng= 15.2551 , zoom= 3) %>% 
    #         addPolygons(color = ~mypalette(avgdeathtotal), opacity = 0.8,stroke=FALSE, label = mytext,
    #                     labelOptions = labelOptions( 
    #                         style = list("font-weight" = "normal", padding = "3px 8px"), 
    #                         textsize = "13px", 
    #                         direction = "auto")) %>%
    #         addLegend( pal=mypalette, values=~avgdeathtotal, opacity=0.8, title = "Casos Covid-19", position = "bottomleft" )%>%
    #         addProviderTiles("CartoDB.Positron")
    #     
    # })
    # 
    
    
})