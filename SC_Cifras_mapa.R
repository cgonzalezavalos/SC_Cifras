library(shiny)
library(datasets)
library(tidyverse)
library(lubridate)
library(flextable)
library(leafem)
library(leaflet)
library(geojson)
library(RColorBrewer)


## set work directory
setwd("/Volumes/SDMAC/shiny/SC_Cifras")

#-------------------------------------
# Tabla de concursos

concursos<-read_csv("datos/concursos.csv")
tabla_resumen_concursos_ministerio<-concursos %>% 
  group_by(Ministerio,nivel,year_convocatoria) %>% 
  summarise(convocatorias=sum(concursos,na.rm = TRUE))

tabla_resumen_concursos_ministerio<-tabla_resumen_concursos_ministerio %>%
  pivot_wider(names_from = year_convocatoria, values_from = convocatorias)

#-------------------------------------
# Tabla de tiempos concursos

tiempos_concursos <- read_csv("datos/TiempoConcurso.csv", 
                              col_types = cols(`Fecha Convocatoria` = col_date(format = "%d-%m-%Y"),
                                               `Fecha Envío Nómina` = col_date(format = "%d-%m-%Y")))
fechas_concursos<-read_csv("datos/fechas_concursos.csv")

fechas_concursos<-fechas_concursos %>% rename(UnionConcurso=CD_CONCURSO)

tiempo_proceso<-tiempos_concursos %>% filter(!is.na(`Fecha Envío Nómina`),
                                             !is.na(`Fecha Convocatoria`),
                                             `Fecha Envío Nómina`>`Fecha Convocatoria`) %>% 
  select(UnionConcurso,`Fecha Convocatoria`,`Fecha Envío Nómina`)

tiempo_proceso$tiempo<-as.numeric(difftime (tiempo_proceso$`Fecha Envío Nómina`,tiempo_proceso$`Fecha Convocatoria`))
tiempo_proceso$year_convocatoria<-as.numeric(year(tiempo_proceso$`Fecha Convocatoria`))
tiempo_proceso<-left_join(tiempo_proceso,fechas_concursos,by='UnionConcurso')
tiempo_proceso<-tiempo_proceso %>% select(1,2,3,4,5,6,7,8,9)
tabla_resumen_tiempo_proceso<-tiempo_proceso %>% 
  group_by(Ministerio,year_convocatoria) %>% 
  summarise(tiempos_promedio_concurso=mean(tiempo))
tabla_resumen_tiempo_proceso$Ministerio<-as.factor(tabla_resumen_tiempo_proceso$Ministerio)


resumen_tiempo_proceso<-tabla_resumen_tiempo_proceso %>%
  pivot_wider(names_from = year_convocatoria, values_from = tiempos_promedio_concurso)

tabla_nivel<-tiempo_proceso %>% 
  group_by(Ministerio,Nivel) %>% 
  summarise(tiempos_promedio_concurso=mean(tiempo))

resumen_nivel<-tabla_nivel %>%
  pivot_wider(names_from = Nivel, values_from = tiempos_promedio_concurso) %>% select(Ministerio,I,II)

#-------------------------------------
# Tabla de cargos
cargos_actuales <- read_csv("datos/cargos_actuales.csv")
cargos_actuales<-cargos_actuales %>% rename(Region_Cargo=Region)
tabla_resumen_cargos<-cargos_actuales %>% filter(ADSCRITO=='ADSCRITO') %>% group_by(Ministerio,Nivel) %>% summarise(cargos=n())
cargos_region_ministerio<-cargos_actuales %>% filter(ADSCRITO=='ADSCRITO') %>% group_by(Ministerio,Nivel, Region_Cargo) %>% summarise(cargos=n())
tabla_cargos_region<-cargos_region_ministerio %>%
  pivot_wider(names_from = Nivel, values_from = cargos)

#-------------------------------------
# Tabla de postulaciones
tabla_postulaciones<-read_csv("datos/postulaciones.csv")
tabla_resumen_postulaciones <- tabla_postulaciones %>% 
  group_by(Ministerio, year_postulacion, Nivel) %>%
  summarise(postulaciones=sum(Postulaciones,na.rm=TRUE))

tabla_resumen_postulaciones<-tabla_resumen_postulaciones %>%
  pivot_wider(names_from = Nivel, values_from = postulaciones)

#-------------------------------------
# Mapa de Chile
# https://github.com/fcortes/Chile-GeoJSON
# https://rstudio.github.io/leaflet/
# https://rdocumentation.org/packages/leafem/versions/0.2.0
# http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html
# https://mhallwor.github.io/_pages/Tidyverse_intro

chile<-geojsonio::geojson_read('https://raw.githubusercontent.com/fcortes/Chile-GeoJSON/master/regiones_edit.geojson',what = "sp")
#getwd()
#geojson::geo_write(chile,"data/Regional.geojson")

colores<-c("#86888a","#144d82","#3a6791","#899cc9","#6BAED6","#4292C6","#2171B5","#08519C","#08306B","#478745","#35c916","#587554","#74C476","#41AB5D","#124a25","#3fbf92")
factpal<-colorFactor(colores, chile$Region)

#-------------------
# datos para mostrar en mapa

#chile<-merge(chile,tabla_cargos_region %>% filter(Ministerio=='Ministerio de Educación'),by.x='Region',by.y='Region_Cargo')


# cargos I nivel
#cargos_x_region_I<-tabla_cargos_region %>% 
#  group_by(Region_Cargo) %>% 
#  summarise(I_N=sum(I,na.rm = TRUE)) %>%
#  select(2)

#cargos_x_region_II<-tabla_cargos_region %>%
#  group_by(Region_Cargo) %>% 
#  summarise(II_N=sum(II,na.rm = TRUE)) %>%
#  select(2)

#cargos_x_region_I[17,1]<-0
#cargos_x_region_II[17,2]<-0
#chile$cargos_x_region_I<-vector(length = 17)
#chile$cargos_x_region_II<-vector(length = 17)
#for (i in 1:17) {
#chile$cargos_x_region_I[i]<-pull(cargos_x_region_I[i,1])
#chile$cargos_x_region_II[i]<-pull(cargos_x_region_II[i,1])
#}


labels <- sprintf("<strong>%s</strong><br/>Cargos de primer nivel: %g<br/>Cargos de segundo nivel: %g",
                  chile$Region,chile$I,chile$II) %>% lapply(htmltools::HTML)



#-------------------------------------

# Use a fluid Bootstrap layout
ui<-fluidPage(
  tags$head(tags$style(HTML("
    .shiny-text-output {
      background-color:#fff;
    }
  "))),
  h1(span("Servicio Civil en Cifras", style = "font-weight: 300"), 
     style = "font-family: 'Arial';
        color: #f54; text-align: left;
        background-image: url('texturebg.png');
        padding: 20px"),
  br(),
  fluidRow(
    column(6 ,offset = 0,
           h4("En este sitio se muestra información de la gestión realizada en conjunto con el Servicio Civil.", 
             style = "font-family: 'Arial';text-align: left")
    )
  ),
  br(),
    # Define the sidebar with one input
    #sidebarPanel(
      fluidRow(
        column(4,offset=0,
          selectInput("Ministerio", "Ministerio:", 
                  choices=(tabla_resumen_tiempo_proceso$Ministerio)),
      hr(),
      helpText("Datos actualizados al 30/05/2022"),
      position='left',
      fluid=TRUE
      )
    ),
    # Create a spot for the barplot
      fluidRow(
        column(4,
          wellPanel(
            h3('Mapa de Chile'),
            hr(),
            leafletOutput("Mapa_1")
            )
          ),
        column(4,
          wellPanel(
            h3('Tiempos concurso x Nivel'),
            hr(),
            tableOutput("tabla_1")
            )
          ),
        column(4,
               wellPanel(
                 h3('Cargos x Nivel'),
                 hr(),
                 tableOutput("tabla_2")
               )
        ),
        column(4,
               wellPanel(
                 h3('Concursos x Nivel'),
                 hr(),
                 tableOutput("tabla_3")
               )
        ),
        column(4,
               wellPanel(
                 h3('Postulaciones x Nivel'),
                 hr(),
                 tableOutput("tabla_4")
               )
        )
      )
)


#-----------------------------------
# Define a server for the Shiny app
server<-function(input, output) {
  # Fill in the spot we created for a plot
  
  output$grafico_1 <- renderPlot({
    # Render a barplot
    ggplot(tabla_resumen_tiempo_proceso %>% filter(Ministerio==input$Ministerio),aes(year_convocatoria ,tiempos_promedio_concurso)) +
      geom_line()+geom_point()+theme_classic()+xlab('Año')+ylab('Duración Promedio de Concurso [Dias]')+
      title(paste0('Tiempo pormedio de concursos por año -',input$Ministerio))
  },width = 200,height = 200)
  output$tabla_1<-renderTable({
    resumen_nivel %>% filter (Ministerio==input$Ministerio)
    },digits = 1)
  output$tabla_2<-renderTable({
    tabla_resumen_cargos %>% filter (Ministerio==input$Ministerio)
  })
  output$tabla_3<-renderTable({
    tabla_resumen_concursos_ministerio %>% filter (Ministerio==input$Ministerio)
  })
  
  output$tabla_4<-renderTable({
    tabla_resumen_postulaciones %>% filter (Ministerio==input$Ministerio)
  })
  output$Mapa_1<- renderLeaflet({
    leaflet(merge(chile,tabla_cargos_region %>%
                            filter(Ministerio==input$Ministerio),by.x='Region',
                  by.y='Region_Cargo')) %>%
      setView(lng=-70,lat=-35,zoom=4)  %>% 
      addPolygons(fillColor = ~factpal(Region),
                  weight = 1,
                  color="white",
                  highlightOptions = highlightOptions(
                    color = "#666",  weight = 5, bringToFront = TRUE, opacity = 0.4),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))})
  
  
}

#-----------------------------------

shinyApp(ui, server)