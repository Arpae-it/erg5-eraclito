library(shiny)
library(leaflet)
library("leaflet.extras")
library("crosstalk")
library(openair)
library(plotly)
library(ggplot2)
library(zoo)
library(dplyr)
library(DT)
library (RCurl)
library(rgdal)
library(rhdf5)
library(stringr)
library(fst)
library(shinyjs)
library(shinydashboard)
# Define UI for data download app ----
options(shiny.maxRequestSize=120*1024^2)
httr::set_config(httr::config(ssl_verifypeer=0L))


data<-read_fst("www/data2.fst")
tmax_r<-read_fst("www/tmax2.fst")
icona<-iconList(blue = makeIcon("www/blue.jpg", iconWidth = 12, iconHeight =12),
                green = makeIcon("www/green.png", iconWidth = 12, iconHeight =12),
                red = makeIcon("www/red.jpg", iconWidth = 12, iconHeight =12))
ui=dashboardPage(
  dashboardHeader(title = 'Dati Clima',
                  tags$li(a(href = 'http://www.arpae.it',
                            icon("power-off"),
                            title = "Torna ad Arpae"),
                          class = "dropdown")),
                  dashboardSidebar(width=300,
                                   useShinyjs(),
                                   
                 dateRangeInput("daterange","Scegli la data",start=Sys.Date()-730,end="2020-12-31",language="it"),
                 textInput("cella","Scegli una cella (00019-02315)",value="00019"),
                # selectInput("variabile","Scegli la variabile per la mappa",choices=c("Tmax","Tmin","Prec"),selected="Tmax"),
                 #dateInput("datamap","Scegli la data per la mappa"),
               div(id = "Inputone",
                 textInput("cellastart","Scegli una riga inizio",value="1"),
                 textInput("cellaend","Scegli una riga fine ",value="21915"))

    ),

    # Main panel for displaying outputs ----
    dashboardBody(width=9,

              plotlyOutput("lista"),
# dataTableOutput("nomi"),
# tableOutput("stat"),
leafletOutput("mappetta"),
#textOutput("latlongu"),
 
downloadButton('downloadData1', 'Download dataset plottato come csv')

     )


    )

# Define server logic to display and download selected file ----


server <- function(input, output,session) {
 hide(id = "Inputone")
#tmax_r<- reactive({
#  tmax_r<-read_fst("www/tmax2.fst",from=as.numeric(input$cellastart),to=as.numeric(input$cellaend))
#})
 
#output$nomi<-renderDataTable({
 # tmax<-tmax_r
#  tmax[,2]<-as.numeric(tmax[,2])
#  tmax[,3]<-as.numeric(tmax[,3])
#  tmax[,4]<-as.numeric(tmax[,4])
# tabella<-tmax[which(tmax$cod %in% input$cella),]
#   tabella<-selectByDate(tabella,start=input$daterange[1],end=input$daterange[2])
# 
#   tabella
#  tmax
#})

output$latlongu<-renderText({
  tmax<-tmax_r
  paste0(tmax[1,1])
})
output$mappetta<-renderLeaflet({
  tmax<-tmax_r
  tabella<-tmax[which(tmax$cod %in% input$cella),]
  tabella<-selectByDate(tabella,start=input$daterange[1],end=input$daterange[2])
leaflet(c)%>%
addTiles()%>%
addMarkers(data$lon,data$lat,icon=icona["green"],popup=(paste0("COD",data$Code)))
})

observeEvent(input$mappetta_marker_click, {
  click <- input$mappetta_marker_click
  station <- data[which(data$lat == click$lat & data$lon == click$lng), ]$COD5
  carica<-data[which(data$lat == click$lat & data$lon == click$lng), ]$ID
  calcola_ini<-(carica-1)*21915+1
  calcola_fin<-calcola_ini+21915-1
  
  updateTextInput(session, "cella","Scegli una cella (00019-02315)",value=station) 
  updateTextInput(session, "cellastart","Scegli una riga inizio",value=calcola_ini)                   
  updateTextInput(session, "cellaend","Scegli una riga fine",value=calcola_fin)                   

  })
output$stat<-renderTable({
  tmax<-tmax_r
  tabella<-tmax[which(tmax$cod %in% input$cella),]
  tabella<-selectByDate(tabella,start=input$daterange[1],end=input$daterange[2])
  tstat<-data.frame(summary(tabella))
  tstat})

output$lista<-renderPlotly({
  tmax<-tmax_r
  tabella<-tmax[which(tmax$cod %in% input$cella),]
  tabella<-selectByDate(tabella,start=input$daterange[1],end=input$daterange[2])
tabella$date<-as.Date(tabella$date,format="%Y-%m-%d")
  grafico<-plot_ly(tabella,x = ~date)%>%
    add_trace( y = as.numeric(tabella$Prec), 
              name = "prec",type="bar",yaxis="y2",opacity=0.6)%>%
    add_trace( y = ~Tmin, name = "tmin",type="scatter",mode='lines',line=list(color="blue"))%>%
  add_trace( y = ~Tmax, name = "tmax",type="scatter",mode='lines',line=list(color="red"))%>%
    
    layout(
      barmode="overlay",
      title = 'Temperature - Precipitazioni',
       yaxis2 = list(
         tickfont = list(color = "red"),
         overlaying = "y",
         side = "right",
         title = "Precipitazioni"
       ),
      xaxis = list(
        type = 'category',
        
        title = 'Date'
      ),
      yaxis = list(
        title = 'T °C',
        side="left",
        type='linear'
      
      ))
})

datasetInput <- reactive({
  tmax<-tmax_r
  tabella<-tmax[which(tmax$cod %in% input$cella),]
  tabella<-selectByDate(tabella,start=input$daterange[1],end=input$daterange[2])
})
output$downloadData1 <- downloadHandler(

  filename = function() { paste0(input$cella,'_',input$daterange[1],'_',input$daterange[2],'.csv') }, content = function(file) {
    write.csv (datasetInput(), file, row.names=FALSE)
  }
)

  }
# Create Shiny app ----
shinyApp(ui, server)
