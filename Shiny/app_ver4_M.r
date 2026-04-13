### dashboard y graficos para LecturadatosFootball.R ####
### published at https://ccatalan.shinyapps.io/KPIsFootball/
### COLORES ASIGNADOS:
### Pressing<- RGBA(100,150,0,0.25)
### Activity<- RGBA(250,200,0,0.50)
### Force<- RGBA(250,200,0,0.50)


# load required packages
library("psych")
library("tidyverse")
library("dplyr")
library("png")
library("plotly")   #¸3D plots surface. "plot_ly()"
library("shiny")  #Ver tb. http://shiny.rstudio.com
library("shinyWidgets")
library("shinydashboard")
library("shinythemes")
if(!require(rsconnect))install.packages("rsconnect");library("rsconnect")

rsconnect::setAccountInfo(name='ccatalan',
			  token='5F00660A799C99D496210DFBAFA47F87',
			  secret='g71Po9rkyRzAXMpymFwAyK0RsFpz0wENv7g2LMEp')

#rsconnect::deployApp('path/to/your/app')
#if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
#if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
#if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
#if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# set mapping colour for each outbreak
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
mi_color <- "#cc4c02"

### DATA
KPIab <- readRDS("Data/1_Fr1-145006_r5_KPIab.rds")
# KPIab <- KPIab %>% mutate(Fr=as.integer(Fr))  #convertimos a integer los Fr para manipular
KPIAb <-readRDS("Data/1_Fr1-145006_r5_KPIAb2.rds")
#KPIAb <- KPIAb %>% mutate(Fr=as.integer(Fr))  #convertimos a integer los Fr para manipular
KPIaB <-readRDS("Data/1_Fr1-145006_r5_KPIaB3.rds")
#KPIaB <- KPIaB %>% mutate(Fr=as.integer(Fr))  #convertimos a integer los Fr para manipular
KPIAB <- readRDS("Data/1_Fr1-145006_r5_KPIAB4.rds")
#KPIAB <- KPIAB %>% mutate(Fr=as.integer(Fr))  #convertimos a integer los Fr para manipular
Ev <- readRDS("Data/1_Fr1-145006_r5_Eventing.rds")

  #normalizacion del nombre de las variables para representarlas automaticamente con los radiobuttons
KPIab <- KPIab%>% mutate(P=Pab,J=Jab,Js=Jab,JsJ=0,Force=Fab,Fs=Fab,FsF=0,W=Wab) #E=Eab)
KPIAb <- KPIAb%>% mutate(P=PAb,J=JAb,Js=JAb_s,JsJ=JAb_s-JAb,Force=FAb,Fs=FAb_s,FsF=FAb_s-FAb,W=WAb) #E=EAb)
KPIaB <- KPIaB%>% mutate(P=PaB,J=JaB,Js=JaB_s,JsJ=JaB_s-JaB,Force=FaB,Fs=FaB_s,FsF=FaB_s-FaB,W=WaB) #E=EaB)
KPIAB <- KPIAB%>% mutate(P=PAB,J=JAB,Js=JAB_s,JsJ=JAB_s-JAB,Force=FAB,Fs=FAB_s,FsF=FAB_s-FAB,W=WAB) #E=EAB)
  #Para dibujar los eventos unimos el valor de P y el de Js
Ev_SaqueEsquina<-Ev %>% mutate(Fr=Start.Frame,Bowing=Team,CornerKick=Subtype) %>% filter(Subtype=="CORNER KICK") %>% select(Fr,CornerKick)
Ev_SaqueMeta<-Ev %>% mutate(Fr=Start.Frame,Bowing=Team,GoalKick=Subtype) %>% filter(Subtype=="GOAL KICK") %>% select(Fr,GoalKick)
Ev_PerdidaBalon <-Ev %>% mutate(Fr=Start.Frame,Bowing=Team,BallLost=Type) %>% filter(Type=="BALL LOST") %>% select(Fr,BallLost)

#Unimos los dataframe con KPIAB para añadir el valor de P y Js
Ev_SaqueEsquina  <-  merge(KPIAB,Ev_SaqueEsquina,by="Fr",all.y=TRUE) %>% relocate(CornerKick,.after=Fr)%>% select(Fr,CornerKick,Bowing,P,Js)
Ev_SaqueMeta  <-  merge(KPIAB,Ev_SaqueMeta,by="Fr",all.y=TRUE) %>% relocate(GoalKick,.after=Fr)%>% select(Fr,GoalKick,Bowing,P,Js)
Ev_PerdidaBalon <-  merge(KPIAB,Ev_PerdidaBalon,by="Fr",all.y=TRUE) %>% relocate(BallLost,.after=Fr)%>% select(Fr,BallLost,Bowing,P,Js)


# objects to be visible across all sessions
# dataAB<-KPIAB %>% mutate(Fr=as.integer(Fr))  #convertimos a integer los Fr para manipular
# dataAB_f <- length(dataAB$Fr)  # numero de frames (ojo que no son frames únicos)
# dataAB_n <- dataAB[dataAB_f,1]-dataAB[1,1]  #el último menos el primero me dan el total de rows del archivo
# mi<-dataAB$Fr[1]  #el primero me da el vslor del frame primero
# ma<-dataAB$Fr[dataAB_f] #el último me da el valor del frame último
mi<-1
ma<-2



### SHINY UI ###
ui<-

  fluidPage(
  withMathJax(),  
  titlePanel(h1("Defensive metrics and KPIs in football. ",align="center")
    ),
      # headerPanel('Defensive metrics and KPIs in Football'),
  sidebarLayout(
    sidebarPanel(
    #p("select team / player analysis:"),
    radioButtons(inputId="kPIs","Defensive KPI analysis :",
                 choices = c("\\( A \\, \\& \\, B\\)"="AB","\\( a \\, \\& \\,  B\\)"="aB",
                              "\\( A \\, \\& \\,  b\\)"="Ab","\\( a \\, \\& \\,  b\\)"="ab"), 
                 selected = "AB"),
    # p("select ball possesion team:"),
    radioButtons(inputId="bowing","Defender team",choices = c('\\( A \\& B\\)' ="HA",'\\( B\\)'="H", 
                                                           '\\(A\\)'="A"), selected = "HA"),
    # Input: Specification of range within an interval ----
      sliderInput(inputId="fr", "FrameRange:",
                  min = mi, max = ma,
                  value = c(mi,ma)),

    #imagen descriptiva
    imageOutput("image2", width = "100%"),
    imageOutput("image1", width = "50%")
    ),

#      
    mainPanel(
      plotlyOutput("plot1AB"),
      plotlyOutput("plot2AB"),
      plotlyOutput("plot3AB"),
      plotlyOutput("plot4AB")
    )   #end main panel
  )  #end of sidebar
)


### SHINY SERVER ###
server <- function(input, output,session) {

    #Per-session objects (en caso de existir)  

  
    sliderValues <- reactive({
    data.frame(    
      Name = c("FrameRange"),
      Value = as.character(c(paste(input$fr, collapse = " "))),
      stringsAsFactors = FALSE)
  })
  
  BallOwing <- reactive({
        Name=c("BallOwing")
        Value = switch (input$bowing,
                       H="A", #la condicion se niega.
                       A="H", #la condicion se niega.
                       HA="NA")
   })
  #valor minimo del slider
  m<-reactive({
    mi<-input$fr[1]
    m<-mi
  })
  #valor máximo del slider
  M<-reactive({
    ma<-input$fr[2]
    M<-ma
  })
  minim<-reactive({
    minim<-dataBase()$Fr[1]  
  })
  maxim<-reactive({
    data_f <- length(dataBase()$Fr)  # numero de rows del archivo
    maxim<-dataBase()$Fr[data_f] 
  })
  
  dataBase<- reactive(
    if (input$kPIs=="AB"){
      dataBase<-KPIAB
    } else if(input$kPIs=="aB"){
      dataBase<-KPIaB
    } else if(input$kPIs=="Ab"){
      dataBase<-KPIAb}
      else if(input$kPIs=="ab"){
      dataBase<-KPIab
      }
  )
  #Para dibujar los eventos de los saques de esquina
  Ev_CornerKick<- reactive(
    if (input$kPIs=="AB"){
      Ev_CornerKick<- Ev_SaqueEsquina
    } else if(input$kPIs=="aB"){
      Ev_CornerKick<-Ev_SaqueEsquina
    } else if(input$kPIs=="Ab"){
      Ev_CornerKick<-Ev_SaqueEsquina}
      else if(input$kPIs=="ab"){
      Ev_CornerKick<-Ev_SaqueEsquina
      }
  )
    #Para dibujar los eventos de los saques de puerta
  Ev_GoalKick<- reactive(
    if (input$kPIs=="AB"){
      Ev_CornerKick<- Ev_SaqueMeta
    } else if(input$kPIs=="aB"){
      Ev_CornerKick<-Ev_SaqueMeta
    } else if(input$kPIs=="Ab"){
      Ev_CornerKick<-Ev_SaqueMeta}
      else if(input$kPIs=="ab"){
      Ev_CornerKick<-Ev_SaqueMeta
      }
  )
    #Para dibujar los eventos de perdidas balon
  Ev_BallLost<- reactive(
    if (input$kPIs=="AB"){
      Ev_BallLost<- Ev_PerdidaBalon
    } else if(input$kPIs=="aB"){
      Ev_BallLost<-Ev_PerdidaBalon
    } else if(input$kPIs=="Ab"){
      Ev_BallLost<-Ev_PerdidaBalon}
      else if(input$kPIs=="ab"){
      Ev_BallLost<-Ev_PerdidaBalon
      }
  )
  
  # To change the min and max value of slider when database is selected
  observe({
    val<-c(minim(),maxim())
    #changing the min and max values od the sliderinput "fr" according the selected database
    updateSliderInput(session, "fr", value = val,min=val[1],max=val[2])
  })
  
  
  output$plot1AB <- renderPlotly({
    k<-dataBase()%>%subset(Fr>=m() & Fr<=M() & Bowing != BallOwing() )
    Ev_CK <- Ev_CornerKick() %>% subset(Fr>=m() & Fr<=M() ) #para dibujar los corners
    Ev_GK <- Ev_GoalKick() %>% subset(Fr>=m() & Fr<=M() ) #para dibujar los saques de meta
    Ev_BL <- Ev_BallLost() %>% subset(Fr>=m() & Fr<=M() ) #para dibujar las perdidas de balon
    
    plot1<-plot_ly(k,x=~k$Fr,y=~k$P,type="bar",name="Pressing",
                   marker= list(color='rgba(100,150,0,1)') ) %>%  #eliminamos type='scatter', mode='bar'
            #line = list(color='rgba(100,150,0,1)',width = 0.5),
            # fill='tozeroy',opacity=0.1,
            # fillcolor='rgba(100,150,0,0.25)') %>%
          add_trace(Ev_CK,x=~Ev_CK$Fr,y=~Ev_CK$P,type='bar',name="Corner Kick",opacity=1,
                               marker=list(color='rgba(255,0,0,0.50)',
                                      line = list(color='rgba(255,0,0,0.50)',dash='dot',
                                      width = 5) #,shape = "linear")
                                      )
                               ) %>%
          add_trace(Ev_GK,x=~Ev_GK$Fr,y=~Ev_GK$P,type='bar',name="Goal Kick",opacity=1,
                               marker=list(color='rgba(255,200,0,0.50)',
                                      line = list(color='rgba(255,200,0,0.50)',dash='dot',
                                      width = 5) #,shape = "linear")
                                      )
                               ) %>%
          add_trace(Ev_BL,x=~Ev_BL$Fr,y=~Ev_BL$P,type='bar',name="Ball Lost",opacity=1,
                               marker=list(color='rgba(60,60,60,0.50)',
                                      line = list(color='rgba(60,60,60,0.50)',dash='dot',
                                      width = 2,shape = 'dot') #,shape = "linear")
                                      )
                               ) %>%
        layout(xaxis=list(title='Frames'),
           yaxis=list(title='Pressing'),
           #shapes=list(type='line', x0= 10000, x1= 10000, y0=0, y1=2000, line=list(dash='dot', width=5)),
           showlegend=TRUE)
    plot1
  })

  output$plot2AB <- renderPlotly({
    k<-dataBase()%>%subset(Fr>=m() & Fr<=M() & Bowing != BallOwing() )
    plot2<-plot_ly(k,x=~k$Fr,y=~k$J,type="bar",name="Activity",opacity=0.2,
                               marker=list(color='rgba(250,200,100,0.95)',
                                           line = list(color='rgba(250,200,100,0.95)') #,width = 0.4,shape = "linear")
                                      )
                  ) %>%
            # line = list(color='rgba(250,200,100,0.5)',width = 0.4, shape = "linear"),
            # fill='tonexty',opacity=1,
            # fillcolor='rgba(250,200,100,0.25)')
          add_trace(y=~k$JsJ,type='bar',name="Activity Scalar",opacity=1,
                               marker=list(color='rgba(250,200,100,0.95)',
                                      line = list(color='rgba(250,200,100,0.95)') #,width = 0.4,shape = "linear")
                                      )
                               ) %>%
            # line = list(color='rgba(250,200,100,0.5)',width = 0.4,shape = "linear",
            # fill='tozeroy',opacity=0.1)
            #   ) %>%

    layout(xaxis=list(title='Frames'), yaxis=list(title='Pressure activities'),barmode='stack',showlegend=TRUE)
    plot2
  })
  #   output$plot3AB <- renderPlotly({
  #   k<-dataBase()%>%subset(Fr>=m() & Fr<=M() & Bowing != BallOwing())
  #   plot3<-plot_ly(k,x=~k$Fr,y=~k$Force,type='bar',name="Force",opacity=0,2,
  #                  marker=list(color='rgba(0,50,200,0.5)',
  #                             line = list(color='rgba(0,50,200,0.5)') #,width = 0.4,shape = "linear")
  #                         )
  #                 )       
  #           # line = list(color='rgba(0,50,200,0.5)',width = 0.4,shape = "linear"),
  #           # fill='tozeroy',opacity=1,
  #           # fillcolor='rgba(0,50,200,0.5)')
  #   plot3<-plot3 %>% add_trace(y=~k$FsF,type='bar', name="Force Scalar",opacity=1,
  #                              marker=list(color='rgba(0,50,200,0.5)',
  #                                     line = list(color='rgba(0,50,200,0.5)') #,width = 0.4,shape = "linear")
  #                                     )
  #                             ) %>%
  #           # line = list(color='rgba(0,50,200,0.5)',width = 0.4,shape = "linear",
  #           # fill='tozeroy',opacity=0.1)
  #           #   ) %>%
  #   layout(xaxis=list(title='Frames'), yaxis=list(title='Defensive forces'),barmode='stack',showlegend=TRUE)
  #   plot3
  # })
# 
#     output$plot4AB <- renderPlotly({
#     k<-dataBase()%>%subset(Fr>=m() & Fr<=M() & Bowing != BallOwing())
#     plot4<-plot_ly(k,x=~k$Fr,y=~k$W,type="scatter",mode='lines',name="Work",
#     #         fillcolor='rgba(250,0,0,1)',      
#     #         fill='tozeroy',opacity=0.1,
#              line = list(color='rgba(250,0,0,1)',width = 2,shape = "linear"))
#     plot4<-plot4 %>% add_trace(x=~k$Fr,y=~k$E,type='scatter',mode='lines', name="Energy",
#             line = list(color='rgba(200,50,250,1)',width = 2,shape = "linear")
#     #        fill='tozeroy',opacity=1)
#               ) %>%                         
#     layout(xaxis=list(title='Frames'), yaxis=list(title='Defensive Work and energy'),showlegend=TRUE)
#     plot4    
#   })
  
   output$image1<- renderImage({
    #if (is.null(input$image1))
    #return(NULL)
    # Read image1 width and height. These are reactive values, 
    #so this expression will re-run whenever these values change.
    width  <- session$clientData$output_image1_width
    #height <- session$clientData$output_image1_height
    #pixelsOrig <- 641*1024
    #pixelsNew <- width*height
    #imgScale <- pixelsNew/pixelsOrig
    
    #image source
    img <- readPNG("Shiny/Presion3i.png")
    # A temp file to save the output.
    outfile <- tempfile(fileext ='.png') 
    writePNG(img,target=outfile)
    
    # Return a list containing information about the image
     return(list(
        src = outfile,
        contentType="image/png",
        width = width,
        #height = height,
        alt = "Defensive metrics Image description"
        ))
    },deleteFile = "TRUE")
  
  output$image2<- renderImage({
    #if (is.null(input$image2))
    #return(NULL)
    # Read image1 width and height. These are reactive values, 
    #so this expression will re-run whenever these values change.
    width  <- session$clientData$output_image2_width
    #height <- session$clientData$output_image2_height
    
    #image source
    img <- readPNG("Shiny/Presion3b.png")

    # A temp file to save the output.
    outfile <- tempfile(fileext ='.png') 
    writePNG(img,target=outfile)
    
    # Return a list containing information about the image
     return(list(
        src = outfile,
        contentType="image/png",
        width = width,
        #height = height,
        alt = "Defensive metrics Image description"
        ))
    },deleteFile = "TRUE")

}

### SHINY APP 
shinyApp(ui=ui, server=server)




