library(shiny)

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(gridGraphics)
library(ggrepel)
library(svglite)
library(grid)
library(foreign)
library(shinyjs)
library(shinydashboard)
library(Rttf2pt1)
library(readxl)
library(extrafont)
library(extrafontdb)


# Define UI for application that draws a polar coordinates analysis
ui <- shinyUI(fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("Polar Coordinates Analysis"),
  
  # # Sidebar with a list input for select input dataset from HOISAN, focal behavior, scale, font, font size and vector width
  sidebarLayout(
    sidebarPanel(
      fileInput('Dataset', 'Select HOISAN Polar Coordinates Analysis output file',
                accept=c(".xlsx",".xls",'dltext/csv','text/comma-separated-values,text/plain','.csv')),
      radioButtons("upload_type", "Data format", choices = c("xls", "csv"), selected = "xls",inline=T),
      actionButton('reset', 'Reset'),
      conditionalPanel(
        condition = "input.upload_type=='csv'",
        radioButtons("sep", "Separator", choices = c(Comma = ',', Semicolon = ';',Tab = '\t', "Double Quote"= '"'), selected = ',',inline=T),
        radioButtons("dec", "Decimal", choices = c(Comma= ',', "Period" = '.'),selected = '.',inline=T)),
        sliderInput("Scale_01","Scale",min=0, max=100,value=c(0,100),step=0.01),
        sliderInput("ticks_y_loc","Ticks Location",min=0, max=360,value=c(135,150),step=1),
      radioButtons("multiple", "Multiple plots?", choices = c("yes", "no"), selected = "no",inline=T),
      conditionalPanel(
        condition = "input.multiple=='yes'",
        fileInput('Dataset_02', 'Dataset 02',
                  accept=c(".xlsx",".xls",'dltext/csv','text/comma-separated-values,text/plain','.csv')),
        radioButtons("upload_type_02", "Data format", choices = c("xls", "csv"), selected = "xls",inline=T),
        actionButton('reset_02', 'Reset'),
      conditionalPanel(
        condition = "input.upload_type_02=='csv'",
        radioButtons("sep", "Separator", choices = c(Comma = ',', Semicolon = ';',Tab = '\t', "Double Quote"= '"'), selected = ',',inline=T),
        radioButtons("dec", "Decimal", choices = c(Comma= ',', "Period" = '.'),selected = '.',inline=T)),
      tags$hr(),
      sliderInput("Scale_02","Scale",min=0, max=100,value=c(0,100),step=0.01),
      sliderInput("ticks_y_loc_02","Ticks Location",min=0, max=360,value=c(135,150),step=1)),
      radioButtons("multiple2", "Multiple plots?", choices = c("yes", "no"), selected = "no",inline=T),
      conditionalPanel(
        condition = "input.multiple2=='yes'",
        fileInput('Dataset_03', 'Dataset 03',
                  accept=c(".xlsx",".xls",'dltext/csv','text/comma-separated-values,text/plain','.csv')),
        radioButtons("upload_type_03", "Data format", choices = c("xls", "csv"), selected = "xls",inline=T),
        sliderInput("Scale_03","Scale",min=0, max=100,value=c(0,100),step=0.01),
        sliderInput("ticks_y_loc_03","Ticks Location",min=0, max=360,value=c(135,150),step=1),
        actionButton('reset_03', 'Reset')),
      conditionalPanel(
        condition = "input.upload_type_03=='csv'",
        radioButtons("sep", "Separator", choices = c(Comma = ',', Semicolon = ';',Tab = '\t', "Double Quote"= '"'), selected = ',',inline=T),
        radioButtons("dec", "Decimal", choices = c(Comma= ',', "Period" = '.'),selected = '.',inline=T)),
      tags$hr(),
      radioButtons("multiple3", "Multiple plots?", choices = c("yes", "no"), selected = "no",inline=T),
      conditionalPanel(
        condition = "input.multiple3=='yes'",
        fileInput('Dataset_04', 'Dataset 04',
                  accept=c(".xlsx",".xls",'dltext/csv','text/comma-separated-values,text/plain','.csv')),
        radioButtons("upload_type_04", "Data format", choices = c("xls", "csv"), selected = "xls",inline=T),
        sliderInput("Scale_04","Scale",min=0, max=100,value=c(0,100),step=0.01),
        sliderInput("ticks_y_loc_04","Ticks Location",min=0, max=360,value=c(135,150),step=1),
        actionButton('reset_04', 'Reset')),
      conditionalPanel(
        condition = "input.upload_type_04=='csv'",
        radioButtons("sep", "Separator", choices = c(Comma = ',', Semicolon = ';',Tab = '\t', "Double Quote"= '"'), selected = ',',inline=T),
        radioButtons("dec", "Decimal", choices = c(Comma= ',', "Period" = '.'),selected = '.',inline=T)),
      selectInput("Category","Focal Behavior:",""),
      selectInput("font.family","Choose Font:",choices=c("Helvetica","Times New Roman", "Arial", "Futura", "Calibri", "Lucida Sans Unicode")),
      numericInput("font.size","Font size:", value = 4, min = 4, max = 32, step = 0.5),
      numericInput("vector.width","Width:",value = 1, min = 0.5, max = 5, step = 0.05),
      radioButtons("downloadType", "Download Type", choices = list("pdf", "svg","png"),selected = "svg",inline=T),
      downloadButton(outputId = "save",label = "Download single plot"),
      downloadButton(outputId = "figura",label = "Download figure")
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Single Plot",
                 column(6,
                        plotOutput("plot",click = "plot_click"),verbatimTextOutput("info"),height="auto"),
                 column(6,
                        plotOutput("plot01",click = "plot_click"),height="auto"),
                 column(6,
                        plotOutput("plot02",click = "plot_click"),height="auto"),
                 column(6,
                        plotOutput("plot03",click = "plot_click"),height="auto")),
        tabPanel("Figure", plotOutput("plotgraph",click = "plot_click")
        )))
    
  ))
)

#### Define server logic required to draw a polar coordinates graph
server <- (function(input, output,session) {
  
  rv <- reactiveValues(data = NULL)

## Graph 1
  
  p <- reactive({
    Dataset = input$Dataset
    req(input$Dataset)
    req(!rv$clear)
    if (is.null(Dataset)) {
      return(NULL)
    } else if (input$upload_type== "xls") {
      sheet <- excel_sheets(Dataset$datapath)
      Dataset = as.data.frame(read_xlsx(Dataset$datapath,sheet= sheet,na="NeuN"))
      cat <- Dataset$Categoria
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
      
      if (is.null(input$Dataset_02)){
      updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0) + 1), step=0.01)}
      
    } else if (input$upload_type== "csv") {
      Dataset = read.csv2(Dataset$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
      
      cat <- Dataset[,1]
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_0 <- gsub("[(*)]","",Dataset$Radio)
      escala_0 <- as.numeric(gsub(",",".", escala_0))
      escala_0 <- as.numeric(gsub("[(*)]","",Dataset$Longitud))
      updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0) + 5), step=0.01)
      } 
    
    polar <- structure(list(degree = as.numeric(gsub(",",".",Dataset$Angulo)), value = escala_0,cat=as.factor(Dataset$Categoria)), .Names = c("degree","value", "Categoría"), class = "data.frame", row.names = attr(Dataset, "row.names"))
    rbPal <- colorRampPalette(c('blue',"purple",'red'))
    ybreaks = as.integer(c(input$Scale_01[1]:input$Scale_01[2])+1)
    rbPal <- colorRampPalette(c('blue',"purple",'red'))
    polar$Col <- rbPal(3)[cut(escala_0, breaks = c(0,1.96,2.58,Inf),right = FALSE)]
    data_sub=as.data.frame(subset(polar, value!=0))
    
    theme_polar <- function(yvals, xgeo = 133, ygeo = 0, 
                            color = "grey50", size = 1, 
                            ylab = "y",
                            textsize = 3,
                            ticks = 10,
                            ylimit = max(abs(yvals),
                            plot.margin=unit(c(0.25,0.25,0.25,0.25),"cm"))
    ){
      #Add ticks programatically
      ticks_y <- ybreaks
      ticks_y_sig <- c(1.96,2.58)
      
      
      #Add axis
      theme.list <- 
        list(
          ylim(0, ylimit),
          annotate("text", x = input$ticks_y_loc[2], y = ticks_y_sig, color=c("purple","red"),size = textsize,label = ticks_y_sig)
        )
      
      #Add ticks of x axis
      nlist <- length(theme.list)
      
      #Add labels to the y-ticks
      theme.list[[nlist+2]] <- annotate("text", size = textsize,x = input$ticks_y_loc[1],y = ticks_y,color=color,label = paste(ticks_y))
      
      
      return(theme.list)
    }	
    
    
    ggplot(data_sub, aes(x=degree, y=value,color = data_sub$Col))+
      scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360),expand = c(0, 0))+
      coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
      geom_text(x =  45, y = input$Scale_01[2]* 1.45, label = "I",   color="black", size = 6) +
      geom_text(x = -45, y = input$Scale_01[2]* 1.45, label = "IV",  color="black", size = 6) +
      geom_text(x = -45, y = input$Scale_01[2]*-1.45, label = "II",  color="black", size = 6) +
      geom_text(x =  45, y = input$Scale_01[2]*-1.45, label = "III", color="black", size = 6) +
      scale_color_manual(data_sub$Col,labels = c('p < .005', 'p < .001'))+
      geom_hline(aes(yintercept=1.96), colour="purple",size=0.1,linetype = 2)+
      geom_hline(aes(yintercept=2.58), colour="red",size=0.1,linetype = 2)+	
      geom_hline(yintercept = as.integer(c(input$Scale_01[1]:input$Scale_01[2])+1), colour = "black", size = 0.1,linetype = 2) +
      geom_hline(yintercept = input$Scale_01[2], colour = "black", size = 0.2,linetype = 1) +
      geom_vline(xintercept = seq(0, 360, by = 45), colour = "black", size = 0.1) +
      theme_bw()+
      theme_polar(yvals=input$Scale_01)+
      theme(panel.grid.major = element_blank())+
      geom_segment(aes(y=0, xend=degree, yend=value), arrow=arrow(length=unit(0.5,"cm")),colour=data_sub$Col,size=input$vector.width)+
      # ajustar la posición de las etiquetas de las categorías
      geom_text_repel(label = data_sub$Categoría,
                      vjust = ifelse(data_sub$degree >= 180, 0.25, -0.25), 
                      hjust = ifelse(data_sub$degree >= 180, 0.25, -0.50),
                      family=input$font.family,point.padding = 2,
                      size = input$font.size,colour=data_sub$Col) +
      theme(panel.border = element_blank(),
            axis.title   = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks   = element_blank(),
            axis.text.x  = element_text(size=11, color = "grey50"),
            panel.grid   = element_blank())
  })
  
## Graph 2
  
    p01 <- reactive({
      Dataset = input$Dataset
      req(input$Dataset)
      req(!rv$clear)
      if (is.null(Dataset)) {
        return(NULL)
      } else if (input$upload_type== "xls") {
        sheet <- excel_sheets(Dataset$datapath)
        Dataset = as.data.frame(read_xlsx(Dataset$datapath,sheet= sheet,na="NeuN"))
        cat <- Dataset$Categoria
        updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
        escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
        updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0) + 5), step=0.01)

      } else if (input$upload_type== "csv") {
        Dataset = read.csv2(Dataset$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
        
        cat <- Dataset[,1]
        updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
        escala_0 <- gsub("[(*)]","",Dataset$Radio)
        escala_0 <- as.numeric(gsub(",",".", escala_0))
        escala_0 <- as.numeric(gsub("[(*)]","",Dataset$Longitud))
        updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0) + 5))
        
      } 
    
    Dataset_02 = input$Dataset_02
    req(input$Dataset_02)
    req(!rv$clear)
    if (is.null(Dataset_02)) {
      return(NULL)
    } else if (input$upload_type_02== "xls") {
      sheet <- excel_sheets(Dataset_02$datapath)
      
      Dataset_02 = as.data.frame(read_xlsx(Dataset_02$datapath,sheet= sheet,na="NeuN"))
      
      cat <- Dataset_02$Categoria
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
      
      updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_01, escala_0)  + 5), step=0.01)
      
      if (is.null(input$Dataset_03)){
      updateSliderInput(session, "Scale_02", min = 0, max=round(max(escala_01) + 1), step=0.01)}
      
      if(input$Scale_02[2] >= input$Scale_01[2] ){
      updateSliderInput(session, "Scale_01", min = 0, max = round(max(escala_01 + 5)), step=0.01)}
      
    } else if (input$upload_type_02== "csv") {
      Dataset_02 = read.csv2(Dataset_02$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
      
      cat <- Dataset_02[,1]
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_01 <- gsub("[(*)]","",Dataset_02$Radio)
      escala_01 <- as.numeric(gsub(",",".", escala_01))
      escala_01 <- as.numeric(gsub("[(*)]","",Dataset_02$Longitud))
      updateSliderInput(session, "Scale_02", min = 0, max=round(max(c(escala_01,escala_0))+0.5))
      
    } 
    
    polar_01 <- structure(list(degree = as.numeric(gsub(",",".",Dataset_02$Angulo)), value = escala_01, cat=as.factor(Dataset_02$Categoria)), .Names = c("degree","value", "Categoría"), class = "data.frame", row.names = attr(Dataset_02, "row.names"))
    rbPal <- colorRampPalette(c('blue',"purple",'red'))
    ybreaks_01 = as.integer(c(input$Scale_02[1]:input$Scale_02[2])+1)
    rbPal <- colorRampPalette(c('blue',"purple",'red'))
    polar_01$Col <- rbPal(3)[cut(escala_01, breaks = c(0,1.96,2.58,Inf),right = FALSE)]
    data_sub_01=as.data.frame(subset(polar_01, value!=0))
    
    theme_polar <- function(yvals, xgeo = 133, ygeo = 0, 
                            color = "grey50", size = 1, 
                            ylab = "y",
                            textsize = 3,
                            ticks = 10,
                            ylimit = max(abs(yvals),
                            plot.margin=unit(c(0.25,0.25,0.25,0.25),"cm"))
    ){
      
      theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
      #Add ticks programatically
      ticks_y <- ybreaks_01
      ticks_y_sig <- c(1.96,2.58)
      
      
      #Add axis
      theme.list <- 
        list(
          ylim(0, ylimit),
          annotate("text", x = input$ticks_y_loc_02[2], y = ticks_y_sig, color=c("purple","red"),size = textsize,label = ticks_y_sig)
        )
      
      #Add ticks of x axis
      nlist <- length(theme.list)
      
      #Add labels to the y-ticks
      theme.list[[nlist+2]] <- annotate("text", size = textsize,x = input$ticks_y_loc_02[1],y = ticks_y,color=color,label = paste(ticks_y))
      
      
      return(theme.list)
    }	
    ggplot(data_sub_01, aes(x=degree, y=value,color = data_sub_01$Col))+
      scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360),expand = c(0, 0))+
      coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
      geom_segment(aes(y=0, xend=degree, yend=value), arrow=arrow(length=unit(0.5,"cm")),colour=data_sub_01$Col,size=input$vector.width)+
      geom_text_repel(label = data_sub_01$Categoría,vjust = ifelse(data_sub_01$degree >= 180, 0.25, -0.25), hjust = ifelse(data_sub_01$degree >= 180, 0.25, -0.50), family=input$font.family,point.padding = NA,size = input$font.size,colour=data_sub_01$Col) +
      geom_text(x =  45,  y = input$Scale_02[2]* 1.45, label = "I",   color="black", size = 6) +
      geom_text(x = -45,  y = input$Scale_02[2]* 1.45, label = "IV",  color="black", size = 6) +
      geom_text(x = -45,  y = input$Scale_02[2]*-1.45, label = "II",  color="black", size = 6) +
      geom_text(x =  45,  y = input$Scale_02[2]*-1.45, label = "III", color="black", size = 6) +
      scale_color_manual(data_sub_01$Col,labels = c('p < .005', 'p < .001'))+
      geom_hline(aes(yintercept=1.96), colour="purple",size=0.1,linetype = 2)+
      geom_hline(aes(yintercept=2.58), colour="red",size=0.1,linetype = 2)+	
      geom_hline(yintercept = as.integer(c(input$Scale_02[1]:input$Scale_02[2])+1), colour = "black", size = 0.1,linetype = 2) +
      geom_hline(yintercept = input$Scale_02[2], colour = "black", size = 0.2,linetype = 1) +
      geom_vline(xintercept = seq(0, 360, by = 45), colour = "black", size = 0.1) +
      theme_bw()+
      theme(panel.grid.major = element_blank())+
      theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())+
      theme_polar(yvals=input$Scale_02)
    
  })
  
    
## Graph 3
    
  p02 <- reactive({
    Dataset = input$Dataset
    req(input$Dataset)
    req(!rv$clear)
    if (is.null(Dataset)) {
      return(NULL)
    } else if (input$upload_type== "xls") {
      
      sheet <- excel_sheets(Dataset$datapath)
      Dataset = as.data.frame(read_xlsx(Dataset$datapath,sheet= sheet,na="NeuN"))
      cat <- Dataset$Categoria
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
      updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0) + 5), step=0.01)

      
    } else if (input$upload_type== "csv") {
      Dataset = read.csv2(Dataset$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
      
      cat <- Dataset[,1]
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_0 <- gsub("[(*)]","",Dataset$Radio)
      escala_0 <- as.numeric(gsub(",",".", escala_0))
      escala_0 <- as.numeric(gsub("[(*)]","",Dataset$Longitud))
      updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_01, escala_0)+ 5))
      
    } 
    
    Dataset_02 = input$Dataset_02
    req(input$Dataset_02)
    req(!rv$clear)
    if (is.null(Dataset_02)) {
      return(NULL)
    } else if (input$upload_type_02== "xls") {
      sheet <- excel_sheets(Dataset_02$datapath)
      
      Dataset_02 = as.data.frame(read_xlsx(Dataset_02$datapath,sheet= sheet,na="NeuN"))
      
      cat <- Dataset_02$Categoria
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
      
      updateSliderInput(session, "Scale_01", min = 0, max=round(max(c(escala_01, escala_0)) + 5), step=0.01)
      updateSliderInput(session, "Scale_02", min = 0, max=round(max(c(escala_01, escala_0)) + 5), step=0.01)
      
    } else if (input$upload_type_02== "csv") {
      Dataset_02 = read.csv2(Dataset_02$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
      
      cat <- Dataset_02[,1]
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_01 <- gsub("[(*)]","",Dataset_02$Radio)
      escala_01 <- as.numeric(gsub(",",".", escala_01))
      escala_01 <- as.numeric(gsub("[(*)]","",Dataset_02$Longitud))
      updateSliderInput(session, "Scale_02", min = 0, max=round(max(c(escala_01,escala_0))+ 5))
      
    } 
    
    Dataset_03 = input$Dataset_03
    req(input$Dataset_03)
    req(!rv$clear)
    if (is.null(Dataset_03)) {
      return(NULL)
    } else if (input$upload_type_03== "xls") {
      sheet <- excel_sheets(Dataset_03$datapath)
      Dataset_03 = as.data.frame(read_xlsx(Dataset_03$datapath,sheet=sheet,na="NeuN"))
      escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
      escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
      escala_03 <- as.numeric(gsub(",",".",Dataset_03[,6]))
      
      updateSliderInput(session, "Scale_01", min = 0, max=round(max(c(escala_03,escala_01,escala_0))+ 5), step=0.01)
      updateSliderInput(session, "Scale_02", min = 0, max=round(max(c(escala_03,escala_01,escala_0))+ 5), step=0.01)
      
      if (is.null(input$Dataset_04)){
      updateSliderInput(session, "Scale_03", min = 0, max=round(max(c(escala_03,escala_01,escala_0))+ 1), step=0.01)}
      
      if(input$Scale_02[2] >= input$Scale_01[2] || input$Scale_03[2] >= input$Scale_02[2] || input$Scale_03[2] >= input$Scale_01[2]){
        updateSliderInput(session, "Scale_01", min = 0, max = round(max(escala_03 + 5)), step=0.01)}
      
      if(input$Scale_02[2] >= input$Scale_01[2] || input$Scale_03[2] >= input$Scale_02[2] || input$Scale_03[2] >= input$Scale_01[2]){
        updateSliderInput(session, "Scale_02", min = 0, max = round(max(escala_03 + 5)), step=0.01)}
      
    } else if (input$upload_type_03== "csv") {
      Dataset_03 = read.csv2(Dataset_03$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
      
      cat <- Dataset_03[,1]
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala <- gsub("[(*)]","",Dataset_03$Radio)
      escala <- as.numeric(gsub(",",".", escala))
      escala <- as.numeric(gsub("[(*)]","",Dataset_03$Longitud))
      updateSliderInput(session, "Scale_03", min = 0, max=round(max(c(escala_03,escala_01,escala_0))+ 5))
      
    } 
    
    polar_02 <- structure(list(degree = as.numeric(gsub(",",".",Dataset_03$Angulo)), value = escala_03,cat=as.factor(Dataset_03$Categoria)), .Names = c("degree","value", "Categoría"), class = "data.frame", row.names = attr(Dataset_03, "row.names"))
    rbPal <- colorRampPalette(c('blue',"purple",'red'))
    ybreaks = as.integer(c(input$Scale_03[1]:input$Scale_02[2])+1)
    rbPal <- colorRampPalette(c('blue',"purple",'red'))
    polar_02$Col <- rbPal(3)[cut(escala_03, breaks = c(0,1.96,2.58,Inf),right = FALSE)]
    data_sub_02=as.data.frame(subset(polar_02, value!=0))
    
    theme_polar <- function(yvals, xgeo = 133, ygeo = 0, 
                            color = "grey50", size = 1, 
                            ylab = "y",
                            textsize = 3,
                            ticks = 10,
                            ylimit = max(abs(yvals),
                            plot.margin=unit(c(0.25,0.25,0.25,0.25),"cm"))
    ){
      
      theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
      #Add ticks programatically
      ticks_y <- ybreaks
      ticks_y_sig <- c(1.96,2.58)
      
      
      #Add axis
      theme.list <- 
        list(
          ylim(0, ylimit),
          annotate("text", x = input$ticks_y_loc_03[2], y = ticks_y_sig, color=c("purple","red"),size = textsize,label = ticks_y_sig)
        )
      
      #Add ticks of x axis
      nlist <- length(theme.list)
      
      #Add labels to the y-ticks
      theme.list[[nlist+2]] <- annotate("text", size = textsize,x = input$ticks_y_loc_03[1],y = ticks_y,color=color,label = paste(ticks_y))
      
      
      return(theme.list)
    }	
    
    
    ggplot(data_sub_02, aes(x=degree, y=value,color = data_sub_02$Col))+
      scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360),expand = c(0, 0))+
      coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
      geom_segment(aes(y=0, xend=degree, yend=value), arrow=arrow(length=unit(0.5,"cm")),colour=data_sub_02$Col,size=input$vector.width)+
      geom_text_repel(label = data_sub_02$Categoría,vjust = ifelse(data_sub_02$degree >= 180, 0.25, -0.25), hjust = ifelse(data_sub_02$degree >= 180, 0.25, -0.50), family=input$font.family,point.padding = NA,size = input$font.size,colour=data_sub_02$Col) +
      geom_text(x =  45, y = input$Scale_03[2]* 1.45, label = "I",   color="black", size = 6) +
      geom_text(x = -45, y = input$Scale_03[2]* 1.45, label = "IV",  color="black", size = 6) +
      geom_text(x = -45, y = input$Scale_03[2]*-1.45, label = "II",  color="black", size = 6) +
      geom_text(x =  45, y = input$Scale_03[2]*-1.45, label = "III", color="black", size = 6) +
      scale_color_manual(data_sub_02$Col,labels = c('p < .005', 'p < .001'))+
      geom_hline(aes(yintercept=1.96), colour="purple",size=0.1,linetype = 2)+
      geom_hline(aes(yintercept=2.58), colour="red",size=0.1,linetype = 2)+	
      geom_hline(yintercept = as.integer(c(input$Scale_03[1]:input$Scale_03[2])+1), colour = "black", size = 0.1,linetype = 2) +
      geom_hline(yintercept = input$Scale_03[2], colour = "black", size = 0.2,linetype = 1) +
      geom_vline(xintercept = seq(0, 360, by = 45), colour = "black", size = 0.1) +
      theme_bw() +
      theme(panel.grid.major = element_blank()) +
      theme_polar(yvals=input$Scale_03) +
      theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
    
  })
  

## Graph 4
  
  p03 <- reactive({
    
    Dataset = input$Dataset
    req(input$Dataset)
    req(!rv$clear)
    if (is.null(Dataset)) {
      return(NULL)
    } else if (input$upload_type== "xls") {
      sheet <- excel_sheets(Dataset$datapath)
      Dataset = as.data.frame(read_xlsx(Dataset$datapath,sheet= sheet,na="NeuN"))
      cat <- Dataset$Categoria
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
      updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0) + 5), step=0.01)
      
    } else if (input$upload_type== "csv") {
      Dataset = read.csv2(Dataset$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
      
      cat <- Dataset[,1]
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_0 <- gsub("[(*)]","",Dataset$Radio)
      escala_0 <- as.numeric(gsub(",",".", escala_0))
      escala_0 <- as.numeric(gsub("[(*)]","",Dataset$Longitud))
      updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0) + 5))
      
    } 
    
    Dataset_02 = input$Dataset_02
    req(input$Dataset_02)
    req(!rv$clear)
    if (is.null(Dataset_02)) {
      return(NULL)
    } else if (input$upload_type_02== "xls") {
      sheet <- excel_sheets(Dataset_02$datapath)
      
      Dataset_02 = as.data.frame(read_xlsx(Dataset_02$datapath,sheet= sheet,na="NeuN"))
      
      cat <- Dataset_02$Categoria
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
      
      updateSliderInput(session, "Scale_02", min = 0, max=round(max(c(escala_01,escala_0)) + 5))
      
    } else if (input$upload_type_02== "csv") {
      Dataset_02 = read.csv2(Dataset_02$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
      
      cat <- Dataset_02[,1]
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_01 <- gsub("[(*)]","",Dataset_02$Radio)
      escala_01 <- as.numeric(gsub(",",".", escala_01))
      escala_01 <- as.numeric(gsub("[(*)]","",Dataset_02$Longitud))
      updateSliderInput(session, "Scale_02", min = 0, max=round(max(c(escala_01,escala_0))+ 5))
      
    } 
    
    Dataset_03 = input$Dataset_03
    req(input$Dataset_03)
    req(!rv$clear)
    if (is.null(Dataset_03)) {
      return(NULL)
    } else if (input$upload_type_03== "xls") {
      sheet <- excel_sheets(Dataset_03$datapath)
      Dataset_03 = as.data.frame(read_xlsx(Dataset_03$datapath,sheet=sheet,na="NeuN"))
      escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
      escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
      escala_03 <- as.numeric(gsub(",",".",Dataset_03[,6]))
      updateSliderInput(session, "Scale_03", min = 0, max=round(max(c(escala_03,escala_01,escala_0))+ 5))
      
    } else if (input$upload_type_03== "csv") {
      Dataset_03 = read.csv2(Dataset_03$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
      
      cat <- Dataset_03[,1]
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala <- gsub("[(*)]","",Dataset_03$Radio)
      escala <- as.numeric(gsub(",",".", escala))
      escala <- as.numeric(gsub("[(*)]","",Dataset_03$Longitud))
      updateSliderInput(session, "Scale_03", min = 0, max=round(max(c(escala_03,escala_01,escala_0))+ 5))
      
    } 
    
    
    Dataset_04 = input$Dataset_04
    req(input$Dataset_04)
    req(!rv$clear)
    if (is.null(Dataset_04)) {
      return(NULL)
    } else if (input$upload_type_04== "xls") {
      sheet <- excel_sheets(Dataset_04$datapath)
      Dataset_04 = as.data.frame(read_xlsx(Dataset_04$datapath,sheet=sheet,na="NeuN"))
      escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
      escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
      escala_03 <- as.numeric(gsub(",",".",Dataset_03[,6]))
      escala_04 <- as.numeric(gsub(",",".",Dataset_04[,6]))
      
      updateSliderInput(session, "Scale_04", min = 0, max=round(max(c(escala_04, escala_03, escala_01, escala_0)) + 1), step=0.01)
 
      if(input$Scale_04[2] >= input$Scale_01[2] || input$Scale_04[2] >= input$Scale_02[2] || input$Scale_04[2] >= input$Scale_03[2]){
        updateSliderInput(session, "Scale_01", min = 0, max = round(max(escala_04 + 5)), step=0.01)
        updateSliderInput(session, "Scale_02", min = 0, max = round(max(escala_04 + 5)), step=0.01)
        updateSliderInput(session, "Scale_03", min = 0, max = round(max(escala_04 + 5)), step=0.01)}
      
      if(input$Scale_04[2] >= input$Scale_01[2]){
        updateSliderInput(session, "Scale_01", min = 0, max = round(max(escala_04 + 5)), step=0.01)}
      
      
    } else if (input$upload_type_04== "csv") {
      Dataset_04 = read.csv2(Dataset_04$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
      
      cat <- Dataset_04[,1]
      updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
      escala_04 <- gsub("[(*)]","",Dataset_04$Radio)
      escala_04 <- as.numeric(gsub(",",".", escala_04))
      escala_04 <- as.numeric(gsub("[(*)]","",Dataset_04$Longitud))
      updateSliderInput(session, "Scale", min = 0, max=round(max(c(escala_04, escala_03, escala_01, escala_0)) + 5))
      
    } 
    
    polar_03 <- structure(list(degree = as.numeric(gsub(",",".",Dataset_04$Angulo)), value = escala_04,cat=as.factor(Dataset_04$Categoria)), .Names = c("degree","value", "Categoría"), class = "data.frame", row.names = attr(Dataset_04, "row.names"))
    rbPal <- colorRampPalette(c('blue',"purple",'red'))
    ybreaks = as.integer(c(input$Scale_04[1]:input$Scale_04[2])+1)
    rbPal <- colorRampPalette(c('blue',"purple",'red'))
    polar_03$Col <- rbPal(3)[cut(escala_04, breaks = c(0,1.96,2.58,Inf),right = FALSE)]
    data_sub_03=as.data.frame(subset(polar_03, value!=0))
  
    
    theme_polar <- function(yvals, xgeo = 133, ygeo = 0, 
                            color = "grey50", size = 1, 
                            ylab = "y",
                            textsize = 3,
                            ticks = 10,
                            ylimit = max(abs(yvals),
                            plot.margin=unit(c(0.25,0.25,0.25,0.25),"cm"))
    ){
      
      theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
      #Add ticks programatically
      ticks_y <- ybreaks
      ticks_y_sig <- c(1.96,2.58)
      
      
      #Add axis
      theme.list <- 
        list(
          ylim(0, ylimit),
          annotate("text", x = input$ticks_y_loc_04[2], y = ticks_y_sig, color=c("purple","red"),size = textsize,label = ticks_y_sig)
        )
      
      #Add ticks of x axis
      nlist <- length(theme.list)
      
      #Add labels to the y-ticks
      theme.list[[nlist+2]] <- annotate("text", size = textsize,x = input$ticks_y_loc_04[1],y = ticks_y,color=color,label = paste(ticks_y))
      
      
      return(theme.list)
    }	
    
    
    ggplot(data_sub_03, aes(x=degree, y=value,color = data_sub_03$Col))+
      scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360),expand = c(0, 0))+
      coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
      geom_segment(aes(y=0, xend=degree, yend=value), arrow=arrow(length=unit(0.5,"cm")),colour=data_sub_03$Col,size=input$vector.width)+
      geom_text_repel(label = data_sub_03$Categoría,vjust = ifelse(data_sub_03$degree >= 180, 0.25, -0.25), hjust = ifelse(data_sub_03$degree >= 180, 0.25, -0.50), family=input$font.family,point.padding = NA,size = input$font.size,colour=data_sub_03$Col) +
      geom_text(x =  45, y = input$Scale_04[2]* 1.45, label = "I"  , color="black", size = 6) +
      geom_text(x = -45, y = input$Scale_04[2]* 1.45, label = "IV" , color="black", size = 6) +
      geom_text(x = -45, y = input$Scale_04[2]*-1.45, label = "II" , color="black", size = 6) +
      geom_text(x =  45, y = input$Scale_04[2]*-1.45, label = "III", color="black", size = 6) +
      scale_color_manual(data_sub_03$Col,labels = c('p < .005', 'p < .001'))+
      geom_hline(aes(yintercept=1.96), colour="purple",size=0.1,linetype = 2)+
      geom_hline(aes(yintercept=2.58), colour="red",size=0.1,linetype = 2)+	
      geom_hline(yintercept = as.integer(ybreaks), colour = "black", size = 0.1,linetype = 2) +
      geom_hline(yintercept = input$Scale_04[2], colour = "black", size = 0.2,linetype = 1) +
      geom_vline(xintercept = seq(0, 360, by = 45), colour = "black", size = 0.1) +
      theme_bw()+
      theme(panel.grid.major = element_blank()) +
      theme_polar(yvals=input$Scale_04) +
      theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
    
  })
  

# Output
  
## Figure
  
  figure <- reactive({
    ptlist <- list(p(),p01(),p02(),p03())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    if (length(ptlist)==0) return(NULL)
    figure <- grid.arrange(grobs=ptlist)
    print(figure)
    return(figure)
  })

  observeEvent(input$Dataset, {
    rv$clear <- FALSE
  }, priority = 1000)
  
  observeEvent(input$Dataset_02, {
    rv$clear <- FALSE
  }, priority = 1000)
  
  observeEvent(input$reset, {
    rv$data <- NULL
    rv$clear <- TRUE
    reset('inFile')
  }, priority = 1000)
  
  observeEvent(input$reset_02, {
    rv$data <- NULL
    rv$clear <- TRUE
    reset('inFile')
  }, priority = 1000)
  
## Individual Plots
  
  output$plot <- renderPlot({p()} ,height = function(){session$clientData$output_plot_width})
  output$plot01 <- renderPlot({p01()} ,height = function(){session$clientData$output_plot01_width})
  output$plot02 <- renderPlot({p02()} ,height = function(){session$clientData$output_plot02_width})
  output$plot03 <- renderPlot({p03()} ,height = function(){session$clientData$output_plot03_width})
   
# 2 graphs
  observe({
    Dataset = input$Dataset
    req(input$Dataset)
    req(!rv$clear)
    if (is.null(Dataset)) {
      return(NULL)
      
    } else if (is.null(input$Dataset_02)) {
      output$plotgraph = renderPlot({p()},height = function(){session$clientData$output_plot01_width})
      
    } else if (input$multiple=='yes') {
      
      output$plotgraph = renderPlot({
        ptlist <- list(p(),p01())
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete] 
        if (length(ptlist)==0) return(NULL)
        figure <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
      },height = function(){session$clientData$output_plot01_width})
      
      output$figura <- downloadHandler(
        filename = function(){
          paste("figure",input$downloadType,sep=".")
        },
        content = function(file){
          ptlist <- list(p(),p01())
          to_delete <- !sapply(ptlist,is.null)
          ptlist <- ptlist[to_delete] 
          if (length(ptlist)==0) return(NULL)
          f2 <-  grid.arrange(grobs=ptlist,ncol=length(ptlist))
          ggsave(file, f2, scale = 2)
        })
      
    } else if (input$multiple2=='yes')
      
      output$plotgraph = renderPlot({
        ptlist <- list(p(),p01(),p02())
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete] 
        if (length(ptlist)==0) return(NULL)
        figure <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
        print(figure)
        return(figure)
      },height = function(){session$clientData$output_plot01_width})
    
  })
  
# 3 graphs
  
  observe({
    Dataset = input$Dataset
    req(input$Dataset)
    req(!rv$clear)
    if (is.null(Dataset)) {
      return(NULL)
    }
    else if (is.null(input$Dataset_03)) {
      output$plotgraph = renderPlot({p()},height = function(){session$clientData$output_plot01_width})
      
    } else if (input$multiple2=='yes') {
      
      output$plotgraph = renderPlot({
        ptlist <- list(p(),p01(),p02())
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete] 
        if (length(ptlist)==0) return(NULL)
        figure <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
        print(figure)
        return(figure)
      },height = function(){session$clientData$output_plot01_width})
      
      
      output$figura <- downloadHandler(
        filename = function(){
          paste("figure",input$downloadType,sep=".")
        },
        content = function(file){
          ptlist <- list(p(), p01(), p02())
          f3 <-  grid.arrange(grobs=ptlist, ncol=length(ptlist))
          ggsave(file, f3, scale = 2)
        })
      
    } else if (input$multiple3=='yes')
      
      output$plotgraph = renderPlot({
        ptlist <- list(p(),p01(),p02(),p03())
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete] 
        if (length(ptlist)==0) return(NULL)
        figure <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
        print(figure)
        return(figure)
      },height = function(){session$clientData$output_plot01_width})
    
   
  })

# 4 graphs
  
  observe({
    Dataset = input$Dataset
    req(input$Dataset)
    req(!rv$clear)
    if (is.null(Dataset)) {
      return(NULL)
    }
    else if (is.null(input$Dataset_04)) {
      output$plotgraph = renderPlot({p()},height = function(){session$clientData$output_plot01_width})
      
    } else if (input$multiple2=='yes') {
      
      output$plotgraph = renderPlot({
        ptlist <- list(p(),p01(),p02(),p03())
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete] 
        if (length(ptlist)==0) return(NULL)
        figure <- grid.arrange(grobs=ptlist)
        print(figure)
        return(figure)
      },height = function(){session$clientData$output_plot01_width})
      
    } else if (input$multiple3=='yes')
      
      output$plotgraph = renderPlot({
        ptlist <- list(p(),p01(),p02(),p03())
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete] 
        if (length(ptlist)==0) return(NULL)
        figure <- grid.arrange(grobs=ptlist)
        print(figure)
        return(figure)
      },height = function(){session$clientData$output_plot01_width})
    
    output$figura <- downloadHandler(
      filename = function(){
        paste("figure",input$downloadType,sep=".")
      },
      content = function(file){
        ptlist <- list(p(), p01(), p02(),p03())
        f4 <-  grid.arrange(grobs=ptlist, ncol=2, nrow = 2)
        ggsave(file, f4, scale = 2)
      })
   
  })
  
  output$save <- downloadHandler(
    filename = function(){
      paste("polar",input$downloadType,sep=".")
    },
    content = function(file){
      if (input$downloadType == "svg"){
        ggsave(file,width=7,height = 7, units="in")
      } else if (input$downloadType == "pdf") {
        ggsave(file,width=7, height = 7, units="in")
      } else if (input$downloadType == "png") {
        ggsave(file,width=7,height = 7, units="in")
      }
    })
  
})

# Run the application 
shinyApp(ui = ui, server = server)