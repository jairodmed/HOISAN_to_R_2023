#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DiagrammeR)

# Define UI
ui <-  navbarPage("ObseRtools",
                  theme=shinythemes::shinytheme("flatly"),
                  
                  ### WELCOME PAGE ----
                  tabPanel("Welcome",
                           
                           # code for ggplot plot resize
                           tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                           # end of code
                           
                           fluidRow(
                               column(9,
                                      wellPanel(
                                          
                                          h1("ObseRtools: The shiny app for observational methodology",align = "center"),
                                          br(),
                                          h4(em(strong("ObseRtools: An R-tool for observational data analysis")),align = "center"),
                                          br(),
                                          h6("Rodríguez-Medina, J., Hernández-Mendo, A., & Anguera, M. T. (2021).", strong(" De HOISAN a R: Una aplicación web interactiva para la representación gráfica de coordenadas polares."), align="center"),
                                          br(),
                                          br(),
                                          h2("Features"),
                                          p(em("ObseRtools")," is an open-source tool for executing a comprehensive observational data analysis."),
                                          br(),
                                          p("It was programmed in R language to be flexible and facilitate integration with other statistical and graphical packages.
                                 Indeed, ObseRtools has the flexibility to be quickly upgraded and integrated. 
                                 Its development can address a large and active community of developers formed by prominent researchers."),
                                          br(),
                                          p(em("ObseRtools"),"provides various routines for importing data from HOISAN, 
                                 Lince, and SDIS-QSEQ, performing data analysis and building data matrices for descriptive, lag sequential and polar coordinates analysis."),
                                          br(),
                                          p("For an introduction and live examples, visit the ",
                                            a("ObseRtools website.", 
                                              href = "https://github.com/jairodmed/HOISAN_to_R")),
                                          br(),
                                          br(),
                                          br(),
                                          h2("Example"),
                                          br(),
                                          p("Step 1 - Download an example at the following", a("link.",href = "https://uvaes-my.sharepoint.com/:x:/g/personal/jairo_rodriguez_medina_uva_es/EYqpw2dCNI1DniRJmplPfEcBs6bFNJngZ-c1R1I-sI1-xg?e=eKYVdE", target="_blank")),  
                                          p("Step 2 - In the ",strong("Load ") ,"menu, select ",strong("'HOISAN'")," as database and ",strong("'xlsx'")," as file format."),
                                          p("Step 3 - Choose and load the file", strong("canal_riviere_madre_inicia_juego_x_normotípico.xls")," using the ",strong("browse")," button."),
                                          p("Step 4 - ", strong(em("Then, enjoy working with the app!"))),
                                          br()
                                      )
                                      
                               )
                           )
                  ),
                  
                  
                  
                  # navbarMenu(("About"),
                  #            tabPanel(title = "Help", 
                  #                     includeHTML("bibliometrix-vignette.html"))
                  #            ),
                  
                  
                  ### Loading page ----
                  ### DATASET MENU ----
                  navbarMenu("Data",
                             "  ",
                             "  ",
                             ### Import or Load files ####
                             tabPanel("Import or Load files",
                                      sidebarLayout(
                                          sidebarPanel(
                                              width = 3,
                                              h3(em(strong("Import or Load "))),
                                              br(),
                                              selectInput(
                                                  "load",
                                                  label = "Please, choose what to do",
                                                  choices = c(
                                                      " " = "null",
                                                      "Import raw file(s)" = "import",
                                                      "Load HOISAN file(s)" = "load",
                                                      "Use a sample data" = "demo"
                                                  ),
                                                  selected = "null"
                                              ),
                                              conditionalPanel(
                                                  condition = "input.load == 'demo'",
                                                  helpText(h4(strong("Canal y Rivière, 1999")),
                                                           h5(strong("Dataset 'Canal, R., y Rivière, Á. (1999)'")),
                                                           em("Estudio del juego y las expresiones emocionales en los niños autistas",
                                                              "mediante el análisis secuencial de retardo"),
                                                           br(),
                                                           em("En, M. T. Anguera (coord.), Observación en Psicología Clínica: Aplicaciones")
                                                  )
                                              ),
                                              #br(),
                                              conditionalPanel(
                                                  condition = "input.load == 'import'",
                                                  selectInput(
                                                      "dbsource",
                                                      label = "Database",
                                                      choices = c(
                                                          "HOISAN" = "hoisan",
                                                          "LINCEPLUS" = "lince",
                                                          "SDIS-GSEQ" = "sdis"
                                                      ),
                                                      selected = "hoisan"
                                                  )
                                              ),
                                              conditionalPanel(
                                                  condition = "input.load != 'null' & input.load != 'demo'",
                                                  conditionalPanel(
                                                      condition = "input.load == 'load'",
                                                      helpText(em("Load a collection in XLSX or CSV format previously exported from HOISAN")
                                                      )),
                                                  fileInput(
                                                      "file1",
                                                      "Choose a file",
                                                      multiple = FALSE,
                                                      accept = c(
                                                          ".csv",
                                                          ".xlsx",
                                                          ".xls"
                                                      )
                                                  )
                                              ),
                                              #h6("Here accept single .txt/.bib/.csv/.xslx/.RData files, or multiple .txt/.bib/.csv files compressed in a single .zip archive."),
                                              conditionalPanel(condition = "input.load != 'null'",
                                                               actionButton("applyLoad", "Start ")),
                                              tags$hr(),
                                              
                                              uiOutput("textLog"),
                                              #shinycssloaders::withSpinner(verbatimTextOutput("log")),
                                              
                                              tags$hr(),
                                              
                                              h3(em(strong(
                                                  "Export a ObseRtools file "
                                              ))),
                                              br(),
                                              
                                              selectInput(
                                                  'save_file',
                                                  'Save as:',
                                                  choices = c(
                                                      ' ' = 'null',
                                                      'Excel' = 'xlsx',
                                                      'R Data Format' = 'RData'
                                                  ),
                                                  selected = 'null'
                                              ),
                                              conditionalPanel(condition = "input.save_file != 'null'",
                                                               downloadButton("collection.save", "Export"))
                                          ),
                                          mainPanel(
                                              ## color of datatable
                                              tags$head(tags$style(
                                                  HTML(
                                                      "table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #9c4242 !important;
                                  }
                                  "
                                                  )
                                              )),
                                              tags$style(
                                                  HTML(
                                                      ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                  color: #000000 !important;
                  }"
                                                  )
                                              ),
                                              shinycssloaders::withSpinner(DT::DTOutput("contents"))
                                          )
                                      ))
                             
                  ),
                  
                  ### Lag Sequential Analysis ----
                  
                  navbarMenu("Lag Sequential Analysis",
                             tabPanel(title = "Lag Sequential Analysis",
                                      sidebarLayout(
                                          sidebarPanel(title = "Lag Sequential Analysis",
                                                       fileInput('SDIS', 'Select sds or mds file',
                                                                 accept=c(".xlsx",".xls",'dltext/csv','text/comma-separated-values,text/plain','.csv')),
                                                       radioButtons("upload_type", "Data format", choices = c("xls", "csv"), selected = "xls",inline=T),
                                                       conditionalPanel(
                                                           condition = "input.upload_type=='csv'",
                                                           radioButtons("sep", "Separator", choices = c(Comma = ',', Semicolon = ';',Tab = '\t', "Double Quote"= '"'), selected = ',',inline=T),
                                                           radioButtons("dec", "Decimal", choices = c(Comma= ',', "Period" = '.'),selected = '.',inline=T)),
                                                       numericInput("minlag", "From Lag:", 1, min = -10, max = 100, step = 1, width = 75), 
                                                       numericInput("maxlag", "To Lag:", 1, min = -10, max = 100, step = 1, width = 75),
                                                       
                                          ),
                                          mainPanel(
                                              tabsetPanel(
                                                  tabPanel("Data", tableOutput("data")),
                                                  tabPanel("Joint Frequency",
                                                           shiny::fluidRow(
                                                               column(width = 6, uiOutput("jf")),
                                                               column(width = 6, uiOutput("jf_plot")))),
                                                  tabPanel("Exp. Frequency",
                                                           shiny::fluidRow(
                                                               column(width = 6, uiOutput("exp")),
                                                               column(width = 6, uiOutput("exp_plot")))),
                                                  tabPanel("Cond. Prob.", 
                                                           shiny::fluidRow(
                                                               column(width = 6, uiOutput("cond")),
                                                               column(width = 6, uiOutput("cond_plot")))),
                                                  tabPanel("Residual", uiOutput("res")),
                                                  tabPanel("Adj. Residual",
                                                           shiny::fluidRow(
                                                               column(width = 6, uiOutput("adj")),
                                                               column(width = 6, uiOutput("adj_plot")))),
                                                  tabPanel("p-value adj res.", uiOutput("p_val")),
                                                  tabPanel("Pattern", 
                                                           shiny::fluidRow(
                                                               column(width = 6, uiOutput("ptt")),
                                                               column(width = 6, grVizOutput("ptt_plot",width = "100%", height = "760px")))),
                                                  tabPanel("Pattern Plot", grVizOutput("ptt_plot_2",width = "100%", height = "760px"))
                                              ))))
                             
                  ),
                  
                  ### Polar Coordinates Analysis ----
                  
                  navbarMenu("Polar Coordinates Analysis",
                             tabPanel("Polar Coordinates Analysis",
                                      sidebarLayout(
                                          sidebarPanel(title = "Polar Coordinates Analysis",
                                                       fileInput('Dataset', 'Select HOISAN Polar Coordinates Analysis output file',
                                                                 accept=c(".xlsx",".xls",'dltext/csv','text/comma-separated-values,text/plain','.csv')),
                                                       radioButtons("upload_type", "Data format", choices = c("xls", "csv"), selected = "xls",inline=T),
                                                       actionButton('reset', 'Reset'),
                                                       conditionalPanel(
                                                           condition = "input.upload_type=='csv'",
                                                           radioButtons("sep", "Separator", choices = c(Comma = ',', Semicolon = ';',Tab = '\t', "Double Quote"= '"'), selected = ',',inline=T),
                                                           radioButtons("dec", "Decimal", choices = c(Comma= ',', "Period" = '.'),selected = '.',inline=T)),
                                                       sliderInput("Scale_01","Scale",min=0, max=100,value=c(0,100),step=0.01),
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
                                                           sliderInput("Scale_02","Scale",min=0, max=5,value=c(0,5),step=0.01)),
                                                       radioButtons("multiple2", "Multiple plots?", choices = c("yes", "no"), selected = "no",inline=T),
                                                       tags$hr(),
                                                       conditionalPanel(
                                                           condition = "input.multiple2=='yes'",
                                                           fileInput('Dataset_03', 'Dataset 03',
                                                                     accept=c(".xlsx",".xls",'dltext/csv','text/comma-separated-values,text/plain','.csv')),
                                                           radioButtons("upload_type_03", "Data format", choices = c("xls", "csv"), selected = "xls",inline=T),
                                                           sliderInput("Scale_03","Scale",min=0, max=5,value=c(0,5),step=0.01),
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
                                                           sliderInput("Scale_04","Scale",min=0, max=5,value=c(0,5),step=0.01),
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
                                                       downloadButton(outputId = "figura",label = "Descargar figura")
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
                                                  )))))
                             
                  ),
                  
                  ### Register button ----
                  navbarMenu("Register",
                             tabPanel(actionLink("stop_radiant", "Stop", icon = icon("power-off"), 
                                                 onclick = "setTimeout(function(){window.close();}, 100); ")
                             )
                             
                             
                             
                  ),
                  
                  ### Quit button ----
                  navbarMenu("Quit",
                             tabPanel(actionLink("stop_radiant", "Stop", icon = icon("power-off"), 
                                                 onclick = "setTimeout(function(){window.close();}, 100); ")
                             )
                             
                             
                             
                  ) 
                  
                  ## End of UI           
)