library(shiny)
library(ggplot2)
library(markdown)


shinyUI(fluidPage(theme="bootstrap.yeti.css",
                  tags$head(includeScript("google-analytics.js")),
                  navbarPage("Prospective Lynch Syndrome Database",
                             tabPanel("Carrier without cancer",
                                      sidebarLayout(
                                          sidebarPanel(    
                                              HTML("<p><b>Calculation of cumulative cancer risk</b></p><br>"),
                                              selectInput("ctype", label = em("Cancer type"), 
                                                          list("Any cancer type" = "PANCANCER", "Colorectal cancer" = "CRC", "Ovarian cancer" = "OVARIAN", "Endometrial cancer" = "END","Urine bladder/kidney/ureter cancer" = "URO", "Gastric/small intestine/biliary tract/pancreas cancer" = "UGI")
                                              ),
                                              
                                              sliderInput("Age", label = em("Current age"),
                                                          min = 25, max = 70, value = 25),
                                             
                                              selectInput("Sex", label=em("Gender"),
                                                    list("Female" = "FEMALE", "Male" = "MALE")
                                             ),
                                             HTML('<i>'),
                                                selectInput("genecarrier", label = "Genetic variant", 
                                                          list("path_MLH1" = "MLH1", "path_MSH2" = "MSH2", "path_MSH6" = "MSH6", "path_PMS2"="PMS2")
                                                 ),
                                             HTML('</i>')
                                          ),
                                          mainPanel(
                                                  #tags$head(HTML("<script type='text/javascript' src='js/google.js'></script>")),
                                                  #tabsetPanel(
                                                      #HTML('<br><br>'),
                                                      #tabPanel("Plot", plotOutput("plot_incidence_gene")),
                                                      #tabPanel("Table",tableOutput("table_ALL")), type="tabs"
                                                  #),
                                                  
                                                  plotOutput("plot_incidence_gene"),
                                                  HTML('<div style=\"position:relative; left:7em\">'),
                                                  htmlOutput('text1'),
                                                  HTML('<br>'),
                                                  tableOutput('table_ALL'),
                                                  HTML('</div>'),
                                              
                                              
                                              #HTML('<br><br>'),
                                              conditionalPanel(
                                                  condition = "input.genecarrier == 'PMS2'",
                                                  HTML('<br><div style=\"position:relative; left:7em\">'),
                                                  HTML("Warning: PMS2 results unreliable - "),
                                                  actionLink("seeAbout", "see About"),
                                                  HTML('</div>')
                                              ),
                                              HTML('<br><br>'),
                                              #HTML('<a href=\"javascript:window.print()\">Print this page</a>'),
                                             #htmlOutput("UI.Date"),
                                              HTML('<br><br>')
                                              
                                          )
                                    )
                                ),
                             tabPanel("About",
                                      sidebarLayout(
                                          sidebarPanel(    
                                              HTML("<p>"),
                                              strong("Recent news"),
                                              HTML("</p><br>[ Dec-08-2015 ]&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>LScarisk.org launched</i>") 
                                          ),
                                          mainPanel(
                                              includeMarkdown('about.md')
                                          )
                                      )
                            )
                             
,id="mainNavbar")))
