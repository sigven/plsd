library(shiny)
library(ggplot2)
library(markdown)


shinyUI(fluidPage(theme="bootstrap.united.css",
                  navbarPage("Lynch Syndrome Prospective Database",
                             tabPanel("Mutation carrier without cancer",
                                      sidebarLayout(
                                          sidebarPanel(    
                                              HTML("<p></p>"),
                                              selectInput("ctype", label = "Cancer type", 
                                                          list("Any cancer type" = "PANCANCER", "Colorectal cancer" = "CRC", "Ovarian cancer" = "OVARY", "Endometrial cancer" = "END","Urine bladder/kidney/ureter cancer" = "URO", "Gastric/small intestine/biliary tract/pancreas cancer" = "UGI")
                                              ),
                                              
                                              sliderInput("Age", label = "Age",
                                                          min = 25, max = 70, value = 25),
                                             
                                              selectInput("Sex", label="Sex",
                                                    list("Female" = "FEMALE", "Male" = "MALE")
                                             ),
                                                selectInput("genecarrier", label = "Gene", 
                                                          list("MLH1" = "MLH1", "MSH2" = "MSH2", "MSH6" = "MSH6", "PMS2"="PMS2")
                                                 )
                                          ),
                                          mainPanel(
                                                  #tabsetPanel(
                                                      #HTML('<br><br>'),
                                                      #tabPanel("Plot", plotOutput("plot_incidence_gene")),
                                                      #tabPanel("Table",tableOutput("table_ALL")), type="tabs"
                                                  #),
                                                  htmlOutput('text1'),
                                                  plotOutput("plot_incidence_gene"),
                                                  HTML('<div style=\"position:relative; left:7em\">'),
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
                                              HTML("</p><br>[ 01-21-2015 ]&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>LScarisk.org launched</i>") 
                                          ),
                                          mainPanel(
                                              includeMarkdown('about.md')
                                          )
                                      )
                            )
                             
,id="mainNavbar")))
