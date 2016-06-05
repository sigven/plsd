library(shiny)
library(ggplot2)
library(markdown)


shinyUI(fluidPage(theme="bootstrap.united.css",
                  tags$head(includeScript("google-analytics.js")),
                  navbarPage("Prospective Lynch Syndrome Database       ",
                             tabPanel("Carrier without previous cancer",
                                      sidebarLayout(
                                          sidebarPanel(
                                              HTML("<p><b>Calculation of cumulative risk for first cancer</b></p><br>"),
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
                                              HTML('<br><br><br>')

                                          )
                                    )
                                ),
                             tabPanel("Carrier with previous cancer",
                                      sidebarLayout(
                                        sidebarPanel(
                                          HTML("<p><b>Calculation of cumulative risk for subsequent cancer</b></p><br>"),
                                          selectInput("ctype2", label = em("Cancer type"),
                                                      list("Any cancer type" = "PANCANCER", "Colorectal cancer" = "CRC")
                                          ),

                                          sliderInput("Age2", label = em("Current age"),
                                                      min = 25, max = 70, value = 25),

                                          HTML('<i>'),
                                          selectInput("genecarrier2", label = "Genetic variant",
                                                      list("path_MLH1" = "MLH1", "path_MSH2" = "MSH2", "path_MSH6" = "MSH6")
                                          ),
                                          HTML('</i>')
                                        ),
                                        mainPanel(
                                          plotOutput("plot_incidence_gene_subsequent_cancer"),
                                          HTML('<div style=\"position:relative; left:7em\">'),
                                          #htmlOutput('text1'),
                                          HTML('<br>'),
                                          tableOutput('table_ALL2'),
                                          HTML('</div>'),

                                          HTML('<br>'),
                                          conditionalPanel(
                                             condition = "input.genecarrier == 'PMS2'",
                                             HTML('<br><div style=\"position:relative; left:7em\">'),
                                             HTML("Warning: PMS2 results unreliable - "),
                                             actionLink("seeAbout", "see About"),
                                             HTML('</div>')
                                          ),
                                          HTML('<br><br>')

                                        )
                                      )
                             ),
                             tabPanel("About",
                                      sidebarLayout(
                                          sidebarPanel(    
                                              HTML("<p>"),
                                              strong("Recent news"),
                                              HTML("</p><br>[ Dec-08-2015 ]&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>LScarisk.org launched</i>"),
                                              HTML("<br>[ Jun-03-2016 ]&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>Risk estimates for subsequent cancers have been added</i>")
                                          ),
                                          mainPanel(
                                              includeMarkdown('about.md')
                                          )
                                      )
                            )
                             
,id="mainNavbar",fluid=T,collapsible=T,position="static-top")))
