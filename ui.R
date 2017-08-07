library(shiny)
library(ggplot2)
library(markdown)
library(shinythemes)


shinyUI(fluidPage(theme = shinythemes::shinytheme("united"),
                  tags$head(includeScript("google-analytics.js")),
                  navbarPage("Prospective Lynch Syndrome Database (PLSD) - cumulative risk for cancer by age, genetic variant, and gender",
                             tabPanel("Any cancer",
                                      sidebarLayout(
                                        sidebarPanel(
                                          HTML("<p><b>Calculation of cumulative risk for cancer in selected organ(s) irrespective of cancer(s) in any other organ</b></p><br>"),
                                          selectInput("ctype3", label = em("Organ"),
                                                      list("Any organ" = "PANCANCER", "Colon" = "153","Stomach" = "151","Duodenum" = "152","Sigmoid/rectum" = "154","Bile duct/gall bladder" = "156","Pancreas" = "157","Breast" = "174","Endometrium" = "182","Ovaries" = "183","Prostate" = "185","Urinary bladder" = "188","Ureter/kidney" = "189","Brain" = "191","Stomach/duodenum/bile duct/gall bladder/pancreas" = "151_152_156_157","Urine bladder/ureter/kidney" = "188_189","Endometrium/ovaries" = "182_183", "Colon/sigmoid/rectum" = "153_154")
                                          ),
                                          
                                          sliderInput("Age3", label = em("Current age"),min = 25, max = 70, value = 25),
                                          conditionalPanel(
                                            condition = "input.ctype3 == '182' || input.ctype3 == '174' || input.ctype3 == '183' || input.ctype3 == '182_183'",
                                            selectInput("Sex3", label=em("Gender"),list("Female" = "FEMALE"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.ctype3 == '185'",
                                            selectInput("Sex4", label=em("Gender"),list("Male" = "MALE"))
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.ctype3 == '153' || input.ctype3 == '154'",
                                            selectInput("Sex5", label=em("Gender"),list("Female" = "FEMALE", "Male" = "MALE"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.ctype3 == 'PANCANCER' || input.ctype3 == '151' || input.ctype3 == '152' || input.ctype3 == '156' || input.ctype3 == '157' || input.ctype3 == '188' || input.ctype3 == '189' || input.ctype3 == '191' || input.ctype3 == '151_152_156_157' || input.ctype3 == '188_189' || input.ctype3 == '153_154'",
                                            selectInput("Sex6", label=em("Gender"),list("Both genders combined" = "BOTH"))
                                          ),
                                          
                                          HTML('<i>'),
                                          selectInput("genecarrier3", label = "Genetic variant",
                                                      list("path_MLH1" = "MLH1", "path_MSH2" = "MSH2", "path_MSH6" = "MSH6", "path_PMS2" = 'PMS2')
                                          ),
                                          HTML('</i>')
                                        ),
                                        mainPanel(
                                          plotOutput("plot_incidence_gene_organ_cancer", height = "500px"),
                                          HTML('<div style=\"position:relative; left:6em\">'),
                                          #htmlOutput('text1'),
                                          HTML('<br>'),
                                          #DT::dataTableOutput('table_ALL3'),
                                          tableOutput('table_ALL3'),
                                          HTML('</div>'),
                                          
                                          HTML('<br>'),
                                          conditionalPanel(
                                            condition = "input.genecarrier3 == 'PMS2'",
                                            HTML('<br><div style=\"position:relative; left:7em\">'),
                                            HTML("Warning: PMS2 results unreliable - "),
                                            actionLink("seeAbout", "see About"),
                                            HTML('</div>')
                                          ),
                                          HTML('<br><br>')
                                          
                                        )
                                      )
                             ),
                  #navbarPage("Prospective Lynch Syndrome Database (PLSD)",
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
                                          
                                          plotOutput("plot_incidence_gene", height = "500px"),
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
                             #tabPanel("|"),
                             #shinythemes::themeSelector(),
                             
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
                                          plotOutput("plot_incidence_gene_subsequent_cancer", height = "500px"),
                                          HTML('<div style=\"position:relative; left:7em\">'),
                                          #htmlOutput('text1'),
                                          HTML('<br>'),
                                          tableOutput('table_ALL2'),
                                          HTML('</div>'),
                                          
                                          HTML('<br>'),
                                          conditionalPanel(
                                            condition = "input.genecarrier2 == 'PMS2'",
                                            HTML('<br><div style=\"position:relative; left:7em\">'),
                                            HTML("Warning: PMS2 results unreliable - "),
                                            actionLink("seeAbout", "see About"),
                                            HTML('</div>')
                                          ),
                                          HTML('<br><br>')
                                          
                                        )
                                      )
                             ),
                             
                            #tabPanel('|'),
                             #shinythemes::themeSelector(),
                             tabPanel("About",
                                      sidebarLayout(
                                        sidebarPanel(    
                                          HTML("<p>"),
                                          strong("Recent news"),
                                          HTML("</p><br>[ Dec-08-2015 ]&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>LScarisk.org launched</i>"),
                                          HTML("<br>[ Jun-03-2016 ]&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>Risk estimates for subsequent cancers have been added</i>"),
                                          HTML("<br>[ Jul-31-2017 ]&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>Risk estimates up to 75 years for any cancer have been added</i>")
                                        ),
                                        mainPanel(
                                          includeMarkdown('about.md')
                                        )
                                      )
                             )
                             
                             ,id="mainNavbar",fluid=F,collapsible=T,position="static-top", windowTitle="PLSD")))
