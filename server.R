library(shiny)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(magrittr)

load('ls_risk_data.rda')

shinyServer(function(input, output, session){

    values <- reactiveValues()

    observe({
            values$lastLinkValue <- input$seeAbout[1] - 1
            if (input$seeAbout[1] != values$lastLinkValue && values$lastLinkValue >= 0){
                updateTabsetPanel(session, "mainNavbar", selected = "About")
            }
    })
 
    output$UI.Date <- renderText({
        Commentary.Date <- paste0("<p style=\"font-size:80%\"><i>Last data update: 15 January 2015</i></p>")
        return(Commentary.Date)
    })
    output$Sex <- renderPrint({ input$Sex })
    output$ctype <- renderPrint({ input$ctype })
    output$Age <- renderPrint({ input$Age })
    output$text1 <- renderText({
        paste('<div style=\"position:relative; left:6em\">Calculated cumulative risk for patient aged ',input$Age, 'today:</div>')
    })
    
  output$plot_incidence_gene <- renderPlot({
      
      df <- dplyr::filter(all_cum_ind, Cancer == input$ctype & Sex == input$Sex & Age >= input$Age & Gene == input$genecarrier)      
      if(nrow(df) > 1){
          df$tmp <- rep(1,nrow(df))        
          for (i in 2:nrow(df)) {
              df$tmp[i] <- df$tmp[i - 1] - (df$tmp[i - 1] * df$Annual_incidence_rate[i - 1])
          }
          df$cum_risk_percent_dynamic <- (1 - df$tmp) * 100
      }
      else{
          df$cum_risk_percent_dynamic <- df$Annual_incidence_rate * 100
      }
      df$SEXGENE <- paste(df$Sex,df$Gene, sep=" - ")
      #cancer_type <- 'Any cancer type'
      cancer_type <- paste('Any cancer type',input$Sex,input$genecarrier,sep = ' - ')
      if(input$ctype == 'CRC'){
          #cancer_type <- 'Colorectal cancer'
          cancer_type <- paste('Colorectal cancer',input$Sex,input$genecarrier,sep = ' - ')
      }
      if(input$ctype == 'OVARY'){
          #cancer_type <- 'Ovarian cancer'
          cancer_type <- paste('Ovarian cancer',input$Sex,input$genecarrier,sep = ' - ')
          
      }
      if(input$ctype == 'UGI'){
          #cancer_type <- 'Gastric/small intestine/biliary tract/pancreas cancer'
          cancer_type <- paste('Gastric/small intestine/biliary tract/pancreas cancer',input$Sex,input$genecarrier,sep = ' - ')
      }
      if(input$ctype == 'URO'){
          #cancer_type <- 'Urine bladder/kidney/ureter cancer'
          cancer_type <- paste('Urine bladder/kidney/ureter cancer',input$Sex,input$genecarrier,sep = ' - ')
      }
      if(input$ctype == 'END'){
          cancer_type <- paste('Endometrial cancer',input$Sex,input$genecarrier,sep = ' - ')
      }
    
      p <- ggplot(df, aes(x=Age,y=cum_risk_percent_dynamic,group=SEXGENE,colour=SEXGENE)) +
          geom_line(size=1) + geom_point(size=3) +
          theme_classic() +
          ggtitle(cancer_type) + 
          scale_x_continuous(breaks=seq(25,70,5),limits=c(25,70)) +
          scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100)) +
          scale_color_brewer(palette='Dark2') +
          theme(legend.title = element_blank(),
                legend.position = "none",
                panel.grid.major = element_line(colour = 'black', size = 0.5, linetype = 'dashed'),
                axis.text.x=element_text(family="Helvetica",size=14), 
                axis.title.x=element_text(family="Helvetica",size=14,vjust=-0.5), 
                axis.text.y=element_text(family="Helvetica",size=14),
                axis.title.y=element_text(family="Helvetica",size=14,vjust=1.5), 
                plot.margin = (unit(c(2, 2, 2, 2), "cm")), 
                plot.title = element_text(family="Helvetica",size=14,vjust=2),
                legend.text = element_blank()) +
                #legend.text=element_text(family="Helvetica",size=14)) +
          ylab('Cumulative risk of first cancer (%)')
      
      print(p)
  }, height="auto",width="auto")
  
  

  output$table_ALL <- renderTable({
      df <- as.data.frame(dplyr::filter(all_cum_ind, Cancer ==input$ctype & Sex == input$Sex & Gene == input$genecarrier & Age >= input$Age))
      
      if(nrow(df) > 1){
          df$tmp <- rep(1,nrow(df))        
          for (i in 2:nrow(df)) {
              df$tmp[i] <- df$tmp[i - 1] - (df$tmp[i - 1] * df$Annual_incidence_rate[i - 1])
          }
          df$cum_risk_percent_dynamic <- as.integer(round((1 - df$tmp) * 100))
      }
      else{
          df$cum_risk_percent_dynamic <- as.integer(round(df$Annual_incidence_rate * 100))
      }
      
      df <- as.data.frame(dplyr::filter(df, Age == 25 | Age == 40 | Age == 50 | Age == 60 | Age == 70))
      
      df <- df %>% dplyr::select(Age,Sex,Gene,cum_risk_percent_dynamic)
      
      colnames(df) <- c('Age','Sex','Gene','Risk (%)')      
      df
      
  },include.rownames=F)
  

})
