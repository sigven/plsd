library(shiny)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(magrittr)


#all_cum_ind_previous_cancer <- read.table(file="next_cancer.txt",sep="\t",header=T,quote="",stringsAsFactors = F)
#all_cum_ind_previous_cancer$Annual_incidence_rate <- as.numeric(stringr::str_replace(tmp$Annual_incidence_rate,",","."))
#save(all_cum_ind_previous_cancer,file="ls_risk_data_subsequent.rda")

load('ls_risk_data.rda')
load('ls_risk_data_subsequent.rda')

shinyServer(function(input, output, session){

    values <- reactiveValues()

    observe({
            values$lastLinkValue <- input$seeAbout[1] - 1
            if (input$seeAbout[1] != values$lastLinkValue && values$lastLinkValue >= 0){
                updateTabsetPanel(session, "mainNavbar", selected = "About")
            }
    })
 
    output$UI.Date <- renderText({
        Commentary.Date <- paste0("<p style=\"font-size:80%\"><i>Last data update: December 3rd 2015</i></p>")
        return(Commentary.Date)
    })
    #output$Sex <- renderPrint({ input$Sex })
    output$ctype <- renderPrint({ input$ctype })
    output$Age <- renderPrint({ input$Age })
   
    #output$text1 <- renderText({
        #paste('<div style=\"position:relative; left:0em\">Calculated cumulative risk for first cancer for',tolower(input$Sex),'carrier of <i>',paste0('path_',input$genecarrier), '</i> with a current age of',input$Age, ':</div>')
    #})
    
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
      cancer_type <- paste('Any cancer type',tolower(input$Sex),sep = ' - ')
      if(input$ctype == 'CRC'){
          cancer_type <- paste('Colorectal cancer',tolower(input$Sex),sep = ' - ')
      }
      if(input$ctype == 'OVARIAN'){
          cancer_type <- paste('Ovarian cancer',tolower(input$Sex),sep = ' - ')
          
      }
      if(input$ctype == 'UGI'){
          cancer_type <- paste('Gastric/small intestine/biliary tract/pancreas cancer',tolower(input$Sex),sep = ' - ')
      }
      if(input$ctype == 'URO'){
          cancer_type <- paste('Urine bladder/kidney/ureter cancer',tolower(input$Sex),sep = ' - ')
      }
      if(input$ctype == 'END'){
          cancer_type <- paste('Endometrial cancer',tolower(input$Sex),sep = ' - ')
      }
      
      pathogenic_variant <- paste0('path_',input$genecarrier)
    
      p <- ggplot(df, aes(x=Age,y=cum_risk_percent_dynamic,group=SEXGENE,colour=SEXGENE)) +
          geom_line(size=1) + geom_point(size=3) +
          theme_bw() +
          ggtitle(bquote(atop(bold(.(cancer_type)), italic(.(pathogenic_variant))))) + 
          scale_x_continuous(breaks=seq(25,70,5),limits=c(25,70)) +
          scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100)) +
          scale_color_brewer(palette='Dark2') +
          theme(legend.title = element_blank(),
                legend.position = "none",
                panel.grid.major = element_line(colour = 'black', size = 0.5, linetype = 'dashed'),
                panel.border = element_rect(size=1, colour = "black"),
                axis.text.x=element_text(family="Helvetica",size=14), 
                axis.title.x=element_text(family="Helvetica",size=14,vjust=-0.5), 
                axis.text.y=element_text(family="Helvetica",size=14),
                axis.title.y=element_text(family="Helvetica",size=14,vjust=1.5), 
                plot.margin = (unit(c(2, 0, 2, 2), "cm")), 
                plot.title = element_text(family="Helvetica",size=14,vjust=2),
                legend.text = element_blank()) +
                #legend.text=element_text(family="Helvetica",size=14)) +
          ylab('Cumulative risk of first cancer (%)')
      
      print(p)
  }, height="auto",width="auto")
  
  
  output$plot_incidence_gene_subsequent_cancer <- renderPlot({
    
    df <- dplyr::filter(all_cum_ind_previous_cancer, Cancer == input$ctype2 & Age >= input$Age2 & Gene == input$genecarrier2)      
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
    cancer_type <- paste('Any cancer type')
    if(input$ctype2 == 'CRC'){
      cancer_type <- paste('Colorectal cancer')
    }
    
    pathogenic_variant <- paste0('path_',input$genecarrier2)
    
    p <- ggplot(df, aes(x=Age,y=cum_risk_percent_dynamic,group=SEXGENE,colour=SEXGENE)) +
      geom_line(size=1) + geom_point(size=3) +
      theme_bw() +
      ggtitle(bquote(atop(bold(.(cancer_type)), italic(.(pathogenic_variant))))) + 
      scale_x_continuous(breaks=seq(25,70,5),limits=c(25,70)) +
      scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100)) +
      scale_color_brewer(palette='Dark2') +
      theme(legend.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_line(colour = 'black', size = 0.5, linetype = 'dashed'),
            panel.border = element_rect(size=1, colour = "black"),
            axis.text.x=element_text(family="Helvetica",size=14), 
            axis.title.x=element_text(family="Helvetica",size=14,vjust=-0.5), 
            axis.text.y=element_text(family="Helvetica",size=14),
            axis.title.y=element_text(family="Helvetica",size=14,vjust=1.5), 
            plot.margin = (unit(c(2, 0, 2, 2), "cm")), 
            plot.title = element_text(family="Helvetica",size=14,vjust=2),
            legend.text = element_blank()) +
      #legend.text=element_text(family="Helvetica",size=14)) +
      ylab('Cumulative risk of subsequent cancer (%)')
    
    print(p)
  }, height="auto",width="auto")
  
  

  output$table_ALL <- renderTable({
      df <- as.data.frame(dplyr::filter(all_cum_ind, Cancer == input$ctype & Sex == input$Sex & Gene == input$genecarrier & Age >= input$Age))
      
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
      df$Gene <- paste0('path_',df$Gene)
      df <- df %>% dplyr::select(Age,cum_risk_percent_dynamic)
      
      colnames(df) <- c('Age','Risk (%)')      
      df
      
  },include.rownames=F)
  

  output$table_ALL2 <- renderTable({
    df <- as.data.frame(dplyr::filter(all_cum_ind_previous_cancer, Cancer == input$ctype2 & Gene == input$genecarrier2 & Age >= input$Age2))
    
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
    df$Gene <- paste0('path_',df$Gene)
    df <- df %>% dplyr::select(Age,cum_risk_percent_dynamic)
    
    colnames(df) <- c('Age','Risk (%)')      
    df
    
  },include.rownames=F)
  
})
