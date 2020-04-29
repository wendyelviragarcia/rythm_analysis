# Define server logic to read selected file ----

server <- function(input, output) {
  #options(shiny.maxRequestSize=30*1024^2) 
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        #df <- read_textgrid(input$file1$datapath[1])
        nFiles<- length(input$file1$datapath)
        files<- input$file1$datapath
        
        df<- data.frame("file"= c(1:length(files)),"speechRate"=c(1:length(files)),"PerV"=c(1:length(files)),"PerC"=c(1:length(files)),"VarcoV"=c(1:length(files)),"VarcoC"=c(1:length(files)),"DeltaV"=c(1:length(files)),"DeltaC"=c(1:length(files)),"VrPVI"=c(1:length(files)), "CrPVI"=c(1:length(files)),"VnPVI"=c(1:length(files)) )
        allC <- data.frame("file"=character(0),"durationC"=numeric(0))
        allV <- data.frame("file"=character(0),"durationA"=numeric(0))
        
        loopIndex<- 0
        for (loopIndex in 1:nFiles){
          #for (file in files) {
          #loopIndex=loopIndex+1
          #textgrid<- read_textgrid(file)
          textgrid<- read_textgrid(files[loopIndex])
          fileName<- input$file1$name[loopIndex]
          textgrid$text[textgrid$text=="PTK"] <- "C"
          textgrid$text[textgrid$text=="a"] <- "V"
          textgrid$text[textgrid$text=="c"] <- "C"
          textgrid$text[textgrid$text=="v"] <- "V"
          #computes dur in ms for comparison purposes with other metrics (Arvaniti 2011)
          textgrid$duration<-(textgrid$xmax-textgrid$xmin)*1000
          textGridNonSilent<-textgrid[textgrid$text!="",]
          speechTime<-sum(textGridNonSilent$duration)
          
          durCs <-textgrid[textgrid$text=="C",]$duration
          durVs <-textgrid[textgrid$text=="V",]$duration
          
          dataFileC <- data.frame(rep(fileName, length(durCs)),durCs)
          dataFileV <- data.frame(rep(fileName, length(durVs)),durVs)
          allC<- rbind (dataFileC,allC)
          allV <-rbind (allV,dataFileV)
          speechTime<-sum(textGridNonSilent$duration)
          
          
          consonantTime= sum(textgrid[textgrid$text=="C",]$duration)
          vowelTime= sum(textgrid[textgrid$text=="V",]$duration)
          frequ= table(textgrid$text)
          nCons =unname(frequ[names(frequ)=="C"])
          nVows =unname(frequ[names(frequ)=="V"])
          speechRate= (nCons+nVows)/speechTime
          PercentageV = (vowelTime/speechTime)*100
          PercentageC = (consonantTime/speechTime)*100
          deltaC <- sd(textgrid[textgrid$text=="C",]$duration)
          deltaV <- sd(textgrid[textgrid$text=="V",]$duration)
          
          #VarcoC=100*DC/meanC (Dellwo 2006)
          varcoC=100*deltaC/mean(textgrid[textgrid$text=="C",]$duration)
          varcoV= 100*deltaV/mean(textgrid[textgrid$text=="V",]$duration)
          
          
          df[loopIndex,1 ]<- fileName
          df[loopIndex,2]<- speechRate
          df[loopIndex,3]<- PercentageV
          df[loopIndex,4]<- PercentageC
          df[loopIndex,5]<- varcoV
          df[loopIndex,6]<- varcoC
          df[loopIndex,7]<- deltaV
          df[loopIndex,8]<- deltaC
          
          #compute the rPVI  FOR VOWELS
          
          #compute the rPVI  FOR VOWELS
          
          myA<- which(textgrid$text=="V" )
          myC<- which(textgrid$text=="C")
          # Check if all vowel intervals have a C afterwards
          expectedC<- myA+1
          checkingC <- expectedC %in% myC
          haveCafter= data.frame(myA,checkingC)
          myA<- haveCafter$myA[haveCafter$checking==TRUE]
          
          indexA <-0
          difsA <- rep(NA, length(myA))
          for (A in myA){
            indexA=indexA+1
            difsA[indexA] <- abs(textgrid$duration[A]-textgrid$duration[A+1])
          }
          #difsCA ready for further analisys compute
          # rPVI
          VrPVI<- mean(difsA)
          df[loopIndex,9]<- VrPVI
          
          # compute nPVI 
          # It computes the difference between the duration of each vocalic interval 
          # and the one the follows then divides it by the average duration of all vocalic intervals. 
          # The mean of the values obtained is computed and finally multiplied by 100. 
          denom<- mean(textgrid$duration[textgrid$text=="V"] )
          VnPVI<- 100*(mean(difsA/denom))
          df[loopIndex,11]<- VnPVI
          
          ## compute the rPVI  FOR CONSONANTS
          # Check if all C intervals have a vowel afterwards
          expectedA<- myC+1
          checkingA <- expectedA %in% myA
          haveAafter= data.frame(myC,checkingA)
          myC<- haveAafter$myC[haveAafter$checking==TRUE]
          
          indexC <-0
          difsC <- rep(NA, length(myC))
          for (C in myC){
            indexC=indexC+1
            difsC[indexC] <- abs(textgrid$duration[C]-textgrid$duration[C+1])
          }
          
          #difsC ready for further analisys
          CrPVI<- mean(difsC)
          df[loopIndex,10]<- CrPVI
          
        }
        
        #df creado y completo
        # df
        # allC
        # allV
        
        
        df.summary.PV <- df %>% group_by(file) %>%
          summarize(ymin = min(PerV),
                    ymax = max(PerV),
                    ymean = mean(PerV))
        
        df.summary.VV <- df %>% group_by(file) %>%
          summarize(ymin = min(VarcoV),
                    ymax = max(VarcoV),
                    ymean = mean(VarcoV))
        
        df.summary.DC <- df %>% group_by(file) %>%
          summarize(ymin = min(DeltaC),
                    ymax = max(DeltaC),
                    ymean = mean(DeltaC))
        
        df.summary.DV <- df %>% group_by(file) %>%
          summarize(ymin = min(DeltaV),
                    ymax = max(DeltaV),
                    ymean = mean(DeltaV))
        
        df.summary.VnPVI <- df %>% group_by(file) %>%
          summarize(ymin = min(VnPVI),
                    ymax = max(VnPVI),
                    ymean = mean(VnPVI))
        
        df.summary.CrPVI <- df %>% group_by(file) %>%
          summarize(ymin = min(CrPVI),
                    ymax = max(CrPVI),
                    ymean = mean(CrPVI))
        
        names(allC)[1] <- "file"
        names(allV)[1] <- "file"
        allC$file <- as.character(allC$file)
        allV$file <- as.character(allV$file)
        
        allC$file <- substr(allC$file,1,nchar(allC$file)-9)
        allV$file <- substr(allV$file,1,nchar(allV$file)-9)
        
        allC$file <- as.factor(allC$file)
        allV$file <- as.factor(allV$file)
        
        p1<- ggplot(allC, aes(x=file, y=durCs,colour = file)) +
          geom_boxplot()+ylab("(ms)")+xlab("File")+
          theme(axis.text.x = element_text(angle = 90))+
          ggtitle("Consonants Duration")
        
        p2<- ggplot(allV, aes(x=file, y=durVs,colour = file)) +
          geom_boxplot()+ylab("(ms)")+xlab("File")+
          theme(axis.text.x = element_text(angle = 90))+
          ggtitle("Vowels Duration")
        
        output$plot1 <- renderPlot({
          grid.arrange(p1, p2, ncol=2)
          
        }) 
        
        output$plot5 <- renderPlot({
          ggplot(data = df,aes(x = CrPVI, y = VnPVI, colour = file)) +
            geom_point()+
            labs(subtitle="Consonantal raw PVI by Vocalic normalized PVI", 
                 y="VnPVI", 
                 x="CrPVI", 
                 title="CrPVI by VnPVI", 
                 caption = "Metric Grabe & Low (2002)")
        }) 
        
        output$plot2 <- renderPlot({
          ggplot(data = df,aes(x = VarcoV, y = PerV, colour = file)) +
            geom_point()+
            geom_errorbar(aes(ymin = df.summary.PV$ymin, ymax = df.summary.PV$ymax))+
            geom_errorbarh(aes(xmin = df.summary.VV$ymin,xmax = df.summary.VV$ymax))+
            labs(subtitle="%V/varcoV",
                 y="%V", 
                 x="VarcoV", 
                 title="%V/varcoV by file", 
                 caption = "GNU. Made with www.wendyelvira.ga ")
        }) 
        
        
        
        output$plot3 <- renderPlot({
          ggplot(data = df,aes(x = PerV, y = DeltaC, colour = file)) +
            geom_point()+
            geom_errorbar(aes(ymin = df.summary.DC$ymin, ymax = df.summary.DC$ymin))+
            geom_errorbarh(aes(xmin = df.summary.PV$ymin, xmax = df.summary.PV$ymax))+
            
            labs(subtitle="%V/∆C", 
                 y="∆C", 
                 x="%V", 
                 title="%V/∆C", 
                 caption = "GNU. Made with www.wendyelvira.ga ")
        }) 
        
        
        output$plot4 <- renderPlot({
          ggplot(data = df,aes(x = DeltaV, y = DeltaC, colour = file)) +
            geom_point()+
            geom_errorbar(aes(ymin = df.summary.DC$ymin, ymax = df.summary.DC$ymin))+
            geom_errorbarh(aes(xmin = df.summary.DV$ymin,xmax = df.summary.DV$ymax))+
            labs(subtitle="∆V/∆C", 
                 y="∆C", 
                 x="∆V",  
                 title="∆V/∆C", 
                 caption = "GNU. Made with www.wendyelvira.ga ")
        }) 
        
        
        
        
        
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    #if(input$disp == "head") {
    #  return(head(df))
    #}
    #else {
      return(df)
    #}
    
  })
  
}