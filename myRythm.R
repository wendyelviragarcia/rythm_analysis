####
# shyny app for rythm metrics computing and display
####
library(shiny)
library(readtextgrid)
library(ggplot2)
library(dplyr)

options(shiny.maxRequestSize=30*1024^2) 

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Compute files rythm"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose your TextGrids. They must have C and V labelled intervals",
                multiple = TRUE),
      
      # Horizontal line ----
      
      
      
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      checkboxGroupInput("disp", "Metrics",
                   choices = c(SpeechRate = "SpeechRate", PercentageConsonants = "PercentageConsonants",
                               PercentageVowel = "PercentageVowel", VarcoV="Varco", VarcoC = "VarcoC"),
                   selected = c("SpeechRate","PercentageVowel","PercentageConsonants")),
      
      tags$hr(),
      # Input: Select number of rows to display ----
      checkboxGroupInput("disp", "Graphs",
                         choices = c(PerV = "PerV", Varcos = "Varcos",
                                     Deltas = "Deltas"),
                         selected = c("PerV","Varcos","PercentageConsonants")),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      h4("Numeric Results"),
      tableOutput("contents"),
      h4("Graphs"),
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3")
      
      
    )
    
  )
)

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
        
        df<- data.frame("file"= c(1:length(files)),"speechRate"=c(1:length(files)),"PerV"=c(1:length(files)),"PerC"=c(1:length(files)),"VarcoV"=c(1:length(files)),"VarcoC"=c(1:length(files)),"DeltaV"=c(1:length(files)),"DeltaC"=c(1:length(files)) )
        loopIndex<- 0
        for (loopIndex in 1:nFiles){
          #for (file in files) {
          #loopIndex=loopIndex+1
          #textgrid<- read_textgrid(file)
          textgrid<- read_textgrid(files[loopIndex])
          textgrid$text[textgrid$text=="PTK"] <- "C"
          textgrid$text[textgrid$text=="a"] <- "V"
          
          textgrid$duration<-(textgrid$xmax-textgrid$xmin)
          textGridNonSilent<-textgrid[textgrid$text!="",]
          speechTime<-sum(textGridNonSilent$duration)
          
          consonantTime= sum(textgrid[textgrid$text=="C",]$duration)
          vowelTime= sum(textgrid[textgrid$text=="V",]$duration)
          frequ= table(textgrid$text)
          nCons =unname(frequ[names(frequ)=="C"])
          nVows =unname(frequ[names(frequ)=="V"])
          speechRate= (nCons+nVows)/speechTime
          PercentageV = vowelTime/speechTime
          PercentageC = consonantTime/speechTime
          varcoC=sd(textgrid[textgrid$text=="C",]$duration)/mean(textgrid[textgrid$text=="C",]$duration)
          varcoV= sd(textgrid[textgrid$text=="V",]$duration)/mean(textgrid[textgrid$text=="V",]$duration)
          deltaC <- sd(textgrid[textgrid$text=="C",]$duration)
          deltaV <- sd(textgrid[textgrid$text=="V",]$duration)
          
          df[loopIndex,1 ]<- textgrid$file[1]
          df[loopIndex,2]<- speechRate
          df[loopIndex,3]<- PercentageV
          df[loopIndex,4]<- PercentageC
          df[loopIndex,5]<- varcoV
          df[loopIndex,6]<- varcoC
          df[loopIndex,7]<- deltaV
          df[loopIndex,8]<- deltaC
        }
        
        #df creado y completo
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
        
        output$plot1 <- renderPlot({
          ggplot(data = df,aes(x = VarcoV, y = PerV, colour = file)) +
            geom_point()+
            geom_errorbar(aes(ymin = df.summary.PV$ymin, ymax = df.summary.PV$ymax))+
            geom_errorbarh(aes(xmin = df.summary.VV$ymin,xmax = df.summary.VV$ymax))+
            labs(subtitle="%V/varcoV",
                 y="%V", 
                 x="varcoV", 
                 title="%V/varcoV by file", 
                 caption = "GNU. Made with www.wendyelvira.ga ")
        }) 
        
        
        
        output$plot2 <- renderPlot({
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
        
        
        output$plot3 <- renderPlot({
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

# Create Shiny app ----
shinyApp(ui, server)