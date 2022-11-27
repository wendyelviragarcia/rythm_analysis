

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Compute files rythm"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 3,
      # Input: Select a file ----
      fileInput("file1", "Choose your TextGrids. They must have C and V labelled intervals",
                multiple = TRUE),
    
      # Horizontal line ----
      tags$hr(),
      
      
      # Input: select tier where segment annotation is
      numericInput("tier", h4("N tier by-sound annotation"),3, 1,100,1),
 
      # Input: select annotation type
      selectInput("annotation", h4("Annotation type"), 
                  choices = list("Phonetic" = 1,"Orthographic" = 2, "C and V" = 3,
                                 "PTK" = 4), selected = 1),
      
      # Input: Select number of rows to display ----
      #checkboxGroupInput("disp", "Metrics",
      #             choices = c(SpeechRate = "SpeechRate", PercentageConsonants = "PercentageConsonants",
      #                        PercentageVowel = "PercentageVowel", VarcoV="Varco", VarcoC = "VarcoC"),
      #            selected = c("SpeechRate","PercentageVowel","PercentageConsonants")),
      
      tags$hr(),
      # Input: Select number of rows to display ----
      # checkboxGroupInput("disp", "Graphs",
      #                  choices = c(PerV = "PerV", Varcos = "Varcos",
      #                               Deltas = "Deltas"),
      #                   selected = c("PerV","Varcos","PercentageConsonants")),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      h4("Numeric Results"),
      tableOutput("contents"),
      h4("Graphs"),
      plotOutput("plot1"),
      plotOutput("plot2")
      #plotOutput("plot3"),
      #plotOutput("plot4"),
     # plotOutput("plot5")
      
    )
    
  )
)
