library(shiny)
library(dplyr)
library(ggplot2)

results <- read.csv("model-outputs.csv")
results$Container <- factor(results$Container,levels=c("box2","box1","bag1","bag2"))


plot_result <- function (pragListenerLevel,
                         defArtMeaning,
                         cmpHighScopeConstrualProbValue,
                         posHighScopeConstrualProbValue,
                         allowUninformativeThresholdsValue,
                         contexts) {
  gg_df <- subset(results,
                    DefArtMeaning==defArtMeaning & 
                    Model=="haddock_model.wppl"  & 
                    ListenerLevel==pragListenerLevel & 
                    posHighScopeConstrualProb==posHighScopeConstrualProbValue & 
                    cmpHighScopeConstrualProb==cmpHighScopeConstrualProbValue &
                    allowUninformativeThresholds==allowUninformativeThresholdsValue & 
                    Context==contexts)
  
  ggplot(gg_df, aes(x=Condition, y=Probability, fill=Container)) + 
    geom_bar(position="fill", stat="identity") +
    fillScale +
    theme_bw() +
    theme(axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=12),  
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          legend.title=element_text(size=12), 
          legend.text=element_text(size=12)
    ) +
    ylim(0,1) +
    xlab("Display Type") +
    ylab("Probability of Referent Choice") +
    ggtitle("Model predictions",paste(defArtMeaning,paste0("L",pragListenerLevel),paste0("posHiScope:", posHighScopeConstrualProbValue),paste0("cmpHiScope:", cmpHighScopeConstrualProbValue),allowUninformativeThresholdsValue,sep=", ")) +
    facet_wrap(~Adjective)
  
}

myColors <- c("#2392E9","lightblue","#F0E442","#C44D00")
names(myColors) <- levels(results$Container)
fillScale <- scale_fill_manual(name = "Container",values = myColors)



# Define the UI
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Select model parameters"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      #selectInput("adj", "Adjective:", 
       #           choices=levels(as.factor(results$Adjective))),
      selectInput("defArtMeaning", "Definite Article Meaning:", 
                  choices=levels(as.factor(results$DefArtMeaning)),
                  selected="bumford"),
      selectInput("cmpHighScopeConstrualProbValue", "Cmp high scope prob:", 
                  choices=as.numeric(levels(as.factor(results$cmpHighScopeConstrualProb))),
                  selected=0.9),
      selectInput("posHighScopeConstrualProbValue", "Pos high scope prob:", 
                  choices=as.numeric(levels(as.factor(results$posHighScopeConstrualProb))),
                  selected=0),
      selectInput("allowUninformativeThresholdsValue", "Allow uninformative thresholds:", 
                  choices=levels(as.factor(results$allowUninformativeThresholds)),
                  selected="disallow"),
      selectInput("contexts", "Context Coordination:", 
                  choices=levels(as.factor(results$Context)),
                  selected="no-cc"),
      selectInput("pragListenerLevel", "Pragmatic listener level:", 
                  choices=levels(as.factor(results$ListenerLevel))),
      hr()
      #helpText("Help text")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("plot")  
    )
  )
)


# Define the server code
server <- function(input, output) {
  
  output$plot <- renderPlot({
    pragListenerLevel <- input$pragListenerLevel
    defArtMeaning <- input$defArtMeaning
    adj <- input$adj
    cmpHighScopeConstrualProbValue <- input$cmpHighScopeConstrualProbValue
    posHighScopeConstrualProbValue <- input$posHighScopeConstrualProbValue
    allowUninformativeThresholdsValue <- input$allowUninformativeThresholdsValue
    contexts <- input$contexts
    
    plot_result(pragListenerLevel,
                defArtMeaning,
                cmpHighScopeConstrualProbValue,
                posHighScopeConstrualProbValue,
                allowUninformativeThresholdsValue,
                contexts)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
