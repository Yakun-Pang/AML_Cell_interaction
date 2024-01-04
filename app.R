rm(list=ls())
library(shiny)
library(ggplot2)
library(MetBrewer)

##load data
df_aml<-readRDS(file = "AML_interaction.rds")
df_hbm<-readRDS(file = "HBM_interaction.rds")
## Only run examples in interactive R sessions


df_aml$Category<-'AML'
df_hbm$Category<-'HBM'
df<-rbind(df_aml,df_hbm)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("Cell Cell interaction"),
      selectInput(inputId = "sample", label = "Sample",
                  choices =  unique(df$Sample),
                  multiple = TRUE,
                  selected = "AML882"),
      selectInput(inputId = "sender", "Sender",
                  choices = ""),
      selectInput(inputId = "receptor", "Receptor",
                  choices =""),
      sliderInput("P_value",
                  label = "P value:",
                  min = 0,
                  max = 0.1,
                  value = c(0,0.1),
                  step=0.01),
      actionButton("do", "Output"),
    ),
    mainPanel(
      plotOutput("Plot"),
      DT::dataTableOutput(outputId = "table")
    )
  )
)


server <- function(input, output,session) {
  observeEvent(input$sample,{
    updateSelectInput(session,'sender',
                      choices=unique(df$Sender[df$Sample==input$sample]))
  }) 
  observeEvent(input$sender,{
    updateSelectInput(session,'receptor',
                      choices=unique(df$Receptor[df$Sample==input$sample & df$Sender==input$sender]))
  }) 
  observeEvent(input$do, {
    filtered_data <- reactive({
      subset(df,
             Sample%in% input$sample & Sender %in% input$sender & Receptor%in% input$receptor &
               pval <= input$P_value)})
    
    
    output$Plot <- renderPlot({
      
      p <- ggplot(filtered_data(), aes(x =Sample, y = pathway, size = -log10(pval+0.00001), fill = as.numeric(mean))) +
        geom_point(shape = 21) +
        scale_fill_gradientn(colors=met.brewer("Hokusai1",direction =-1),name = "Mean")+
        #scale_fill_met_c("Hokusai1", name = "Mean",direction=-1)+
        labs(x = "", y = "")+
        theme(axis.text.x =element_text(face="bold", angle = 90,
                                        size=8),
              axis.text.y = element_text(face="bold",hjust = 0,
                                         size=8))
      p
      
    })
    
    output$table <- DT::renderDataTable({
      filtered_data()
    })
    
  })
}

enableBookmarking(store = "url")

shinyApp(ui, server)

