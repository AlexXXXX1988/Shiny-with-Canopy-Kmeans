library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Canopy Kmeans Clustering"),
  sidebarPanel(
    selectInput("datasrc", "Choose a data source:",
                list("upload from local file" = "upload",
                     "generate a random dataset" = "gen")),
    conditionalPanel(
      condition="input.datasrc=='upload'",
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE)
      ),
    conditionalPanel(
      condition="input.datasrc=='gen'",
      numericInput("samplesize","Sample Size", 400)
    ),
    checkboxInput("unfctn","Unification",T),
    sliderInput("t1t2","T1 and T2 Range %:",
                min = 1, max = 100, value = c(20,30)),
    numericInput("omtsmll","Omit Clusters Has the Size Small than %", .01),
    submitButton("Update View")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Canopies",
               h4("T1 & T2"),
               verbatimTextOutput("t1t2"),
               h4("Result of Canopy"),
               plotOutput("canopies")),
      tabPanel("K-means",
               h4("Result of K-means Clustering"),
               plotOutput("kmeans")),
      tabPanel("Content",
               h4("Center of K-means"),
               verbatimTextOutput("summary"),
               h4("Table for Canopy and Clustering"),
               tableOutput("cnt"))
    ))
  )
)
