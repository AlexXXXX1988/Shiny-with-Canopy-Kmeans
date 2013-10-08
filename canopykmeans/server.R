library(shiny)
library(ggplot2)
setwd("/Users/Alex/Desktop/shiny")
source("function.r")
shinyServer(function(input, output) {
  data<-reactive({
    if(input$datasrc=="upload"){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      temp<-read.csv(inFile$datapath, header=input$header)
      names(temp)<-c("x","y")
      temp
    }else{
      sample<-input$samplesize
      x<-c(rnorm(sample/4),rnorm(sample/4)*2.5+7,rnorm(sample/4)/3-5,rnorm(sample/4)*4-7)
      y<-c(rnorm(sample/4),rnorm(sample/4)*2.5-8,rnorm(sample/4)/3+2,rnorm(sample/4)*4+5)
      xy<-data.frame(x=x,y=y)
      xy
    }
  })
  dat<-reactive({
    df<-as.data.frame(data())
    if(input$unfctn==T){apply(df,2,unif)}else df
  })
  cnprslt<-reactive({
    temp<-dat()
    xyrng<-apply(temp,2,range)
    rng<-min(xyrng[2,]-xyrng[1,])
    t1<-rng*input$t1t2[2]/100
    t2<-rng*input$t1t2[1]/100
    rslt<-canopy(temp,t1,t2,input$omtsmll)
    cntr<-listtodf(rslt[[2]])
    rslt[[1]]$cluster<-kmeans(temp,cntr)[[1]]
    rslt$t1t2<-c(t1,t2)
    rslt
  })
  output$canopies<-renderPlot({
    df<-cnprslt()[[1]]
    crclt1<-lapply(cnprslt()[[2]],function(x)circle(x,cnprslt()$t1t2[1]))
    crclt2<-lapply(cnprslt()[[2]],function(x)circle(x,cnprslt()$t1t2[2]))
    p<-ggplot(df,aes(x,y,col=factor(label)))+geom_point()
    for(i in 1:as.numeric(cnprslt()[[3]])){
      p<-p+geom_path(data=crclt1[[i]],aes(x,y,col=factor("t1")))
      p<-p+geom_path(data=crclt2[[i]],aes(x,y,col=factor("t2")))
    }
    print(p) 
  }
    )
  output$t1t2<-renderPrint(data.frame(T1=cnprslt()$t1t2[1],T2=cnprslt()$t1t2[2]))
  output$kmeans<-renderPlot({
      df<-cnprslt()[[1]]
      p<-ggplot(df,aes(x,y,col=factor(cluster)))+geom_point()
      print(p)
      })
  output$summary<-renderPrint(listtodf(cnprslt()[[2]]))
  output$cnt<-renderTable(cnprslt()[[1]])
}
)