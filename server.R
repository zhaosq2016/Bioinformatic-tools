library(ggplot2)
library(pheatmap)

server <- function(input, output, session){
  filedata1 <- reactive({
    infile1 <- input$filename1
    if (is.null(infile1)){
      return(NULL)
    }
    read.table(infile1$datapath,sep = "\t", header = T)
  })
  
  filedata2 <- reactive({
    infile2 <- input$filename2
    if (is.null(infile2)){
      return(NULL)
    }
    read.table(infile2$datapath,sep = "\t", head = T, row.names = 1)
  })
  
  observe({
    updateNumericInput(session, "bubbleheigh", value = 4)
    updateNumericInput(session, "bubblewidth", value = 10)
  })
  
  observe({
    updateNumericInput(session, "cellheight", value = 1)
    updateNumericInput(session, "cellwidth", value = 20)
  })
  
  observe({
    updateNumericInput(session, "heatmapheigh", value = 20)
    updateNumericInput(session, "heatmapwidth", value = 10)
  })
  
  bubble_plot <- function(){
    df <- filedata1()
    p <- ggplot(df,aes(len,PreLength))+
      geom_point(aes(size=CountsMIRb,color=Expression))+
      scale_fill_manual(breaks = c("high", "middle", "low"),values=c("red", "yellow", "green"))+
      facet_grid(.~group)
    p
  }
  
  output$bubblechartplot <- renderPlot({
    if (!is.null(filedata1())){
      bubble_plot()
    }
  })
  
  output$downloadbubble <- downloadHandler(
    filename = function() { paste0("Bubblechart", '.pdf') },
    contentType = "image/pdf",
    content = function(file) {
      pdf(file, width = input$bubblewidth, height = input$bubbleheigh)
      print(bubble_plot())
      dev.off()
    }
  )
  
  show_clusterrows <- reactive({
    clusterrowname <- input$clusterrows
    if (clusterrowname == "FALSE")
      return( FALSE )
    else
      return( TRUE )
  })
  
  show_clustercols <- reactive({
    clustercolname <- input$clustercols
    if (clustercolname == "FALSE")
      return( FALSE )
    else
      return( TRUE )
  })
  
  show_rowname <- reactive({
    rowname <- input$rownames
    if (rowname == "FALSE")
      return( FALSE )
    else
      return( TRUE )
  })
  
  show_colname <- reactive({
    colname <- input$colnames
    if (colname == "FALSE")
      return( FALSE )
    else
      return( TRUE )
  })
  
  show_color <- reactive({
    colorname <- input$color
    if (colorname == "green yellow red")
      return( colorRampPalette(c("green4", "yellow", "red"))(500) )
    if (colorname == "blue yellow red")
      return( colorRampPalette(c("navy", "yellow", "red"))(500) )
  })
  
  heatmap_plot <-function(){
    df <- filedata2()
    pheatmap((df+0.0001), color = show_color(),
             scale= input$scale,
             cluster_rows = show_clusterrows(), cluster_cols = show_clustercols(),
             cellwidth=input$cellwidth, cellheight= input$cellheight, 
             show_rownames = show_rowname(), show_colnames = show_colname(),
             clustering_method_rows = input$algorithm
             )
  }
  
  output$heatmapchartplot <- renderPlot({
    if (!is.null(filedata2())){
      heatmap_plot()
    }
  })
  
  output$downloadheatmap <- downloadHandler(
    filename = function() { paste0("Heatmapchart", '.pdf') },
    contentType = "image/pdf",
    content = function(file) {
      pdf(file, width = input$heatmapwidth, height = input$heatmapheigh)
      print(heatmap_plot())
      dev.off()
    }
  )
  
}