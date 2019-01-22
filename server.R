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
  
  filedata3 <- reactive({
    infile3 <- input$filename3
    if (is.null(infile3)){
      return(NULL)
    }
    read.table(infile3$datapath,sep = "\t", header = T)
  })
  
  
  observe({
    updateNumericInput(session, "bubbleheigh", value = 4)
    updateNumericInput(session, "bubblewidth", value = 10)
  })
  
  observe({
    updateNumericInput(session, "bubbleheigh2", value = 4)
    updateNumericInput(session, "bubblewidth2", value = 6)
  })
  
  observe({
    updateNumericInput(session, "cellheight", value = 1)
    updateNumericInput(session, "cellwidth", value = 20)
  })
  
  observe({
    updateNumericInput(session, "heatmapheigh", value = 20)
    updateNumericInput(session, "heatmapwidth", value = 10)
  })
  
  ##################################################################
  #bubble1
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
  
  #######################################
  #bubble2
  bubble_plot2 <- function(){
    df <- filedata3()
    groups <- input$grouping
    if (groups == "FALSE")
      return(
        p = ggplot(df,aes(factor,description)) +
          geom_point(aes(size=DEGnumber,color=-1*log10(pvalue+0.00000001))) + 
          scale_colour_gradient(low="green4",high="red") + 
          labs(color=expression(-log10),size="Gene number",x="Factor",y="Term",title="Enrichment analysis")
        
      )
    if (groups == "TRUE")
      return(
        p = ggplot(df,aes(group,description)) +
          geom_point(aes(size=DEGnumber,color=-1*log10(pvalue+0.00000001))) + 
          scale_colour_gradient(low="green4",high="red") + 
          labs(color=expression(-log10),size="Gene number",x="Group",y="Term",title="Enrichment analysis")
      )
    p
  }
  
  output$bubblechartplot2 <- renderPlot({
    if (!is.null(filedata3())){
      bubble_plot2()
    }
  })
  
  output$downloadbubble2 <- downloadHandler(
    filename = function() { paste0("Bubblechart2", '.pdf') },
    contentType = "image/pdf",
    content = function(file) {
      pdf(file, width = input$bubblewidth2, height = input$bubbleheigh2)
      print(bubble_plot2())
      dev.off()
    }
  )
  #######################################################
  #heatmap
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
  
  show_number <- reactive({
    number <- input$displaynumber
    if (number == "FALSE")
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
    colorname <- input$color
    if (colorname == "default")
      return(
        pheatmap(df, 
                 scale= input$scale,
                 cluster_rows = show_clusterrows(), cluster_cols = show_clustercols(),
                 cellwidth=input$cellwidth, cellheight= input$cellheight, 
                 show_rownames = show_rowname(), show_colnames = show_colname(), display_numbers=show_number(),
                 clustering_method_rows = input$algorithm
        )
      )
    else
      return(
        pheatmap(df, color = show_color(),
                 scale= input$scale,
                 cluster_rows = show_clusterrows(), cluster_cols = show_clustercols(),
                 cellwidth=input$cellwidth, cellheight= input$cellheight, 
                 show_rownames = show_rowname(), show_colnames = show_colname(), display_numbers=show_number(),
                 clustering_method_rows = input$algorithm
        )
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
  #####################################################
  #scatterplotchart
  filedata4 <- reactive({
    infile4 <- input$filename4
    if (is.null(infile4)){
      return(NULL)
    }
    read.table(infile4$datapath,sep = "\t", head = T)
  })
  
  observe({
    updateNumericInput(session, "scatterplotheigh", value = 4)
    updateNumericInput(session, "scatterplotwidth", value = 10)
  })
  
  show_tropic <- reactive({
    number <- input$tropic
    if (number == "FALSE")
      return(  )
    else
      return( geom_smooth(method =lm, se = F,colour="black",size=0.5) )
  })
  
  color_select <- reactive({
    colorname <- input$contour
    if (colorname == "color1")
      return(  )
    if (colorname == "color2")
      return( scale_colour_gradient2("",low ="blue",mid ="red", high="red", midpoint =0.045) )
    if (colorname == "color3")
      return( scale_color_gradientn(colours=topo.colors(7)) )
    if (colorname == "color4")
      return( scale_color_gradientn(colours=rev(rainbow(7))))
    if (colorname == "color5")
      return( scale_color_gradientn(colours=terrain.colors(7)) )
  })
  
  scatter_plot<-function(){
    df <- filedata4()
    p <- ggplot(df,aes(y=df[,1], x= df[,2]))+ geom_point(aes(colour=df[,2]),colour="black",size=0.2,alpha=0.3) +
      stat_density_2d(aes(colour=..level..))
    p1<- p + show_tropic() + color_select()
    
    p2<-p1 + theme_bw()+ theme(panel.border = element_blank(),
                      legend.position ="none",
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.line = element_line(size =0.4, linetype ="solid", colour ="black")) + 
      xlab(colnames(df)[1]) +  ylab(colnames(df)[2])
    p2
  }
  
  output$scatterplotchart <- renderPlot({
    if (!is.null(filedata4())){
      scatter_plot()
    }
  })
  
  output$downloadscatterplot <- downloadHandler(
    filename = function() { paste0("scatterplotchart", '.pdf') },
    contentType = "image/pdf",
    content = function(file) {
      pdf(file, width = input$scatterplotwidth, height = input$scatterplotheigh)
      print(scatter_plot())
      dev.off()
    }
  )
###############################################################################
#volcanochart
  filedata5 <- reactive({
    infile5 <- input$filename5
    if (is.null(infile5)){
      return(NULL)
    }
    read.table(infile5$datapath,sep = "\t", head = T)
  })
  
  observe({
    updateNumericInput(session, "volcanoheigh", value = 10)
    updateNumericInput(session, "volcanowidth", value = 10)
  })
  
  color_volcano <- reactive({
    colorname <- input$colorDEG
    if (colorname == "blue_black_red")
      return( scale_color_manual(values =c("blue","black", "red")) )
    if (colorname == "green_black_red")
      return( scale_color_manual(values =c("green","black", "red")) )
  })
  
  number_FDR <- reactive({
    FDRnumber <- input$FDRselect
    if (FDRnumber == "0.001")
      return(0.001 )
    if (FDRnumber == "0.01")
      return( 0.01)
    if (FDRnumber == "0.05")
      return( 0.05 )
  })
  
  number_FC <- reactive({
    FDRnumber <- input$foldchange
    if (FDRnumber == "2")
      return( 1 )
    if (FDRnumber == "1.5")
      return( 0.5849625 )
  })
  
  change_file1<-function(){
    ddf <- filedata5()
    for(i in 1:nrow(ddf)){
      if(ddf[i,2]> number_FC() & ddf[i,3] < number_FDR()){
        ddf[i,4]<-"up"
      }else{
          if(ddf[i,2]< -number_FC() & ddf[i,3] < number_FDR()){
            ddf[i,4]<-"down"
          }else{
              ddf[i,4]<-"no"}
    }
    colnames(ddf)[4] <-"significant"
    }
    return(ddf)
  }
  
  FDR_line <- reactive({
    FDRnumber <- input$FDRselect
    if (FDRnumber == "0.001")
      return( geom_hline(yintercept= 3,linetype=4))
    if (FDRnumber == "0.01")
      return( geom_hline(yintercept= 2,linetype=4))
    if (FDRnumber == "0.05")
      return( geom_hline(yintercept= 1.30103,linetype=4))
  })
  
  FC_line <- reactive({
    FDRnumber <- input$foldchange
    if (FDRnumber == "2")
      return( geom_vline(xintercept=c(-1,1),linetype=4))
    if (FDRnumber == "1.5")
      return( geom_vline(xintercept=c(-0.5849625,0.5849625),linetype=4))
  })
  
  
  volcano_plot<-function(){
    df<-change_file1()
    p<- ggplot(df,aes(df[,2],-1*log10(df[,3]+1*10^(-30)))) +
      geom_point(aes(color = significant)) + color_volcano() + 
      xlim(-5,5) + ylim(0,31) +
      labs(title="Volcanoplot",x=expression(log[2](FC)), y=expression(-log[10](FDR))) + 
      FDR_line() + FC_line()
    p
  }
  
  output$volcanochart <- renderPlot({
    if (!is.null(filedata5())){
      volcano_plot()
    }
  })
  
  output$downloadvolcanochart <- downloadHandler(
    filename = function() { paste0("volcanochart", '.pdf') },
    contentType = "image/pdf",
    content = function(file) {
      pdf(file, width = input$volcanotwidth, height = input$volcanoheigh)
      print(volcano_plot())
      dev.off()
    }
  )
  
}