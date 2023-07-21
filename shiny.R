library(shiny)
library(data.table)
library(ggplot2)

ui <- fluidPage(
  textInput("gene", "Enter gene:", "ptg000018l.2011"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  nuts <- fread("nuts.csv")
  genes <- fread("Tissue_specific_exp_for_fruit-spec_genes.csv")[!is.na(GDD)]
  
  output$plot <- renderPlot({
    genedata <- genes[Gene == input$gene]
    plotdf <- merge(nuts, genedata, by = c("Tissue", "GDD"), all.x = T)
    plotdf <- plotdf[order(plotdf$order),]
    
    p <- ggplot(plotdf[plotdf$class == "c",]) +
      aes(x, y, fill = as.numeric(Exp), group = elem_idx) +
      geom_polygon(color = "black") +
      geom_polygon(data = plotdf[plotdf$class == "b",], color = "black") +
      geom_polygon(data = plotdf[plotdf$class == "a",], color = "black") +
      scale_y_reverse() +
      scale_fill_gradient2(low = "#ffffff",
                           mid = "#1EE30D",
                           high = "#086144") +
      theme(axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
      labs(fill = "Gene expression")+ 
      coord_fixed()
    
    print(p)
  })
}

shinyApp(ui = ui, server = server)