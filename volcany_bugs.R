###################################################
### PENDIENTES VOLCANY APP
###################################################


# 1. Calculate log2FC if required

##UI
p("If you need to calculate your log2 FC select the following checkbox. If already calculated leave it unselected")
checkboxInput("checkbox", label = "Calculate log2 Fold change", value = FALSE)
selectInput("FoldChange", "Select a FoldChange column", choices= NULL)
## SERVER: version 1
observeEvent(input$checkbox, {
  df <- data_plot()
  variables <- names(df)
  updateSelectInput(session,"FoldChange", choices = variables)
  })

final_data <- reactive({
     df <- data_plot()
    if(input$checkbox){
      df["log2FoldChange"] <- log2(df[input$FoldChange])
    }
})

## SERVER: version 2

new_padj <- function(){
  req(input$pvalue_column)
  df <- data_plot()
  pvalue <- df[input$pvalue_column]
  padj <- stats::p.adjust(pvalue, method= "BH")
  return(padj)
}
data_plot2 <- reactiveVal()
observeEvent(input$pvalue_column,{
   df <- cbind(data_plot(),new_padj())
   data_plot2(df)
 })


     
# 2. Downloading volcano plot: not working with Plotly 

##SERVER
     output$dwplot <- downloadHandler(
       filename = function(){
         paste0("volcano_plot.", input$imgformat)
       },
       content = function(file){
         switch(input$imgformat,
                pdf = pdf(file , width = 5, height = 5),
                png = png(file),
                jpg = jpg(file) # Not working
                )
       print(myPlot())
        dev.off()
       }
     )
    
# 3. Select data from table an highlight in the plot: I'm not sure to use plotly
     #https://yihui.shinyapps.io/DT-rows/
     # Importantísimo fijarse en el output que da el input$table_rows_selected.
     # En este caso, te da como output el número de fila seleccionada de la tabla
    
       
# 4. highlight points in ggplot
      #https://www.littlemissdata.com/blog/highlight. As explained there, there are two ways. I prefer using gghighlight()
       
# 5. Making filtered table more efficient
    # https://rstudio.github.io/DT/shiny.html
     
# 6. Making filtered table interactive
     # apartado 10.3.2 https://mastering-shiny.org/action-dynamic.html
       make_ui
       var <- names(df)
       
       for(val in var){
         if(is.numeric(df[[val]])){
           print(paste0(val, "is numeric"))
         }else{
           if(is.character(df[[val]])){
             print(paste0(val, "is character"))
           }else{
             if(is.factor(df[[val]])){
               print(paste0(val, "is factor"))
             }else{
               print("Format not supported")
             }
           }
         }
       }

# 7. Understanding how reactivity works in shiny
       
       
# A1. Creating a filtered table by groups (not necessary, but I put it because it was done)
  ## SERVER
       # Showing groups
       
       # data_table <- reactive({
       #   data_table <- final_data()
       #   if (input$table_groups == 1){
       #     data_table <- dplyr::filter(data_table,
       #                                 data_table$group == "Not_Significant")
       #   }
       #   if (input$table_groups == 2){
       #     data_table <- dplyr::filter(data_table,
       #                                 data_table$group == "Significant")
       #   }
       #   if (input$table_groups == 3){
       #     data_table <- dplyr::filter(data_table,
       #                                 data_table$group == "FoldChange")
       #   }
       #   if (input$table_groups == 4){
       #     data_table <- dplyr::filter(data_table,
       #                                 data_table$group == "Significant&FoldChange")
       #   }
       #
       #   return(data_table)
       # })

       # output$filtered_table <- renderDataTable({
       #   data_table() %>%
       #     transmute(
       #       Ensembl,
       #       Gene_Symbol = Symbol,
       #       Description,
       #       Entrez,
       #       `log fold change` = signif(log2FoldChange, digits = 2),
       #       `-log10p-value` = signif(padj, digits = 2)
       #     )
       # })