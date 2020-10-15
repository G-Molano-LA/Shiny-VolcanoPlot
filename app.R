#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
library(readxl)
library(plotly)
library(DT)
library(gghighlight)
library(reactlog)
reactlog_enable()

# DEFINED FUNCTIONS

MyPlot <- function(data, col){ # REVISAR: subset important columns (aes: x, y and others)
  df  <- data
  gp  <-
    ggplot(df, aes(x = log2FoldChange, y = minusLog10Pvalue, color = col, text = paste0(
      "Gen: ", Symbol, "\n",
      "Ensembl ID: ", Ensembl, "\n",
      "log2(FoldChange): ",log2FoldChange, "\n",
      "-log10(pvalue): ",minusLog10Pvalue , "\n",
      sep= ""
    ))) +
    geom_point()+
    gghighlight() +
    xlab("log fold change") +
    ylab("-log10(P-value)")
  
  ggplotly(gp, tooltip= "text") %>%
    highlight(on = "plotly_click", color = "white", persistent = FALSE, opacityDim = getOption("opacityDim", 0.1)) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "Volcano_plot")
    )
}


# Define UI for application that creates a volcano plot
ui <- 
  navbarPage("Transcriptoma plots",
             tabPanel("Import",
                      titlePanel("Import and explore data"),
                      sidebarLayout(
                        sidebarPanel(
                          # File upload
                          fileInput("dataplot", label = h3("Upload your file", multiple = TRUE),
                                    accept = c(".csv",". csv2",".tsv",".scsv", ".txt", ".xls", ".xlsx")),
                          width = 3,
                          helpText("Note:'.csv', '. csv2', '.tsv', '.scsv', '.txt', '.xls', '.xlsx' formats are recognized"),
                          
                          hr(),
                          
                          # Calculate pvalue of outlayer elements classified by NA
                          checkboxInput("pvalue_cal", "Calculate p-value of NA elements"),
                          conditionalPanel(
                            condition = "input.pvalue_cal == true",
                            selectInput("pvalue_column", "Select a pvalue column", choices = NULL)
                          ),
                          
                         
                          
                          
                          # Filtered table
                          p(""),
                          checkboxGroupInput("show_vars", "Columns in data to show", choices= NULL)
                        ),
                        
                        mainPanel(
                          # Creating filtering table
                          column(
                            width = 6,
                            h3("Filtered Table"),
                            DT::DTOutput("filtered_tb"),
                            downloadButton("dwtable", "Download Filtered table"),
                            selectInput("table_format", "Select a format", choices = c("tsv", "csv"))
                          )
                        )
                      )
             ),
             
             tabPanel("VolcanoPlot",
                      titlePanel("Plot and data selection"),
                      sidebarLayout(
                        sidebarPanel(
                          # Selecting columns from data
                          p("Select data for Volcano Plot"),
                          selectInput("log2FoldChange", label= "Select log(FoldChange) column (x axis)", choices= NULL),
                          selectInput("padj", "Select the type of p-value (y axis)", choices= NULL),
                          
                          
                          hr(),
                          
                          # P-value and FC Input
                          checkboxInput("threshold", "Select your threshold"),
                          conditionalPanel(
                            condition = "input.threshold == true",
                            numericInput("foldchange", "log2(FoldChange)", value = 4),
                            numericInput("pvalue", "p-value", value = 0.05)
                          )
                        ),
                        
                        mainPanel(
                          plotlyOutput("volcanoPlot",height = "500px", width = "auto"),
                          h3("Selected Data"), 
                          dataTableOutput("selectedProbesTable"),
                          downloadButton("dwselected", "Download Selected Data"),
                          selectInput("data_format", "Select a format", choices = c("tsv", "csv"))
                          
                        )
                      )
                      
                      
             ),
             tabPanel(title = "User Guide",
                      h3("User Guide"),
                      br(),
                      h4(strong("Import and export data")),
                      br(),
                      p("Import and export data tab is used for feeding the web-tool with data provided in a file and then apply the filters to obtain a data table with the desired entries and with the characteristics chosen."),
                      tags$li("On the left side of the tab, there is a panel that allows the user to upload a  file with a size smaller than 100 Mb. In case of having a heavier file, then it is required to send an email to the IBU. In this side bar the user can also choose the data columns that must be displayed in the filtered table."),
                      br(),
                      tags$li("On the right side of the tab, the user can find a filtering table. This data table will be displayed with the chosen columns. In this data table the user will be able to choose some entries which will be highlighted in the plot. This filtered table can be downloaded in two formats '.csv' and '.tsv'"),
                      br(),
                      h4(strong("Plot and data selection")),
                      br(),
                      p("VolcanoPlot tab is used for showing the results."),
                      p("Once the file is uploaded, a message will be displayed asking the user to introduce the columns of the dataset containing the necessary data to create the Volcano Plot."),
                      tags$li("On the left side of the tab, the user can select the two columns of the dataset in order to create the Volcano Plot. Furthermore, by selecting the checkbox, both thresolds, the logFoldChange value and the significance value, will be available to be modified."),
                      br(),
                      tags$li("On the right side of the tab, the used will find the Volcano Plot displayed once the columns are selected. This Volcano plot is interactive, it allows the user to:"),
                      tags$ul(
                        tags$li("Display the information of each entry in the plot by going over the dot with the cursor."),
                        tags$li("Select individual dots of the plot by clicking once on them. This entries will be stored in the 'Selected datatable'."),
                        tags$li("Change the color of the selected dots by first clicking on the bar over the plot and selecting a color, then clicking the entry."),
                        tags$li("Remove all the selected data by double-clicking over the plot.")),
                      p("All the selected entries and Volcano Plot can be downloaded in different formats.")
             )
  )
  
                    

# Define server logic required to create a volcano plot
server <- function(input, output, session) {
  
  #Increase the maximum upload
  options(shiny.maxRequestSize=100*1024^2)
    
 
    # Reading data
    data_plot <- reactive({
      validate(
        need(input$dataplot != "", "Select a data set")
      )
        req(input$dataplot)    #require that the input is available(stop the script if is not)
       # inFile <- input$dataplot
        
        #Check the end of the filename
        ext <- tools::file_ext(input$dataplot$name)
        df <- switch(ext,
               csv   = vroom::vroom(input$dataplot$datapath, delim = ",", col_names = TRUE),
               csv2  = vroom::vroom(input$dataplot$datapath, delim = ",", col_names = TRUE),
               tsv   = vroom::vroom(input$dataplot$datapath, delim = "\t", col_names = TRUE),
               scsv   = vroom::vroom(input$dataplot$datapath, delim = ";", col_names = TRUE),
               xlsx  = openxlsx::read.xlsx(input$dataplot$datapath, colNames = TRUE, rowNames = FALSE),
               xls   = openxlsx::read.xlsx(input$dataplot$datapath, colNames = TRUE, rowNames = FALSE),
               txt   = utils::read.delim2(inpute$dataplot$datapath, header = TRUE, sep = "\t", dec = ","),
               validate("Invalid file; Please upload a .csv, .csv2, .txt, .tsv, .scsv, .xls or .xsls file")
               )
        df["Ensembl"]<- row.names(df)
         return(df)
    })
    
    
   
    
    # Selecting columns from data.frame
    observeEvent(input$dataplot, {
    df <- data_plot()
    req(!is.null(data_plot()))
    variables <- names(df)
    updateSelectInput(session, "log2FoldChange", choices= variables)
    updateSelectInput(session, "pvalue_column", choices= variables)
    updateSelectInput(session, "padj", choices = variables)
    updateCheckboxGroupInput(session,"show_vars", "Columns in data to show", choices = variables, selected = variables)
    })
    
    # Filtered table
    output$filtered_tb <- renderDT({
      df <- data_plot()
      datatable(df[, input$show_vars, drop = FALSE])
    })
    
    # Selecting data from the table to the grafic
    sel_data <- reactiveVal() # Pendiente de acabar.
    
    # Downloading data:NOT WORKING
    # output$dwtable <- downloadHandler(
    #   filename = function(){
    #     paste0("filtered_table.", input$table_format)
    #   },
    #   content = function(file){
    #     switch(input$table_format,
    #            tsv = vroom::vroom_write(data_table(), file),
    #            csv = vroom::vroom_write(data_table(), file)
    #     )
    #   }
    # )
    # 
    # 
    
    # Reading the p-value and FC defined
    pval <- reactive({
      val <- -log10(input$pvalue)
      return(val)
    })
    
    FC <- reactive({
      val <-(input$foldchange)
      return(val)
    })
    
    #Classifiying by p-value and FC, if is required
    final_data <- reactive({
      df <- data_plot()
      req(is.numeric(df[[input$log2FoldChange]]))
      req(is.numeric(df[[input$padj]]))
      df["minusLog10Pvalue"] <- -log10(df[input$padj])
       
      # SUBSET HERE: minusLog10Pvalue, log2FC, Ensembl, Symbol  
      if(input$threshold){
        df["group"] <- "Not_Significant"
        df[which(df['minusLog10Pvalue'] > pval() & abs(df[input$log2FoldChange]) < FC() ),"group"] <- "Significant"
        df[which(df['minusLog10Pvalue'] < pval() & abs(df[input$log2FoldChange]) > FC() ),"group"] <- "FoldChange"
        df[which(df['minusLog10Pvalue'] > pval() & abs(df[input$log2FoldChange]) > FC() ),"group"] <- "Significant&FoldChange"
      }
      return(df)
      })

    
    # Drawing the volcano plot
   
    output$volcanoPlot <- renderPlotly({
      df <- final_data() 
      s <- input$filtered_tb_rows_selected
      if (!is.null(s)) {
        df1 <-filter(df, row.names(df) %in% s)
        MyPlot(data = df, col = NULL)
      }else{
        MyPlot(data =df, col = NULL)
      }
      
    })
    
    #Selecting dots from the plot
    gens_sel <- reactiveVal()
    
    observeEvent(event_data(("plotly_click"), source="volcanoPlot"),{
      sel_gens <- event_data(("plotly_click"), source="volcanoPlot")
      gens_old_new <- c(gens_sel(), sel_gens)
      gens_sel(unique(gens_old_new))
    })
    
        # clear the set of gens when a double-click occurs
    observeEvent(event_data("plotly_doubleclick"), {
      gens_sel(NULL)
    })
    
    
    output$selectedProbesTable <- renderDataTable({
      req(event_data("plotly_click", source="A")) #Making sure that app doesn't do anything before a click
      df <- final_data()
      df1 <- filter(df, row.names(df) %in% gens_sel()) %>%
           transmute(
               Ensembl,
               Gene_Symbol = Symbol,
               Description,
               Entrez,
               `log fold change` = signif(log2FoldChange, digits = 2),
               `-log10p-value` = signif(padj, digits = 2)
           )
      options = list(dom = "tip", pageLength = 10, searching = FALSE)
      df1
    })
     
        
    
    # Downloading selected data from dots:NOT WORKING
    # output$dwselected <- downloadHandler(
    #   filename = function(){
    #     paste0("selected_data.", input$data_format)
    #   },
    #   content = function(file){
    #     switch(input$table_format,
    #            tsv = vroom::vroom_write(data_table(), file),
    #            csv = vroom::vroom_write(data_table(), file)
    #     )
    #   }
    # )
 
    

    
}

# Run the application
shinyApp(ui, server, options = list(height = 600))

