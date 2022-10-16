
library(Matrix)
library(shiny)
library(shinyWidgets)
library(tidyverse)
source("ui_util.R")
source("util_matlab2R_helper_functions.R")

# For javascript
library(shinyjs)

# For notifications
library(shinytoastr)

# For tooltips
library(shinyBS) 
library(shinyhelper)
library(tippy)

options(shiny.maxRequestSize=30*1024^2)

#folder = "data/"
#Tsample <- read.csv(paste(folder, "Drugbank_DDI_edgelist.csv", sep=""))
#Tsample <- read.csv(paste(folder, "STRING_PPI_edgelist.csv", sep=""))

# Dtrain = colSums(Wtrain)
# Dtest = colSums(Wtest)
# D = Dtrain + Dtest

# Dsorted <- sort(D, decreasing = FALSE)
# prc <- (1:length(Dsorted)) / length(Dsorted)
# Dsorted = c(0, Dsorted)
# prc = c(0, prc)
# 
# # get last uniques
# ib <- length(Dsorted)-match(unique(Dsorted),rev(Dsorted))+1
# Dsorted = Dsorted[ib]
# prc = prc[ib]

# target = 0.8
# indx = match(T, prc >= target)
# prc_val = prc[indx]
# degree_val = Dsorted[indx]

# richNodes = D > degree_val

# W = Wtest | Wtrain

# nRichRich = nnzero(W[richNodes, richNodes])/2
# nRichPoor = nnzero(W[richNodes, !richNodes])
# nPoorPoor = nnzero(W[!richNodes, !richNodes])/2
# nEdge = nRichRich + nRichPoor + nPoorPoor
# influence = (nPoorPoor + nRichPoor/2) / nEdge

ui <- fluidPage(
    useToastr(),
    useShinyjs(),
    
    title = "Colipe-App",

    tags$h1("Colipe-App", style = "color:#0072BD;font-weight:bold;"),
    #    tags$div(style = "max-width:510px;", helper(tags$h4(style = "margin-top:0px;", "Evaluating the evaluation setting for link prediction algorithms"), 
    tags$div(style = "max-width:545px;", helper(tags$h4(style = "margin-top:0px;", "Assessing the bias in evaluation data for link prediction algorithms"), 
            , type = "markdown", id = "colipe_description_helper_tooltip", content = "colipe_description")), 
    tags$h4(""), 
    tippy_this("colipe_description_helper_tooltip", "<span style='font-size:14px; margin: 0px;'> Click to learn more <span>", allowHTML = TRUE),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helper(multiChoicePicker("samplenetwork", "Select Network:", c("Biogrid PPI", "Biogrid PPI (Across Time)", "STRING PPI", "Drugbank DDI", "PSP KS", "TRRUST TF", "CTD DDA", "NDFRT DDA"), selected = "Biogrid PPI"),
            , type = "markdown", id = "samplenetwork_tooltip_icon", content = "select_sample_network"),
            tags$hr(style = "margin:8px 0px 6px 0px;"),
            helper(tags$div(style = "margin: 0px;", id = "upload_data_div", 
                     tags$div(
                       fileInput("file1", "Upload Data:", accept = c(".csv")),
                       tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                       tags$style(".checkbox {margin-bottom: 0px;}"),
                     ),
                     #tags$div(
                     #materialSwitch(inputId = "isbipartite", label = "Is Bipartite Network: ", status = "warning", value = F)
                     #)
                     ), 
                   type = "markdown", id = "input_data_tooltip_icon", content = "input_data_format"), 
            tippy_this("input_data_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Click to learn about the input data format.<span>", allowHTML = TRUE),
            tippy_this("samplenetwork_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Select a sample network dataset to run the analysis. <span>", allowHTML = TRUE), 
            tags$hr(style = "margin:6px 0px 4px 0px;"),
             helper(sliderInput("targetRichPerc",
                  "Rich node percentage:",
                  min = 1,
                  max = 50,
                  value = 20, 
                  post = "%"), 
             type = "markdown", id = "richnode_perc_tooltip_icon", content = "rich_node_perc"),
            tippy_this("richnode_perc_tooltip_icon", "<span style='font-size:14px; margin: 0px;'> Select percentage of nodes designated as rich nodes, having the highest degrees in the network. <span>", allowHTML = TRUE), 
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "mainTabset",
                        tabPanel("Summary",     
            
        fluidRow(
            
           column(4, 
                  tags$div(style = "max-width: 300px; margin-left: auto; margin-right: auto;", 
                           tags$p(style = "text-align: center; margin-bottom: -2px; margin-top: 8px; font-size: 16pt;", "Imbalance in edge distribution"),
                           shinycssloaders::withSpinner(plotOutput("distPlot"), hide.ui = FALSE)
                  )
           ),
           column(4, 
                  tags$div(style = "max-width: 230px; margin-left: auto; margin-right: auto;", 
                           tags$p(style = "text-align: center; margin-bottom: -2px; margin-top: 8px; font-size: 16pt;", "Influence on evaluation"),
                           shinycssloaders::withSpinner(plotOutput("distPlot2"), hide.ui = FALSE)
                  )
             ),
           column(4, 
                  tags$div(style = "max-width: 230px; margin-left: auto; margin-right: auto;", 
                           tags$p(style = "text-align: center; margin-bottom: -2px; margin-top: 8px; font-size: 16pt;", "Incentive towards bias in predictions"),
                           shinycssloaders::withSpinner(plotOutput("distPlot3"), hide.ui = FALSE)
                  )
           )
        )
            ),
            tabPanel("Imbalance Analysis", 
                 fluidRow(
                 column(5,
                 tags$div(style = "max-width: 520px; margin-left: auto; margin-right: auto;", 
                          shinycssloaders::withSpinner(plotOutput("distPlot6"), hide.ui = FALSE)
                 )),
                 column(7, 
                 tags$div(style = "max-width: 520px; margin-left: auto; margin-right: auto;", 
                          shinycssloaders::withSpinner(plotOutput("distPlot5"), hide.ui = FALSE),
                          tags$div(style = "max-width: 350px; margin-left: auto; margin-right: auto;",
                                   tags$div(style = "max-width: 70px; float: right; vertical-align: top; right: 0%; top:0px;", 
                                            dropdownButton(
                                              size = "sm", 
                                              icon = icon("cog"), #status = "info", 
                                              right = T, 
                                              up = T, 
                                              tooltip = tooltipOptions(title = "Click to see plot options.", placement = "top"), 
                                              tags$h4(style = "font-weight:bold; margin-bottom: 10px;", "Plot Options"), 
                                              sliderInput("imbalance_edgeplot_fontsize",
                                                          "Font Size:", min = 10, 
                                                          max = 30, value = 20),
                                              materialSwitch(inputId = "imbalance_edgeplot_showlegend", label = "Show Legend", status = "danger", value = T),
                                              tags$p(style = "margin-bottom: 10px;", "")
                                            )),
                               sliderInput("targetPoorPerc",
                                           "Poor node percentage:",
                                           min = 10,
                                           max = 90,
                                           value = 50, 
                                           post = "%")
                          ),

                 ))
                 ),
                 ), 
            tabPanel("Separability Analysis", 
                     tags$div(style = "max-width: 600px; margin-left: auto; margin-right: auto;", 
                              shinycssloaders::withSpinner(plotOutput("distPlot4"), hide.ui = FALSE)
                     ))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observe_helpers(withMathJax = TRUE, help_dir = "helpfiles")
    
    upload_name <- reactiveVal("")
    myvalue <- reactiveVal("sample")
  
    sampleNetworkValue <- reactive({
        message(input$samplenetwork)
        switch(input$samplenetwork, 
               "Biogrid PPI" = "BIOGRID_PPI_edgelist",
               "Biogrid PPI (Across Time)" = "BIOGRID_PPI_acrosstime_edgelist",
               "STRING PPI" = "STRING_PPI_edgelist", 
               "Drugbank DDI" = "Drugbank_DDI_edgelist",
               "PSP KS" = "PSP_KS_edgelist", 
               "TRRUST TF" = "TRRUST_TF_edgelist", 
               "CTD DDA" = "CTD_DDA_edgelist", 
               "NDFRT DDA" = "NDFRT_DDA_edgelist")
    })
    
    sampleNetworkIsBipartite <- reactive({
        switch(input$samplenetwork, 
               "Biogrid PPI" = F, 
               "Biogrid PPI (Across Time)" = F,
               "STRING PPI" = F, 
               "Drugbank DDI" = F, 
               "PSP KS" = T, 
               "TRRUST TF" = T, 
               "CTD DDA" = T, 
               "NDFRT DDA" = T)
    })
    
    parse_sample_data <- reactive({
        folder = "data/"
        Tsample <- read.csv(paste(folder, sampleNetworkValue(), ".csv", sep=""))
        out = list(Tdata = Tsample, isBipartite = sampleNetworkIsBipartite(), isUpload = FALSE)
    })
    
    observeEvent(input$samplenetwork, {
      myvalue("sample")
      reset('file1')
      # cat(paste(as.character(Sys.time()), " - " ,  session_id(), ": ", upload_name(), "-", network_value(), "\n", sep = ""), file = "logs/combined_log.txt", append = T)
    })
    
    observeEvent(input$file1, {
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      upload_dataset()
      req(upload_dataset())
      # cat(paste(as.character(Sys.time()), " - " ,  session_id(), ": ", upload_name(), "-", network_value(), "\n", sep = ""), file = "logs/combined_log.txt", append = T)
    })
    
    upload_dataset <- reactive({
      library(tools)
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      fileInfo <- input$file1
      ext = file_ext(inFile$datapath)
      switch(ext, 
             "csv" = x <- read.csv(inFile$datapath),
             validate(
               need(FALSE, "Invalid file type.")
             )
      )
      myvalue("upload")
      upload_name(fileInfo$name)
      message(cat("Dataset is uploaded: ", fileInfo$name))
      
      validate(
        need(x$NodeIdentifier1, "File format error: NodeIdentifier1 column is missing."),
        need(x$NodeIdentifier2, "File format error: NodeIdentifier2 column is missing."),
        need(x$InTestSet, "File format error: InTestSet column is missing.")
      )
      return(x)
    })
    
    parse_upload_dataset <- reactive({
      req(upload_dataset())
      Tdata = upload_dataset()
      nMatch = nnzero(!is.na(match(Tdata$NodeIdentifier1, Tdata$NodeIdentifier2)))
      #isBipartite = input$isbipartite
      isBipartite = FALSE
      if(nMatch == 0 && isBipartite == FALSE){
        delay(50, toastr_info("The uploaded dataset is detected to be bipartite and will be analyzed as such.", closeButton = F))
        #isolate(input$isbipartite)
        isBipartite = TRUE
      }
      out = list(Tdata = Tdata, isBipartite = isBipartite, isUpload = T)
    })
    
    reactive_dataset <- reactive({
      #req(initialized())
      switch (myvalue(),
              "sample" = D <- parse_sample_data(),
              "upload" = D <- parse_upload_dataset(),
              validate(
                need(FALSE, "Waiting for data...")
              )
      )
      return (D)
    })
    
    
    preprocess_data <- reactive({
        req(reactive_dataset())
        Tsample <- reactive_dataset()$Tdata
        isBipartite = reactive_dataset()$isBipartite
        
        if(!isBipartite){
            ids = unique(c(Tsample$NodeIdentifier1, Tsample$NodeIdentifier2))
            Tsample$id1 = match(Tsample$NodeIdentifier1, ids)
            Tsample$id2 = match(Tsample$NodeIdentifier2, ids)
            nRow = length(ids)
            nCol = length(ids)
        }else{
            ids1 = unique(Tsample$NodeIdentifier1)
            ids2 = unique(Tsample$NodeIdentifier2)
            Tsample$id1 = match(Tsample$NodeIdentifier1, ids1)
            Tsample$id2 = match(Tsample$NodeIdentifier2, ids2)
            nRow = length(ids1)
            nCol = length(ids2)
        }
        
        Tsample$InTestSet = Tsample$InTestSet == "True"
        Ttrain = Tsample[!Tsample$InTestSet, ]
        Ttest = Tsample[Tsample$InTestSet, ]
        
        Wtrain <- sparseMatrix(
            i = Ttrain$id1,
            j = Ttrain$id2,
            x = T,
            dims = c(nRow, nCol)
        )
        
        Wtest <- sparseMatrix(
            i = Ttest$id1,
            j = Ttest$id2,
            x = T,
            dims = c(nRow, nCol)
        )
        
        if(!isBipartite){ # Make symmetric
            Wtrain = Wtrain | t(Wtrain);
            Wtest = Wtest | t(Wtest);
        }
        
        out = list(Wtrain = Wtrain, Wtest = Wtest, nRow = nRow, nCol = nCol, isBipartite = isBipartite)
    })
    
    findRichNodes <- function(D, target, invert = F){
        Dsorted <- sort(D, decreasing = FALSE)
        prc <- (1:length(Dsorted)) / length(Dsorted)
        Dsorted = c(0, Dsorted)
        prc = c(0, prc)
        
        # get last uniques
        ib <- length(Dsorted)-match(unique(Dsorted),rev(Dsorted))+1
        Dsorted = Dsorted[ib]
        prc = prc[ib]
        
        #target = 1 - input$targetRichPerc/100
        indx = match(TRUE, prc >= target)
        prc_val = prc[indx]
        degree_val = Dsorted[indx]
        if(invert){
            richNodes = D <= degree_val
        } else {
            richNodes = D > degree_val
        }
    }
    
    compute_degrees <- reactive({
        req(preprocess_data())
        Wtrain <- preprocess_data()$Wtrain
        Wtest <- preprocess_data()$Wtest
        isBipartite <- preprocess_data()$isBipartite
        
        if(isBipartite){
            Dcol = colSums(Wtrain) + colSums(Wtest)
            Drow = rowSums(Wtrain) + rowSums(Wtest)
        } else {
            Dcol = colSums(Wtrain) + colSums(Wtest)
            Drow = Dcol
        }
        
        out = list(Drow = Drow, Dcol = Dcol)
    })
    
    imbalance_results <- reactive({
        req(preprocess_data())
        Wtrain <- preprocess_data()$Wtrain
        Wtest <- preprocess_data()$Wtest
        isBipartite <- preprocess_data()$isBipartite
        
        dg <- compute_degrees()
        target = 1 - input$targetRichPerc/100
        
        if(isBipartite){
            richCols = findRichNodes(dg$Dcol, target)
            richRows = findRichNodes(dg$Drow, target)
        } else{
            richCols = findRichNodes(dg$Dcol, target)
            richRows = richCols
        }
        
        W = Wtest | Wtrain
        nRichRich = nnzero(W[richRows, richCols])
        nRichPoor = nnzero(W[richRows, !richCols])
        nPoorRich = nnzero(W[!richRows, richCols])
        nPoorPoor = nnzero(W[!richRows, !richCols])
        nnRichPoor = nRichPoor + nPoorRich
        
        # nRichRich = nnzero(W[richNodes, richNodes])/2
        # nRichPoor = nnzero(W[richNodes, !richNodes])
        # nPoorPoor = nnzero(W[!richNodes, !richNodes])/2
        nEdge = nRichRich + nRichPoor + nPoorPoor + nPoorRich
        influence = (nPoorPoor + nnRichPoor/2) / nEdge
        x = list()
        x$percRichRich = nRichRich/nEdge
        x$percRichPoor = (nRichRich+nnRichPoor)/nEdge
        x$influence = influence
        x$targetPerc = target
        x$richCols = richCols
        x$richRows = richRows
        x$richTargetPerc = input$targetRichPerc/100
        return(x)
    })
    
    prepareNodeCategoryHistogramData <- function(D, node_cats_r){
        Dsorted <- sort(D, decreasing = FALSE)
        prc <- (1:length(Dsorted)) / length(Dsorted)
        Dsorted = c(1, Dsorted)
        prc = c(0, prc)
        
        ib <- length(Dsorted)-match(unique(Dsorted),rev(Dsorted))+1
        Dsorted = Dsorted[ib]
        prc = prc[ib]
        
        num_cat = c(0, 0, 0)
        ymin = c(0, 0, 0)
        perc_cat = c(0, 0, 0)
        deg_mins = c(0, 0, 0)
        deg_maxs = c(0, 0, 0)
        for (iRow in 1:length(node_cats_r)){
            indices_row = node_cats_r[[iRow]]
            if(length(indices_row) == 0){
                continue;
            }
            deg_mins[iRow] =  min(D[indices_row])
            deg_maxs[iRow] =  max(D[indices_row])
            num_cat[iRow] = nnzero(indices_row)
            perc_cat[iRow] = nnzero(indices_row) / length(indices_row)
        }
        nedge = sum(num_cat)
        
        df1 = data.frame(x = Dsorted, y = prc)
        df2 = data.frame(deg_mins = deg_mins, deg_maxs = deg_maxs, perc_cat = perc_cat, ymin = ymin, num_cat = num_cat)
        return(list(df1=df1, df2=df2,nedge=nedge))
    }
    
    imbalance_results_part2 <- reactive({
        req(preprocess_data())
        req(imbalance_results())
        Wtrain <- preprocess_data()$Wtrain
        Wtest <- preprocess_data()$Wtest
        isBipartite <- preprocess_data()$isBipartite
        
        dg <- compute_degrees()
        target = input$targetPoorPerc/100
        
        richCols = imbalance_results()$richCols
        richRows = imbalance_results()$richRows
        richTargetPerc = imbalance_results()$richTargetPerc
        moderateTarget = 1 - richTargetPerc + target
        
        validate(
            need((richTargetPerc+target)<1, "Total percentage for rich and poor nodes has to be less than 100%."),
            need(moderateTarget>0, "Moderate node set is empty.")
        )
        
        if(isBipartite){
            poorCols = findRichNodes(dg$Dcol, target, invert = T)
            poorRows = findRichNodes(dg$Drow, target, invert = T)
        } else{
            poorCols = findRichNodes(dg$Dcol, target, invert = T)
            poorRows = poorCols
        }
        
        W = Wtrain | Wtest
        
        moderateCols = !poorCols & !richCols;
        moderateRows = !poorRows & !richRows;
        
        V = matrix(nrow = 3, ncol = 3);
        node_cats_r = list(poorRows, moderateRows, richRows)
        node_cats_c = list(poorCols, moderateCols, richCols)
        for (iRow in 1:length(node_cats_r)){
            for (iColumn in 1:length(node_cats_c)){
                indices_row = node_cats_r[[iRow]]
                indices_col = node_cats_c[[iColumn]]
                nX = nnzero(W[indices_row, indices_col])
                V[iRow, iColumn] = nX
            }
        }
        if(isBipartite){
          P = V / sum(sum(V))
        } else {
          nTotal = sum(sum(V))/2
          for (iRow in 1:length(node_cats_r)){
            V[iRow, iRow] = V[iRow, iRow] / 2
          }
          P = V / nTotal
        }
        
        x = list()
        x$V = V
        x$P = P
        x$row_data = prepareNodeCategoryHistogramData(dg$Drow, node_cats_r)
        x$column_data = prepareNodeCategoryHistogramData(dg$Dcol, node_cats_c)
        return(x)
    })
    
    separability_analysis <- reactive({
        req(preprocess_data())
        Wtrain <- preprocess_data()$Wtrain
        Wtest <- preprocess_data()$Wtest
        nRow = nrow(Wtrain)
        nCol = ncol(Wtrain)
        
        nNegativeSample = 1e6
        i1 = sample.int(nRow, nNegativeSample, replace = T)
        i2 = sample.int(nCol, nNegativeSample, replace = T)
        indices = unique(sub2ind(nRow, nCol, i1, i2))
        
        W = Wtest | Wtrain
        positives = which(W)
        negatives = setdiff(indices, positives)
        
        degreeRow = rowSums(Wtrain)
        degreeCol = colSums(Wtrain)
        
        out <- ind2sub(nRow, nCol, positives);
        d1pos = degreeRow[out$r]
        d2pos = degreeCol[out$c]
        proddegPos = sqrt(d1pos * d2pos);
        
        out <- ind2sub(nRow, nCol, negatives);
        d1neg = degreeRow[out$r]
        d2neg = degreeCol[out$c]
        proddegNeg = sqrt(d1neg * d2neg);
        
        out = suppressWarnings(ks.test(proddegPos, proddegNeg))
        ksstat = out$statistic
        
        sortedProddegPos <- sort(proddegPos, decreasing = FALSE)
        percProddegPos <- (1:length(sortedProddegPos))/length(sortedProddegPos)
        sortedProddegPosU = unique(sortedProddegPos)
        ind = match(sortedProddegPosU, sortedProddegPos)
        percProddegPos = percProddegPos[ind]
        
        sortedProddegNeg <- sort(proddegNeg, decreasing = FALSE)
        percProddegNeg <- (1:length(sortedProddegNeg))/length(sortedProddegNeg)
        sortedProddegNegU = unique(sortedProddegNeg)
        ind = match(sortedProddegNegU, sortedProddegNeg)
        percProddegNeg = percProddegNeg[ind]
        
        out = list(percProddegNeg = percProddegNeg, sortedProddegNegU = sortedProddegNegU, 
                   percProddegPos = percProddegPos, sortedProddegPosU = sortedProddegPosU, 
                   ksstat = ksstat)
    })
    
    output$distPlot <- renderPlot({
        req(imbalance_results())
        a1 = "#0072BD55";
        a2 = "#0072BD"
        df = data.frame(x = "", y = imbalance_results()$percRichRich, y2 = imbalance_results()$percRichPoor)
        p<-ggplot(data=df, aes(x=x, y=y)) +
            geom_bar(aes(x=x, y=y2, fill = a1), stat="identity", col = "#000000", size = 1.25) + 
            geom_text(aes(label=paste(as.character(round(100*y)), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25, size = 7) + 
            geom_text(aes(x = x, y = y2, label=paste(as.character(round(100*y2)), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25, size = 7) + 
            geom_bar(stat="identity", aes(fill  = a2), col = "#000000", size = 1.25) + 
            scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
            theme_minimal() +
            theme(text = element_text(size=18),
                  axis.text.x = element_text(size = 16, angle=90, hjust=1, face = "bold"),
                  legend.key.height = unit(1.25, "cm")) + 
            scale_fill_identity("", guide = "legend", breaks = c(a1, a2), labels = c("Rich-Poor", "Rich-Rich"))
       # p <- p + ggtitle("Edge distribution")
        #p <- p + theme(legend.position="bottom")
        p <- p + labs(x = "", y = "Number of edges")
        p
    })
    
    output$distPlot2 <- renderPlot({
        req(imbalance_results())
        df = data.frame(x = "", y = imbalance_results()$influence)
        
        p<-ggplot(data=df, aes(x=x, y=y)) +
            geom_bar(stat="identity", fill  = "#0072BD", col = "#000000", size = 1.25) + 
            geom_text(aes(x = x, y = y, label=paste(as.character(round(100*y)), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25, size = 7) + 
            geom_hline(yintercept=imbalance_results()$targetPerc, linetype = "dashed", size = 1.25) + 
            scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
            theme_minimal() +
            theme(text = element_text(size=18),
                  axis.text.x = element_text(size = 16, angle=90, hjust=1, face = "bold"),
                  legend.key.height = unit(1.25, "cm"))
       # p <- p + ggtitle("Influence on \n evaluation")
        p <- p + labs(x = "", y = "Influence of the poor nodes")
        p
        
    })
    
    output$distPlot3 <- renderPlot({
        req(separability_analysis())
        df = data.frame(x = "", y = separability_analysis()$ksstat)
        
        p<-ggplot(data=df, aes(x=x, y=y)) +
            geom_bar(stat="identity", fill  = "#0072BD", col = "#000000", size = 1.25) + 
            geom_text(aes(x = x, y = y, label=paste(as.character(round(100*y)), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25, size = 7) + 
            scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
            theme_minimal() +
            theme(text = element_text(size=18),
                  axis.text.x = element_text(size = 16, angle=90, hjust=1, face = "bold"),
                  legend.key.height = unit(1.25, "cm"))
        p <- p + labs(x = "", y = "Predictive power of node degree info.")
        p
    })
    
    output$distPlot4 <- renderPlot({
        req(separability_analysis())
        sp <- separability_analysis()
        
        df1 = data.frame(x = sp$percProddegPos, y = sp$sortedProddegPosU)
        df2 = data.frame(x = sp$percProddegNeg, y = sp$sortedProddegNegU)
        
        v1 = quantile(df1$y, 0.9)
        v2 = quantile(df2$y, 0.9)
        v = round(max(v1, v2))
        
        posColor = "#D95319"
        negColor = "#0072BD"
        p <-ggplot() +
            geom_line(data = df1, aes(x = x, y = y, col = posColor), size = 1.25) +
            geom_line(data = df2, aes(x = x, y = y, col = negColor), size = 1.25) +
            geom_vline(xintercept=0.5, linetype = "dotted", size = 1, color = "#33333377") +
            theme_minimal() +
            theme(text = element_text(size=18),
                  axis.text.x = element_text(size = 15, angle=0, hjust=1),
                  legend.key.height = unit(1.25, "cm")) +
            ylim(0, v) +
            scale_x_continuous(labels = scales::percent, limits = c(0, 1), breaks = c(0, 0.25, 0.50, 0.75, 1)) +
             scale_color_identity("", guide = "legend", breaks = c(posColor, negColor), labels = c("Positives", "Negatives"))
        p <- p + labs(x = "Cumulative distribution function (CDF)", y = "Pref. Attachment score")
        p
    })
    
    output$distPlot5 <- renderPlot({
      #  V = round(runif(9, 0, 324))
      #  dim(V) = c(3, 3)
       # P = V / sum(sum(V))
        
        V = imbalance_results_part2()$V
        P = imbalance_results_part2()$P
        
        font_size_base = input$imbalance_edgeplot_fontsize/20;
        
        node_cat_names = c("1", "2", "3")
        indices = which(V>=0)
        d = ind2sub(mat=V, indices=indices)
        df = data.frame(x = node_cat_names[d$r], y = node_cat_names[d$c], z = V[indices], 
        p = P[indices], plab = paste("(", round_m(100*P[indices], digits=1), "%)", sep = ""))
        
        v_max = max(df$z)
        v_start = floor_m(min(df$z), sigdigits = 1)
      #  v_start = min(100, floor_m(min(df$z), sigdigits = 1))
        v_end = ceil(v_max, sigdigits = 2)
        v_mid = round_m(sqrt(v_start * v_end), sigdigits = 2)
        brk = c(v_start, v_mid, v_end)
        lms = c(v_start, v_end)
        p<-ggplot(df, aes(x=x, y=y, fill= z)) + 
            geom_tile(color = "#222222CC") + 
            geom_text(aes(label=z, vjust = "bottom"), nudge_y = 0.03, size = 6.5 * font_size_base) +
            geom_text(aes(label=plab, vjust = "top"), nudge_y = -0.03, size = 6.5 * font_size_base) +
            scale_x_discrete("Rows", labels = c("1" = "Poor", "2" = "Moderate", "3" = "Rich")) +
            scale_y_discrete("Columns", labels = c("1" = "Poor", "2" = "Moderate", "3" = "Rich")) +
            scale_fill_gradient(name = "", trans = "log", 
                                high = "#FF0000", low = "#FCFCFC", 
                                breaks = brk, limits = lms) +
            theme_minimal() +
            theme(text = element_text(size=17*font_size_base),
                  axis.text.x = element_text(size = 15 * font_size_base, angle=0, face = "bold"),
                  axis.text.y = element_text(size = 15 * font_size_base, angle=0, face = "bold"),
                  legend.key.height = unit(1.25, "cm"))
        p <- p + ggtitle(element_text("Number of Edges", hjust= "center"))
        if(!input$imbalance_edgeplot_showlegend){
          p = p + theme(legend.position="none")
        }
        
        #p <- p + labs(x = "", y = "Informedness of pref. attachment")
        p
    })
    
    output$distPlot6 <- renderPlot({
        req(imbalance_results_part2())
        dat = imbalance_results_part2()$row_data
        
        nedge = dat$nedge
        df1 = dat$df1
        df2 = dat$df2
        deg_maxs = df2$deg_maxs
        num_cat = df2$num_cat
       # P = imbalance_results_part2()$P
        message(df2$perc_cat)
       # df1 = data.frame(x = Dsorted, y = prc)
      #  df2 = data.frame(deg_mins = deg_mins, deg_maxs = deg_maxs, perc_cat = perc_cat, ymin = ymin)
        p<-ggplot() + 
            geom_text(data=df2, aes(x = sqrt(deg_mins*deg_maxs), y = perc_cat*nedge, label=paste(as.character(round(100*perc_cat)), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25, size = 6) + 
            geom_line(data=df1, aes(x=x, y=y*nedge), size = 1, color = "#D95319") +
            geom_rect(data=df2, aes(xmin=deg_mins, xmax=deg_maxs, ymin = ymin*nedge, ymax = perc_cat*nedge), 
                      fill = "#0072BD99", color = "#333333", size = 1)
        p = p + theme_minimal()
        p = p + theme(text = element_text(size=16),
                      axis.text.x = element_text(size = 14, angle=0),
                      axis.text.y = element_text(size = 14, angle=0),
                      axis.title.y = element_text(colour = "#334455"), 
                      legend.key.height = unit(1.25, "cm"))
        p = p + scale_x_continuous(name="Node Degrees", breaks = c(1, deg_maxs[1], deg_maxs[2], deg_maxs[3]), trans = "log", limits = c(1, ceil(max(deg_maxs[3]), sigdigit=1)))
        #p = p + scale_x_continuous(name="Node Degrees", breaks = c(1, 10, 100, 1000, 10000), trans = "log", limits = c(1, max(num_cat)))
        #p = p + scale_x_continuous(name="Node Degrees", breaks = c(0, deg_maxs[1], deg_maxs[2], deg_maxs[3]))
        p = p + scale_y_continuous(name ="Number of nodes", 
                                   sec.axis = sec_axis(~./nedge, 
                                                       breaks = c(0, 0.25, 0.5, 0.75, 1), 
                                                       labels = scales::percent, 
                                                       name=element_text("Cumulative Distribution", colour = "yellow")))
        p <- p + ggtitle(element_text("Number of Nodes", hjust= "center"))
        p
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
