library(shiny)
library(dplyr)
library(janitor)
library(ggvis)
library(tidymodels)
library(shinyjs)
options(shiny.maxRequestSize = 30*1024^2)


# -- csv reader module -- ##
## - UI - ##
csvFileUI <- function(id, label = 'csv file'){
  ns<- NS(id)
  
  tagList(
    fileInput(ns('file'),label),
    checkboxInput(ns('heading'), 
                  'Has Heading', value=T),
    selectInput(ns('quotes'),
                'Quotes',
                choices = c(
                  "None" = "",
                  "Double quote" = "\"",
                  "Single quote" = "'"
                ))
  )
}

## - SERVER - ##
csvFileServer<- function(id){
  moduleServer(
    id,
    ##module function
    function(input,output,session){
      userFile<- reactive({
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      dataframe <- reactive({
        read.csv(userFile()$datapath,
                 header = input$heading,
                 quote = input$quote) %>% janitor::clean_names()
      })
      
      return(dataframe)
    }
  )
}


ui <- 
  fluidPage(
    navbarPage('Data Explorer',
               # ---DATA UPLOAD PAGE ----
               tabPanel('Upload Data',
                        titlePanel('Upload Data'),
                        sidebarLayout(
                          sidebarPanel(em("Please upload data in .csv format only!"), 
                                       csvFileUI("datafile123", 'User Data:')),
                          mainPanel('Data Viewer', 
                                    tabsetPanel(
                                      tabPanel('Summary Tables',
                                               fluidRow(
                                                 column(width = 12,
                                                        h4('Basic Data Summary'),
                                                        tableOutput('introTable'),style = 'overflow-y: auto; overflow-x: auto;')
                                                        ),
                                               br(),
                                               fluidRow(
                                                 column(width = 12,
                                                        h4('Data Summary'),
                                                 tableOutput('summaryTable'),style = 'overflow-y: auto; 
                                             overflow-x: auto;')
                                                        ), 
                                             br(),
                                             fluidRow(
                                               column(width = 12,
                                                      plotOutput('introPlot'))
                                                      )
                                                  ),
                                      tabPanel('View Data',
                                        fluidRow(
                                          column(width = 12, 
                                                 dataTableOutput('viewDataTable'), 
                                                 style = 'overflow-y: auto; 
                                             overflow-x: auto;')
                                                )
                                        ),
                                      
                                    )
                                    )
                          )
                        ),
               
               # ---EXPLORATORY DATA ANALYSIS PAGE ----
               tabPanel('Exploratory Data Analysis',
                        titlePanel('EDA'),
                        sidebarLayout(
                          sidebarPanel('sideBarPanel2',
                                       uiOutput('edaVarSelect'),
                                       actionButton('button','Plot'),actionButton("resetInput", "Reset inputs")),
                          mainPanel('mainPanel2',
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Frequency Plots",
                                                           fluidRow(plotOutput("barPlot")),
                                                           fluidRow(plotOutput('histPlot'))
                                                        ),
                                                tabPanel('Correlation Plot',
                                                           fluidRow(plotOutput("corrPlot"))
                                                         ),
                                                tabPanel('Bee Swarm Plot',
                                                           fluidRow(h4('swarmPlot'),
                                                                    div('For swarm plot, the plots will try to plot each variable on the same plot please select one variable at a time!'),
                                                                    plotOutput('swarmPlot')
                                                                    )
                                                         )
                                    )
                                    )
                                    )
                        ),
               # ---DATA PREPROCESSING PAGE ----
               # ---Panel 3 ---
               tabPanel('Data Preprocessing & Modeling',
                        titlePanel('Preprocessing & Modeling'),
                        sidebarLayout(
                          sidebarPanel('sideBarPanel3',
                                       uiOutput('predictorSelect'),
                                       uiOutput('outcomeSelect'),
                                       selectInput('scaleData',
                                                   'Would you like to scale the data?', 
                                                   choices = c('standardize','normalize','none')),
                                       selectInput('modelSelect', 
                                                   'Select the type of modeling you would like to do?', 
                                                   choices = c('classification','regression')),
                                       conditionalPanel(
                                         condition = "input.modelSelect == 'classification'",
                                         selectInput("classificationMethod", "Classification Methods",
                                                     list("logistic regression", "random forest", "svm"))
                                       ),
                                       conditionalPanel(
                                         condition = "input.modelSelect =='regression'",
                                         selectInput("regressionMethod", "Regression Methods",
                                                     list("linear regression", "svm","xgboost", "random forest"))
                                       ),
                                       hr(),
                                       actionButton('buttonPreprocess','Prep Data')
                                       ),
                          mainPanel('mainPanel3',tabsetPanel(type = "tabs",
                                                             tabPanel('Data Preprocessing',
                                                                      fluidRow(
                                                                        column(
                                                                          width = 6,
                                                                          h4('Model formula Summary'),
                                                                          (verbatimTextOutput('recipeSummary')
                                                             )
                                                             ),
                                                             column(width = 6,h4('Workflow Summary'),verbatimTextOutput('wfSummary'))),

                                                             fluidRow(h4('Preview of data transformation using the above workflow and recipe'),
                                                                      tableOutput('dataTransformPreview')
                                                             ),
                                                             fluidPage(downloadButton("downloadData", "Download Prepped Data")
                                                             )
                                                             
                                                             ),
                                                             tabPanel('Modeling',
                                                                      mainPanel(
                                                                        conditionalPanel(condition = "input.modelSelect == 'classification'",
                                                                                         
                                                                                         fluidRow(plotOutput('rocPlot')),br(), fluidRow(plotOutput('calibrationPlot'))
                                                                                         ),
                                                                        conditionalPanel(
                                                                          condition = "input.modelSelect =='regression'",
                                                                          h4('regression'), 
                                                                          fluidRow(plotOutput('PredActualPlot')))
                                                                      )
                                                             ),
                                                             tabPanel('View Predictions',fluidRow(h4('Test Data predictions'), 
                                                                                                  dataTableOutput('predictionTables')))
                          )
                          )))
               
      
    )
  )
  

###########################################
###########################################
###-------------SERVER------------------###
###########################################
###########################################
server <- function(input, output, session) {
  ## -- Panel 1 (Upload Data) -- ##
  ## -- data upload -- ##
  datafile <- csvFileServer("datafile123")
  
  
  ## -- uploaded data visualization -- ##
  output$introTable <- renderTable({
    datafile() %>% DataExplorer::introduce()
  })
  
  
  
  output$summaryTable<- renderTable({
    datafile() %>% 
      skimr::skim() %>%  arrange(desc(n_missing)) %>% 
      rmarkdown::paged_table()
  })
  
  output$viewDataTable <- renderDataTable({
    datafile()
  })
  
  output$introPlot <- renderPlot({
    datafile() %>% DataExplorer::plot_intro()
  })
  #######################
  
  # -- data selectors -- ##

  
  output$edaVarSelect <- 
    renderUI({
      options<-datafile() %>% names()
      checkboxGroupInput('edaVarSelect', 'Select variables to explore', choices = options)    
    })
  
  output$data <- eventReactive(input$button,{
    df <- datafile() %>% select(input$edaVarSelect)
  })
  
  observeEvent(input$resetInput, {
    options<-datafile() %>% names()
    updateCheckboxGroupInput(session, "edaVarSelect",choices = options, selected = NULL)
  })
  #########################
  
  ## -- Panel 2 Visualization & EDAs -- ##
  
  ## -- Dynamic Data Selection --##
  dataPlots <- eventReactive(input$button,{
    df<- datafile() %>% select(as.character(input$edaVarSelect))
  })
  
  ## -- bar plot -- ##
  bPlot <- eventReactive(input$button, {
    DataExplorer::plot_bar(dataPlots())
  })
  output$barPlot <- renderPlot({
    
    bPlot()
  })
  
  ## -- Frequency chart / histogram -- ##
  hPlot <- eventReactive(input$button, {
    DataExplorer::plot_histogram(dataPlots())
  })
  output$histPlot <- renderPlot({
    hPlot()
  })
  
  ## -- correlations -- ##
  cPlot <- eventReactive(input$button, {
    DataExplorer::plot_correlation(na.omit(dataPlots()))
  })
  output$corrPlot <- renderPlot({
    cPlot()
  })
  
  ## -- Bee swarm plot -- ##
  beePlot <- eventReactive(input$button, {
    df<- dataPlots()
    
    df <- df %>% select_if(is.numeric)
    
    beeswarm::beeswarm(df)
    # numPlots<- length(names(df))
    # par(mfrow = c(numPlots,1))
    # # browser()
    # for (i in numPlots){
    #   beeswarm::beeswarm(df[i])
    # }
    
  })
  
  
  output$swarmPlot <- renderPlot({
    beePlot()
  })

## -- Panel 3 -- ###

  
  output$predictorSelect <-
    renderUI({
      options <- datafile()
      varSelectInput('predictorSelect',
                     'Please select the predictor variables:',
                     multiple = T, data = options)
    })
  output$outcomeSelect <-
    renderUI({
      options<-datafile()
      varSelectInput('outcomeSelect', 'Select outcome variable', data = options)
    })
  # ----- split data
  splitData <- eventReactive(input$buttonPreprocess,{
    set.seed(123)
    
    df<- datafile() %>% 
      select(as.character(input$predictorSelect)) %>% 
      cbind(datafile() %>% 
              select(as.character(input$outcomeSelect))) %>% mutate_if(is.logical,function(x) ifelse(x ==T,1,0))
    if(input$modelSelect == 'classification'){
      df<- df %>% 
        mutate(
        across(paste0(as.character(input$outcomeSelect)), factor)
      ) %>% initial_split()
      
    } else{
      df<- initial_split(df)
    }
  })
  

  
  trnData<- reactive({training(splitData())})
  tstData<- reactive({testing(splitData())})
  
 
  # ----- model recipe
  
  rec_formula<- reactive({
    # NEED TO BE ABLE TO FACTORIZE TRAINING & TESTING DATA OUTCOME VARIABLE. RIGHT NOW ONLY CAN FIT REGRESSION MODEL B/C OUTCOME VARIABLE DOES NOT CONVERT TO FACTOR AUTOMATICALLY

    
    if(input$scaleData == 'standardize'){
      r <-recipe(as.formula(paste0(
        input$outcomeSelect,
        " ~ ."
      )),
      data = trnData()
      ) %>%
        step_normalize(all_numeric_predictors()) %>% 
        step_dummy(all_nominal_predictors())
    } else if (input$scaleData == 'normalize'){
      r<-recipe(as.formula(paste0(
        input$outcomeSelect,
        " ~ ."
      )),
      data = trnData()
      ) %>%
        step_range(all_numeric_predictors()) %>% 
        step_dummy(all_nominal_predictors())
    } else if (input$scaleData == 'none'){
      r<-recipe(as.formula(paste0(
        input$outcomeSelect,
        " ~ ."
      )),
      data = trnData()
      ) %>%
        step_dummy(all_nominal_predictors())
    }
    

  })
  
  
  output$recipeSummary <- renderPrint({
    rr <- req(rec_formula())
    cat("reactive recipe gives\n")
    print(summary(rr))
  })
  # ------ data transformation
  dataTransform <- reactive({
    preppedRecipe <- recipes::prep(rec_formula(), data = trnData())
    trainDataPreped<- preppedRecipe %>% juice()
    testDataPreped <- recipes::bake(preppedRecipe, new_data = tstData())

    rbind(trainDataPreped, testDataPreped)
  })

  output$dataTransformPreview <- renderTable({head(dataTransform())})

  # download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dataTransform(), file)
    }
  )
  
  # ----- choosing model
  mod<- reactive({
    modelSelectMode <- paste0(switch(input$modelSelect,
                'classification' = 'classification', 
                'regression' = 'regression'))
    if (input$classificationMethod == 'logistic regression'& modelSelectMode == 'classification'){
      model<- logistic_reg(engine = 'glm', 
                           mode =modelSelectMode)
    } else if(input$classificationMethod == 'random forest'& modelSelectMode == 'classification'){
      model<- rand_forest(min_n = 10,
                          engine='ranger',
                          mode = paste0(modelSelectMode))
    } else if(modelSelectMode == 'classification'& input$classificationMethod == 'svm'){
      model<- svm_linear(engine = 'kernlab',
                         mode = paste0(modelSelectMode))
    } else if(input$regressionMethod == 'linear regression'& modelSelectMode == 'regression'){
      model<- linear_reg(engine = 'glm',
                         mode = paste0(modelSelectMode))
    }else if(input$regressionMethod == 'xgboost'& modelSelectMode == 'regression'){
      model <- boost_tree(mode = paste0(modelSelectMode),
                          engine = "xgboost")
    }else if(input$regressionMethod == 'random forest'& modelSelectMode == 'regression'){
      model<- rand_forest(min_n = 10,
                          engine='ranger',
                          mode = paste0(modelSelectMode))
    } else if(input$regressionMethod == 'svm'& modelSelectMode == 'regression'){
      model<- svm_linear(engine = 'kernlab',
                         mode = paste0(modelSelectMode))
    }
    return(model)
    
  })
  # ----- create workflow
  wf<- reactive({
    workflow() %>% 
      add_recipe(rec_formula()) %>% 
      add_model(mod())
  })
  
  output$wfSummary <- renderPrint({
    w<- req(wf())
    print(w)
  })
  
  #----- modeling panel page
  #---- fit model ----
  wfFit <- reactive({
    wf() %>% fit(data =trnData())
  })
  
  
  output$predictionTables <- renderDataTable({
    predArr()

  })
  

  
  #create prediction array
  predArr<- reactive({
    augment(wfFit(),tstData())
  })
  
  #plot roc_auc
  #*****error
  plotRocAuc <- reactive({

    if(input$modelSelect == 'classification'){
      
      df <- predArr() 
      
      y <- df %>% select(paste0(input$outcomeSelect)) %>% as_vector()
      y_hat <- df %>% select(.pred_class) %>% as_vector()
      prob_1<-df %>% select(.pred_1) %>% as_vector()
      prob_0 <- df %>% select(.pred_0) %>% as_vector()
      
      # rocArr<- pROC::roc(trueArr,estArr)
      rocArr<- PRROC::roc.curve(y, #true values
                                y_hat, # predicted values
                                weights.class1 = prob_1, # prob of true
                                weights.class0 = prob_0, # prob of false

                                curve = T, 
                                sorted = F, 
                                rand.compute = T)
      
      
      plot(rocArr, color = 'blue',
           main= paste0(input$classificationMethod, ' ROC Plot'), ylab = 'TPR', rand.plot = T
           )
    }
    
  })
  output$rocPlot<-renderPlot({plotRocAuc()})
  
  aucValue <- reactive({
    if(input$modelSelect == 'classification'){
      df <- predArr()

      trueArr <- predArr() %>%
        select(input$outcomeSelect) %>% as_vector()
      estArr<-df %>% select(.pred_1) %>% as_vector()
      pr_auc_vec(truth = trueArr, estimate = estArr)
      # predArr()
    }
  })
  # output$aucVal<- renderPrint({
  #   aucValue()
  # })
  
  #plot calibration plot
  plotCalibration <- reactive({
    if(input$modelSelect == 'classification'){
      
      df <- predArr() 
      
      y <- df %>% select(paste0(input$outcomeSelect)) %>% as_vector()
      prob_1<-df %>% select(.pred_1) %>% as_vector()
      # y_hat <- df %>% select(.pred_class) %>% as_vector()
      # prob_0 <- df %>% select(.pred_0) %>% as_vector()
      
     
    
    gbm::calibrate.plot(y = y, p = prob_1,main = 'Calibration Plot')
    
    }
  })
  output$calibrationPlot<-renderPlot({plotCalibration()})
  
  #metrics for regression
  regressionMetrics<- reactive({
    if(input$modelSelect == 'regression'){
      
      trueArr <- predArr() %>% 
        select(input$outcomeSelect) %>% as_vector()
      estArr <- predArr() %>% 
        select(.pred) %>%  as_vector()
      rsq_vec(truth = trueArr, 
          estimate = estArr)
    }
  })
  output$PredActualPlot<-renderPlot({predActPlot()})
  predActPlot <- reactive({
    if(input$modelSelect == 'regression'){
      ggplot(data = predArr(),
             aes_string(y = input$outcomeSelect))+
        geom_point(aes(x = .pred))+ 
        # geom_abline(intercept = 0,
        #             slope = 1,
        #             color = "red",
        #             size = .5)+
        ggtitle('Actual vs Predicted', 
                subtitle = paste('r2 : ',regressionMetrics()
                                       
                                 )
                )
    }
  })
  
}

shinyApp(ui, server)