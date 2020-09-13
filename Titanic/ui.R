library(shiny)
library(shinythemes)

disable <- function(x) {
    if (inherits(x, 'shiny.tag')) {
        if (x$name %in% c('input', 'select'))
            x$attribs$disabled <- 'disabled'
        x$children <- disable(x$children)
    }
    else if (is.list(x) && length(x) > 0) {
        for (i in 1:length(x))
            x[[i]] <- disable(x[[i]])
    }
    x
}

shinyUI(
    fluidPage(theme = shinytheme("slate"), titlePanel('Machine Learning on Titanic Survivor Dataset'),
              
              navbarPage('',
                         tabPanel('Data Summary',
                                  sidebarLayout(
                                      sidebarPanel(
                                          #          sliderInput('sliderTrainValidation', 
                                          #            'Select percentage of training sample for validation', 
                                          #            min=0, max=50, value=40),
                                          radioButtons('RawOrProc', 'Which data for summary?',
                                                       c('Processed'='p', 'Raw Data'='r')
                                          ),
                                          radioButtons('SummaryOf', 'Display data summary for predictors of:',
                                                       c('All data (Training + Validation + Test)'='a', 
                                                         'Training + Validation'='tv', 'Training only'='t')
                                          )
                                      ),
                                      mainPanel(
                                          h4('Predictors'),
                                          verbatimTextOutput('PredictorsSummaryOut'),
                                          h4('Outcome (Training and Validation only)'),
                                          verbatimTextOutput('OutcomeSummaryOut')
                                      )
                                  )
                         ),
                         
                         # explore data
                         tabPanel('Explore Data',
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput('singlePlotGeom', 'Select plot type', 
                                                      choices=c('point', 'boxplot', 'histogram', 'density', 'jitter'),
                                                      selected='jitter'),
                                          uiOutput('expXaxisVarSelector'),
                                          uiOutput('expYaxisVarSelector'),
                                          uiOutput('expColorVarSelector')
                                      ),
                                      mainPanel(
                                          h4('One and Two Variable Plot'),
                                          plotOutput('expSinglePlot'),
                                          h4('Pairs Plot (only non-zero variance variables shown)'),
                                          plotOutput('expPairsPlot', width='100%', height='800px')
                                      )
                                  )
                         ),
                         
                         tabPanel('Prediction Model',
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput('preProcessMethods', 'Select data preprocessing method(s)',
                                                      choices=c(
                                                          'Center Data' = 'center', 
                                                          'Scale Data' = 'scale', 
                                                          'Box Cox Transform Data' = 'BoxCox',
                                                          'Yeo-Johnson Transform Data' = 'YeoJohnson',
                                                          'Inpute missing data with k-nearest neighbors' = 'knnImpute',
                                                          'Principle Component Analysis (95% variance)' = 'pca'
                                                      ),
                                                      selected='BoxCox', 
                                                      multiple=TRUE
                                          ),
                                          uiOutput('featureSelectInput'),
                                          selectInput('machLearnAlgorithm', 
                                                      'Select the model or machine learning algorithm',
                                                      choices= c('Generalized Linear Model (logit)' = 'glm',
                                                                 'Random Forests (may take a few minutes)' = 'rf',
                                                                 'Gradient Boosting' = 'gbm',
                                                                 'Boosted Generalized Linear Model' = 'glmboost',
                                                                 'Linear Discriminant Analysis' = 'lda',
                                                                 'Naive Bayes' = 'nb'), 
                                                      selected='glm')
                                      ),
                                      mainPanel(
                                          h4('Final model fit'),
                                          verbatimTextOutput('finalModel'),
                                          h4('Summary'),
                                          verbatimTextOutput('summaryModel')
                                      )
                                  )
                         ),
                         
                         # Evaluate model
                         tabPanel('Model Evaluation',
                                  fluidRow(
                                      column(6,
                                             wellPanel(
                                                 h4('Estimated In Sample Accuracy (within training data)'),
                                                 verbatimTextOutput('inSampleAccuracy')
                                             )
                                      ),
                                      column(6,
                                             wellPanel(
                                                 h4('Estimated Out of Sample Accuracy (within verification data)'),
                                                 verbatimTextOutput('outOfSampleAccuracy')
                                             )
                                      )
                                  )
                         ),
                         
                         tabPanel('Test Data ',
                                  h4('Predicted survival of individuals in the test data set'),
                                  tableOutput('testPredictions')
                         )
                         
              )
    )
)