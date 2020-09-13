library(shiny)
library(curl)
library(e1071)
library(data.table)
library(caret)
library(randomForest) 
library(gbm)    
library(mboost) 
library(klaR)   
library(plyr)
library(dplyr)

test.url <- 'https://github.com/coursera-tjf/ddp_shiny/raw/master/data/test.csv'
train.url <- 'https://github.com/coursera-tjf/ddp_shiny/raw/master/data/train.csv'


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

getProcessData <- function(p) {

    raw.training.all <- fread(train.url)
    # handle missing values 
    raw.training.all <- raw.training.all[complete.cases(raw.training.all),]
    raw.training.outcome <- raw.training.all[,"Survived",with=F]
    raw.training.predictors <- raw.training.all[,-"Survived",with=F]
    raw.testing.predictors <- fread(test.url)
    
    # process data
    process.data <- function(x) {
        # factorize variables
        if ('Survived' %in% colnames(x))
            x$Survived <- factor(x$Survived, levels=c(0,1), labels=c('No', 'Yes'))
        x$Pclass <- factor(x$Pclass, levels=c(1,2,3), labels=c('1st', '2nd', '3rd'))
        x$Sex <- factor(x$Sex, levels=c('male', 'female'), labels=c('male', 'female'))
        x$Embarked <- factor(x$Embarked, levels=c('C', 'Q', 'S'),
                             labels=c('Cherbourg', 'Queenstown', 'Southampton'))
        # factorize rest 
        x$Name <- factor(x$Name)
        x$Ticket <- factor(x$Ticket)
        x$Cabin <- factor(x$Cabin)
        x
    }
    # process data
    proc.training.all <- process.data(raw.training.all)
    proc.training.outcome <- proc.training.all[,"Survived", with=F]
    proc.training.predictors <- proc.training.all[,-"Survived",with=F]
    proc.testing.predictors <- process.data(raw.testing.predictors)
    
    # create training, validation, and training sets
    inValidation <- createDataPartition(y=proc.training.outcome$Survived, p=p/100, list=FALSE)
    proc.validation <- proc.training.all[inValidation,]
    proc.training <- proc.training.all[-inValidation,]
    proc.testing <- proc.testing.predictors
    # raw version
    raw.validation <- raw.training.all[inValidation,]
    raw.training <- raw.training.all[-inValidation,]
    raw.testing <- raw.testing.predictors
    # bundle into list 
    list(rtr=raw.training, rv=raw.validation, rte=raw.testing, ptr=proc.training,
         pv=proc.validation, pte=proc.testing)
}

shinyServer(
    function(input, output) {
  
        dataInput <- reactive({

            getProcessData(40) 
        })
        
        ## Data Summary
        output$PredictorsSummaryOut <- renderPrint({ 
            summary(
                if(input$RawOrProc == 'p') {      
                    switch(input$SummaryOf,
                           a = rbind(dataInput()$ptr[,-"Survived",with=F], 
                                     dataInput()$pv[,-"Survived",with=F], dataInput()$pte),
                           tv = rbind(dataInput()$ptr[,-"Survived",with=F], 
                                      dataInput()$pv[,-"Survived",with=F]),
                           t = dataInput()$ptr[,-"Survived",with=F]
                    )
                } else {                         
                    switch(input$SummaryOf,
                           a = rbind(dataInput()$rtr[,-"Survived",with=F], 
                                     dataInput()$rv[,-"Survived",with=F], dataInput()$rte),
                           tv = rbind(dataInput()$rtr[,-"Survived",with=F], 
                                      dataInput()$rv[,-"Survived",with=F]),
                           t = dataInput()$rtr[,-"Survived",with=F]
                    )
                } 
            )
        })
        output$OutcomeSummaryOut <- renderPrint({ 
            summary(
                if(input$RawOrProc == 'p') {      
                    switch(input$SummaryOf,
                           a = rbind(dataInput()$ptr[,"Survived",with=F], 
                                     dataInput()$pv[,"Survived",with=F]),
                           tv = rbind(dataInput()$ptr[,"Survived",with=F], 
                                      dataInput()$pv[,"Survived",with=F]),
                           t = rbind(dataInput()$ptr[,"Survived",with=F]) 
                    )
                } else {                          
                    switch(input$SummaryOf,
                           a = rbind(dataInput()$rtr[,"Survived",with=F], 
                                     dataInput()$rv[,"Survived",with=F]),
                           tv = rbind(dataInput()$rtr[,"Survived",with=F], 
                                      dataInput()$rv[,"Survived",with=F]),
                           t = rbind(dataInput()$rtr[,"Survived",with=F]) 
                    )
                } 
            )
        })
        
        ## Explore Data
        # pairs plot 
        xcolsIgnore <- c('PassengerId', 'Name','Ticket', 'Cabin')#, 'Survived')
        output$expPairsPlot <- renderPlot({
            
            featurePlot(x=dataInput()$ptr[,-xcolsIgnore, with=F], 
                        y=dataInput()$ptr$Survived, 
                        plot='pairs', auto.key=list(columns=2))
        })

        output$expXaxisVarSelector <- renderUI({
            selectInput('expXaxisVar', 'Variable on x-axis', 
                        choices=as.list(colnames(dataInput()$ptr)), selected='Pclass')
        })

        getYaxisVarSelector <- function(geom) { 

            widget <- selectInput('expYaxisVar', 'Variable on y-axis', 
                                  choices=as.list(colnames(dataInput()$ptr)), selected='Sex')
            wy <- widget
            woy <- disable(widget)
            switch(geom,
                   point = wy,
                   boxplot = wy,
                   histogram = woy,
                   density = woy,
                   jitter = wy
            )
        }
        output$expYaxisVarSelector <- renderUI({
            getYaxisVarSelector(input$singlePlotGeom)
        })
        output$expColorVarSelector <- renderUI({
            selectInput('expColorVar', 'Variable to color by', 
                        choices=as.list(c('None', colnames(dataInput()$ptr))),
                        selected='Survived')
        })
        # create ggplot statement based on geom
        add_ggplot <- function(geom) {
            gx <- ggplot(dataInput()$ptr, aes_string(x=input$expXaxisVar))
            gxy <- ggplot(dataInput()$ptr, aes_string(x=input$expXaxisVar, y=input$expYaxisVar))
            switch(geom,
                   point = gxy,
                   boxplot = gxy,
                   histogram = gx,
                   density = gx,
                   jitter = gxy
            )
        }
        # create ggplot geom
        add_geom <- function(geom) {
            switch(geom,
                   point = geom_point(aes_string(color=input$expColorVar)),
                   boxplot = geom_boxplot(aes_string(color=input$expColorVar)),
                   histogram = geom_histogram(aes_string(color=input$expColorVar)),
                   density = geom_density(aes_string(color=input$expColorVar)),
                   jitter = geom_jitter(aes_string(color=input$expColorVar))
            )
        }
        output$expSinglePlot <- renderPlot({
            g <- add_ggplot(input$singlePlotGeom) + add_geom(input$singlePlotGeom)
            print(g)
        })
        
        ## Prediction Model
        # create feature selection
        output$featureSelectInput <- renderUI({
            selectInput('featureSelect', 'Select features to generate model', 
                        choices=as.list(colnames(dataInput()$ptr[,-"Survived",with=F] )),
                        multiple = TRUE, selected=c('Sex', 'Age', 'Pclass'))
        })
        # apply model to training set
        applyModel <- function(modelType, features) {
            if (modelType == 'gbm')
                train(Survived ~ ., 
                      data=select(dataInput()$ptr, one_of(c('Survived', features))), 
                      method=modelType, preProcess=input$preProcessMethods, verbose=F)
            else
                train(Survived ~ ., 
                      data=select(dataInput()$ptr, one_of(c('Survived', features))), 
                      method=modelType, preProcess=input$preProcessMethods)
        }
        # reactive functions to run and evaluate model
        runModel <- reactive({
            applyModel(input$machLearnAlgorithm, input$featureSelect)
        })
        
        output$summaryModel <- renderPrint({
            if (!is.null(names(summary(runModel()))))
                summary(runModel())
            else
                'Same as Final Model Fit above'
        })
        # summary of final model
        output$finalModel <- renderPrint({
            runModel()
        })
        
        evalModel <- function(testData, features) {
            predictions <- predict(runModel(), select(testData, one_of(features)))
            # if length or predictions does not match length of testData, remove incomplete cases
            if (length(predictions) != nrow(testData))
                truthes <- testData[complete.cases(testData),]$Survived
            else
                truthes <- testData$Survived
            # generate confusion matrix
            confusionMatrix(predictions, truthes)
        }
        # accuracy of final model
        output$inSampleAccuracy <- renderPrint({
            evalModel(dataInput()$ptr, input$featureSelect)
        })
        output$outOfSampleAccuracy <- renderPrint({
            evalModel(dataInput()$pv, input$featureSelect)
        })
        
        # test data set predictions
        output$testPredictions <- renderTable({
            predictions <- predict(runModel(), select(dataInput()$pte, one_of(input$featureSelect)))
            predictions <- cbind(dataInput()$pte$PassengerId, as.character(predictions))
            colnames(predictions) <- c('PassengerId', 'Survived')
            predictions
        })
        
        
        
    }
)