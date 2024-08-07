library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(e1071)
library(rpart)
library(psych)
library(corrplot)
library(caret)
library(pROC)
library(ROCR)
library(pROC)
library(pdp)
library(randomForest)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Exploration de données"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chargement de données", tabName = "data_upload", icon = icon("database")),
      menuItem("Analyses", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Modélisation", tabName = "modelling", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_upload",
              fluidRow(
                box(title = "Upload", status = "primary", solidHeader = TRUE, width = 6,
                    fileInput("file", "Choose a CSV file:"),
                    checkboxInput("header", "Does the file have a header row?", TRUE),
                    actionButton("loadData", "Load Data"),
                    br(), br(),
                ),
                box(title = "Preproccesing", status = "primary", solidHeader = TRUE, width = 6,
                    selectInput("categorical_variables", "Select Categorical and Nominal Variables:", multiple = TRUE, choices = NULL),
                    selectInput("column_to_convert", "Select Column to Convert", choices = NULL),
                    actionButton("convert_to_numeric", "Convert to Numeric"),
                    br(), br(),
                    selectInput("training_variable", "Variable for Training:", choices = ""),
                    selectInput("missing_columns", "Select Variables with Missing Values:", multiple = TRUE, choices = NULL),
                    actionButton("removeMissingCols", "Remove Selected Columns"),
                    actionButton("removeMissingRows", "Remove Rows with NA"),
                    actionButton("remove_duplicates", "Remove Duplicates"),
                    br(), br(),
                    numericInput("rows_to_delete", "Number of Rows to Delete", value = 0, min = 0),
                    numericInput("target_value", "Target Value to Delete", value = 4),
                    actionButton("delete_rows", "Delete Rows"),
                    br(), br(),
                    actionButton("normalize", "Normalize Data"),
                    actionButton("zscore_normalize", "Normalize Data (Z-score)"),
                    br(), br(),
                    actionButton("refreshData", "Refresh Data")
                )
              )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Aperçu des données", status = "primary", solidHeader = TRUE,
                    style = "overflow-x: auto;", # Apply style here
                    DTOutput("table"),
                    width = 12
                ),
                box(title = "Variable Selection", status = "primary", solidHeader = TRUE, width = 2 ,
                    h3("Variable Selection"),
                    selectInput("x_variable", "X Variable:", choices = ""),
                    selectInput("y_variable", "Y Variable:", choices = "")
                ),
                box(title = "Statistiques descriptives unidimensionnelle", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      
                      tabPanel("Histogramme", numericInput("binwidth_input", "Binwidth:", value = 0.2, min = 0.01, step = 0.01), plotlyOutput("histogram")),
                      tabPanel("Box Plot", plotlyOutput("boxplot")),
                      tabPanel("Density Plot", plotlyOutput("densityplot")),
                      tabPanel("Extra", verbatimTextOutput("univariate_analysis")),
                      tabPanel("Résume", verbatimTextOutput("summary"))
                      
                    ),
                    width = 5 # Half width
                ),
                box(title = "Analyse bidimensionnelle", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("correlation plot",plotlyOutput("bivariate_analysis")),
                      tabPanel("Correlation Matrix", plotOutput("correlation_matrix_plot"))
                    ),
                    width = 5 # Half width
                ),
                box(title = "Label Analysis", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("Bar Plot", plotOutput(outputId = "label_analysis", height = 500, width = 600)),
                      tabPanel("Pie Chart", plotOutput("pie_chart",height = 500, width = 600))
                    ),
                    width = 6 # Half width
                )
              )
      ),
      
      
      # Modelling Tab
      tabItem(tabName = "modelling",
              fluidRow(
                box(title = "SVM", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("résume",verbatimTextOutput("model_results")),
                      tabPanel("Evaluation", 
                               plotOutput("confusion_matrix_plot"),
                               plotOutput("roc_auc_curve_plot")
                      ),
                      tabPanel("PDPs", uiOutput("pdp_output")),
                      
                      conditionalPanel(
                        condition = "input.train_model > 0",
                        downloadButton("downloadSVM", "Download SVM Model") # Add this line
                      )                               ),width = 6),
                box(title = "Random Forest", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("résume",verbatimTextOutput("model_results_RF")),
                      tabPanel("Evaluation", 
                               plotOutput("confusion_matrix_plot_RF"),
                               plotOutput("roc_auc_curve_plot_RF")
                      ),
                      tabPanel("Feature Importances",
                               plotOutput("feature_importance_plot")
                      ),
                      conditionalPanel(
                        condition = "input.train_model > 0",
                        downloadButton("downloadRF", "Download Random Forest Model") # Add this line
                      )
                      
                    ), width = 6
                ),
                box(title = "Logistic Regression", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("résume",verbatimTextOutput("model_results_LR")),
                      tabPanel("Evaluation", 
                               plotOutput("confusion_matrix_plot_LR"),
                               plotOutput("roc_auc_curve_plot_LR")
                      ),
                      tabPanel("Feature Importances",
                               plotOutput("feature_importance_plot_LR")
                      ),
                      conditionalPanel(
                        condition = "input.train_model > 0",
                        downloadButton("downloadLR", "Download Logistic Regression Model") # Add this line
                      )
                      
                    ), width = 6
                ),
                box(title = "Spliting Data", status = "primary", solidHeader = TRUE,
                    column(6, 
                           sliderInput("training_percentage", "Training Set Percentage:", value = 70, min = 1, max = 99, step = 1, ticks = FALSE, width = "100%"),
                           sliderInput("test_percentage", "Test Set Percentage:", value = 30, min = 1, max = 99, step = 1, ticks = FALSE, width = "100%")
                    ),
                    column(6, 
                           actionButton("train_model", "Entraîner le modèle")
                    )
                )
              )
      )
    )))






server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)
  trained_model <- reactiveVal(NULL)
  test_set <- reactiveVal(NULL)
  target_variable <- reactive({input$training_variable})
  training_set <- reactiveVal()
  feature_names <- reactive(NULL)
  trained_model_RF <- reactiveVal(NULL)
  logistic_model <- reactiveVal(NULL)
  
  observe({
    if (!target_variable() %in% names(data())) {
      print(paste("The target variable", target_variable(), "is not in the dataset."))
    }
  })  
  
  observeEvent(input$file, {
    if (!is.null(input$file) && !identical(input$file, "")) {
      file_ext <- tools::file_ext(input$file$name)
      if (tolower(file_ext) == "csv") {
        dataInput <- read.csv(input$file$datapath, header = input$header)
        dataInput <- dataInput[,-1]
        last_column <- names(dataInput)[ncol(dataInput)]
        dataInput[[last_column]] <- as.factor(dataInput[[last_column]])
        data(dataInput)
        updateSelectInput(session, "x_variable", choices = names(dataInput))
        updateSelectInput(session, "y_variable", choices = names(dataInput))
        updateSelectInput(session, "training_variable", choices = names(dataInput),selected = last_column)
        updateSelectInput(session, "categorical_variables", choices = names(dataInput))
        updateSelectInput(session, "missing_columns", choices = names(dataInput)[colSums(is.na(dataInput)) > 0])
        showModal(modalDialog("Le fichier a été chargé avec succès.", easyClose = TRUE))
        default_training_variable <- "Class"
        updateSelectInput(session, "training_variable", selected = default_training_variable)
      } else {
        showModal(modalDialog("Veuillez charger un fichier CSV valide.", title = "Erreur de chargement", easyClose = TRUE))
      }
    }
  })
  
  observeEvent(input$normalize, {
    data_normalized <- as.data.frame(scale(data()))
    data(data_normalized)
  })
  
  observeEvent(input$zscore_normalize, {
    req(data())  # Ensure that 'data' is available before proceeding
    
    # Perform Z-score normalization
    data_normalized <- as.data.frame(scale(data()))
    
    # Update the 'data' reactive value with the normalized data
    data(data_normalized)
    
    # You can also provide some feedback to the user, e.g., via a notification
    showNotification("Les données ont été normalisées avec Z-score.", type = "message")
  })
  
  observeEvent(input$train_model, {
    req(data())
    
    if (!target_variable() %in% colnames(data())) {
      showModal(modalDialog("La variable cible sélectionnée n'est pas valide.", title = "Erreur", easyClose = TRUE))
      return()
    }
    
    training_percentage <- input$training_percentage
    test_percentage <- input$test_percentage
    
    if (is.null(training_percentage) || is.null(test_percentage) || training_percentage + test_percentage != 100) {
      showModal(modalDialog("Veuillez spécifier des pourcentages valides pour l'ensemble d'entraînement et l'ensemble de test.", title = "Erreur", easyClose = TRUE))
      return()
    }
    
    training_rows <- round(training_percentage * nrow(data()) / 100)
    test_rows <- nrow(data()) - training_rows
    
    training_set(data()[1:training_rows, ])
    test_set(data()[(training_rows + 1):nrow(data()), ])
    
    updated_data <- test_set()
    updated_data[[target_variable()]] <- as.factor(updated_data[[target_variable()]])
    test_set(updated_data)
    
    if (!target_variable() %in% colnames(data())) {
      showModal(modalDialog("La variable cible sélectionnée n'est pas valide.", title = "Erreur", easyClose = TRUE))
      return()
    }
    svm_model <- svm(as.formula(paste(target_variable(), "~ .")), data = training_set(), probability = TRUE)
    test_predictions <- predict(svm_model, newdata = test_set(), probability = TRUE)
    confusion_matrix <- confusionMatrix(test_predictions, test_set()[[target_variable()]])
    precision <- confusion_matrix$byClass['Precision']
    recall <- confusion_matrix$byClass['Recall']
    f1_score <- confusion_matrix$byClass['F1']
    trained_model(list(model = svm_model, metrics = confusion_matrix, precision = precision, recall = recall, f1_score = f1_score))
    weights <- t(svm_model$coefs) %*% svm_model$SV
    
    rf_model <- randomForest(as.formula(paste(target_variable(), "~ .")), data = training_set(), ntree = 500)
    
    test_predictions_RF <- predict(rf_model, newdata = test_set())
    
    confusion_matrix_RF <- confusionMatrix(test_predictions_RF, test_set()[[target_variable()]])
    accuracy_RF <- confusion_matrix_RF$overall['Accuracy']
    precision_RF <- confusion_matrix_RF$byClass['Precision']
    recall_RF <- confusion_matrix_RF$byClass['Recall']
    F1_RF <- 2 * (precision_RF * recall_RF) / (precision_RF + recall_RF)
    
    output$feature_importance_plot <- renderPlot({
      req(trained_model_RF())
      rf_model <- trained_model_RF()$model
      req(rf_model) 
      importances <- importance(rf_model)
      if (is.matrix(importances) && !is.null(rownames(importances))) {
        importance_df <- data.frame(
          feature = rownames(importances),
          importance = importances[, 1], 
          stringsAsFactors = FALSE
        )
        importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
        ggplot(importance_df, aes(x = reorder(feature, importance), y = importance)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(title = "Feature Importances", x = "Features", y = "Importance") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        stop("importance() did not return a matrix with row names.")
      }
    })
    
    
    
    trained_model_RF(list(
      model = rf_model,
      metrics_RF = confusion_matrix_RF,
      accuracy_RF = accuracy_RF,
      precision_RF = precision_RF,
      recall_RF = recall_RF,
      F1_RF = F1_RF
    ))
    
    # Train the logistic regression model
    logistic_model <- glm(as.formula(paste(target_variable(), "~ .")), data = training_set(), family = binomial())
    
    # Make predictions on the test set
    test_probabilities_logistic <- predict(logistic_model, newdata = test_set(), type = "response")
    
    # Convert probabilities to predicted class based on the threshold
    predicted_values_logistic <- ifelse(test_probabilities_logistic > 0.5, 4, 2)  # Assuming binary classification with values 2 and 4
    
    # Generate confusion matrix
    confusion_matrix_logistic <- confusionMatrix(factor(predicted_values_logistic, levels = c(2, 4)), test_set()[[target_variable()]])
    
    # Calculate performance metrics
    accuracy_logistic <- confusion_matrix_logistic$overall['Accuracy']
    precision_logistic <- confusion_matrix_logistic$byClass['Precision']
    recall_logistic <- confusion_matrix_logistic$byClass['Recall']
    F1_logistic <- 2 * (precision_logistic * recall_logistic) / (precision_logistic + recall_logistic)
    
    # Store the trained model and performance metrics in the logistic_model reactive value
    logistic_model_data <- list(
      model = logistic_model,
      metrics = confusion_matrix_logistic,
      accuracy = accuracy_logistic,
      precision = precision_logistic,
      recall = recall_logistic,
      F1 = F1_logistic
    )
    
    logistic_model(logistic_model_data)
    
    # Train the logistic regression model
    logistic_model <- glm(as.formula(paste(target_variable(), "~ .")), data = training_set(), family = binomial())
    
    # Extract coefficients and convert to odds ratios
    coefficients <- exp(coef(logistic_model))
    
    # Create plot
    output$feature_importance_plot_LR <- renderPlot({
      coef_df <- data.frame(Feature = names(coefficients), OddsRatio = coefficients)
      ggplot(coef_df, aes(x = reorder(Feature, OddsRatio), y = OddsRatio)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Feature Odds Ratios", x = "Features", y = "Odds Ratio") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    
    output$downloadSVM <- downloadHandler(
      filename = function() {
        "svm_model.rds"
      },
      content = function(file) {
        saveRDS(svm_model, file)
      }
    )
    
    output$downloadRF <- downloadHandler(
      filename = function() {
        "rf_model.rds"
      },
      content = function(file) {
        saveRDS(rf_model, file)
      }
    )
    
    output$downloadLR <- downloadHandler(
      filename = function() {
        "LR_model.rds"
      },
      content = function(file) {
        saveRDS(logistic_model, file)
      }
    )
    
  })
  
  output$model_results <- renderPrint({
    req(trained_model())
    metrics <- trained_model()$metrics
    precision <- trained_model()$precision
    recall <- trained_model()$recall
    f1_score <- trained_model()$f1_score
    cat("Confusion Matrix:\n")
    print(metrics$table)
    cat("\nAccuracy:", metrics$overall['Accuracy'], "\n")
    cat("Precision:", precision, "\n")
    cat("Recall:", recall, "\n")
    cat("F1 Score:", f1_score, "\n")
  })
  
  output$model_results_RF <- renderPrint({
    req(trained_model_RF())
    metrics_RF <- trained_model_RF()$metrics_RF
    accuracy_RF <- trained_model_RF()$accuracy_RF
    precision_RF <- trained_model_RF()$precision_RF
    recall_RF <- trained_model_RF()$recall_RF
    F1_RF <- trained_model_RF()$F1_RF
    cat("Confusion Matrix:\n")
    print(metrics_RF$table)
    cat("\nAccuracy:", accuracy_RF, "\n")
    cat("Precision:", precision_RF, "\n")
    cat("Recall:", recall_RF, "\n")
    cat("F1 Score:", F1_RF, "\n")
  })
  
  output$pdp_output <- renderUI({
    req(trained_model(), training_set())
    feature_names <- setdiff(names(training_set()), target_variable())
    plot_output_list <- lapply(seq_along(feature_names), function(i) {
      plotlyOutput(outputId = paste0("pdp_plot_", i))
    })
    do.call(tagList, plot_output_list)
  })
  
  observe({
    req(trained_model(), training_set())
    feature_names <- setdiff(names(training_set()), target_variable())
    lapply(seq_along(feature_names), function(i) {
      local({
        feature_name <- feature_names[i]
        output[[paste0("pdp_plot_", i)]] <- renderPlotly({
          req(trained_model(), training_set())
          pdp_feature <- partial(trained_model()$model, pred.var = feature_name, train = training_set())
          plotly::ggplotly(ggplot2::autoplot(pdp_feature))
        })
      })
    })
  })
  
  output$confusion_matrix_plot <- renderPlot({
    req(trained_model())
    confusionMatrix <- trained_model()$metrics$table
    confusion_df <- as.data.frame(confusionMatrix)
    colnames(confusion_df) <- c("Prediction", "Reference", "Freq")
    ggplot(confusion_df, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Freq/sum(Freq)*100)), vjust = 1) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$confusion_matrix_plot_RF <- renderPlot({
    req(trained_model_RF())
    confusionMatrix_RF <- trained_model_RF()$metrics_RF$table
    confusion_df_RF <- as.data.frame(confusionMatrix_RF)
    colnames(confusion_df_RF) <- c("Prediction", "Reference", "Freq")
    ggplot(confusion_df_RF, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Freq/sum(Freq)*100)), vjust = 1) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Confusion Matrix for Random Forest Model", x = "Actual", y = "Predicted") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$roc_auc_curve_plot_RF <- renderPlot({
    req(trained_model_RF(), test_set())
    test_probabilities_RF <- predict(trained_model_RF()$model, newdata = test_set(), type = "prob")[,2]
    true_outcomes_RF <- as.numeric(test_set()[[target_variable()]]) - 1
    roc_obj_RF <- roc(response = true_outcomes_RF, predictor = test_probabilities_RF)
    roc_data_RF <- data.frame(
      specificity = 1 - roc_obj_RF$specificities,
      sensitivity = roc_obj_RF$sensitivities
    )
    roc_plot_RF <- ggroc(roc_obj_RF, colour = 'steelblue', size = 2) +
      geom_ribbon(aes(x = 1 - specificity, ymin = 0, ymax = sensitivity), 
                  fill = 'steelblue', alpha = 0.2) +
      annotate("text", x = 0.6, y = 0.2, label = paste0("AUC = ", round(auc(roc_obj_RF), 2)), 
               color = "red", size = 5) +
      ggtitle(paste0('ROC Curve for Random Forest Model (AUC = ', round(auc(roc_obj_RF), 2), ')')) +
      theme_minimal()
    print(roc_plot_RF)
  })
  
  output$roc_auc_curve_plot <- renderPlot({
    req(trained_model(), test_set())
    
    test_probabilities <- attr(predict(trained_model()$model, newdata = test_set(), probability = TRUE), "probabilities")[,2]
    true_outcomes <- as.numeric(test_set()[[target_variable()]]) - 1  # Assuming the positive class is coded as 2
    roc_obj <- roc(true_outcomes, test_probabilities)
    roc_df <- data.frame(
      sensitivity = roc_obj$sensitivities,
      specificity = roc_obj$specificities
    )
    roc_plot <- ggroc(roc_obj, colour = 'steelblue', size = 2) +
      geom_ribbon(data = roc_df, aes(x = 1 - specificity, ymin = 0, ymax = sensitivity), fill = 'steelblue', alpha = 0.2) +
      annotate("text", x = 0.6, y = 0.2, label = paste0("AUC = ", round(auc(roc_obj), 2)), color = "red", size = 5) +
      ggtitle(paste0('ROC Curve ', '(AUC = ', round(auc(roc_obj), 2), ')')) +
      theme_minimal()
    print(roc_plot)
  })
  
  
  output$roc_auc_curve_plot_LR<- renderPlot({
    req(logistic_model(), test_set())
    test_probabilities_logistic <- predict(logistic_model()$model, newdata = test_set(), type = "response")
    true_outcomes_logistic <- as.numeric(test_set()[[target_variable()]]) - 1
    roc_obj_logistic <- roc(response = true_outcomes_logistic, predictor = test_probabilities_logistic)
    roc_data_logistic <- data.frame(
      specificity = 1 - roc_obj_logistic$specificities,
      sensitivity = roc_obj_logistic$sensitivities
    )
    roc_plot_logistic <- ggroc(roc_obj_logistic, colour = 'steelblue', size = 2) +
      geom_ribbon(aes(x = 1 - specificity, ymin = 0, ymax = sensitivity), 
                  fill = 'steelblue', alpha = 0.2) +
      annotate("text", x = 0.6, y = 0.2, label = paste0("AUC = ", round(auc(roc_obj_logistic), 2)), 
               color = "red", size = 5) +
      ggtitle(paste0('ROC Curve for Logistic Regression Model (AUC = ', round(auc(roc_obj_logistic), 2), ')')) +
      theme_minimal()
    print(roc_plot_logistic)
  })
  
  output$confusion_matrix_plot_LR <- renderPlot({
    req(logistic_model())
    confusionMatrix_logistic <- logistic_model()$metrics$table
    confusion_df_logistic <- as.data.frame(confusionMatrix_logistic)
    colnames(confusion_df_logistic) <- c("Prediction", "Reference", "Freq")
    ggplot(confusion_df_logistic, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Freq/sum(Freq)*100)), vjust = 1) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Confusion Matrix for Logistic Regression Model", x = "Actual", y = "Predicted") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$model_results_LR <- renderPrint({
    req(logistic_model())
    metrics_logistic <- logistic_model()$metrics
    precision_logistic <- logistic_model()$precision
    recall_logistic <- logistic_model()$recall
    f1_score_logistic <- logistic_model()$F1
    cat("Confusion Matrix:\n")
    print(metrics_logistic$table)
    cat("\nAccuracy:", metrics_logistic$overall['Accuracy'], "\n")
    cat("Precision:", precision_logistic, "\n")
    cat("Recall:", recall_logistic, "\n")
    cat("F1 Score:", f1_score_logistic, "\n")
  })
  
  
  observeEvent(input$loadData, {
    if (!is.null(data())) {
      output$table <- renderDT({ datatable(data()) })
      output$summary <- renderPrint({ summary(data()) })
      updateSelectInput(session, "column_to_convert", choices = names(data()))
    }
  })
  
  observeEvent(input$categorical_variables, {
    req(data())
    dataInput <- data()
    for (var in input$categorical_variables) {
      if (var %in% colnames(dataInput)) {
        dataInput[[var]] <- as.factor(dataInput[[var]])
      }
    }
    data(dataInput)
  })
  
  output$histogram <- renderPlotly({
    req(data(), input$x_variable)
    x_name <- input$x_variable
    binwidth <- input$binwidth_input
    ggplotly(
      ggplot(data(), aes(x = data()[[x_name]])) + 
        geom_histogram(binwidth = binwidth, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Histogram", x = x_name, y = "Frequency")
    )
  })
  
  output$boxplot <- renderPlotly({
    req(data(), input$x_variable)
    x_name <- input$x_variable
    ggplotly(
      ggplot(data(), aes(y = data()[[x_name]])) +
        geom_boxplot(fill = "green", color = "black", alpha = 0.7) +
        labs(title = "Box Plot", y = x_name)
    )
  })
  
  output$densityplot <- renderPlotly({
    req(data(), input$x_variable)
    x_name <- input$x_variable
    ggplotly(
      ggplot(data(), aes(x = data()[[x_name]])) + 
        geom_density(fill = "purple", color = "black", alpha = 0.7) +
        labs(title = "Density Plot", x = x_name, y = "Density")
    )
  })
  
  output$univariate_analysis <- renderPrint({
    req(data())
    var <- input$training_variable
    if (is.null(var) || var == "") {
      return("Veuillez sélectionner une variable pour l'analyse unidimensionnelle.")
    } else if (!var %in% colnames(data())) {
      return("La variable sélectionnée n'existe pas dans les données.")
    }
    result <- capture.output({
      cat("Analyse unidimensionnelle pour la variable :", var, "\n")
      cat("Nombre de valeurs manquantes :", sum(is.na(data()[[var]])), "\n")
      if (is.numeric(data()[[var]])) {
        cat("Moyenne :", mean(data()[[var]], na.rm = TRUE), "\n")
        cat("Écart-type :", sd(data()[[var]], na.rm = TRUE), "\n")
        cat("Valeurs uniques :", length(unique(na.omit(data()[[var]]))), "\n\n")
      } else if (is.factor(data()[[var]])) {
        cat("Nombre de catégories :", length(levels(data()[[var]])), "\n")
        cat("Fréquence des catégories :\n")
        print(table(data()[[var]]))
      } else {
        cat("Type de variable non pris en charge pour l'analyse unidimensionnelle.", "\n")
      }
    })
    
    result
  })
  output$bivariate_analysis <- renderPlotly({
    req(data(), input$x_variable, input$y_variable)
    x_variable_name <- input$x_variable
    y_variable_name <- input$y_variable
    # Create a scatter plot to visualize the correlation
    plot_ly(data(), x = ~data()[[x_variable_name]], y = ~data()[[y_variable_name]],
            type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = x_variable_name), yaxis = list(title = y_variable_name))
  })
  
  observeEvent(input$convert_to_numeric, {
    req(data(), input$column_to_convert)
    dataInput <- data()
    if (input$column_to_convert %in% colnames(dataInput)) {
      dataInput[[input$column_to_convert]] <- as.integer(dataInput[[input$column_to_convert]])
      data(dataInput)
      updateSelectInput(session, "column_to_convert", choices = names(dataInput))
      showModal(modalDialog("The column has been converted to numeric successfully.", easyClose = TRUE))
    } else {
      showModal(modalDialog("Please select a valid column to convert.", title = "Conversion Error", easyClose = TRUE))
    }
  })
  observeEvent(input$removeMissingCols, {
    req(data())
    if (!is.null(input$missing_columns) && length(input$missing_columns) > 0) {
      data(data()[, !names(data()) %in% input$missing_columns])
      updateSelectInput(session, "missing_columns", choices = names(data())[colSums(is.na(data())) > 0])
      output$table <- renderDT({ datatable(data()) })
      showModal(modalDialog("Les variables sélectionnées ont été supprimées.", easyClose = TRUE))
    } else {
      showModal(modalDialog("Veuillez sélectionner des variables à supprimer.", title = "Aucune sélection", easyClose = TRUE))
    }
  })
  observeEvent(input$removeMissingRows, {
    req(data())
    dataInput <- data()
    dataInput <- na.omit(dataInput)
    data(dataInput)
    
    updateSelectInput(session, "x_variable", choices = names(dataInput))
    updateSelectInput(session, "y_variable", choices = names(dataInput))
    updateSelectInput(session, "training_variable", choices = names(dataInput),selected = input$training_variable)
    updateSelectInput(session, "categorical_variables", choices = names(dataInput))
    updateSelectInput(session, "missing_columns", choices = names(dataInput)[colSums(is.na(dataInput)) > 0])
    updateSelectInput(session, "column_to_convert", choices = names(dataInput))
    
    showModal(modalDialog("Rows with missing values have been removed successfully.", easyClose = TRUE))
  })
  
  observeEvent(input$refreshData, {
    req(data())
    dataInput <- data()
    updateSelectInput(session, "x_variable", choices = names(dataInput))
    updateSelectInput(session, "y_variable", choices = names(dataInput))
    updateSelectInput(session, "training_variable", choices = names(dataInput),selected = input$training_variable)
    updateSelectInput(session, "missing_columns", choices = names(dataInput)[colSums(is.na(dataInput)) > 0])
    showModal(modalDialog("Les données ont été actualisées avec succès.", easyClose = TRUE))
  })
  output$correlation_matrix_plot <- renderPlot({
    req(data())
    data_excluded <- data()[-c(1, ncol(data()))]
    correlation_matrix <- cor(data_excluded, use = "pairwise.complete.obs")
    corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
  })
  
  output$label_analysis  <- renderPlot({
    ggplot(data(), aes(x=data()[,ncol(data())], fill=data()[,ncol(data())]))  + 
      geom_bar() +
      scale_y_continuous() +
      geom_text(aes(y = (after_stat(count)),label =  scales::percent((after_stat(count))/sum(after_stat(count)))),
                stat="count",vjust=-1) +       
      geom_text(stat='count', aes(label=after_stat(count)), vjust=3) +
      theme(legend.position="none") +
      labs(title = "Frequency of each category",
           x = 'Categorical variable',
           y = 'Frequency')
  })
  
  observeEvent(input$delete_rows, {
    req(data())
    dataInput <- data()
    rows_to_delete <- which(dataInput$Class == input$target_value)
    if (length(rows_to_delete) < input$rows_to_delete) {
      showModal(modalDialog("There are fewer rows available to delete than the number specified. All available rows will be deleted.", easyClose = TRUE))
      dataInput <- dataInput[-rows_to_delete, ]
    } else {
      dataInput <- dataInput[-rows_to_delete[1:input$rows_to_delete], ]
    }
    data(dataInput)
    updateSelectInput(session, "x_variable", choices = names(dataInput))
    updateSelectInput(session, "y_variable", choices = names(dataInput))
    updateSelectInput(session, "training_variable", choices = names(dataInput),selected = input$training_variable)
    updateSelectInput(session, "categorical_variables", choices = names(dataInput))
    updateSelectInput(session, "missing_columns", choices = names(dataInput)[colSums(is.na(dataInput)) > 0])
    updateSelectInput(session, "column_to_convert", choices = names(dataInput))
    showModal(modalDialog("Rows have been deleted successfully.", easyClose = TRUE))
  })
  
  observeEvent(input$remove_duplicates, {
    req(data())  # Ensure that 'data' is available before proceeding
    
    # Calculate the number of rows before deduplication
    n_before <- nrow(data())
    
    # Remove duplicates
    data_unique <- data() %>%
      distinct(.keep_all = TRUE)
    
    # Calculate the number of rows after deduplication
    n_after <- nrow(data_unique)
    
    # Calculate the number of duplicates removed
    n_duplicates <- n_before - n_after
    
    # Provide feedback to the user about the number of duplicates removed
    showNotification(paste("Le nombre de doublons supprimés est de :", n_duplicates), type = "message")
    
    # Update the 'data' reactive value with the deduplicated data
    data(data_unique)
    
    # Provide feedback to the user
    showNotification("Les doublons ont été supprimés.", type = "message")
  })
  
  output$pie_chart <- renderPlot({
    req(data())
    last_column <- names(data())[ncol(data())]
    ggplot(data(), aes(x = factor(1), fill = !!sym(last_column))) +
      geom_bar(width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Pie Chart for the label", fill = last_column)
  })
  
}
shinyApp(ui = ui, server = server)