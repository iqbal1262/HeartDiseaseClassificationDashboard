library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(tools)
library(DT)
library(tidyr)
library(psych)

setwd('D:/Sekolah/Kuliah/Semester 6/Data Mining dan Visualisasi/Final Project') ; getwd()
rawdf <- read.csv('heart.csv') ; df <- read.csv('heartaman.csv')[,-1]

shinyServer(function(input, output){
  # Raw Dataset ==============================================
  output$rawdata <- renderDataTable(rawdf)
  
  output$desc_table1 <- DT::renderDataTable({
    desc_stats <- rawdf %>%
      select_if(is.numeric) %>%
      psych::describe() %>%
      as.data.frame()
    
    # Pembulatan 3 angka di belakang koma
    desc_stats <- desc_stats %>%
      mutate(across(where(is.numeric), round, 3))
    
    DT::datatable(desc_stats, options = list(pageLength = 10))
  })
  
  output$piechart <- renderPlot({
    # Mengambil nama kolom dari input$sel_var
    column <- input$cat_var
    
    # Menghitung jumlah frekuensi setiap kategori
    data_summary <- as.data.frame(table(rawdf[[column]]))
    
    # Ubah nama kolom hasil penghitungan
    colnames(data_summary) <- c("Kategori", "Frekuensi")
    
    # Buat pie chart dengan ggplot2
    pie_chart <- ggplot(data_summary, aes(x = "", y = Frekuensi, fill = Kategori)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +  # Menggunakan coord_polar untuk membuat pie chart
      geom_text(aes(label = paste0(Frekuensi)), position = position_stack(vjust = 0.5)) +  # Menambahkan label kategori dan frekuensi
      scale_fill_discrete(name = "Kategori") +  # Menambahkan legend dengan nama "Category"
      theme_minimal()   # Tema minimal untuk tampilan yang bersih
      # labs(
      #   title = "Pie Chart Data Kategorik"
      # ) +  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
    
    # Tampilkan pie chart
    print(pie_chart)
  })
  
  output$traindf <- renderDataTable(train)
  output$testdf <- renderDataTable(test)
  
  # Render data table based on selected variable
  output$table <- renderDT({
    # Mengambil nama kolom dari input$sel_var
    column <- input$cat_var
    
    # Menghitung frekuensi kemunculan
    freq_table <- table(rawdf[[column]])
    
    # Ubah menjadi dataframe untuk tampilan dengan DT
    freq_df <- as.data.frame(freq_table)
    colnames(freq_df) <- c("Kategori", "Frekuensi")
    
    # Kembalikan tampilan datatable
    datatable(freq_df)
  })
  
  output$hist1 <- renderPlot({
    # Mengambil nama kolom dari input$sel_var
    column <- input$num_var
    
    # Buat histogram dengan ggplot2
    ggplot(rawdf, aes(x = !!sym(column))) +
      geom_histogram(binwidth = 5, fill = "#ADD8E6", color = "#708090", aes(y = ..density..)) +
      geom_density(alpha = 0.1, fill = "#FF6666") +
      labs(
        x = column,
        y = "Density"
      ) +
      theme_minimal()
  })
  
  output$box1 <- renderPlot({
    # Mengambil nama kolom dari input$sel_var
    column <- input$num_var
    
    # Buat boxplot dengan ggplot2
    ggplot(rawdf, aes(x = 1, y = !!sym(column))) +
      geom_boxplot(fill = "#87CEFA", color = "#4682B4") +
      labs(
        x = "",
        y = column
      ) +
      theme_minimal()
  })
  
  output$corr1 <- renderPlot({
    numeric_vars <- c("age", "trtbps", "chol", "thalachh", "oldpeak", "caa")
    
    # Hitung matriks korelasi
    corr_matrix <- cor(rawdf[, numeric_vars], use = "complete.obs")
    
    # Plot matriks korelasi menggunakan ggcorrplot
    ggcorrplot(corr_matrix, 
               hc.order = TRUE, 
               type = "lower", 
               lab = TRUE, lab_size = 5, 
               method = "circle", 
               colors = c("tomato2", "white", "springgreen3"), 
               ggtheme = theme_minimal())
  })
  
  output$plot1 <- renderPlot({
    # Ambil nama kolom dari input$sel_var_cat
    column_cat <- input$sel_var_cat
    
    # Hitung jumlah output 0 dan 1 berdasarkan variabel kategorikal
    output_counts <- rawdf %>%
      group_by(!!sym(column_cat), output) %>%
      summarise(count = n(), .groups = 'drop') %>%
      filter(output %in% c(0, 1)) %>%
      tidyr::spread(output, count, fill = 0) %>%
      mutate(Total = `0` + `1`)
    
    # Ubah data menjadi format panjang (tidy format) untuk ggplot
    output_counts_long <- output_counts %>%
      tidyr::gather(key = "Output", value = "Count", `0`, `1`)
    
    # Buat plot bar stacked dengan label
    ggplot(output_counts_long, aes_string(x = column_cat, y = "Count", fill = "Output", label = "Count")) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(position = position_stack(vjust = 0.5), size = 5, color = "white") +
      labs(
        title = paste("Jumlah Output 0 dan 1 Berdasarkan Variabel", column_cat),
        x = column_cat,
        y = "Jumlah",
        fill = "Output"
      ) +
      theme_minimal() +  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
  })
  
  output$hist2 <- renderPlot({
    # Mengambil nama kolom dari input$sel_var
    column <- input$num_var2
    
    # Buat histogram dengan ggplot2
    ggplot(df, aes(x = !!sym(column))) +
      geom_histogram(binwidth = 5, fill = "#ADD8E6", color = "#708090", aes(y = ..density..)) +
      geom_density(alpha = 0.1, fill = "#FF6666") +
      labs(
        x = column,
        y = "Density"
      ) +
      theme_minimal()
  })

  # Pre Processed Dataset ===================================
  output$desc_table2 <- DT::renderDataTable({
    desc_stats <- df %>%
      select_if(is.numeric) %>%
      psych::describe() %>%
      as.data.frame()
    
    # Pembulatan 3 angka di belakang koma
    desc_stats <- desc_stats %>%
      mutate(across(where(is.numeric), round, 3))
    
    DT::datatable(desc_stats, options = list(pageLength = 10))
  })
  
  output$piechart2 <- renderPlot({
    # Mengambil nama kolom dari input$sel_var
    column <- input$cat_var2
    
    # Menghitung jumlah frekuensi setiap kategori
    data_summary <- as.data.frame(table(df[[column]]))
    
    # Ubah nama kolom hasil penghitungan
    colnames(data_summary) <- c("Kategori", "Frekuensi")
    
    # Buat pie chart dengan ggplot2
    pie_chart <- ggplot(data_summary, aes(x = "", y = Frekuensi, fill = Kategori)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +  # Menggunakan coord_polar untuk membuat pie chart
      geom_text(aes(label = paste0(Frekuensi)), position = position_stack(vjust = 0.5)) +  # Menambahkan label kategori dan frekuensi
      scale_fill_discrete(name = "Kategori") +  # Menambahkan legend dengan nama "Category"
      theme_minimal()   # Tema minimal untuk tampilan yang bersih
    
    # Tampilkan pie chart
    print(pie_chart)
  })
  
  # Render data table based on selected variable
  output$table2 <- renderDT({
    # Mengambil nama kolom dari input$sel_var
    column <- input$cat_var2
    
    # Menghitung frekuensi kemunculan
    freq_table <- table(df[[column]])
    
    # Ubah menjadi dataframe untuk tampilan dengan DT
    freq_df <- as.data.frame(freq_table)
    colnames(freq_df) <- c("Kategori", "Frekuensi")
    
    # Kembalikan tampilan datatable
    datatable(freq_df)
  })
  
    
  output$box2 <- renderPlot({
    # Mengambil nama kolom dari input$sel_var
    column <- input$num_var2
    
    # Buat boxplot dengan ggplot2
    ggplot(df, aes(x = 1, y = !!sym(column))) +
      geom_boxplot(fill = "#87CEFA", color = "#4682B4") +
      labs(
        x = "",
        y = column
      ) +
      theme_minimal()
  })
  
  output$corr2 <- renderPlot({
    numeric_vars <- c("age", "trtbps", "chol", "thalachh", "oldpeak", "caa")
    
    # Hitung matriks korelasi
    corr_matrix <- cor(df[, numeric_vars], use = "complete.obs")
    
    # Plot matriks korelasi menggunakan ggcorrplot
    ggcorrplot(corr_matrix, 
               hc.order = TRUE, 
               type = "lower", 
               lab = TRUE, lab_size = 5, 
               method = "circle", 
               colors = c("tomato2", "white", "springgreen3"), 
               ggtheme = theme_minimal())
  })
  
  output$plot2 <- renderPlot({
    # Ambil nama kolom dari input$sel_var_cat2
    column_cat <- input$sel_var_cat2
    
    # Hitung jumlah output 0 dan 1 berdasarkan variabel kategorikal
    output_counts <- df %>%
      group_by(!!sym(column_cat), output) %>%
      summarise(count = n(), .groups = 'drop') %>%
      filter(output %in% c(0, 1)) %>%
      tidyr::spread(output, count, fill = 0) %>%
      mutate(Total = `0` + `1`)
    
    # Ubah data menjadi format panjang (tidy format) untuk ggplot
    output_counts_long <- output_counts %>%
      tidyr::gather(key = "Output", value = "Count", `0`, `1`)
    
    # Buat plot bar stacked dengan label
    ggplot(output_counts_long, aes_string(x = column_cat, y = "Count", fill = "Output", label = "Count")) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(position = position_stack(vjust = 0.5), size = 5, color = "white") +
      labs(
        title = paste("Jumlah Output 0 dan 1 Berdasarkan Variabel", column_cat),
        x = column_cat,
        y = "Jumlah",
        fill = "Output"
      ) +
      theme_minimal() +  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
  })
  
  # Non-CV Analysis ==============================================
  output$reg_method <- renderUI({
    if(input$reg_method == 'Naive Bayes'){
      method_name <- "Naive Bayes"
    }
    else if(input$reg_method == 'Random Forest'){
      method_name <- "Random Forest"
      }
    else if(input$reg_method == 'Decision Tree'){
      method_name <- "Decision Tree"
    }
    else if(input$reg_method == 'SVM'){
      method_name <- "SVM"
    }
    paste("Definisi Metode", method_name)
  })
  
  output$reg_def <- renderUI({
    if(input$reg_method == 'Naive Bayes'){
      method_def <- "Naive Bayes adalah algoritma klasifikasi probabilistik yang mengasumsikan bahwa fitur-fitur bersifat independen satu sama lain. Ia menggunakan Teorema Bayes untuk menghitung probabilitas suatu kelas berdasarkan nilai fitur. Naive Bayes dilatih pada seluruh dataset test sekaligus. Ini dapat menyebabkan estimasi performa yang terlalu optimis jika dataset kecil atau tidak seimbang."
    }
    else if(input$reg_method == 'Random Forest'){
      method_def <- "Random Forest adalah algoritma ensamble yang menggunakan beberapa pohon keputusan untuk membuat prediksi. Setiap pohon dilatih pada subset data yang berbeda (dengan pengambilan sampel berulang) dan hasilnya digabungkan (biasanya dengan voting mayoritas) untuk membuat prediksi akhir. Random Forest dilatih pada seluruh dataset test tanpa evaluasi silang. Meskipun Random Forest kurang rentan terhadap overfitting dibandingkan metode lain, evaluasi model mungkin tidak seakurat jika dibandingkan dengan hasil CV."
    }
    else if(input$reg_method == 'Decision Tree'){
      method_def <- "Decision Tree adalah algoritma klasifikasi yang mempartisi ruang fitur menjadi region-region homogen berdasarkan kondisi-kondisi pada fitur-fitur input. Setiap node pada pohon keputusan mewakili kondisi pada satu fitur dan setiap cabang mewakili hasil kondisi tersebut. Decision Tree dilatih pada seluruh dataset tanpa evaluasi silang. Ini sering menyebabkan overfitting, terutama pada dataset kecil atau kompleks."
    }
    else if(input$reg_method == 'SVM'){
      method_def <- "SVM adalah algoritma klasifikasi yang mencari hyperplane terbaik yang memisahkan kelas-kelas dalam ruang fitur. Hyperplane terbaik adalah yang memaksimalkan margin antara data dari kelas yang berbeda. SVM dilatih pada seluruh dataset tanpa membagi data menjadi subset. Ini bisa menyebabkan overfitting atau underfitting jika parameter model tidak dipilih dengan hati-hati."
    }
    paste(method_def)
  })
  
  output$acc_reg <- renderDT({
    if(input$reg_method == 'Naive Bayes'){
      accuracyDT <- nb_summary
    }
    else if(input$reg_method == 'Random Forest'){
      accuracyDT <- rf_summary
      }
    else if(input$reg_method == 'Decision Tree'){
      accuracyDT <- dt_summary
    }
    else if(input$reg_method == 'SVM'){
      accuracyDT <- svm_terbaik_summary[,-5]
    }
    # accuracyDT <- as.data.frame((conf_mtrx$overall))
    accuracyDT <- accuracyDT %>%
      mutate(across(where(is.numeric), round, 3))
    names(accuracyDT) <- c("Accuracy", "Sensitivity", "Specificity", "AUC")
    accuracyDT <- as.data.frame(t(accuracyDT))
    
    # Menambahkan nama kolom
    colnames(accuracyDT) <- c('Nilai')
    accuracyDT$Metrik <- rownames(accuracyDT)
    accuracyDT <- accuracyDT[, c("Metrik", "Nilai")]
    
    datatable(accuracyDT,
              rownames = FALSE,
              options = list(
                dom = 't',
                pageLength = 5,
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ))
  })
  
  output$plot_reg <- renderPlot({
    if(input$reg_method == 'Naive Bayes'){
      perf_nb()
    }
    else if(input$reg_method == 'Random Forest'){
      perf_rf()
    }
    else if(input$reg_method == 'Decision Tree'){
      perf_dt()
    }
    else if(input$reg_method == 'SVM'){
      perf_svm()
    }
    
    abline(a = 0, b = 1, lty = 2, col = "gray")
  })
  
  
  # CV Analysis ==============================================
  output$cv_method <- renderUI({
    if(input$cv_method == 'Naive Bayes'){
      method_name <- "Naive Bayes"
    }
    else if(input$cv_method == 'Random Forest'){
      method_name <- "Random Forest"
    }
    else if(input$cv_method == 'Decision Tree'){
      method_name <- "Decision Tree"
    }
    else if(input$cv_method == 'SVM'){
      method_name <- "SVM"
    }
    paste("Definisi Metode", method_name)
  })
  
  output$cv_def <- renderUI({
    if(input$cv_method == 'Naive Bayes'){
      method_def <- "Naive Bayes adalah algoritma klasifikasi probabilistik yang mengasumsikan bahwa fitur-fitur bersifat independen satu sama lain. Ia menggunakan Teorema Bayes untuk menghitung probabilitas suatu kelas berdasarkan nilai fitur. CV digunakan untuk mengevaluasi performa model dengan membagi dataset menjadi beberapa folds. Model dilatih pada sebagian folds dan diuji pada fold sisanya untuk mengestimasi akurasi yang lebih stabil dan mengurangi risiko overfitting."
    }
    else if(input$cv_method == 'Random Forest'){
      method_def <- "Random Forest adalah algoritma ensamble yang menggunakan beberapa pohon keputusan untuk membuat prediksi. Setiap pohon dilatih pada subset data yang berbeda (dengan pengambilan sampel berulang) dan hasilnya digabungkan (biasanya dengan voting mayoritas) untuk membuat prediksi akhir. CV digunakan untuk memilih parameter model seperti jumlah pohon (n_estimators), kedalaman pohon (max_depth), dan lainnya. CV memastikan bahwa model tidak overfitting pada data pelatihan."
    }
    else if(input$cv_method == 'Decision Tree'){
      method_def <- "Decision Tree adalah algoritma klasifikasi yang mempartisi ruang fitur menjadi region-region homogen berdasarkan kondisi-kondisi pada fitur-fitur input. Setiap node pada pohon keputusan mewakili kondisi pada satu fitur dan setiap cabang mewakili hasil kondisi tersebut. CV digunakan untuk memilih parameter seperti kedalaman maksimum pohon (max_depth), jumlah sampel minimum per node (min_samples_split), dan lainnya. CV membantu dalam menentukan parameter yang memberikan generalisasi terbaik."
    }
    else if(input$cv_method == 'SVM'){
      method_def <- "SVM adalah algoritma klasifikasi yang mencari hyperplane terbaik yang memisahkan kelas-kelas dalam ruang fitur. Hyperplane terbaik adalah yang memaksimalkan margin antara data dari kelas yang berbeda. CV membagi data menjadi beberapa subset (folds) dan menggunakan sebagian data untuk pelatihan dan sisanya untuk pengujian dalam setiap iterasi. Ini membantu dalam memilih parameter model yang memberikan performa terbaik."
    }
    paste(method_def)
  })
  
  output$acc_cv <- renderDT({
    # Inisialisasi variabel yang akan digunakan
    conf_matrix <- NULL
    auc <- NULL
    
    # Kondisi berdasarkan metode yang dipilih
    if (input$cv_method == 'Naive Bayes') {
      conf_matrix <- confusionMatrix(pred_nb_cv, test[, names(test) %in% "output"])
      auc <- data.frame('Metrics' = 'AUC', 'Values' = auc_pred_nb_cv@y.values[[1]])
    } else if (input$cv_method == 'Random Forest') {
      conf_matrix <- confusionMatrix(pred_rf_cv, test[, names(test) %in% "output"])
      auc <- data.frame('Metrics' = 'AUC', 'Values' = auc_pred_rf_cv@y.values[[1]])
    } else if (input$cv_method == 'Decision Tree') {
      conf_matrix <- confusionMatrix(pred_dt_cv, test[, names(test) %in% "output"])
      auc <- data.frame('Metrics' = 'AUC', 'Values' = auc_pred_dt_cv@y.values[[1]])
    } else if (input$cv_method == 'SVM') {
      conf_matrix <- confusionMatrix(y_test, decision_values)
      auc <- data.frame('Metrics' = 'AUC', 'Values' = auc_pred_dt_cv@y.values[[1]])
    }
    
    
    
    # Mengambil metrik dari confusion matrix
    if (!is.null(conf_matrix)) {
      metrics1 <- data.frame('Metrics' = rownames(as.data.frame(conf_matrix$overall))[1], 
                             'Values' = as.data.frame(conf_matrix$overall)[1, ])
      
      metrics2 <- data.frame('Metrics' = rownames(as.data.frame(conf_matrix$byClass))[c(1,2)], 
                             'Values' = as.data.frame(conf_matrix$byClass)[c(1,2), ])
      
      # Menggabungkan metrik dan nilai
      accuracyDT <- rbind(metrics1, metrics2, auc)
      
      # Membulatkan angka
      accuracyDT <- accuracyDT %>%
        mutate(Values = round(Values, 3))
      
      # Menampilkan tabel menggunakan datatable
      datatable(accuracyDT,
                options = list(
                  dom = 't',
                  pageLength = 5,
                  autoWidth = TRUE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))
                ))
    } else {
      # Jika tidak ada confusion matrix, tampilkan pesan error atau kosong
      datatable(data.frame(Metrics = character(0), Values = numeric(0)),
                options = list(
                  dom = 't',
                  pageLength = 5,
                  autoWidth = TRUE,
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))
                ))
    }
  })
  
  output$plot_cv <- renderPlot({
    if(input$cv_method == 'Naive Bayes'){
      plot(perf_nb_cv, col = "blue", lwd = 2, main = "ROC Curve")
      abline(a = 0, b = 1, lty = 2, col = "gray")
    }
    else if(input$cv_method == 'Random Forest'){
      plot(perf_rf_cv, col = "blue", lwd = 2, main = "ROC Curve")
      abline(a = 0, b = 1, lty = 2, col = "gray")
    }
    else if(input$cv_method == 'Decision Tree'){
      plot(perf_dt_cv, col = "blue", lwd = 2, main = "ROC Curve")
      abline(a = 0, b = 1, lty = 2, col = "gray")
    }
    else if(input$cv_method == 'SVM'){
      plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve")
      # abline(a = 0, b = 1, lty = 2, col = "gray")
    }
  })
  
  observeEvent(input$predict_btn, {
    # Retrieve and convert inputs
    thall <- as.numeric(input$thall)
    cp <- as.numeric(input$cp)
    caa <- as.numeric(input$caa)
    oldpeak <- as.numeric(input$oldpeak)
    thalachh <- as.numeric(input$thalachh)
    exng <- as.numeric(input$exng)
    
    # Ensure all values are numeric before performing any operations
    if (is.na(thall) | is.na(cp) | is.na(caa) | is.na(oldpeak) | is.na(thalachh) | is.na(exng)) {
      showNotification("Please check your inputs. All inputs must be numeric.", type = "error")
      return(NULL)
    }
    
    # Perform calculations here using the numeric values
    # Example: 
    # result <- some_function(thall, cp, caa, oldpeak, thalachh, exng)
    # output$result <- renderText({ result })
  })

  
  observeEvent(input$predict_btn, {
    thall <- as.numeric(input$thall)
    cp <- as.numeric(input$cp)
    caa <- as.numeric(input$caa)
    oldpeak <- as.numeric(input$oldpeak)
    thalachh <- as.numeric(input$thalachh)
    exng <- as.numeric(input$exng)
    data_input <- data.frame(
      thall = factor(thall),
      cp = factor(cp),
      caa = (caa - mean(heart_important$caa)) / sd(heart_important$caa),
      oldpeak = (oldpeak - mean(heart_important$oldpeak)) / sd(heart_important$oldpeak),
      thalachh = (thalachh - mean(heart_important$thalachh)) / sd(heart_important$thalachh),
      exng = factor(exng)
    )
    
    prediksi <- predict(model_naive_terbaik, newdata = data_input, type = "class")
    prediksi <- as.numeric(as.matrix(prediksi[1]))
    
    # output$prediction_output <- renderPrint({
    #   prediksi
    # })
    output$prediction_output <- renderUI({
      if(prediksi == 0){
        paste('Anda berisiko rendah terkena penyakit jantung.')
      } else{
        paste('Anda berisiko tinggi terkena penyakit jantung.')
      }
    })
  })
  
  
  
  # =====================================================================================
  
  output$info_box <- renderInfoBox({infoBox('Lorem Ipsum', 'Lorem ipsum dolor sit amet',
                                   icon = icon('poo'), color = 'maroon')})
  
  output$val_box <- renderValueBox({
    valueBox(12*60, 'Waktu Sambat (Jam)', icon = icon('fire'),
             color = 'yellow')
  })
  
  output$proj_name <- {(
    renderText(input$projname)
  )}
  
  output$proj_code <- {(
    renderText(input$projcode)
  )}
  
  output$tech_used <- {(
    renderText(input$tech)
  )}
  
  output$location <- {(
    renderText(input$loc)
  )}
  
  output$no_days <- {(
    renderText(input$days)
  )}
})