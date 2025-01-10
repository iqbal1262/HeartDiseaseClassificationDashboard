library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

setwd('D:/Sekolah/Kuliah/Semester 6/Data Mining dan Visualisasi/Final Project') ; getwd()


# Header ===================================================
header <- dashboardHeader(title = 'Final Project Datmin B',
                          dropdownMenu(type = 'notifications',
                                       icon = icon('user'),
                                       messageItem(from = 'Yuri Aditya Rahmad R.',
                                                message = '5003211006'),
                                       messageItem(from = 'Muhammad Iqbal Ainur R.',
                                                   message = '5003211055')
                          ))

# Sidebar ==================================================
sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem('About The Dataset', tabName = 'about', icon = icon('database')),
    menuSubItem('Raw Data', tabName = 'raw'),
    menuSubItem('Pre Processed Data', tabName = 'processed'),
  menuItem('Analisis Data Lanjutan', tabName = 'detail', icon = icon('magnifying-glass-chart')),
  menuItem('Check Your Heart Health', tabName = 'check', icon = icon('laptop-medical'))
  )
)

# Body =====================================================
# dashBody <- dashboardBody(
#   fluidRow(
#     box(plotOutput('histogram')),
#     box(sliderInput('bins', 'Number of Breaks', 1, 100, 50))
#   )
# )

dashBody <- dashboardBody(
  tabItems(
    # tabItem(tabName = 'dashboard',
    #         fluidRow(
    #           column(width = 12,
    #           infoBox('Dataset Source', 'www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset/data',
    #                   icon = icon('kaggle')),
    #           infoBox('Analysis Progress', '60%', icon = icon('warning'),
    #                   color = 'yellow'),
    #           infoBoxOutput('info_box')
    #         )),
    #         
    #         fluidRow(
    #           valueBox(15*300, 'Waktu Pengerjaan (Jam)',
    #                    color = 'red', icon = icon('hourglass-3')),
    #           valueBoxOutput('val_box')
    #         ),
    #         
    #         fluidRow(
    #           box(title = 'Histogram of Age Data',
    #               status = 'primary', solidHeader = T,
    #               background = 'aqua',
    #               plotOutput('histogram')),
    #           box(title = 'Dashboard Controller',
    #               'Do not add too much command', br(), br(),
    #               status = 'warning', solidHeader = T,
    #               background = 'red',
    #               sliderInput('bins', 'Number of Breaks', 1, 100, 50),
    #               textInput('search_op', label = 'Search Opportunities', value = 1234))
    #         ),
    #         
    #         fluidRow(
    #           tabBox(
    #             tabPanel(title = 'Histogram of Age Data',
    #                      status = 'primary', solidHeader = T,
    #                      background = 'aqua',
    #                      plotOutput('chol_hist')),
    #             tabPanel(title = 'Dashboard Controller',
    #                      'Do not add too much command', br(), br(),
    #                      status = 'warning', solidHeader = T,
    #                      background = 'red',
    #                      sliderInput('bins2', 'Number of Breaks', 1, 100, 50),
    #                      textInput('search_op', label = 'Search Opportunities', value = 1234))
    #           )
    #         )),
    tabItem(tabName = 'about',
            fluidRow(column(width = 12,
                     infoBox(HTML('<p style="font-weight: bold;">source</p>'), HTML('<p style="font-weight: normal;">www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset/data</p>'), width = 12,
                             icon = icon('kaggle'),
                             ),
                     infoBox(
                       '5003211006', 
                       tags$div('Yuri Aditya Rahmad Ramadhan', style = 'font-size: 20px;'),
                       width = 4, color = "blue", icon = icon('user')
                     ),
                     infoBox(
                       '5003211055', 
                       tags$div('Muhammad Iqbal Ainur', style = 'font-size: 20px;'),
                       width = 4, color = "blue", icon = icon('user')
                     )
                     )),
            
            tabBox(width = 12,
              tabPanel(title = 'About',
                       icon = icon('file'),
                       fluidRow(
                         column(width = 6, tags$img(src = 'heart_attack.jpg', width = '100%'), tags$br(), tags$a("Photo by Robina Weermeijer on Unsplash"), align = "center"),
                         column(width = 6, tags$br(),
                                tags$p('Dataset ini berisi informasi medis dari pasien yang digunakan untuk memprediksi risiko serangan jantung. Variabel-variabel yang ada mencakup usia dan jenis kelamin pasien, adanya angina yang diinduksi oleh latihan, jumlah pembuluh darah utama yang tersumbat, jenis nyeri dada, tekanan darah saat istirahat, kadar kolesterol, gula darah puasa, hasil elektrokardiografi saat istirahat, serta denyut jantung maksimum yang dicapai. Variabel target dalam dataset ini menunjukkan risiko serangan jantung, di mana nilai 0 menunjukkan risiko lebih rendah dan nilai 1 menunjukkan risiko lebih tinggi. Data ini dapat digunakan untuk analisis dan prediksi klinis guna meningkatkan diagnosis dan penanganan penyakit jantung.'))
                       )),
              tabPanel('Data Frame', dataTableOutput('rawdata'), icon = icon('table')),
              tabPanel('Index', tags$p('Dataset heart berisi informasi medis tentang pasien yang digunakan untuk memprediksi risiko serangan jantung. Berikut adalah deskripsi ringkas dari variabel-variabel dalam dataset tersebut:'),
                       tags$br(),
                       tags$p('1. Age: Usia pasien.'),
                       tags$p('2. Sex: Jenis kelamin pasien (1 = male; 0 = female).'),
                       tags$p('3. exang: Angina yang diinduksi oleh latihan (1 = ya; 0 = tidak).'),
                       tags$p('4. ca: Jumlah pembuluh darah utama (0-3).'),
                       tags$p('5. cp: Jenis nyeri dada.'),
                       tags$ul(
                         tags$li('Value 1: typical angina'),
                         tags$li('Value 2: atypical angina'),
                         tags$li('Value 3: non-anginal pain'),
                         tags$li('Value 4: asymptomatic')
                       ),
                       tags$p('6. trtbps: Tekanan darah saat istirahat (dalam mm Hg).'),
                       tags$p('7. chol: Kolesterol dalam mg/dl diambil dari sensor BMI.'),
                       tags$p('8. fbs: Gula darah puasa (> 120 mg/dl) (1 = benar; 0 = salah).'),
                       tags$p('9. rest_ecg: Hasil elektrokardiografi saat istirahat.'),
                       tags$ul(
                         tags$li('Value 0: normal'),
                         tags$li('Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)'),
                         tags$li('Value 2: showing probable or definite left ventricular hypertrophy by Estes\' criteria')
                       ),
                       tags$p('10. thalach: Denyut jantung maksimum yang dicapai.'),
                       tags$p('11. oldpeak: Penurunan ST yang terjadi setelah latihan dibandingkan saat istirahat. ST adalah bagian dari grafik EKG yang menunjukkan aktivitas jantung.'),
                       tags$p('12. slope: Kemiringan garis pada segmen ST setelah latihan maksimal — 0: turun; 1: datar; 2: naik.'),
                       tags$p('13. thal: Sebuah gangguan darah yang disebut talasemia, dengan tiga jenis: 0: normal, 1:  cacat tetap, atau 2:cacat yang dapat dipulihkan.'),
                       tags$p('14. target: Hasil (0 = risiko lebih rendah serangan jantung; 1 = risiko lebih tinggi serangan jantung).'))
            )
            ),
    
    tabItem(tabName = 'raw',
            h1('Raw Dataset', style = 'font-weight: bold;'),
            fluidRow(
              box(
                title = 'Statistika Deskriptif',
                status = 'info',
                solidHeader = TRUE,
                width = 12,
                DT::dataTableOutput('desc_table1')
              )
            ),
            
            h2('Data Kategorik', style = 'font-weight: bold;'),
            fluidRow(
              box(title = 'Pie Chart Data Kategorik',
                  status = 'primary', solidHeader = T,
                  plotOutput('piechart')),
              box(title = 'Dashboard Controller',
                  status = 'info', solidHeader = T,
                  selectInput(inputId = 'cat_var',
                              label = 'Pilih Variabel Kategorik',
                              choices = c('Jenis Kelamin (sex)' = 'sex',
                                          'Jenis Nyeri Dada (cp)' = 'cp',
                                          'Gula Darah Puasa (fbs)' = 'fbs',
                                          'Hasil Elektrokardiografi Saat Istirahat (restecg)' = 'restecg',
                                          'Angina yang diinduksi oleh latihan (exng)' = 'exng',
                                          'Kemiringan Garis ST Setelah Latihan Maksimal (slp)' = 'slp',
                                          'Taalsemia (thall)' = 'thall', 'Risiko Serangan Jantung (output)' = 'output',
                                          'Jumlah Pembuluh Darah Utama (caa)' = "caa"))),
              box(
                title = 'Dataframe Frekuensi',
                status = 'info',
                solidHeader = TRUE,
                DTOutput('table')
              )
              
            ),
            
            h2('Data Numerik', style = 'font-weight: bold;'),
            
            fluidRow(
              box(title = 'Dashboard Controller',
                  status = 'info', solidHeader = T,
                  selectInput(inputId = 'num_var',
                              label = 'Pilih Variabel Numerik',
                              choices = c('Usia (age)' = "age",
                                          'Tekanan Darah Saat Istirahat (trtbps)' = "trtbps",
                                          'Kolesterol (chol)' = "chol",
                                          'Denyut Jantung Maksimum yang Dicapai (thalachh)' = "thalachh",
                                          'Penurunan ST yang Terjadi Setelah Latihan (oldpeak)' = "oldpeak")))),
            
            fluidRow(
              box(title = 'Histogram Data Numerik',
                  status = 'primary', solidHeader = T,
                  plotOutput('hist1')),
              box(title = 'Box Plot Data Numerik',
                  status = 'primary', solidHeader = T,
                  plotOutput('box1'))
            ),
            
            fluidRow(
              box(
                title = 'Matriks Korelasi Variabel Numerik',
                status = 'primary',
                solidHeader = TRUE,
                width = 12,
                plotOutput('corr1', height = "500px")
              )
            ),
            
            h2('Visualisasi Lainnya', style = 'font-weight: bold;'),
            
            fluidRow(
              box(
                title = 'Dashboard Controller',
                status = 'info',
                solidHeader = TRUE,
                selectInput(
                  inputId = 'sel_var_cat',
                  label = 'Pilih Variabel Kategorik',
                  choices = c('Jenis Kelamin (sex)' = 'sex',
                              'Jenis Nyeri Dada (cp)' = 'cp',
                              'Gula Darah Puasa (fbs)' = 'fbs',
                              'Hasil Elektrokardiografi Saat Istirahat (restecg)' = 'restecg',
                              'Angina yang diinduksi oleh latihan (exng)' = 'exng',
                              'Kemiringan Garis ST Setelah Latihan Maksimal (slp)' = 'slp',
                              'Taalsemia (thall)' = 'thall')
                )
            ),
            
            fluidRow(
              column(width = 1),  # Left padding
              column(
                width = 10,
                box(
                  title = 'Plot Berdasarkan Variabel',
                  status = 'primary',
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput('plot1', height = "500px")
                )
              ),
              column(width = 1)  # Right padding
            )
            
            )
            ),
    
    tabItem(tabName = 'processed',
            h1('Pre Processed Dataset', style = 'font-weight: bold;'),
            fluidRow(
              box(
                title = 'Statistika Deskriptif',
                status = 'info',
                solidHeader = TRUE,
                width = 12,
                DT::dataTableOutput('desc_table2')
              )
            ),
            
            h2('Data Kategorik', style = 'font-weight: bold;'),
            fluidRow(
              box(title = 'Pie Chart Data Kategorik',
                  status = 'primary', solidHeader = T,
                  plotOutput('piechart2')),
              box(title = 'Dashboard Controller',
                  status = 'info', solidHeader = T,
                  selectInput(inputId = 'cat_var2',
                              label = 'Pilih Variabel Kategorik',
                              choices = c('Jenis Kelamin (sex)' = 'sex',
                                          'Jenis Nyeri Dada (cp)' = 'cp',
                                          'Gula Darah Puasa (fbs)' = 'fbs',
                                          'Hasil Elektrokardiografi Saat Istirahat (restecg)' = 'restecg',
                                          'Angina yang diinduksi oleh latihan (exng)' = 'exng',
                                          'Kemiringan Garis ST Setelah Latihan Maksimal (slp)' = 'slp',
                                          'Taalsemia (thall)' = 'thall', 'Risiko Serangan Jantung (output)' = 'output',
                                          'Jumlah Pembuluh Darah Utama (caa)' = "caa"))),
              box(
                title = 'Dataframe Frekuensi',
                status = 'info',
                solidHeader = TRUE,
                DTOutput('table2')
              )
            ),
            
            h2('Data Numerik', style = 'font-weight: bold;'),
            
            fluidRow(
              box(title = 'Dashboard Controller',
                  status = 'info', solidHeader = T,
                  selectInput(inputId = 'num_var2',
                              label = 'Pilih Variabel Numerik',
                              choices = c('Usia (age)' = "age",
                                          'Tekanan Darah Saat Istirahat (trtbps)' = "trtbps",
                                          'Kolesterol (chol)' = "chol",
                                          'Denyut Jantung Maksimum yang Dicapai (thalachh)' = "thalachh",
                                          'Penurunan ST yang Terjadi Setelah Latihan (oldpeak)' = "oldpeak")))
              
            ),
            
            fluidRow(
              box(title = 'Histogram Data Numerik',
                  status = 'primary', solidHeader = T,
                  plotOutput('hist2')),
              box(title = 'Box Plot Data Numerik',
                  status = 'primary', solidHeader = T,
                  plotOutput('box2'))
            ),
            
            fluidRow(
              box(
                title = 'Matriks Korelasi Variabel Numerik',
                status = 'primary',
                solidHeader = TRUE,
                width = 12,
                plotOutput('corr2')
              )
            ),
            
            h2('Visualisasi Lainnya', style = 'font-weight: bold;'),
          
            fluidRow(
              box(
                title = 'Dashboard Controller',
                status = 'info',
                solidHeader = TRUE,
                selectInput(
                  inputId = 'sel_var_cat2',
                  label = 'Pilih Variabel Kategorik',
                  choices = c('Jenis Kelamin (sex)' = 'sex',
                              'Jenis Nyeri Dada (cp)' = 'cp',
                              'Gula Darah Puasa (fbs)' = 'fbs',
                              'Hasil Elektrokardiografi Saat Istirahat (restecg)' = 'restecg',
                              'Angina yang diinduksi oleh latihan (exng)' = 'exng',
                              'Kemiringan Garis ST Setelah Latihan Maksimal (slp)' = 'slp',
                              'Taalsemia (thall)' = 'thall')
                )
              ),
              
              fluidRow(
                column(width = 1),  # Left padding
                column(
                  width = 10,
                  box(
                    title = 'Plot Berdasarkan Variabel',
                    status = 'primary',
                    solidHeader = TRUE,
                    width = 12,
                    plotOutput('plot2', height = "500px")
                  )
                ),
                column(width = 1)  # Right padding
              )
              
            )
    ),
    
    tabItem(tabName = 'detail',
            h1('Analisis Data Lanjutan', style = 'font-weight: bold;'),
            
            h2('Split Data Train dan Test', style = 'font-weight: bold;'),
            fluidRow(
              valueBox('194 rows x 7 columns', 'Dimensi Data Train',
                       color = 'green', icon = icon('table')),
              valueBox('98 rows x 7 columns', 'Dimensi Data Test',
                       color = 'green', icon = icon('table'))
            ),
            
            fluidRow(tabBox(
              width = 12,
              tabPanel('Train Dataset', icon = icon('database'),
                       dataTableOutput('traindf')
              ),
              tabPanel('Test Dataset', icon = icon('database'),
                       dataTableOutput('testdf')
              )
            )),
            
            h2('Klasifikasi Reguler', style = 'font-weight: bold;'),
            fluidRow(
              box(title = 'Dashboard Controller',
                  status = 'info', solidHeader = TRUE,
                  selectInput(inputId = 'reg_method',
                              label = 'Pilih Metode Klasifikasi',
                              choices = c('Naive Bayes','Random Forest', 'Decision Tree', 'SVM'))),
              box(title = uiOutput("reg_method"), 
                  status = "success", 
                  solidHeader = TRUE,
                  uiOutput("reg_def")
              )
            ),
            
            fluidRow(
              box(title = 'ROC Plot', width = 8, plotOutput("plot_reg"),
                  status = 'primary', solidHeader = TRUE),
              box(title = 'Metrics of Analysis', width = 3,
                  DTOutput('acc_reg'),
                  status = 'primary', solidHeader = TRUE)
            ),
            
            h2('Klasifikasi dengan Cross-Validation', style = 'font-weight: bold;'),
            fluidRow(
              box(title = 'Dashboard Controller',
                  status = 'info', solidHeader = TRUE,
                  selectInput(inputId = 'cv_method',
                              label = 'Pilih Metode Klasifikasi',
                              choices = c('Naive Bayes','Random Forest', 'Decision Tree', 'SVM'))),
              box(title = uiOutput("cv_method"), 
                  status = "success", 
                  solidHeader = TRUE,
                  uiOutput("cv_def")
              )
            ),
            
            fluidRow(
              box(title = 'ROC Plot', width = 8, plotOutput("plot_cv"),
                  status = 'primary', solidHeader = TRUE),
              box(title = 'Metrics of Analysis', width = 3,
                  DTOutput('acc_cv'),
                  status = 'primary', solidHeader = TRUE)
            )
            
            # fluidRow(
            #   box('Confussion Matrix',
            #       DTOutput("conf_matrix_table")
            #       ),
            #   box('Accuracies',
            #       DTOutput('accDT'))
            # )
            
            ),
    tabItem(tabName = 'check',
            h1('Cek Kesehatan Risiko Penyakit Jantung Anda', style = 'font-weight: bold;'),
            h2('Masukkan Nilai yang Sesuai'),
            fluidRow(
              box(
                title = "Variabel Input", 
                status = "success", 
                solidHeader = TRUE,
                selectInput("thall", "Talasemia:", 
                            choices = list("Tidak ada" = 0, "Cacat tetap" = 1, "Cacat reversibel" = 2), 
                            selected = 2),
                selectInput("cp", "Jenis Nyeri Dada:", 
                            choices = list("Typical Angina" = 1, "Atypical Angina" = 2, "Non-Anginal Pain" = 3, "Asymptomatic" = 4), 
                            selected = 1),
                selectInput("caa", "Jumlah Pembuluh Darah Utama:", 
                            choices = list("0 pembuluh" = 0, "1 pembuluh" = 1, "2 pembuluh" = 2, "3 pembuluh" = 3), 
                            selected = 0),
                numericInput("oldpeak", "Penurunan ST Setelah Latihan:", value = 3),
                numericInput("thalachh", "Denyut Jantung Maksimum yang Dicapai:", value = 1),
                selectInput("exng", "Angina yang Diinduksi oleh Latihan:", 
                            choices = list("Tidak" = 0, "Ya" = 1), 
                            selected = 0),
                actionButton("predict_btn", "Cek Kondisi")
              ),
              box(title = "Hasil Prediksi", status = "warning", solidHeader = TRUE,
                  uiOutput("prediction_output")
              )
            )
            )
    
  )
)

# Fluid Header =============================================
head_panel <- headerPanel(title = 'Text Input Shiny Widget')

# Sidebar Panel ============================================
sidebarpanel <- sidebarPanel(
  textInput('projcode', 'Enter your project code'),
  textInput('projname', 'Input your project name'),
  textInput('tech', 'Which technology you are using?'),
  radioButtons('loc', 'What is your location?',
               choices = c('On-site', 'Off-site', 'Hybrid')),
  sliderInput('days', 'Number of Days Spent', 
              0, 100, c(10,20))
)

# Main Panel ===============================================
mainpanel <- mainPanel(
  textOutput('proj_code'),
  textOutput('proj_name'),
  textOutput('tech_used'),
  textOutput('location'),
  textOutput('no_days')
)

# UI =======================================================
shinyUI(

  dashboardPage(
    title = 'My Demo App',
    header,
    sidebar,
    dashBody
  )

)

# shinyUI(
#   fluidPage(
#     head_panel,
#     sidebarLayout(
#       sidebarpanel,
#       mainpanel
#     )
#   )
# )

