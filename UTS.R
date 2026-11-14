
# ============================================================
# MEDICAL COST ANALYTICS DASHBOARD
# Multiple Linear Regression - R Shiny
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(sandwich)
library(lmtest)
library(car)
library(shinyWidgets)

# ------------------------------------------------------------
# 1. MEMBACA DAN MENYIAPKAN DATA
# ------------------------------------------------------------

data <- read.csv("insurance_clean.csv")

data$sex <- factor(data$sex)
data$smoker <- factor(data$smoker)
data$region <- factor(data$region)

model <- lm(
  charges ~ age + sex + bmi + children + smoker + region,
  data = data
)

prediksi <- predict(model)

residual <- residuals(model)

rmse <- sqrt(mean((data$charges - prediksi)^2))
mae <- mean(abs(data$charges - prediksi))
mape <- mean(abs((data$charges - prediksi) / data$charges)) * 100

robust <- coeftest(
  model,
  vcov. = vcovHC(model, type = "HC3")
)

# ------------------------------------------------------------
# 2. DESAIN DASHBOARD
# ------------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = span(
      icon("chart-line"),
      " MEDICAL COST ANALYTICS"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 250,
    
    sidebarMenu(
      id = "menu",
      
      menuItem(
        "Research Profile",
        tabName = "research",
        icon = icon("book")
      ),
      
      menuItem("Overview", tabName = "overview",
               icon = icon("gauge-high")),
      
      menuItem("Data Explorer", tabName = "data",
               icon = icon("database")),
      
      menuItem("Visualization", tabName = "visual",
               icon = icon("line-chart")),
      
      menuItem("Regression Analysis", tabName = "regression",
               icon = icon("chart-line")),
      
      menuItem(
        "Kelemahan Model",
        tabName = "limitations",
        icon = icon("exclamation-triangle")
      ),
      
      menuItem("Prediction", tabName = "prediction",
               icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    
    # --------------------------------------------------------
    # CSS CUSTOM DESIGN
    # --------------------------------------------------------
    
    tags$head(
      tags$style(HTML("
        body, .content-wrapper {
          background-color: #080E22 !important;
          color: #EAF1FF !important;
        }

        .main-header .logo {
          background-color: #101D3B !important;
          color: #54A8FF !important;
          font-weight: bold;
          letter-spacing: 1px;
        }

        .main-header .navbar {
          background-color: #101D3B !important;
        }

        .main-sidebar {
          background-color: #0D1730 !important;
        }

        .sidebar-menu li a {
          color: #A9BCE0 !important;
        }

        .sidebar-menu li.active a {
          background-color: #1B3B79 !important;
          color: white !important;
          border-left: 4px solid #36A4FF;
        }

        .content-header h1 {
          color: #FFFFFF !important;
          font-weight: bold;
        }

        .box {
          background-color: #121D39 !important;
          border: 1px solid #263B68 !important;
          border-radius: 12px !important;
          color: #EAF1FF !important;
        }

        .box-header {
          color: #60AFFF !important;
          border-bottom: 1px solid #283C67 !important;
        }

        .small-box {
          border-radius: 12px !important;
          border: 1px solid #294779 !important;
          box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        }

        .small-box h3 {
          font-weight: bold;
        }

        .content {
          padding: 20px;
        }

        .form-control {
          background-color: #172646 !important;
          color: white !important;
          border: 1px solid #36558C !important;
        }

        .dataTables_wrapper {
          color: #EAF1FF !important;
        }

        table.dataTable {
          background-color: #121D39 !important;
          color: white !important;
        }

        .main-footer {
          background-color: #0D1730 !important;
          color: #8199C4 !important;
        }
      "))
    ),
    
    tabItems(
      

      
      
      # ======================================================
      # HALAMAN 1: RESEARCH PROFILE
      # ======================================================
      
      tabItem(
        tabName = "research",
        
        fluidRow(
          box(
            title = "RESEARCH OVERVIEW",
            width = 12,
            status = "primary",
            
            h2(
              "Medical Cost Analytics",
              style = "color:#55A8FF;font-weight:bold;"
            ),
            
            h4(
              "Analisis Faktor-Faktor yang Memengaruhi Biaya Asuransi Kesehatan Menggunakan Regresi Linear Berganda"
            ),
            
            br(),
            
            p(
              "Penelitian ini bertujuan untuk menganalisis hubungan
        karakteristik individu dengan biaya medis serta
        mengidentifikasi variabel-variabel yang berhubungan
        secara statistik dengan besarnya biaya medis."
            ),
            
            tags$a(
              href = "https://www.kaggle.com/datasets/mirichoi0218/insurance",
              target = "_blank",
              "Lihat Sumber Dataset di Kaggle"
            )
          )
        ),
        
        # LATAR BELAKANG
        
        fluidRow(
          box(
            title = "Research Background",
            width = 12,
            status = "primary",
            
            p(
              "Biaya pelayanan kesehatan merupakan salah satu aspek
        penting dalam industri asuransi kesehatan. Besarnya
        biaya medis yang harus ditanggung setiap individu
        dapat berbeda-beda berdasarkan karakteristik
        individu tersebut."
            ),
            
            p(
              "Karakteristik seperti usia, jenis kelamin, BMI,
        jumlah anak, status merokok, dan wilayah tempat
        tinggal diduga memiliki hubungan dengan variasi
        biaya medis."
            ),
            
            p(
              "Oleh karena itu, penelitian ini menggunakan
        regresi linear berganda untuk menganalisis
        hubungan antara beberapa variabel penjelas
        dengan biaya medis sebagai variabel respons."
            )
          )
        ),
        
        # IDENTITAS DATASET
        
        fluidRow(
          valueBox(
            1338,
            "Observasi Awal",
            icon = icon("database"),
            color = "blue",
            width = 3
          ),
          
          valueBox(
            nrow(data),
            "Observasi Akhir",
            icon = icon("users"),
            color = "aqua",
            width = 3
          ),
          
          valueBox(
            ncol(data),
            "Jumlah Variabel",
            icon = icon("list"),
            color = "purple",
            width = 3
          ),
          
          valueBox(
            1,
            "Data Duplikat Dihapus",
            icon = icon("check-circle"),
            color = "green",
            width = 3
          )
        ),
        
        fluidRow(
          box(
            title = "Dataset Information",
            width = 12,
            status = "primary",
            
            tags$table(
              class = "table table-bordered",
              
              tags$tr(
                tags$th("Informasi"),
                tags$th("Keterangan")
              ),
              
              tags$tr(
                tags$td("Nama Dataset"),
                tags$td("Medical Cost Personal Dataset")
              ),
              
              tags$tr(
                tags$td("Sumber"),
                tags$td("Kaggle - Miri Choi")
              ),
              
              tags$tr(
                tags$td("Nama File"),
                tags$td("insurance.csv")
              ),
              
              tags$tr(
                tags$td("Observasi Awal"),
                tags$td("1.338")
              ),
              
              tags$tr(
                tags$td("Observasi Setelah Pembersihan"),
                tags$td("1.337")
              ),
              
              tags$tr(
                tags$td("Missing Value"),
                tags$td("0")
              ),
              
              tags$tr(
                tags$td("Data Duplikat"),
                tags$td("1 observasi dihapus")
              ),
              
              tags$tr(
                tags$td("Jumlah Variabel"),
                tags$td("7 variabel")
              )
            )
          )
        ),
        
        # DEFINISI VARIABEL
        
        fluidRow(
          box(
            title = "Variable Description",
            width = 12,
            status = "primary",
            
            DTOutput("variable_table")
          )
        ),
        
        # TUJUAN PENELITIAN
        
        fluidRow(
          box(
            title = "Research Objectives",
            width = 6,
            status = "primary",
            
            tags$ol(
              tags$li("Mengetahui karakteristik dan distribusi setiap variabel."),
              
              tags$li("Menganalisis hubungan karakteristik individu dengan biaya medis."),
              
              tags$li("Mengetahui perbedaan biaya medis berdasarkan status merokok."),
              
              tags$li("Menganalisis hubungan variabel penjelas dengan biaya medis melalui regresi linear berganda."),
              
              tags$li("Menguji asumsi regresi dan mengevaluasi kemampuan prediksi model."),
              
              tags$li("Mengembangkan aplikasi interaktif menggunakan R Shiny.")
            )
          ),
          
          box(
            title = "Research Questions",
            width = 6,
            status = "primary",
            
            tags$ol(
              tags$li("Bagaimana karakteristik dan distribusi setiap variabel?"),
              
              tags$li("Bagaimana hubungan usia dan BMI dengan biaya medis?"),
              
              tags$li("Apakah terdapat perbedaan biaya medis berdasarkan status merokok?"),
              
              tags$li("Bagaimana hubungan enam variabel penjelas dengan biaya medis dalam model regresi?"),
              
              tags$li("Bagaimana hasil pengujian asumsi dan evaluasi model?"),
              
              tags$li("Bagaimana penerapan R Shiny untuk menyajikan hasil analisis?")
            )
          )
        ),
        
        # METODE ANALISIS
        
        fluidRow(
          box(
            title = "Research Methodology",
            width = 12,
            status = "primary",
            
            tags$ol(
              tags$li("Data collection and preparation"),
              
              tags$li("Exploratory Data Analysis (EDA)"),
              
              tags$li("Correlation and comparative analysis"),
              
              tags$li("Multiple Linear Regression"),
              
              tags$li("Regression assumption testing"),
              
              tags$li("Robust HC3 estimation"),
              
              tags$li("Model evaluation and prediction"),
              
              tags$li("Interactive visualization using R Shiny")
            )
          )
        )
      ),
      
      # ======================================================
      # HALAMAN 1: OVERVIEW
      # ======================================================
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          box(
            width = 12,
            title = "MEDICAL COST ANALYTICS PLATFORM",
            status = "primary",
            p(
              "Dashboard interaktif untuk eksplorasi data dan",
              "analisis regresi linear berganda pada biaya medis."
            )
          )
        ),
        
        fluidRow(
          valueBox(
            nrow(data),
            "Total Individu",
            icon = icon("users"),
            color = "blue",
            width = 3
          ),
          
          valueBox(
            paste0(
              round(summary(model)$r.squared * 100, 2),
              "%"
            ),
            "R-Squared",
            icon = icon("chart-line"),
            color = "aqua",
            width = 3
          ),
          
          valueBox(
            format(round(rmse, 2), big.mark = ","),
            "RMSE",
            icon = icon("bullseye"),
            color = "purple",
            width = 3
          ),
          
          valueBox(
            paste0(round(mape, 2), "%"),
            "MAPE",
            icon = icon("percent"),
            color = "teal",
            width = 3
          )
        ),
        
        
        fluidRow(
          box(
            title = "Actual vs Predicted",
            width = 7,
            status = "primary",
            
            plotlyOutput("overview_scatter", height = 350),
            
            # INTERPRETASI GRAFIK
            tags$div(
              style = "
        background-color: #101E3B;
        border-left: 4px solid #36A3FF;
        padding: 15px;
        margin-top: 15px;
        border-radius: 8px;
        color: #E5EDFF;
      ",
              
              tags$h4(
                "Interpretasi Grafik",
                style = "color: #55A8FF; font-weight: bold;"
              ),
              
              tags$p(
                paste(
                  "Berdasarkan grafik perbandingan nilai aktual dan prediksi,",
                  "sebagian besar hasil prediksi mengikuti pola kenaikan",
                  "biaya medis aktual. Model memiliki R-Squared sebesar",
                  "75,07%, yang menunjukkan bahwa model mampu menjelaskan",
                  "sekitar 75,07% variasi biaya medis. Namun, masih terdapat",
                  "sejumlah titik dengan selisih cukup besar antara nilai",
                  "aktual dan prediksi, sehingga model masih memiliki",
                  "kesalahan prediksi yang perlu diperhatikan."
                )
              )
            )
          ),
        
          
          box(
            title = "Smoking Distribution",
            width = 5,
            status = "primary",
            plotlyOutput("overview_smoker", height = 350)
          )
        )
      ),
      
      # ======================================================
      # HALAMAN 2: DATA EXPLORER
      # ======================================================
      
      
      
      tabItem(
        tabName = "data",
        
        # DATA FILTERING
        fluidRow(
          box(
            title = "Data Filtering",
            width = 12,
            status = "primary",
            
            sliderInput(
              inputId = "filter_age",
              label = "Rentang Usia:",
              min = min(data$age),
              max = max(data$age),
              value = c(min(data$age), max(data$age))
            ),
            
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "filter_sex",
                  label = "Jenis Kelamin:",
                  choices = c(
                    "Semua" = "Semua",
                    "Perempuan" = "female",
                    "Laki-laki" = "male"
                  ),
                  selected = "Semua"
                )
              ),
              
              column(
                width = 4,
                selectInput(
                  inputId = "filter_region",
                  label = "Wilayah:",
                  choices = c(
                    "Semua" = "Semua",
                    "Northeast" = "northeast",
                    "Northwest" = "northwest",
                    "Southeast" = "southeast",
                    "Southwest" = "southwest"
                  ),
                  selected = "Semua"
                )
              ),
              
              column(
                width = 4,
                selectInput(
                  inputId = "filter_smoker",
                  label = "Status Merokok:",
                  choices = c(
                    "Semua" = "Semua",
                    "Tidak Merokok" = "no",
                    "Merokok" = "yes"
                  ),
                  selected = "Semua"
                )
              )
            ),
            
            actionButton(
              inputId = "reset_filter",
              label = "Reset Filter",
              icon = icon("refresh"),
              class = "btn-primary"
            )
          )
        ),
        
        # PILIHAN ANALISIS
        fluidRow(
          box(
            width = 6,
            status = "primary",
            background = "blue",
            actionButton(
              inputId = "show_overview",
              label = tagList(
                icon("database"),
                "Dataset Overview"
              ),
              class = "btn-primary btn-block"
            ),
            p(
              "Ringkasan dan preview dataset.",
              style = "color: #D5E3F5;"
            )
          ),
          
          box(
            width = 6,
            status = "primary",
            background = "navy",
            actionButton(
              inputId = "show_statistics",
              label = tagList(
                icon("calculator"),
                "Descriptive Statistics"
              ),
              class = "btn-primary btn-block"
            ),
            p(
              "Statistik deskriptif data.",
              style = "color: #D5E3F5;"
            )
          )
        ),
          
        
        # DATASET OVERVIEW
        conditionalPanel(
          condition = "output.active_analysis == 'overview'",
          fluidRow(
            box(
              title = "Dataset Overview",
              width = 12,
              status = "primary",
              DTOutput("data_table")
            )
          )
        ),
        
        # DESCRIPTIVE STATISTICS
        conditionalPanel(
          condition = "output.active_analysis == 'statistics'",
          
          fluidRow(
            box(
              title = "Descriptive Statistics",
              width = 12,
              status = "primary",
              DTOutput("descriptive_table")
            )
          ),
          
          fluidRow(
            box(
              title = "Categorical Variables Distribution",
              width = 12,
              status = "primary",
              DTOutput("categorical_table")
            )
          )
        ),
     
       # DATA EXPLORER KPI CARDS
       fluidRow(
         valueBoxOutput("kpi_observations", width = 3),
         valueBoxOutput("kpi_missing", width = 3),
         valueBoxOutput("kpi_duplicates", width = 3),
         valueBoxOutput("kpi_completeness", width = 3)
       )
      ),
      
      
      # ======================================================
      # HALAMAN 3: VISUALIZATION
      # ======================================================
      
      tabItem(
        tabName = "visual",
        
        # VISUALIZATION HEADER
        fluidRow(
          box(
            title = "Analysis Level",
            width = 12,
            status = "primary",
            
            radioGroupButtons(
              inputId = "viz_analysis",
              choices = c(
                "Univariat",
                "Bivariat",
                "Multivariat"
              ),
              selected = "Univariat",
              justified = TRUE,
              status = "primary"
            )
          )
        ),
        
        # VARIABLE TYPE
        fluidRow(
          box(
            title = "Variable Type",
            width = 12,
            status = "primary",
            
            radioGroupButtons(
              inputId = "viz_type",
              choices = c("Numerik", "Kategorik"),
              selected = "Numerik",
              justified = TRUE,
              status = "primary"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Variable Selection",
            width = 12,
            status = "primary",
            uiOutput("viz_variable_selector")
          )
        ),
        fluidRow(
          box(
            title = "Chart Selection",
            width = 12,
            status = "primary",
            uiOutput("viz_chart_selector")
          )
        ),
        
        fluidRow(
          box(
            title = "Visualization Result",
            width = 12,
            status = "primary",
            plotlyOutput("viz_plot", height = 500)
          )
        )
      ),

      
      # ==========================================================
      # REGRESSION ANALYSIS - MAIN PAGE & DETAIL PAGE
      # ==========================================================
      
      tabItem(
        tabName = "regression",
        
        uiOutput("regression_page")
        
      ),
      
      # ==========================================================
      # KELEMAHAN MODEL
      # ==========================================================
      
      tabItem(
        tabName = "limitations",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h2(
              "Kelemahan Model Regresi",
              style = "font-weight:700;"
            ),
            
            p(
              "Halaman ini menjelaskan keterbatasan model regresi linear berganda berdasarkan hasil pengujian dan karakteristik data yang digunakan."
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            
            h3("1. Keterbatasan Uji Asumsi Klasik"),
            
            uiOutput("limitation_assumption")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h3("2. Keterbatasan Model Regresi"),
            
            p(
              "Model regresi hanya menggunakan variabel age, sex, bmi, children, smoker, dan region untuk menjelaskan variasi biaya medis (charges)."
            ),
            
            p(
              "Masih terdapat kemungkinan bahwa faktor lain di luar variabel penelitian turut memengaruhi besarnya biaya medis."
            ),
            
            p(
              "Selain itu, hasil analisis regresi bergantung pada kesesuaian model dan karakteristik data yang digunakan."
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            h3("3. Pertimbangan dalam Melanjutkan Analisis"),
            
            p(
              "Meskipun terdapat kemungkinan beberapa asumsi klasik tidak terpenuhi, analisis regresi tetap dilanjutkan sesuai dengan ketentuan dan tujuan pengerjaan soal."
            ),
            
            p(
              "Hasil pengujian tetap disajikan sebagai bahan evaluasi terhadap model. Namun, hasil estimasi, pengujian signifikansi, dan interpretasi regresi perlu dilakukan secara hati-hati dengan mempertimbangkan keterbatasan yang ditemukan."
            )
          )
        )
      ),
      
      
      # ======================================================
      # HALAMAN 6: PREDICTION
      # ======================================================
      
      tabItem(
        tabName = "prediction",
        
        fluidRow(
          box(
            title = "Individual Medical Cost Prediction",
            width = 5,
            status = "primary",
            
            numericInput(
              "age_input", "Age",
              value = 30, min = 18, max = 64
            ),
            
            selectInput(
              "sex_input", "Sex",
              choices = levels(data$sex)
            ),
            
            numericInput(
              "bmi_input", "BMI",
              value = 30, min = 15, max = 55
            ),
            
            numericInput(
              "children_input", "Children",
              value = 0, min = 0, max = 5
            ),
            
            selectInput(
              "smoker_input", "Smoker",
              choices = levels(data$smoker)
            ),
            
            selectInput(
              "region_input", "Region",
              choices = levels(data$region)
            ),
            
            actionButton(
              "predict_button",
              "Calculate Prediction",
              icon = icon("calculator"),
              class = "btn-primary"
            )
          ),
          
          box(
            title = "Prediction Result",
            width = 7,
            status = "primary",
            
            uiOutput("prediction_result")
          )
        )
      )
    )
  )
)

# ============================================================
# 3. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  
  output$variable_table <- renderDT({
    
    variabel <- data.frame(
      
      Variabel = c(
        "age",
        "sex",
        "bmi",
        "children",
        "smoker",
        "region",
        "charges"
      ),
      
      Jenis = c(
        "Numerik",
        "Kategorik",
        "Numerik",
        "Numerik Diskrit",
        "Kategorik",
        "Kategorik",
        "Numerik"
      ),
      
      Definisi = c(
        "Usia individu dalam tahun",
        "Jenis kelamin individu",
        "Indeks massa tubuh (kg/m2)",
        "Jumlah anak atau tanggungan",
        "Status kebiasaan merokok",
        "Wilayah tempat tinggal",
        "Biaya medis yang ditagihkan"
      ),
      
      Peran = c(
        "Variabel penjelas",
        "Variabel penjelas",
        "Variabel penjelas",
        "Variabel penjelas",
        "Variabel penjelas",
        "Variabel penjelas",
        "Variabel respons"
      )
    )
    
    DT::datatable(
      variabel,
      rownames = FALSE,
      options = list(
        pageLength = 7,
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # OVERVIEW
  # ----------------------------------------------------------
  
  
  output$overview_scatter <- renderPlotly({
    
    nilai_min <- min(c(data$charges, prediksi), na.rm = TRUE)
    nilai_max <- max(c(data$charges, prediksi), na.rm = TRUE)
    
    plot_ly(
      x = data$charges,
      y = prediksi,
      type = "scatter",
      mode = "markers",
      name = "Actual vs Predicted",
      marker = list(
        color = "#35B8F4",
        opacity = 0.65,
        size = 7
      )
    ) %>%
      
      # Garis ideal y = x
      add_trace(
        x = c(nilai_min, nilai_max),
        y = c(nilai_min, nilai_max),
        type = "scatter",
        mode = "lines",
        name = "Ideal Prediction (y = x)",
        line = list(
          color = "#FF5B5B",
          width = 2,
          dash = "dash"
        ),
        inherit = FALSE
      ) %>%
      
      layout(
        title = list(text = "Actual vs Predicted"),
        xaxis = list(
          title = "Actual Charges",
          range = c(nilai_min, nilai_max),
          scaleanchor = "y",
          scaleratio = 1
        ),
        yaxis = list(
          title = "Predicted Charges",
          range = c(nilai_min, nilai_max)
        ),
        paper_bgcolor = "#121D39",
        plot_bgcolor = "#121D39",
        font = list(color = "#FFFFFF"),
        legend = list(
          orientation = "h",
          y = -0.2
        )
      )
  })
  
  
  output$overview_smoker <- renderPlotly({
    
    smoker_count <- as.data.frame(table(data$smoker))
    
    plot_ly(
      smoker_count,
      labels = ~Var1,
      values = ~Freq,
      type = "pie",
      hole = 0.65,
      marker = list(
        colors = c("#286BFF", "#21C8E8")
      )
    ) %>%
      
      layout(
        paper_bgcolor = "#121D39",
        font = list(color = "#FFFFFF")
      )
  })
  
  # ----------------------------------------------------------
  # DATA EXPLORER
  # ----------------------------------------------------------
  
  output$data_table <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  # DATA FILTERING
  filtered_data <- reactive({
    
    hasil <- data
    
    hasil <- hasil %>%
      filter(
        age >= input$filter_age[1],
        age <= input$filter_age[2]
      )
    
    if (input$filter_sex != "Semua") {
      hasil <- hasil %>%
        filter(sex == input$filter_sex)
    }
    
    if (input$filter_region != "Semua") {
      hasil <- hasil %>%
        filter(region == input$filter_region)
    }
    
    if (input$filter_smoker != "Semua") {
      hasil <- hasil %>%
        filter(smoker == input$filter_smoker)
    }
    
    hasil
  })
  
  # RESET FILTER
  observeEvent(input$reset_filter, {
    
    updateSliderInput(
      session,
      "filter_age",
      value = c(min(data$age), max(data$age))
    )
    
    updateSelectInput(session, "filter_sex", selected = "Semua")
    updateSelectInput(session, "filter_region", selected = "Semua")
    updateSelectInput(session, "filter_smoker", selected = "Semua")
    
  })
  
  # ANALYSIS NAVIGATION
  active_analysis <- reactiveVal("overview")
  
  observeEvent(input$show_overview, {
    active_analysis("overview")
  })
  
  observeEvent(input$show_statistics, {
    active_analysis("statistics")
  })
  output$active_analysis <- reactive({
    active_analysis()
  })
  
  outputOptions(
    output,
    "active_analysis",
    suspendWhenHidden = FALSE
  )
  

  
  # AGE DISTRIBUTION
  output$age_plot <- renderPlotly({
    
    ggplotly(
      ggplot(filtered_data(), aes(x = age)) +
        geom_histogram(
          bins = 25,
          fill = "#2688FF",
          color = "#FFFFFF"
        ) +
        labs(
          title = "Age Distribution",
          x = "Age",
          y = "Frequency"
        ) +
        theme_minimal()
    )
  })
  
  
  # MEDICAL COST DISTRIBUTION
  output$charges_plot <- renderPlotly({
    
    ggplotly(
      ggplot(filtered_data(), aes(x = charges)) +
        geom_histogram(
          bins = 30,
          fill = "#20B8D9",
          color = "white"
        ) +
        labs(
          title = "Medical Cost Distribution",
          x = "Medical Cost",
          y = "Frequency"
        ) +
        theme_minimal()
    )
  })
  # DESCRIPTIVE STATISTICS TABLE
  output$descriptive_table <- renderDT({
    
    hasil_data <- filtered_data()
    
    statistik <- data.frame(
      Variabel = c(
        "Usia",
        "BMI",
        "Jumlah Anak",
        "Biaya Medis"
      ),
      
      Mean = c(
        mean(hasil_data$age),
        mean(hasil_data$bmi),
        mean(hasil_data$children),
        mean(hasil_data$charges)
      ),
      
      Median = c(
        median(hasil_data$age),
        median(hasil_data$bmi),
        median(hasil_data$children),
        median(hasil_data$charges)
      ),
      
      `Standar Deviasi` = c(
        sd(hasil_data$age),
        sd(hasil_data$bmi),
        sd(hasil_data$children),
        sd(hasil_data$charges)
      ),
      
      Varians = c(
        var(hasil_data$age),
        var(hasil_data$bmi),
        var(hasil_data$children),
        var(hasil_data$charges)
      ),
      
      Minimum = c(
        min(hasil_data$age),
        min(hasil_data$bmi),
        min(hasil_data$children),
        min(hasil_data$charges)
      ),
      
      Maksimum = c(
        max(hasil_data$age),
        max(hasil_data$bmi),
        max(hasil_data$children),
        max(hasil_data$charges)
      ),
      
      check.names = FALSE
    )
    
    datatable(
      statistik,
      rownames = FALSE,
      filter = "none",
      class = "stripe hover compact",
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        searching = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ data",
          info = "Menampilkan _START_ sampai _END_ dari _TOTAL_ data",
          paginate = list(
            previous = "Sebelumnya",
            `next` = "Selanjutnya"
          )
        )
      )
    ) %>%
      formatRound(
        columns = 2:7,
        digits = 2,
        mark = ".",
        dec.mark = ","
      )
    
  })
  # CATEGORICAL VARIABLES DISTRIBUTION
  output$categorical_table <- renderDT({
    
    hasil_data <- filtered_data()
    
    buat_distribusi <- function(x, nama_variabel) {
      
      frekuensi <- table(x)
      
      data.frame(
        Variabel = nama_variabel,
        Kategori = names(frekuensi),
        Frekuensi = as.integer(frekuensi),
        Persentase = round(
          as.numeric(frekuensi) / sum(frekuensi) * 100,
          2
        )
      )
    }
    
    hasil <- rbind(
      buat_distribusi(
        hasil_data$sex,
        "Jenis Kelamin"
      ),
      
      buat_distribusi(
        hasil_data$smoker,
        "Status Merokok"
      ),
      
      buat_distribusi(
        hasil_data$region,
        "Wilayah"
      )
    )
    
    datatable(
      hasil,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE,
        scrollX = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ data",
          info = "Menampilkan _START_ sampai _END_ dari _TOTAL_ data",
          paginate = list(
            previous = "Sebelumnya",
            `next` = "Selanjutnya"
          )
        )
      )
    ) %>%
      formatRound(
        columns = "Persentase",
        digits = 2,
        dec.mark = ","
      )
    
  })
  # DATA QUALITY REPORT
  output$quality_table <- renderDT({
    
    hasil_data <- filtered_data()
    
    total_observasi <- nrow(hasil_data)
    total_variabel <- ncol(hasil_data)
    
    missing_value <- sum(is.na(hasil_data))
    
    data_duplikat <- sum(duplicated(hasil_data))
    
    total_sel <- length(hasil_data)
    
    kelengkapan <- if (total_sel > 0) {
      round(
        (total_sel - missing_value) / total_sel * 100,
        2
      )
    } else {
      NA_real_
    }
    
    laporan_kualitas <- data.frame(
      Indikator = c(
        "Total Observasi",
        "Total Variabel",
        "Missing Value",
        "Data Duplikat",
        "Kelengkapan Data"
      ),
      
      Hasil = c(
        total_observasi,
        total_variabel,
        missing_value,
        data_duplikat,
        ifelse(
          is.na(kelengkapan),
          "NA",
          paste0(kelengkapan, "%")
        )
      )
    )
    
    datatable(
      laporan_kualitas,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        lengthChange = FALSE,
        language = list(
          emptyTable = "Tidak ada data"
        )
      )
    )
    
  })
  # KPI: TOTAL OBSERVATIONS
  output$kpi_observations <- renderValueBox({
    
    hasil_data <- filtered_data()
    
    valueBox(
      value = format(nrow(hasil_data), big.mark = "."),
      subtitle = "Total Observations",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  
  # KPI: MISSING VALUES
  output$kpi_missing <- renderValueBox({
    
    hasil_data <- filtered_data()
    
    valueBox(
      value = sum(is.na(hasil_data)),
      subtitle = "Missing Values",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  
  # KPI: DUPLICATE DATA
  output$kpi_duplicates <- renderValueBox({
    
    hasil_data <- filtered_data()
    
    valueBox(
      value = sum(duplicated(hasil_data)),
      subtitle = "Duplicate Rows",
      icon = icon("copy"),
      color = "yellow"
    )
  })
  
  
  # KPI: DATA COMPLETENESS
  output$kpi_completeness <- renderValueBox({
    
    hasil_data <- filtered_data()
    
    total_sel <- length(hasil_data)
    missing_value <- sum(is.na(hasil_data))
    
    kelengkapan <- if (total_sel > 0) {
      round((total_sel - missing_value) / total_sel * 100, 2)
    } else {
      0
    }
    
    valueBox(
      value = paste0(kelengkapan, "%"),
      subtitle = "Data Completeness",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  
  
  # ----------------------------------------------------------
  # VISUALIZATION - VARIABLE SELECTION
  # ----------------------------------------------------------
  
  
  # ==========================================================
  # VISUALIZATION - VARIABLE SELECTOR
  # ==========================================================
  
  output$viz_variable_selector <- renderUI({
    
    req(input$viz_analysis, input$viz_type)
    
    variabel_numerik <- c(
      "Age (Numerik)" = "age",
      "BMI (Numerik)" = "bmi",
      "Children (Numerik)" = "children",
      "Charges (Numerik)" = "charges"
    )
    
    variabel_kategorik <- c(
      "Sex (Kategorik)" = "sex",
      "Smoker (Kategorik)" = "smoker",
      "Region (Kategorik)" = "region"
    )
    
    if (input$viz_analysis == "Univariat") {
      
      pilihan <- if (input$viz_type == "Numerik") {
        variabel_numerik
      } else {
        variabel_kategorik
      }
      
      jumlah <- 1
      
    } else if (input$viz_analysis == "Bivariat") {
      
      pilihan <- c(variabel_numerik, variabel_kategorik)
      jumlah <- 2
      
    } else {
      
      pilihan <- c(variabel_numerik, variabel_kategorik)
      jumlah <- 3
      
    }
    
    selectizeInput(
      inputId = "viz_variables",
      label = paste("Pilih", jumlah, "variabel untuk analisis:"),
      choices = pilihan,
      multiple = TRUE,
      options = list(
        maxItems = jumlah,
        placeholder = "Pilih variabel..."
      )
    )
    
  })
  
  
  # ==========================================================
  # VISUALIZATION - CHART RECOMMENDATION
  # ==========================================================
  
  output$viz_chart_selector <- renderUI({
    
    req(input$viz_analysis, input$viz_variables)
    
    variabel <- input$viz_variables
    
    jumlah_wajib <- switch(
      input$viz_analysis,
      "Univariat" = 1,
      "Bivariat" = 2,
      "Multivariat" = 3
    )
    
    req(length(variabel) == jumlah_wajib)
    
    numerik <- c("age", "bmi", "children", "charges")
    kategorik <- c("sex", "smoker", "region")
    
    jumlah_numerik <- sum(variabel %in% numerik)
    jumlah_kategorik <- sum(variabel %in% kategorik)
    
    if (input$viz_analysis == "Univariat") {
      
      if (jumlah_numerik == 1) {
        
        pilihan_grafik <- c(
          "Histogram" = "histogram",
          "Boxplot" = "boxplot",
          "Density Plot" = "density"
        )
        
        rekomendasi <- "histogram"
        
      } else {
        
        pilihan_grafik <- c(
          "Bar Chart" = "bar",
          "Pie Chart" = "pie",
          "Pareto Chart" = "pareto"
        )
        
        rekomendasi <- "bar"
        
      }
      
    } else if (input$viz_analysis == "Bivariat") {
      
      if (jumlah_numerik == 2) {
        
        pilihan_grafik <- c(
          "Scatter Plot" = "scatter",
          "Line Plot" = "line",
          "Scatter Plot with Regression" = "regression_scatter"
        )
        
        rekomendasi <- "scatter"
        
      } else if (jumlah_kategorik == 2) {
        
        pilihan_grafik <- c(
          "Grouped Bar Chart" = "grouped_bar",
          "Stacked Bar Chart" = "stacked_bar",
          "Heatmap" = "heatmap"
        )
        
        rekomendasi <- "grouped_bar"
        
      } else {
        
        pilihan_grafik <- c(
          "Boxplot" = "boxplot_group",
          "Violin Plot" = "violin",
          "Mean Plot" = "mean_plot"
        )
        
        rekomendasi <- "boxplot_group"
        
      }
      
    } else {
      
      if (jumlah_numerik == 3) {
        
        pilihan_grafik <- c(
          "3D Scatter Plot" = "scatter3d",
          "Scatter Matrix" = "scatter_matrix",
          "Parallel Coordinates" = "parallel"
        )
        
        rekomendasi <- "scatter3d"
        
      } else if (jumlah_numerik == 2) {
        
        pilihan_grafik <- c(
          "Grouped Scatter Plot" = "grouped_scatter",
          "Faceted Scatter Plot" = "faceted_scatter",
          "Regression by Group" = "regression_by_group"
        )
        
        rekomendasi <- "grouped_scatter"
        
      } else if (jumlah_numerik == 1) {
        
        pilihan_grafik <- c(
          "Grouped Boxplot" = "grouped_boxplot",
          "Grouped Violin Plot" = "grouped_violin",
          "Mean Plot" = "mean_plot"
        )
        
        rekomendasi <- "grouped_boxplot"
        
      } else {
        
        pilihan_grafik <- c(
          "Grouped Bar Chart" = "grouped_bar",
          "Stacked Bar Chart" = "stacked_bar",
          "Heatmap" = "heatmap"
        )
        
        rekomendasi <- "grouped_bar"
        
      }
      
    }
    
    tagList(
      
      helpText(
        paste(
          "Rekomendasi grafik berdasarkan kombinasi variabel:",
          names(pilihan_grafik)[match(
            rekomendasi,
            unname(pilihan_grafik)
          )]
        )
      ),
      
      selectInput(
        inputId = "viz_chart",
        label = "Pilih jenis grafik:",
        choices = pilihan_grafik,
        selected = rekomendasi
      )
      
    )
    
  })
  
  
  # ==========================================================
  # VISUALIZATION - CHART ENGINE
  # UNIVARIAT, BIVARIAT, MULTIVARIAT
  # ==========================================================
  
  output$viz_plot <- renderPlotly({
    
    req(input$viz_analysis, input$viz_variables, input$viz_chart)
    
    variabel <- input$viz_variables
    jenis_grafik <- input$viz_chart
    hasil_data <- data
    
    numerik <- c("age", "bmi", "children", "charges")
    kategorik <- c("sex", "smoker", "region")
    
    jumlah_wajib <- switch(
      input$viz_analysis,
      "Univariat" = 1,
      "Bivariat" = 2,
      "Multivariat" = 3
    )
    
    req(length(variabel) == jumlah_wajib)
    
    jumlah_numerik <- sum(variabel %in% numerik)
    jumlah_kategorik <- sum(variabel %in% kategorik)
    
    
    # --------------------------------------------------------
    # UNIVARIAT
    # --------------------------------------------------------
    
    if (input$viz_analysis == "Univariat") {
      
      var1 <- variabel[1]
      
      if (var1 %in% numerik) {
        
        if (jenis_grafik == "histogram") {
          
          p <- ggplot(
            hasil_data,
            aes(x = .data[[var1]])
          ) +
            geom_histogram(
              bins = 30,
              fill = "#2688FF",
              color = "white",
              alpha = 0.85
            ) +
            labs(
              title = paste("Histogram of", var1),
              x = var1,
              y = "Frequency"
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "boxplot") {
          
          p <- ggplot(
            hasil_data,
            aes(y = .data[[var1]])
          ) +
            geom_boxplot(
              fill = "#20B8D9",
              color = "white"
            ) +
            labs(
              title = paste("Boxplot of", var1),
              y = var1,
              x = NULL
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "density") {
          
          p <- ggplot(
            hasil_data,
            aes(x = .data[[var1]])
          ) +
            geom_density(
              fill = "#2688FF",
              color = "#20B8D9",
              alpha = 0.5,
              linewidth = 1
            ) +
            labs(
              title = paste("Density Plot of", var1),
              x = var1,
              y = "Density"
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        }
        
      } else {
        
        frekuensi <- as.data.frame(
          table(hasil_data[[var1]])
        )
        
        names(frekuensi) <- c("Kategori", "Frekuensi")
        
        frekuensi <- frekuensi[
          order(frekuensi$Frekuensi, decreasing = TRUE),
        ]
        
        if (jenis_grafik == "bar") {
          
          p <- ggplot(
            frekuensi,
            aes(
              x = reorder(Kategori, Frekuensi),
              y = Frekuensi
            )
          ) +
            geom_col(
              fill = "#2688FF",
              width = 0.7
            ) +
            labs(
              title = paste("Bar Chart of", var1),
              x = var1,
              y = "Frequency"
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "pie") {
          
          plot_ly(
            frekuensi,
            labels = ~Kategori,
            values = ~Frekuensi,
            type = "pie",
            textinfo = "label+percent"
          ) %>%
            layout(
              title = paste("Pie Chart of", var1),
              paper_bgcolor = "#121D39",
              font = list(color = "#FFFFFF")
            )
          
        } else if (jenis_grafik == "pareto") {
          
          frekuensi$Persentase_Kumulatif <-
            cumsum(frekuensi$Frekuensi) /
            sum(frekuensi$Frekuensi) * 100
          
          maksimum <- max(frekuensi$Frekuensi)
          
          p <- ggplot(
            frekuensi,
            aes(x = reorder(Kategori, -Frekuensi))
          ) +
            geom_col(
              aes(y = Frekuensi),
              fill = "#2688FF"
            ) +
            geom_line(
              aes(
                y = Persentase_Kumulatif * maksimum / 100,
                group = 1
              ),
              color = "#FF6B6B",
              linewidth = 1
            ) +
            geom_point(
              aes(
                y = Persentase_Kumulatif * maksimum / 100
              ),
              color = "#FF6B6B",
              size = 3
            ) +
            scale_y_continuous(
              name = "Frequency",
              sec.axis = sec_axis(
                ~ . / maksimum * 100,
                name = "Cumulative Percentage (%)"
              )
            ) +
            labs(
              title = paste("Pareto Chart of", var1),
              x = var1
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        }
        
      }
      
      
      # --------------------------------------------------------
      # BIVARIAT
      # --------------------------------------------------------
      
    } else if (input$viz_analysis == "Bivariat") {
      
      var1 <- variabel[1]
      var2 <- variabel[2]
      
      tipe1 <- ifelse(var1 %in% numerik, "numerik", "kategorik")
      tipe2 <- ifelse(var2 %in% numerik, "numerik", "kategorik")
      
      
      # NUMERIK + NUMERIK
      
      if (jumlah_numerik == 2) {
        
        if (jenis_grafik == "scatter") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[var1]],
              y = .data[[var2]]
            )
          ) +
            geom_point(
              color = "#2688FF",
              alpha = 0.6,
              size = 2
            ) +
            labs(
              title = paste(var1, "vs", var2),
              x = var1,
              y = var2
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "line") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[var1]],
              y = .data[[var2]]
            )
          ) +
            geom_line(
              color = "#2688FF",
              linewidth = 1
            ) +
            geom_point(
              color = "#20B8D9"
            ) +
            labs(
              title = paste("Line Plot:", var1, "vs", var2),
              x = var1,
              y = var2
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "regression_scatter") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[var1]],
              y = .data[[var2]]
            )
          ) +
            geom_point(
              color = "#2688FF",
              alpha = 0.6
            ) +
            geom_smooth(
              method = "lm",
              se = TRUE,
              color = "#FF6B6B"
            ) +
            labs(
              title = paste("Regression Scatter:", var1, "vs", var2),
              x = var1,
              y = var2
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        }
        
        
        # KATEGORIK + KATEGORIK
        
      } else if (jumlah_kategorik == 2) {
        
        tabel <- as.data.frame(
          table(
            hasil_data[[var1]],
            hasil_data[[var2]]
          )
        )
        
        names(tabel) <- c("Kategori1", "Kategori2", "Frekuensi")
        
        if (jenis_grafik == "grouped_bar") {
          
          p <- ggplot(
            tabel,
            aes(
              x = Kategori1,
              y = Frekuensi,
              fill = Kategori2
            )
          ) +
            geom_col(position = "dodge") +
            labs(
              title = paste("Grouped Bar Chart:", var1, "dan", var2),
              x = var1,
              y = "Frequency",
              fill = var2
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "stacked_bar") {
          
          p <- ggplot(
            tabel,
            aes(
              x = Kategori1,
              y = Frekuensi,
              fill = Kategori2
            )
          ) +
            geom_col(position = "stack") +
            labs(
              title = paste("Stacked Bar Chart:", var1, "dan", var2),
              x = var1,
              y = "Frequency",
              fill = var2
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "heatmap") {
          
          p <- ggplot(
            tabel,
            aes(
              x = Kategori1,
              y = Kategori2,
              fill = Frekuensi
            )
          ) +
            geom_tile(color = "white") +
            scale_fill_gradient(
              low = "#D6E9FF",
              high = "#1261A0"
            ) +
            labs(
              title = paste("Heatmap:", var1, "dan", var2),
              x = var1,
              y = var2,
              fill = "Frequency"
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        }
        
        
        # NUMERIK + KATEGORIK
        
      } else {
        
        if (tipe1 == "numerik") {
          var_numerik <- var1
          var_kategorik <- var2
        } else {
          var_numerik <- var2
          var_kategorik <- var1
        }
        
        if (jenis_grafik == "boxplot_group") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[var_kategorik]],
              y = .data[[var_numerik]],
              fill = .data[[var_kategorik]]
            )
          ) +
            geom_boxplot(alpha = 0.8) +
            labs(
              title = paste("Boxplot:", var_numerik, "by", var_kategorik),
              x = var_kategorik,
              y = var_numerik
            ) +
            theme_minimal() +
            theme(legend.position = "none")
          
          ggplotly(p)
          
        } else if (jenis_grafik == "violin") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[var_kategorik]],
              y = .data[[var_numerik]],
              fill = .data[[var_kategorik]]
            )
          ) +
            geom_violin(trim = FALSE, alpha = 0.8) +
            labs(
              title = paste("Violin Plot:", var_numerik, "by", var_kategorik),
              x = var_kategorik,
              y = var_numerik
            ) +
            theme_minimal() +
            theme(legend.position = "none")
          
          ggplotly(p)
          
        } else if (jenis_grafik == "mean_plot") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[var_kategorik]],
              y = .data[[var_numerik]],
              fill = .data[[var_kategorik]]
            )
          ) +
            stat_summary(
              fun = mean,
              geom = "col",
              width = 0.6
            ) +
            labs(
              title = paste("Mean Plot:", var_numerik, "by", var_kategorik),
              x = var_kategorik,
              y = paste("Mean", var_numerik)
            ) +
            theme_minimal() +
            theme(legend.position = "none")
          
          ggplotly(p)
          
        }
        
      }
      
      
      # --------------------------------------------------------
      # MULTIVARIAT
      # --------------------------------------------------------
      
    } else if (input$viz_analysis == "Multivariat") {
      
      var1 <- variabel[1]
      var2 <- variabel[2]
      var3 <- variabel[3]
      
      
      # TIGA VARIABEL NUMERIK
      
      if (jumlah_numerik == 3) {
        
        if (jenis_grafik == "scatter3d") {
          
          plot_ly(
            data = hasil_data,
            x = hasil_data[[var1]],
            y = hasil_data[[var2]],
            z = hasil_data[[var3]],
            type = "scatter3d",
            mode = "markers",
            marker = list(
              size = 4,
              color = "#2688FF",
              opacity = 0.7
            )
          ) %>%
            layout(
              title = paste("3D Scatter Plot:", var1, var2, var3),
              scene = list(
                xaxis = list(title = var1),
                yaxis = list(title = var2),
                zaxis = list(title = var3)
              )
            )
          
        } else if (jenis_grafik == "scatter_matrix") {
          
          plot_ly(
            type = "splom",
            dimensions = list(
              list(label = var1, values = hasil_data[[var1]]),
              list(label = var2, values = hasil_data[[var2]]),
              list(label = var3, values = hasil_data[[var3]])
            ),
            marker = list(
              color = "#2688FF",
              size = 4,
              opacity = 0.6
            )
          ) %>%
            layout(
              title = "Scatter Matrix"
            )
          
        } else if (jenis_grafik == "parallel") {
          
          plot_ly(
            type = "parcoords",
            dimensions = list(
              list(label = var1, values = hasil_data[[var1]]),
              list(label = var2, values = hasil_data[[var2]]),
              list(label = var3, values = hasil_data[[var3]])
            )
          ) %>%
            layout(
              title = "Parallel Coordinates Plot"
            )
          
        }
        
        
        # DUA NUMERIK + SATU KATEGORIK
        
      } else if (jumlah_numerik == 2) {
        
        var_numerik <- variabel[numerik %in% variabel]
        var_kategorik <- variabel[kategorik %in% variabel]
        
        xvar <- var_numerik[1]
        yvar <- var_numerik[2]
        groupvar <- var_kategorik[1]
        
        if (jenis_grafik == "grouped_scatter") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[xvar]],
              y = .data[[yvar]],
              color = .data[[groupvar]]
            )
          ) +
            geom_point(
              alpha = 0.7,
              size = 2
            ) +
            labs(
              title = "Grouped Scatter Plot",
              x = xvar,
              y = yvar,
              color = groupvar
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "faceted_scatter") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[xvar]],
              y = .data[[yvar]]
            )
          ) +
            geom_point(
              color = "#2688FF",
              alpha = 0.7
            ) +
            facet_wrap(as.formula(paste("~", groupvar))) +
            labs(
              title = "Faceted Scatter Plot",
              x = xvar,
              y = yvar
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "regression_by_group") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[xvar]],
              y = .data[[yvar]],
              color = .data[[groupvar]]
            )
          ) +
            geom_point(alpha = 0.5) +
            geom_smooth(
              method = "lm",
              se = FALSE
            ) +
            labs(
              title = "Regression by Group",
              x = xvar,
              y = yvar,
              color = groupvar
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        }
        
        
        # SATU NUMERIK + DUA KATEGORIK
        
      } else if (jumlah_numerik == 1) {
        
        var_numerik <- variabel[numerik %in% variabel][1]
        var_kategorik <- variabel[kategorik %in% variabel]
        
        if (jenis_grafik == "grouped_boxplot") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[var_kategorik[1]]],
              y = .data[[var_numerik]],
              fill = .data[[var_kategorik[2]]]
            )
          ) +
            geom_boxplot() +
            labs(
              title = "Grouped Boxplot",
              x = var_kategorik[1],
              y = var_numerik,
              fill = var_kategorik[2]
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "grouped_violin") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[var_kategorik[1]]],
              y = .data[[var_numerik]],
              fill = .data[[var_kategorik[2]]]
            )
          ) +
            geom_violin(
              position = position_dodge(0.8),
              trim = FALSE
            ) +
            labs(
              title = "Grouped Violin Plot",
              x = var_kategorik[1],
              y = var_numerik,
              fill = var_kategorik[2]
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "mean_plot") {
          
          p <- ggplot(
            hasil_data,
            aes(
              x = .data[[var_kategorik[1]]],
              y = .data[[var_numerik]],
              fill = .data[[var_kategorik[2]]]
            )
          ) +
            stat_summary(
              fun = mean,
              geom = "col",
              position = "dodge"
            ) +
            labs(
              title = "Multivariate Mean Plot",
              x = var_kategorik[1],
              y = paste("Mean", var_numerik),
              fill = var_kategorik[2]
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        }
        
        
        # TIGA VARIABEL KATEGORIK
        
      } else {
        
        tabel <- as.data.frame(
          table(
            hasil_data[[var1]],
            hasil_data[[var2]],
            hasil_data[[var3]]
          )
        )
        
        names(tabel) <- c(
          "Kategori1",
          "Kategori2",
          "Kategori3",
          "Frekuensi"
        )
        
        if (jenis_grafik == "grouped_bar") {
          
          p <- ggplot(
            tabel,
            aes(
              x = Kategori1,
              y = Frekuensi,
              fill = Kategori2
            )
          ) +
            geom_col(position = "dodge") +
            facet_wrap(~Kategori3) +
            labs(
              title = "Multivariate Grouped Bar Chart",
              x = var1,
              y = "Frequency",
              fill = var2
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "stacked_bar") {
          
          p <- ggplot(
            tabel,
            aes(
              x = Kategori1,
              y = Frekuensi,
              fill = Kategori2
            )
          ) +
            geom_col(position = "stack") +
            facet_wrap(~Kategori3) +
            labs(
              title = "Multivariate Stacked Bar Chart",
              x = var1,
              y = "Frequency",
              fill = var2
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        } else if (jenis_grafik == "heatmap") {
          
          p <- ggplot(
            tabel,
            aes(
              x = Kategori1,
              y = Kategori2,
              fill = Frekuensi
            )
          ) +
            geom_tile(color = "white") +
            facet_wrap(~Kategori3) +
            scale_fill_gradient(
              low = "#D6E9FF",
              high = "#1261A0"
            ) +
            labs(
              title = "Multivariate Heatmap",
              x = var1,
              y = var2,
              fill = "Frequency"
            ) +
            theme_minimal()
          
          ggplotly(p)
          
        }
        
      }
      
    }
    
  })
  
  # ----------------------------------------------------------
  # VISUALIZATION - BIVARIATE CHART ENGINE
  # ----------------------------------------------------------
  

  
  output$univariate_plot <- renderPlotly({
    
    req(input$viz_analysis == "Univariat")
    req(input$viz_variables)
    
    variabel <- input$viz_variables
    hasil_data <- data
    
    # NUMERICAL VARIABLE
    if (variabel %in% c("age", "bmi", "children", "charges")) {
      
      if (input$viz_type == "Numerik") {
        
        p <- ggplot(hasil_data, aes(x = .data[[variabel]])) +
          geom_histogram(
            bins = 30,
            fill = "#2688FF",
            color = "white",
            alpha = 0.85
          ) +
          labs(
            title = paste("Distribution of", variabel),
            x = variabel,
            y = "Frequency"
          ) +
          theme_minimal()
        
      }
      
      # CATEGORICAL VARIABLE
    } else {
      
      p <- ggplot(
        hasil_data,
        aes(x = .data[[variabel]])
      ) +
        geom_bar(
          fill = "#20B8D9",
          color = "white"
        ) +
        labs(
          title = paste("Distribution of", variabel),
          x = variabel,
          y = "Frequency"
        ) +
        theme_minimal()
      
    }
    
    ggplotly(p)
    
  })
  
  
  
  # ==========================================================
  # REGRESSION ANALYSIS - PAGE NAVIGATION
  # ==========================================================
  
  reg_page <- reactiveVal("home")
  
  
  # ----------------------------------------------------------
  # NAVIGATION BUTTONS
  # ----------------------------------------------------------
  
  observeEvent(input$reg_back, {
    reg_page("home")
  })
  
  observeEvent(input$reg_assumption, {
    reg_page("assumption")
  })
  
  observeEvent(input$reg_model, {
    reg_page("model")
  })
  
  observeEvent(input$reg_f, {
    reg_page("f_test")
  })
  
  observeEvent(input$reg_t, {
    reg_page("t_test")
  })
  
  observeEvent(input$reg_r2, {
    reg_page("r_squared")
  })
  
  observeEvent(input$reg_interpretation, {
    reg_page("interpretation")
  })
  
  
  
  # ==========================================================
  # REGRESSION ANALYSIS - COMPLETE SERVER CODE
  # ==========================================================
  
  # ----------------------------------------------------------
  # 1. NAVIGATION
  # ----------------------------------------------------------
  
  reg_page <- reactiveVal("home")
  
  observeEvent(input$reg_back, {
    reg_page("home")
  })
  
  observeEvent(input$reg_assumption, {
    reg_page("assumption")
  })
  
  observeEvent(input$reg_model, {
    reg_page("model")
  })
  
  observeEvent(input$reg_f, {
    reg_page("f_test")
  })
  
  observeEvent(input$reg_t, {
    reg_page("t_test")
  })
  
  observeEvent(input$reg_r2, {
    reg_page("r_squared")
  })
  
  observeEvent(input$reg_interpretation, {
    reg_page("interpretation")
  })
  
  
  # ----------------------------------------------------------
  # 2. REGRESSION MODEL
  # ----------------------------------------------------------
  
  reg_model <- reactive({
    
    lm(
      charges ~ age + sex + bmi + children + smoker + region,
      data = data
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 3. CLASSICAL ASSUMPTION TEST CALCULATIONS
  # ----------------------------------------------------------
  
  reg_assumption_results <- reactive({
    
    model_hasil <- reg_model()
    residual <- residuals(model_hasil)
    
    # Normality Test
    normality <- shapiro.test(residual)
    
    # Multicollinearity Test
    vif_hasil <- car::vif(model_hasil)
    
    if (is.matrix(vif_hasil)) {
      vif_value <- vif_hasil[, "GVIF^(1/(2*Df))"]
    } else {
      vif_value <- vif_hasil
    }
    
    # Heteroskedasticity Test
    bp_hasil <- lmtest::bptest(model_hasil)
    
    # Autocorrelation Test
    dw_hasil <- lmtest::dwtest(model_hasil)
    
    list(
      normality = normality,
      vif = vif_value,
      vif_max = max(vif_value),
      bp = bp_hasil,
      dw = dw_hasil
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 4. MAIN REGRESSION PAGE AND DETAIL PAGES
  # ----------------------------------------------------------
  
  output$regression_page <- renderUI({
    
    halaman <- reg_page()
    
    if (halaman == "home") {
      
      tagList(
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h2(
              "Regression Analysis",
              style = "font-weight:700;"
            ),
            
            p(
              "Explore, evaluate, and interpret your multiple linear regression model.",
              style = "font-size:15px; color:#AAB8D0;"
            ),
            
            tags$hr(),
            
            p(
              "Select one of the analysis categories below to view detailed results and statistical interpretations."
            )
          )
        ),
        
        fluidRow(
          
          column(
            width = 6,
            
            box(
              width = 12,
              height = 230,
              status = "primary",
              
              icon("shield-alt", class = "fa-2x",
                   style = "color:#55B8FF;"),
              
              h4("Classical Assumption Tests"),
              
              p(
                "Normality, multicollinearity, heteroskedasticity, and autocorrelation.",
                style = "min-height:48px;"
              ),
              
              actionButton(
                "reg_assumption",
                "Explore Results",
                icon = icon("arrow-right"),
                class = "btn-primary btn-block"
              )
            )
          ),
          
          column(
            width = 6,
            
            box(
              width = 12,
              height = 230,
              status = "primary",
              
              icon("calculator", class = "fa-2x",
                   style = "color:#55B8FF;"),
              
              h4("Regression Model"),
              
              p(
                "Regression equation, estimated coefficients, and model summary.",
                style = "min-height:48px;"
              ),
              
              actionButton(
                "reg_model",
                "Explore Results",
                icon = icon("arrow-right"),
                class = "btn-primary btn-block"
              )
            )
          )
          
        ),
        
        fluidRow(
          
          column(
            width = 6,
            
            box(
              width = 12,
              height = 230,
              status = "primary",
              
              icon("chart-line", class = "fa-2x",
                   style = "color:#55B8FF;"),
              
              h4("Simultaneous Significance Test"),
              
              p(
                "Evaluate the simultaneous significance of independent variables using the F-test.",
                style = "min-height:48px;"
              ),
              
              actionButton(
                "reg_f",
                "Explore Results",
                icon = icon("arrow-right"),
                class = "btn-primary btn-block"
              )
            )
          ),
          
          column(
            width = 6,
            
            box(
              width = 12,
              height = 230,
              status = "primary",
              
              icon("chart-bar", class = "fa-2x",
                   style = "color:#55B8FF;"),
              
              h4("Partial Significance Test"),
              
              p(
                "Evaluate the individual significance of each independent variable using the t-test.",
                style = "min-height:48px;"
              ),
              
              actionButton(
                "reg_t",
                "Explore Results",
                icon = icon("arrow-right"),
                class = "btn-primary btn-block"
              )
            )
          )
          
        ),
        
        fluidRow(
          
          column(
            width = 6,
            
            box(
              width = 12,
              height = 230,
              status = "primary",
              
              icon("percent", class = "fa-2x",
                   style = "color:#55B8FF;"),
              
              h4("Coefficient of Determination"),
              
              p(
                "Explore R-Squared and Adjusted R-Squared to evaluate model explanatory power.",
                style = "min-height:48px;"
              ),
              
              actionButton(
                "reg_r2",
                "Explore Results",
                icon = icon("arrow-right"),
                class = "btn-primary btn-block"
              )
            )
          ),
          
          column(
            width = 6,
            
            box(
              width = 12,
              height = 230,
              status = "primary",
              
              icon("file-alt", class = "fa-2x",
                   style = "color:#55B8FF;"),
              
              h4("Overall Interpretation"),
              
              p(
                "View the complete interpretation and conclusions of the regression analysis.",
                style = "min-height:48px;"
              ),
              
              actionButton(
                "reg_interpretation",
                "Explore Results",
                icon = icon("arrow-right"),
                class = "btn-primary btn-block"
              )
            )
          )
          
        )
        
      )
      
      
    } else if (halaman == "assumption") {
      
      tagList(
        
        actionButton(
          "reg_back",
          "Back to Regression Analysis",
          icon = icon("arrow-left"),
          class = "btn-default"
        ),
        
        br(),
        br(),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h2("Classical Assumption Tests"),
            
            p(
              "Evaluation of the four classical assumptions of the multiple linear regression model."
            )
          )
        ),
        
        uiOutput("reg_assumption_dashboard")
        
      )
      
      
    } else if (halaman == "model") {
      
      tagList(
        
        actionButton(
          "reg_back",
          "Back to Regression Analysis",
          icon = icon("arrow-left"),
          class = "btn-default"
        ),
        
        br(),
        br(),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h2("Regression Model"),
            
            p("Estimated regression equation and model coefficients.")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            uiOutput("reg_model_summary")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h4("Regression Equation"),
            
            verbatimTextOutput("reg_formula")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h4("Regression Coefficients"),
            
            DTOutput("reg_coefficient_table")
          )
        )
        
      )
      
      
    } else if (halaman == "f_test") {
      
      tagList(
        
        actionButton(
          "reg_back",
          "Back to Regression Analysis",
          icon = icon("arrow-left"),
          class = "btn-default"
        ),
        
        br(),
        br(),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h2("Simultaneous Significance Test"),
            
            p(
              "F-test for evaluating the simultaneous effect of independent variables."
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            uiOutput("reg_f_test")
          )
        )
        
      )
      
      
    } else if (halaman == "t_test") {
      
      tagList(
        
        actionButton(
          "reg_back",
          "Back to Regression Analysis",
          icon = icon("arrow-left"),
          class = "btn-default"
        ),
        
        br(),
        br(),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h2("Partial Significance Test"),
            
            p(
              "T-test for evaluating the individual effect of each independent variable."
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            DTOutput("reg_t_test_table"),
            
            hr(),
            
            h4("Statistical Interpretation"),
            
            uiOutput("reg_t_interpretation")
          )
        )
        
      )
      
      
    } else if (halaman == "r_squared") {
      
      tagList(
        
        actionButton(
          "reg_back",
          "Back to Regression Analysis",
          icon = icon("arrow-left"),
          class = "btn-default"
        ),
        
        br(),
        br(),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h2("Coefficient of Determination"),
            
            p(
              "Evaluation of the explanatory power of the regression model."
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            uiOutput("reg_r_squared")
          )
        )
        
      )
      
      
    } else if (halaman == "interpretation") {
      
      tagList(
        
        actionButton(
          "reg_back",
          "Back to Regression Analysis",
          icon = icon("arrow-left"),
          class = "btn-default"
        ),
        
        br(),
        br(),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h2("Overall Regression Interpretation"),
            
            p(
              "Comprehensive interpretation of the multiple linear regression analysis."
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            uiOutput("reg_final_interpretation")
          )
        )
        
      )
      
    }
    
  })
  
  
  # ----------------------------------------------------------
  # 5. REGRESSION MODEL SUMMARY
  # ----------------------------------------------------------
  
  output$reg_model_summary <- renderUI({
    
    model <- reg_model()
    hasil <- summary(model)
    
    tagList(
      
      h4("Model Summary"),
      
      fluidRow(
        
        valueBox(
          round(hasil$r.squared, 4),
          "R-Squared",
          icon = icon("chart-line"),
          color = "blue",
          width = 4
        ),
        
        valueBox(
          round(hasil$adj.r.squared, 4),
          "Adjusted R-Squared",
          icon = icon("percent"),
          color = "aqua",
          width = 4
        ),
        
        valueBox(
          round(sqrt(mean(residuals(model)^2)), 2),
          "RMSE",
          icon = icon("calculator"),
          color = "green",
          width = 4
        )
        
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 6. REGRESSION EQUATION
  # ----------------------------------------------------------
  
  output$reg_formula <- renderText({
    
    model <- reg_model()
    koef <- coef(model)
    
    persamaan <- paste0(
      "charges = ",
      paste(
        paste0(
          ifelse(koef[-1] >= 0, "+ ", "- "),
          round(abs(koef[-1]), 4),
          "(", names(koef)[-1], ")"
        ),
        collapse = " "
      )
    )
    
    paste0(
      "charges = ",
      round(koef[1], 4),
      " ",
      persamaan
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 7. REGRESSION COEFFICIENT TABLE
  # ----------------------------------------------------------
  
  output$reg_coefficient_table <- DT::renderDT({
    
    hasil <- summary(reg_model())$coefficients
    
    tabel <- data.frame(
      Variable = rownames(hasil),
      Estimate = hasil[, 1],
      Std_Error = hasil[, 2],
      t_value = hasil[, 3],
      p_value = hasil[, 4],
      row.names = NULL
    )
    
    tabel[, 2:5] <- lapply(tabel[, 2:5], round, 4)
    
    DT::datatable(
      tabel,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 8. SIMULTANEOUS SIGNIFICANCE TEST (F-TEST)
  # ----------------------------------------------------------
  
  output$reg_f_test <- renderUI({
    
    hasil <- summary(reg_model())
    fstat <- hasil$fstatistic
    
    f_value <- unname(fstat[1])
    df1 <- unname(fstat[2])
    df2 <- unname(fstat[3])
    
    p_value <- pf(
      f_value,
      df1,
      df2,
      lower.tail = FALSE
    )
    
    keputusan <- if (p_value < 0.05) {
      "H0 ditolak. Variabel independen secara simultan berpengaruh signifikan terhadap charges."
    } else {
      "H0 tidak ditolak. Belum terdapat bukti bahwa variabel independen secara simultan berpengaruh signifikan terhadap charges."
    }
    
    tagList(
      
      h3("F-Test Result"),
      
      h4(paste("F Statistic:", round(f_value, 4))),
      
      h4(paste("Degrees of Freedom:", df1, "and", df2)),
      
      h4(
        paste(
          "P-value:",
          format.pval(p_value, digits = 4)
        )
      ),
      
      tags$hr(),
      
      h4("Decision"),
      
      p(keputusan),
      
      p("Significance level: 0.05")
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 9. PARTIAL SIGNIFICANCE TEST (T-TEST)
  # ----------------------------------------------------------
  
  output$reg_t_test_table <- DT::renderDT({
    
    hasil <- summary(reg_model())$coefficients
    
    tabel <- data.frame(
      Variable = rownames(hasil),
      Estimate = hasil[, 1],
      Std_Error = hasil[, 2],
      t_value = hasil[, 3],
      p_value = hasil[, 4],
      Decision = ifelse(
        hasil[, 4] < 0.05,
        "Significant",
        "Not Significant"
      ),
      row.names = NULL
    )
    
    tabel[, 2:5] <- lapply(tabel[, 2:5], round, 4)
    
    DT::datatable(
      tabel,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 10. T-TEST INTERPRETATION
  # ----------------------------------------------------------
  
  output$reg_t_interpretation <- renderUI({
    
    hasil <- summary(reg_model())$coefficients
    
    hasil <- hasil[-1, , drop = FALSE]
    
    interpretasi <- lapply(
      seq_len(nrow(hasil)),
      function(i) {
        
        nama <- rownames(hasil)[i]
        p <- hasil[i, 4]
        
        keputusan <- if (p < 0.05) {
          "berpengaruh signifikan"
        } else {
          "tidak berpengaruh signifikan"
        }
        
        p(
          paste0(
            nama,
            " memiliki p-value ",
            format.pval(p, digits = 4),
            ", sehingga variabel tersebut ",
            keputusan,
            " terhadap charges."
          )
        )
        
      }
    )
    
    do.call(tagList, interpretasi)
    
  })
  
  
  # ----------------------------------------------------------
  # 11. COEFFICIENT OF DETERMINATION (R-SQUARED)
  # ----------------------------------------------------------
  
  output$reg_r_squared <- renderUI({
    
    hasil <- summary(reg_model())
    
    r2 <- hasil$r.squared
    adj_r2 <- hasil$adj.r.squared
    
    tagList(
      
      h3("Coefficient of Determination"),
      
      fluidRow(
        
        valueBox(
          paste0(round(r2 * 100, 2), "%"),
          "R-Squared",
          icon = icon("percent"),
          color = "blue",
          width = 6
        ),
        
        valueBox(
          paste0(round(adj_r2 * 100, 2), "%"),
          "Adjusted R-Squared",
          icon = icon("chart-line"),
          color = "aqua",
          width = 6
        )
        
      ),
      
      h4("Interpretasi"),
      
      p(
        paste0(
          "Model regresi mampu menjelaskan sebesar ",
          round(r2 * 100, 2),
          "% variasi biaya medis. Sementara itu, sisanya sebesar ",
          round((1 - r2) * 100, 2),
          "% dijelaskan oleh faktor-faktor lain di luar model."
        )
      ),
      
      p(
        paste0(
          "Nilai Adjusted R-Squared yang diperoleh adalah sebesar ",
          round(adj_r2 * 100, 2),
          "%."
        )
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 12. OVERALL REGRESSION INTERPRETATION
  # ----------------------------------------------------------
  
  output$reg_final_interpretation <- renderUI({
    
    model <- reg_model()
    hasil <- summary(model)
    
    fstat <- hasil$fstatistic
    
    f_value <- unname(fstat[1])
    df1 <- unname(fstat[2])
    df2 <- unname(fstat[3])
    
    p_f <- pf(
      f_value,
      df1,
      df2,
      lower.tail = FALSE
    )
    
    r2 <- hasil$r.squared
    adj_r2 <- hasil$adj.r.squared
    
    keputusan_f <- if (p_f < 0.05) {
      "Model regresi signifikan secara simultan."
    } else {
      "Model regresi belum terbukti signifikan secara simultan."
    }
    
    koef <- hasil$coefficients
    koef <- koef[-1, , drop = FALSE]
    
    interpretasi_variabel <- lapply(
      seq_len(nrow(koef)),
      function(i) {
        
        nama <- rownames(koef)[i]
        
        keputusan <- if (koef[i, 4] < 0.05) {
          "signifikan"
        } else {
          "tidak signifikan"
        }
        
        p(
          paste0(
            nama,
            ": koefisien sebesar ",
            round(koef[i, 1], 4),
            " dengan p-value ",
            format.pval(koef[i, 4], digits = 4),
            ". Variabel ini ",
            keputusan,
            " secara parsial."
          )
        )
        
      }
    )
    
    tagList(
      
      h4("1. Simultaneous Significance"),
      
      p(
        paste0(
          "F-statistic = ",
          round(f_value, 4),
          " dengan p-value ",
          format.pval(p_f, digits = 4),
          ". ",
          keputusan_f
        )
      ),
      
      tags$hr(),
      
      h4("2. Partial Significance"),
      
      do.call(tagList, interpretasi_variabel),
      
      tags$hr(),
      
      h4("3. Coefficient of Determination"),
      
      p(
        paste0(
          "R-Squared sebesar ",
          round(r2 * 100, 2),
          "% menunjukkan bahwa model mampu menjelaskan ",
          round(r2 * 100, 2),
          "% variasi charges."
        )
      ),
      
      p(
        paste0(
          "Adjusted R-Squared sebesar ",
          round(adj_r2 * 100, 2),
          "%."
        )
      ),
      
      tags$hr(),
      
      h4("4. Overall Conclusion"),
      
      p(
        paste0(
          "Berdasarkan hasil analisis regresi linear berganda, ",
          keputusan_f,
          " Nilai R-Squared menunjukkan proporsi variasi charges yang dapat dijelaskan oleh variabel independen dalam model."
        )
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 13. CLASSICAL ASSUMPTION TEST DASHBOARD
  # ----------------------------------------------------------
  
  output$reg_assumption_dashboard <- renderUI({
    
    hasil <- reg_assumption_results()
    
    # NORMALITY
    p_normal <- hasil$normality$p.value
    
    normal_status <- if (p_normal > 0.05) {
      "Memenuhi asumsi normalitas"
    } else {
      "Tidak memenuhi asumsi normalitas"
    }
    
    normal_decision <- if (p_normal > 0.05) {
      "Residual tidak menunjukkan penyimpangan yang signifikan dari distribusi normal."
    } else {
      "Residual menunjukkan penyimpangan yang signifikan dari distribusi normal."
    }
    
    
    # MULTICOLLINEARITY
    vif_status <- if (hasil$vif_max < 5) {
      "Tidak terindikasi multikolinearitas kuat"
    } else {
      "Terindikasi multikolinearitas"
    }
    
    vif_table <- data.frame(
      Variable = names(hasil$vif),
      VIF = round(as.numeric(hasil$vif), 4)
    )
    
    
    # HETEROSKEDASTICITY
    p_bp <- hasil$bp$p.value
    
    bp_status <- if (p_bp > 0.05) {
      "Memenuhi asumsi homoskedastisitas"
    } else {
      "Terindikasi heteroskedastisitas"
    }
    
    bp_decision <- if (p_bp > 0.05) {
      "Tidak terdapat bukti yang cukup mengenai heteroskedastisitas."
    } else {
      "Terdapat indikasi heteroskedastisitas pada model."
    }
    
    
    # AUTOCORRELATION
    p_dw <- hasil$dw$p.value
    
    dw_status <- if (p_dw > 0.05) {
      "Tidak terindikasi autokorelasi positif"
    } else {
      "Terindikasi autokorelasi positif"
    }
    
    dw_decision <- if (p_dw > 0.05) {
      "Tidak terdapat bukti yang cukup mengenai autokorelasi positif."
    } else {
      "Terdapat indikasi autokorelasi positif."
    }
    
    
    tagList(
      
      fluidRow(
        
        box(
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          
          h4("1. Normality Test"),
          
          p("Method: Shapiro-Wilk Test"),
          
          h3(round(
            as.numeric(hasil$normality$statistic), 4
          )),
          
          p("Shapiro-Wilk Statistic"),
          
          h4(
            paste(
              "P-value:",
              format.pval(p_normal, digits = 4)
            )
          ),
          
          tags$hr(),
          
          strong(normal_status),
          
          p(normal_decision)
        ),
        
        box(
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          
          h4("2. Multicollinearity Test"),
          
          p("Method: Variance Inflation Factor (VIF)"),
          
          h3(round(hasil$vif_max, 4)),
          
          p("Maximum VIF"),
          
          DTOutput("reg_vif_table"),
          
          tags$hr(),
          
          strong(vif_status),
          
          p(
            "Interpretation is based on a VIF threshold of 5."
          )
        )
        
      ),
      
      fluidRow(
        
        box(
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          
          h4("3. Heteroskedasticity Test"),
          
          p("Method: Breusch-Pagan Test"),
          
          h3(round(
            as.numeric(hasil$bp$statistic), 4
          )),
          
          p("Breusch-Pagan Statistic"),
          
          h4(
            paste(
              "P-value:",
              format.pval(p_bp, digits = 4)
            )
          ),
          
          tags$hr(),
          
          strong(bp_status),
          
          p(bp_decision)
        ),
        
        box(
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          
          h4("4. Autocorrelation Test"),
          
          p("Method: Durbin-Watson Test"),
          
          h3(round(
            as.numeric(hasil$dw$statistic), 4
          )),
          
          p("Durbin-Watson Statistic"),
          
          h4(
            paste(
              "P-value:",
              format.pval(p_dw, digits = 4)
            )
          ),
          
          tags$hr(),
          
          strong(dw_status),
          
          p(dw_decision),
          
          p(
            "Note: Durbin-Watson is primarily designed for ordered observations. Since this insurance dataset is cross-sectional, interpret this test cautiously."
          )
        )
        
      ),
      
      fluidRow(
        box(
          width = 12,
          status = "info",
          solidHeader = TRUE,
          
          h4("Statistical Notes"),
          
          p(
            "Taraf signifikansi yang digunakan dalam pengujian adalah 0,05. Pada uji Shapiro-Wilk dan Breusch-Pagan, nilai p-value yang lebih besar dari 0,05 menunjukkan bahwa belum terdapat cukup bukti untuk menolak hipotesis nol (H0)."
          ),
          
          p(
            "Pada uji VIF, nilai di bawah 5 digunakan sebagai indikasi bahwa tidak terdapat multikolinearitas yang kuat."
          )
          
        )
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 14. VIF TABLE
  # ----------------------------------------------------------
  
  output$reg_vif_table <- DT::renderDT({
    
    hasil <- reg_assumption_results()
    
    tabel <- data.frame(
      Variable = names(hasil$vif),
      VIF = round(as.numeric(hasil$vif), 4)
    )
    
    DT::datatable(
      tabel,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 10,
        ordering = FALSE
      )
    )
    
  })
  
  
  # ----------------------------------------------------------
  # PREDICTION
  # ----------------------------------------------------------
  
  hasil_prediksi <- eventReactive(
    input$predict_button, {
      
      new_data <- data.frame(
        age = input$age_input,
        
        sex = factor(
          input$sex_input,
          levels = levels(data$sex)
        ),
        
        bmi = input$bmi_input,
        
        children = input$children_input,
        
        smoker = factor(
          input$smoker_input,
          levels = levels(data$smoker)
        ),
        
        region = factor(
          input$region_input,
          levels = levels(data$region)
        )
      )
      
      predict(model, newdata = new_data)
    }
  )
  
  output$prediction_result <- renderUI({
    
    req(hasil_prediksi())
    
    div(
      style = "
        background:#17294D;
        padding:35px;
        border-radius:15px;
        text-align:center;
        border:1px solid #3478C8;
      ",
      
      h3(
        "ESTIMATED MEDICAL COST",
        style = "color:#80BFFF;"
      ),
      
      h1(
        paste(
          format(
            round(hasil_prediksi(), 2),
            big.mark = ","
          )
        ),
        style = "
          color:#FFFFFF;
          font-size:35px;
          font-weight:bold;
        "
      ),
      
      p(
        "Estimated value based on the regression model",
        style = "color:#A8C5ED;"
      )
    )
  })
  # ==========================================================
  # INTERPRETASI KELEMAHAN MODEL
  # ==========================================================
  
  output$limitation_assumption <- renderUI({
    
    hasil <- reg_assumption_results()
    
    # NORMALITAS
    normal_text <- if (hasil$normality$p.value > 0.05) {
      "Uji normalitas terpenuhi karena p-value lebih besar dari 0,05."
    } else {
      "Uji normalitas tidak terpenuhi karena p-value lebih kecil atau sama dengan 0,05."
    }
    
    # MULTIKOLINEARITAS
    vif_text <- if (hasil$vif_max < 5) {
      "Tidak ditemukan indikasi multikolinearitas yang kuat berdasarkan batas VIF sebesar 5."
    } else {
      "Terdapat indikasi multikolinearitas karena nilai VIF maksimum mencapai atau melebihi 5."
    }
    
    # HETEROSKEDASTISITAS
    bp_text <- if (hasil$bp$p.value > 0.05) {
      "Uji heteroskedastisitas terpenuhi karena belum terdapat bukti heteroskedastisitas."
    } else {
      "Uji heteroskedastisitas tidak terpenuhi karena terdapat indikasi heteroskedastisitas."
    }
    
    # AUTOKORELASI
    dw_text <- if (hasil$dw$p.value > 0.05) {
      "Uji Durbin-Watson tidak menunjukkan bukti autokorelasi positif yang signifikan."
    } else {
      "Uji Durbin-Watson menunjukkan indikasi autokorelasi positif."
    }
    
    tagList(
      
      h4("Hasil Evaluasi Asumsi Klasik"),
      
      tags$ul(
        tags$li(normal_text),
        tags$li(vif_text),
        tags$li(bp_text),
        tags$li(dw_text)
      ),
      
      tags$hr(),
      
      h4("Dampak terhadap Analisis"),
      
      p(
        "Ketidakterpenuhan asumsi klasik tertentu dapat memengaruhi keandalan hasil estimasi maupun pengujian statistik. Oleh karena itu, hasil analisis perlu ditafsirkan secara hati-hati dan tidak langsung dianggap memenuhi seluruh persyaratan model regresi."
      )
      
    )
    
  })
}


# ============================================================
# 4. MENJALANKAN APLIKASI
# ============================================================

shinyApp(ui = ui, server = server)

