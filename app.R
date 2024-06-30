  # Set Directory
  options(repos = c(CRAN = "https://cloud.r-project.org"))


  getwd()
  # Set Package and Library
  library(shiny)
  library(argonR)
  library(argonDash)
  library(htmltools)
  library(magrittr)
  library(DT)
  library(shinydashboard)
  library(tidyverse)
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(shinydashboardPlus)
  library(dashboardthemes)
  library(reticulate)
  library(nnet)
  library(caret)
  library(e1071)
  
  
  # Load the models
  rf_model_path <- "rf_model.joblib"
  svm_model_path <- "svm_model.joblib"
  nn_model_path <- "nn_model.joblib"
  # JOBLIB
  joblib <- import("joblib")
  rf_model <- joblib$load(rf_model_path)
  svm_model <- joblib$load(svm_model_path)
  nn_model <- joblib$load(nn_model_path)
  
  # Load the scaler parameters from CSV
  scaler_params <- read.csv('scaler_params.csv')
  
  # Function to apply the scaler
  scale_input <- function(input_matrix, scaler_params) {
    scale(input_matrix, center = scaler_params$mean, scale = scaler_params$scale)
  }
  
  # Load Datasets
  datautama <- read.csv("breast-cancer.csv")
  head(datautama)
  datautama$concave.points_worst
  du <- datautama[c("radius_worst","area_worst","concave.points_worst","diagnosis")]
  
  # Function UI
  #=================================================================================================Statitics Descriptive
  cards_tab <- argonTabItem(
    tabName = "stdesk",
    
    # classic cards
    argonRow(
      argonH1("STATISTICS DESCRIPTIVE", display = 4),
      center = TRUE
    ),
    
    argonRow(
      argonCard(
        width = 12,
        src = NULL,
        icon = icon("cogs"),
        status = "primary",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Descriptive",
        argonRow(
          argonColumn(width = 12, plotOutput("piechartdiag")),
          argonColumn(
            radioButtons(
              inputId = "kategori", 
              inline = TRUE,
              label = "Choose Category",
              choices = c("Maligant dan Benign"="M & B","Maligant"="M","Benign"= "B"), 
              selected = "M & B"
            ),
            center = TRUE,
            argonInfoCard(
              value = uiOutput("Jumlahdata"), 
              title = "Jumlah Data (N)", 
              icon = argonIcon("planet"), 
              icon_background = "danger",
              hover_lift = TRUE,
              gradient = TRUE,
              background_color = "primary",
              width=12
            ),
            argonInfoCard(
              value = uiOutput("rata"), 
              title = "Rata-Rata", 
              icon = argonIcon("app"), 
              icon_background = "warning",
              hover_lift = TRUE,
              gradient = TRUE,
              background_color = "info",
              width = 12
            ),
            argonInfoCard(
              value = uiOutput("std"), 
              title = "Standar Deviasi",
              icon = argonIcon("bulb-61"), 
              icon_background = "info",
              hover_lift = TRUE,
              background_color = "success",
              width = 12
            )
          ),
          argonColumn(
            center = TRUE,
            radioButtons(
              inputId = "variabel", 
              inline = TRUE,
              label = "Choose Variable",
              choices = c("Radius Worst"="radius_worst","Area Worst"="area_worst", "Concave Points Worst"="concave.points_worst"), 
              selected = "radius_worst"
            ),
            argonInfoCard(
              value = uiOutput("max"), 
              title = "Maximum",
              gradient = TRUE,
              icon = argonIcon("planet"), 
              icon_background = "danger",
              hover_lift = TRUE,
              background_color = "primary",
              width=12,
            ),
            argonInfoCard(
              value = uiOutput("med"), 
              title = "Median", 
              gradient = TRUE,
              icon = argonIcon("app"), 
              icon_background = "warning",
              hover_lift = TRUE,
              background_color = "info",
              width = 12
            ),
            argonInfoCard(
              value = uiOutput("min"), 
              title = "Minimum", 
              icon = argonIcon("bulb-61"), 
              icon_background = "info",
              hover_lift = TRUE,
              background_color = "success",
              width = 12
            )
          )
        ),
      ),
      br(), br(),
      argonCard(
        width = 12,
        title = "HISTOGRAM",
        src = NULL,
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "primary",
        background_color ="#230c0c" ,
        gradient = FALSE, 
        floating = FALSE,
        argonRow(
          argonColumn(
            radioButtons(
              "kategori2",
              "Choose Category :",
              c("Maligant And Benign"="M&D",
                "Maligant"="M",
                "Benign"="B")
            ),
            width = 6,
            radioButtons(
              "variabel2", 
              "Choose Variable :",
              c("Radius Worst" = "radius_worst",
                "Area Worst" = "area_worst",
                "Concave Point Worst" = "concave.points_worst"
              )
            )
          ),
          argonColumn(width = 6, plotOutput("histplot"))
        )
      ),    br(), br(),
      argonCard(
        width = 12,
        title = "KORELASI",
        src = NULL,
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "primary",
        background_color ="#230c0c" ,
        gradient = FALSE, 
        floating = FALSE,
        argonRow(
          argonColumn(
            radioButtons(
              "kategori3",
              "Choose Category :",
              c("Maligant And Benign"="M&D",
                "Maligant"="M",
                "Benign"="B")
            ),
            width = 4,
          ),
          argonColumn(
            radioButtons(
              "variabel3", 
              "Choose Variable :",
              c("Radius Worst" = "radius_worst",
                "Area Worst" = "area_worst",
                "Concave Point Worst" = "concave.points_worst"
              )
            ),
            radioButtons(
              "variabel4", 
              "Choose Variable:",
              c("Radius Worst" = "radius_worst",
                "Area Worst" = "area_worst",
                "Concave Point Worst" = "concave.points_worst"
              )
            )
          ),
          argonColumn(width = 4,plotOutput("corrplot"))
        )
      )  
    ),
    br()
  )
  #=================================================================================================author
  avatarSizes <- c("sm", "md", "lg")
  avatarTooltips <- c(NULL, "My avatar", NULL)
  
  items_tab <- argonTabItem(
    tabName = "author",
    argonRow(
      argonH1("OUR TEAM", display = 4),
      center = TRUE
    ),
    argonRow(
      width = 12,
      center = TRUE,
      argonColumn(
        width = 4,
        argonUser(
          title = "NAUFAL LUTHFAN TASBIHI",
          subtitle = "5003211012",
          src = "https://imagetolink.com/ib/GUNPbeVtTr.jpg",
        )
      ),
      argonColumn(
        width = 4,
        argonUser(
          title = "HANIF CHOIRUDDIN",
          subtitle = "5003211063",
          src = "https://imagetolink.com/ib/abaaFy82O0.jpg",
        )
      ),
      argonColumn(
        width = 4,
        argonUser(
          title = "IMAM NUR RIZKY GUSMAN",
          subtitle = "500321121",
          src = "https://imagetolink.com/ib/5ZPJK8BjnS.png",
        )
      )
    )
  )
  images_tab <- argonTabItem(
    tabName = "medias",
    argonRow(
      argonCard(
        width = 12,
        icon = icon("cogs"),
        status = "success",
        title = argonH1("Input Data",display = 4),
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        argonColumn(argonH1("Enter Your Data",display=4),width = 12,center = TRUE),
        argonRow(
          width = 12,
          center = TRUE,
          argonColumn(            
            width = 4,
            center = TRUE,
            sliderInput(
              "rw", 
              "Radius Worst:",
              min = 0, 
              max = 50, 
              value = 25,
              round = FALSE
            )
          ),
          argonColumn(
            width=4,
            center = TRUE,
            sliderInput(
              "aw", 
              "Area Worst:",
              min = 0, 
              max = 5000, 
              value = 2500
            )
          ),
          argonColumn(
            width=4,
            center = TRUE,
            sliderInput(
              "cpw", 
              "Concave Points Worst:",
              min = 0, 
              max = 1, 
              value = 0.025
            )
          )
        )
        
      ),
      argonCard(
        width = 12,
        icon = icon("cogs"),
        status = "success",
        title = argonH1("Output Data",display = 4),
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        argonColumn(argonH1("Result Classification",display=4),width = 12,center = TRUE),
        argonRow(
          width = 12,
          center = TRUE,
          argonColumn(
            center = TRUE,
            argonInfoCard(
              width = 12,
              icon = icon("cogs"),
              
              uiOutput("Hasiloutput")
            ))
        )
        
      )
    )
  )
  #=================================================================================================Daaset
  Deskripsi <-"Breast cancer is the most common cancer amongst women in the world. It accounts for 25% of all cancer cases,and affected over 2.1 Million people in 2015 alone. It starts when cells in the breast begin to grow out of control. These cells usually form tumors that can be seen via X-ray or felt as lumps in the breast area."
  Deskripsi2 <- "The key challenges against it’s detection is how to classify tumors into malignant (cancerous) or benign(non cancerous). We ask you to complete the analysis of classifying these tumors using machine learning (with SVMs) and the Breast Cancer Wisconsin (Diagnostic) Dataset."
  tables_tab <- argonTabItem(
    tabName = "dataset",
    argonRow(
      argonH1("Dataset Breast Cancer",display = 4),
      center = TRUE
    ),
    argonCard(
      title = "About Dataset",
      src = "https://www.kaggle.com/datasets/yasserh/breast-cancer-dataset",
      width = 0,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE, 
      floating = FALSE,
      Deskripsi,
      br(),br(),
      Deskripsi2,
    ),
    argonColumn(DTOutput('tabel_data'),center = FALSE)
  )
  #=================================================================================================Description
  
  tabText2 <- "Breast cancer remains one of the most significant health challenges worldwide, particularly affecting women.
              It is the most common cancer among women and is also one of the principal causes of death among cancers. Early 
              detection is crucial as it significantly improves prognosis and survival rates. According to the World Health Organization,
              breast cancer survival rates vary globally, being highest in developed countries and lowest in the developing world, a disparity largely due to the
              availability of adequate diagnosis and treatment options (WHO, 2020). Traditional diagnostic methods like mammography, while effective, are limited 
              by their high costs, the need for specialized equipment and expertise, and the risk of radiation exposure (Smith et al., 2015)."
  tabsets_tab <- argonTabItem(
    tabName = "description",
    argonRow(
      argonH1("BREAST CANCER", display = 4),
      center = TRUE
    ),
    argonRow(
      # Horizontal Tabset
      argonColumn(
        width = 6,
        center = TRUE,
        argonTabSet(
          id = "tab-1",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "sm",
          width = 12,
          iconList = lapply(X = 1, FUN = argonIcon, name = "atom"),
          argonTab(
            tabName = "WHAT IS BREAST CANCER",
            active = TRUE,
            tabText2
          )
        )
      ),
      argonColumn(
        width = 6,
        argonTabSet(
          id = "tab-1",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "sm",
          width = 12,
          iconList = lapply(X = 1, FUN = argonIcon, name = "atom"),
          argonTab(
            tabName = "WHAT IS BREAST CANCER",
            active = TRUE,
            htmltools::tags$iframe(
              src = "https://www.youtube.com/embed/LMRhix52hR8",
              width = "100%",
              height = "400",
              frameborder = "0",
              allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
              allowfullscreen = NA
            )
          )
        )
      )
    )
  )
  #=================================================================================================Footer
  argonFooter <- argonDashFooter(
    copyrights = "@Team 1, 2024",
    src = "",
    argonFooterMenu(
      argonFooterItem("Data Mining and Visualization C", src = ""),
    )
  )
  #=================================================================================================Header
  argonHeader <- argonDashHeader(
    gradient = TRUE,
    color = "blue",
    separator = TRUE,
    separator_color = "secondary",
    argonCard(
      title = "Welcome to our dashboard! ",
      src = "https://www.its.ac.id/statistika/akademik/program-studi/s1-statistika/",
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE, 
      floating = FALSE,
      "This dashboard was created as part of the Data mining and visualization course assignment, Department of Statistics, Faculty of Science and Data Analytics ITS, with the aim of knowing the classification of breast cancer data."
    )
  )
  #=================================================================================================Sidebar
  argonSidebar <- argonDashSidebar(
    vertical = TRUE,
    skin = "light",
    background = "white",
    size = "lg",
    side = "left",
    id = "my_sidebar",
    brand_url = "http://www.google.com",
    brand_logo = "https://i.postimg.cc/wM5pYgYK/MF-ANDAT-removebg-preview.png",
    argonSidebarHeader(title = "Main Menu"),
    argonSidebarMenu(
      argonSidebarItem(
        tabName = "description",
        icon = argonIcon(name = "planet", color = "warning"),
        "BREAST CANCER"
      ),
      argonSidebarItem(
        tabName = "stdesk",
        icon = argonIcon(name = "tv-2", color = "info"),
        "STATISTICS DESCRIPTIVE"
      ),
      argonSidebarItem(
        tabName = "dataset",
        icon = argonIcon(name = "folder-17", color = "green"),
        "DATASET"
      ),
      argonSidebarItem(
        tabName = "medias",
        icon = argonIcon(name = "ui-04", color = "success"),
        "TESTING MODELS"
      ),
      argonSidebarItem(
        tabName = "author",
        icon = argonIcon(name = "circle-08", color = "pink"),
        "AUTHOR"
      ),
    ),
    argonSidebarDivider(),
    argonSidebarHeader(title = "Other Items")
  )
  voting <- function(predictions) {
    apply(predictions, 1, function(row) {
      vote <- table(row)
      winner <- names(vote)[which.max(vote)]
      return(winner)
    })
  }
  
  
  
  #=================================================================================================Shiny
  # App
  shiny::shinyApp(
    ui = argonDashPage(
      title = "MafiaDatmin",
      author = "PAL-NIP-RIZ",
      description = "BreastDataset",
      sidebar = argonSidebar,
      header = argonHeader,
      body = argonDashBody(
        argonTabItems(
          tabsets_tab,
          cards_tab,
          tables_tab,
          images_tab,
          items_tab
        )
      ),
      footer = argonFooter
    ),
    server = function(input, output) {
      
      # STATISTICS DESCRIPTIVE
      output$piechartdiag <- renderPlot({
        # Create PIE CHART
        frekuensi_tidy <- datautama %>%
          count(diagnosis) %>%
          mutate(prop = n / sum(n) * 100, 
                 label = paste0(diagnosis, ": ", round(prop, 1), "%"))
        
        # Create pie chart using ggplot2 with label
        ggplot(frekuensi_tidy, aes(x = "", y = n, fill = diagnosis)) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start = 0) +
          theme_void() +
          geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
          labs(title = "PIE CHART") +
          theme(legend.position = "right")
      })
      
      # COUNT COLUMN
      output$Jumlahdata <- renderUI({
        count_values <- du %>%
          group_by(diagnosis) %>%
          summarise(count = n())
        jd <- if(input$kategori=="M"){
          print(count_values[1,2])
        } else if(input$kategori=="B"){
          print(count_values[2,2])
        } else {
          print(count_values[1,2] + count_values[2,2])
        }
      })
      
      # MEAN, STD DEV, MIN, MAX, MEDIAN
      output$rata <- renderUI({
        mean_values <- du %>%
          group_by(diagnosis) %>%
          summarise(across(everything(), mean, na.rm = TRUE))
        rt <- if(input$kategori=="M"){
          if(input$variabel=="radius_worst"){
            print(mean_values[1,1]) 
          } else if(input$variabel=="area_worst"){
            print(mean_values[1,2])
          } else {
            print(mean_values[1,3])
          }
        } else if(input$kategori=="B"){
          if(input$variabel=="radius_worst"){
            print(mean_values[2,2]) 
          } else if(input$variabel=="area_worst"){
            print(mean_values[2,3])
          } else {
            print(mean_values[2,4])
          }
        } else {
          if(input$variabel=="radius_worst"){
            print(mean(du[,1])) 
          } else if(input$variabel=="area_worst"){
            print(mean(du[,2]))
          } else {
            print(mean(du[,3]))
          }
        }
      })
      
      output$std <- renderUI({
        stdev_values <- du %>%
          group_by(diagnosis) %>%
          summarise(across(everything(), sd, na.rm = TRUE))
        rt <- if(input$kategori=="M"){
          if(input$variabel=="radius_worst"){
            print(stdev_values[1,2]) 
          } else if(input$variabel=="area_worst"){
            print(stdev_values[1,3])
          } else {
            print(stdev_values[1,4])
          }
        } else if(input$kategori=="B"){
          if(input$variabel=="radius_worst"){
            print(stdev_values[2,2]) 
          } else if(input$variabel=="area_worst"){
            print(stdev_values[2,3])
          } else {
            print(stdev_values[2,4])
          }
        } else {
          if(input$variabel=="radius_worst"){
            print(sd(du[,1])) 
          } else if(input$variabel=="area_worst"){
            print(sd(du[,2]))
          } else {
            print(sd(du[,3]))
          }
        }
      })
      
      output$min <- renderUI({
        min_values <- du %>%
          group_by(diagnosis) %>%
          summarise(across(everything(), min, na.rm = TRUE))
        rt <- if(input$kategori=="M"){
          if(input$variabel=="radius_worst"){
            print(min_values[1,2]) 
          } else if(input$variabel=="area_worst"){
            print(min_values[1,3])
          } else {
            print(min_values[1,4])
          }
        } else if(input$kategori=="B"){
          if(input$variabel=="radius_worst"){
            print(min_values[2,2]) 
          } else if(input$variabel=="area_worst"){
            print(min_values[2,3])
          } else {
            print(min_values[2,4])
          }
        } else {
          if(input$variabel=="radius_worst"){
            print(min(du[,1])) 
          } else if(input$variabel=="area_worst"){
            print(min(du[,2]))
          } else {
            print(min(du[,3]))
          }
        }
      })
      
      output$max <- renderUI({
        max_values <- du %>%
          group_by(diagnosis) %>%
          summarise(across(everything(), max, na.rm = TRUE))
        rt <- if(input$kategori=="M"){
          if(input$variabel=="radius_worst"){
            print(max_values[1,2]) 
          } else if(input$variabel=="area_worst"){
            print(max_values[1,3])
          } else {
            print(max_values[1,4])
          }
        } else if(input$kategori=="B"){
          if(input$variabel=="radius_worst"){
            print(max_values[2,2]) 
          } else if(input$variabel=="area_worst"){
            print(max_values[2,3])
          } else {
            print(max_values[2,4])
          }
        } else {
          if(input$variabel=="radius_worst"){
            print(max(du[,1])) 
          } else if(input$variabel=="area_worst"){
            print(max(du[,2]))
          } else {
            print(max(du[,3]))
          }
        }
      })
      
      output$med <- renderUI({
        med_values <- du %>%
          group_by(diagnosis) %>%
          summarise(across(everything(), median, na.rm = TRUE))
        rt <- if(input$kategori=="M"){
          if(input$variabel=="radius_worst"){
            print(med_values[1,2]) 
          } else if(input$variabel=="area_worst"){
            print(med_values[1,3])
          } else {
            print(med_values[1,4])
          }
        } else if(input$kategori=="B"){
          if(input$variabel=="radius_worst"){
            print(med_values[2,2]) 
          } else if(input$variabel=="area_worst"){
            print(med_values[2,3])
          } else {
            print(med_values[2,4])
          }
        } else {
          if(input$variabel=="radius_worst"){
            print(median(du[,1])) 
          } else if(input$variabel=="area_worst"){
            print(median(du[,2]))
          } else {
            print(median(du[,3]))
          }
        }
      })
      
      # Histogram
      output$histplot <- renderPlot({
        if(input$kategori2=="M&D"){
          du2 <- du[input$variabel2]
        } else {
          data_by_diagnosis <- split(du[input$variabel2], du$diagnosis==input$kategori2)
          du2 <- data_by_diagnosis[[2]] # 2 -> TRUE
        }
        
        if(input$variabel2=="radius_worst"){
          hist(du2$radius_worst,
               main = "Histogram of Radius Worst",
               xlab = "Radius Worst",
               ylab = "Frequency",
               col = "skyblue",
               border = "black")
        } else if(input$variabel2=="area_worst"){
          hist(du2$area_worst,
               main = "Histogram of Area Worst",
               xlab = "Area Worst",
               ylab = "Frequency",
               col = "skyblue",
               border = "black")
        } else {
          hist(du2$concave.points_worst,
               main = "Histogram of Concave Points Worst",
               xlab = "Concave Point Worst",
               ylab = "Frequency",
               col = "skyblue",
               border = "black")
        }
      })
      
      # Correlation
      output$corrplot <- renderPlot({
        if(input$kategori3=="M&D"){
          du3 <- du[c(input$variabel3, input$variabel4)]
        } else {
          data_by_diagnosis3 <- split(du[c(input$variabel3, input$variabel4)], du$diagnosis==input$kategori3)
          du3 <- data_by_diagnosis3[[2]] # 2 -> TRUE
        }
        if(input$variabel3=="radius_worst"){
          yt <- "Radius Worst"
          yin <- du3$radius_worst
        } else if(input$variabel3=="area_worst"){
          yt <- "Area Worst"
          yin <- du3$area_worst
        } else {
          yt <- "Concave Points Worst"
          yin <- du3$concave.points_worst
        }
        if(input$variabel4=="radius_worst"){
          xt <- "Radius Worst"
          xin <- du3$radius_worst
        } else if(input$variabel4=="area_worst"){
          xt <- "Area Worst"
          xin <- du3$area_worst
        } else {
          xt <- "Concave Points Worst"
          xin <- du3$concave.points_worst
        }
        cor_value <- cor(du3[input$variabel3], du3[input$variabel4])
        # Create scatter plot
        plot(xin, yin, 
             main = paste0(xt," Vs ",yt," Correlation Value : (",cor_value,")"), 
             xlab = xt, 
             ylab = yt, 
             pch = 19, 
             col = "blue",
             cex = 1.5)
      })
      
      # Apply the scaler and update the predictions
      output$Hasiloutput <- renderText({
        input_matrix <- data.frame(aw = input$aw, rw = input$rw, cpw = input$cpw)
        input_matrix_scaled <- scale_input(input_matrix, scaler_params)
        svm_pred <- svm_model$predict_proba(as.matrix(input_matrix_scaled))[, 2]
        predictionsvm <- ifelse(svm_pred >= 0.31, "M", "B")
        nn_pred <- nn_model$predict_proba(as.matrix(input_matrix_scaled))[, 2]
        predictionnn <- ifelse(nn_pred >= 0.39, "M", "B")
        rf_pred <- rf_model$predict_proba(as.matrix(input_matrix_scaled))[, 2]
        predictionrf <- ifelse(rf_pred >= 0.72, "M", "B")
        predictions <- data.frame(predictionsvm, predictionnn, predictionrf)
        voted_predictions <- voting(predictions)
        paste("Breast Cancer:", voted_predictions)
        
      })
      
      # Render data table
      output$tabel_data <- renderDT({du})
    }
  )
  
