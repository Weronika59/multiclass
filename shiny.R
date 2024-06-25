library(shiny)
library(tidyverse)
library(stringr)
library(ggplot2)
library(DT)
library(bslib)
library(keras)
library(tensorflow)
library(tidymodels)
library(kableExtra)
library(grDevices)

#######################
x <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(x)
h1 <- readRDS("history1_v2.rds")
h2 <- readRDS("history2_v2.rds")
h3 <- readRDS("history3_v2.rds")
h5 <- readRDS("history5_v2.rds")

m1 <- load_model_tf("model1_v2.keras")
m2 <- load_model_tf("model2_v2.keras")
m3 <- load_model_tf("model3_v2.keras")
m5 <- load_model_tf("model5_v2.keras")

train_features <- readRDS("train_features.rds")
train_targets <- readRDS("train_targets.rds")
test_features <- readRDS("test_features.rds")
test_targets <- readRDS("test_targets.rds")
df_test <- readRDS("df_test.rds")
#######################

num_vars <- c("age","slos", "d.time", "num.co", "edu", "scoma", "charges", "totcst", "totmcst", "avtisst", "hday", "meanbp", "wblc", "hrt", "resp", "temp", "pafi", "alb", "bili", "crea", "sod", "ph", "glucose", "bun", "urine", "adlp", "adls", "adlsc")
fct_vars <- c("death",    "sex" ,     "hospdead", "dzgroup",  "dzclass" , "income",   "race" ,    "diabetes", "dementia", "ca",       "dnr","sfdm2")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "pulse"),
  titlePanel("SUPPORT2"),
  tabsetPanel(
    #zakładki:
    #wstęp
    tabPanel("Wstęp", 
             sidebarLayout(
               sidebarPanel(
                 textOutput(outputId = "opis_badania"),
                 textOutput(outputId = "instr"),
                 fileInput("file1", "Wybierz plik z danymi:",accept = c(".csv"), buttonLabel = "Szukaj...", placeholder = "Nie wybano pliku")
               ),
               mainPanel(
                 DTOutput("dane")
               )
             )
    ),
    #EDA
    tabPanel("EDA",
             mainPanel(
               card(height = 150,
                    textOutput(outputId = "wymiary"),
                    textOutput(outputId = "zmienne"),
                    textOutput(outputId = "bd")
               ),
               accordion(
                 open = "Zmienne numeryczne",
                 accordion_panel(
                   "Zmienne numeryczne",
                   selectInput(inputId = "var_num", label = "Wybierz zmienną numeryczną", choices = num_vars),
                   navset_card_underline(
                     title = "Eksploracja zmiennych numerycznych",
                     nav_panel(title = "Rozkład", plotOutput(outputId = "rozklad_num")),
                     nav_panel(title = "Statystyki", DTOutput(outputId = "statystyki_num"))
                   )
                 ),
                 accordion_panel(
                   "Zmienne kategoryczne",
                   selectInput(inputId = "var_fct", label = "Wybierz zmienną kategoryczną", choices = fct_vars),
                   navset_card_underline(
                     title = "Eksploracja zmiennych kategorycznych",
                     nav_panel(title = "Histogram", plotOutput(outputId = "rozklad_fct")),
                     nav_panel(title = "Liczebność", tableOutput(outputId = "statystyki_fct"))
                   )
                 ),
                 accordion_panel(
                   "Inne",
                   fluidRow(
                     column(6,
                            selectInput(inputId = "var_num2", label = "Wybierz zmienną numeryczną", choices = num_vars)
                     ),
                     column(6,
                            selectInput(inputId = "var_num3", label = "Wybierz drugą zmienną do korelacji", choices = num_vars)
                     )
                   ),
                   navset_card_underline(
                     title = "Wykresy ramka-wąsy zmiennych numerycznych",
                     nav_panel(title = "Rozkład", plotOutput(outputId = "bx_num")),
                     nav_panel(title = "Korelacje", tableOutput(outputId = "cor"))
                   )
                 )
               )
             )
    ),
    #wizualizacje
    tabPanel("Wizualizacje",
             mainPanel(
               accordion(
                 open = "dzclass",
                 accordion_panel(
                   "Wykres gęstości dla wybranej zmiennej w podziale na dzclass",
                   selectInput(inputId = "zmienna1", label = "Wybierz zmienną, dla której zostanie narysowany wykres", choices = num_vars),
                   card(height = 100, "dzclass oznacza kategorię choroby pacjenta: ostra niewydolność nerek/niewydolność wielonarządowa - zakodowane jako 1, przewlekła obturacyjna choroba płuc/zastoinowa niewydolność serca/marskość wątroby - zakodowane jako 2, rak - zakodowane jako 3, śpiączka - zakodowane jako 4."),
                   plotOutput(outputId = "gest1")
                 ),
                 accordion_panel(
                   "Wykres gęstości dla wybranej zmiennej w podziale na ca",
                   selectInput(inputId = "zmienna2", label = "Wybierz zmienną", choices = num_vars),
                   card(height = 50, "ca określa, czy pacjent ma raka - zakodowane jako 1, czy rak się rozprzestrzenił - zakodowane jako 2 lub czy jest zdrowy - zakodowane jako 0."),
                   plotOutput(outputId = "gest2")
                 ),
                 accordion_panel(
                   "Wykres gęstości dla wybranej zmiennej w podziale na diabetes",
                   selectInput(inputId = "zmienna3", label = "Wybierz zmienną", choices = num_vars),
                   card(height = 50, "diabetes informuje, czy pacjent choruje na cukrzycę - 0 jako nie oraz 1 jako tak."),
                   plotOutput(outputId = "gest3")
                 ),
                 accordion_panel(
                   "Wykres gęstości dla wybranej zmiennej w podziale na dementia",
                   selectInput(inputId = "zmienna4", label = "Wybierz zmienną", choices = num_vars),
                   card(height = 50, "dementia informuje, czy pacjent choruje na demencję - 0 jako nie oraz 1 jako tak."),
                   plotOutput(outputId = "gest4")
                 )
               ),
             )
    ),
    #modele i ewaluacja
    tabPanel("Modele i wyniki",
             mainPanel(
               selectInput(inputId = "model", label = "Wybierz model", choices = c("Model 1", "Model 2", "Model 3", "Model 4")),
               card(height = 80, "Każdy z modeli jest uczony na 40 epokach. Zbiór danych jest podzielony na uczący oraz testowy w proporcji 7:3. Zbiór walidacyjny stanowi 20% zbioru uczącego."),
               textOutput(outputId = "architektura"),
               navset_card_underline(
                 title = "Wyniki i ewaluacja",
                 nav_panel(title = "Historia uczenia", plotOutput(outputId = "history")),
                 nav_panel(title = "Metryki", tableOutput(outputId = "metryki"))
               ),
               navset_card_underline(
                 title = "Wyniki predykcji",
                 nav_panel(title = "macierz klasyfikacji dla death", plotOutput(outputId = "cm1")),
                 nav_panel(title = "macierz klasyfikacji dla hospdead", plotOutput(outputId = "cm2")),
                 nav_panel(title = "macierz klasyfikacji dla sfdm2", plotOutput(outputId = "cm3"))
               )
             )
    )
  )
)


server <- function(input, output) {
  #wstęp
  #opis badania
  output$opis_badania <- renderText(expr = "Projekt SUPPORT składał się z dwuletniego prospektywnego badania obserwacyjnego (Faza I), po którym nastąpiło dwuletnie kontrolowane badanie kliniczne (Faza II). Jego celem była poprawa procesu decyzyjnego, aby odpowiedzieć na rosnące krajowe obawy dotyczące utraty kontroli przez pacjentów pod koniec życia i zmniejszyć częstotliwość mechanicznego, bolesnego i długotrwałego procesu umierania. Wszyscy pacjenci w pięciu ośrodkach medycznych w Stanach Zjednoczonych spełnili kryteria włączenia i wykluczenia dla dziewięciu kategorii chorób: ostra niewydolność oddechowa, przewlekła obturacyjna choroba płuc, zastoinowa niewydolność serca, choroba wątroby, śpiączka, rak okrężnicy, rak płuc, niewydolność wielonarządowa z nowotworem złośliwym i niewydolność wielonarządowa z posocznicą. SUPPORT to połączenie pacjentów z dwóch badań, z których każde trwało 2 lata. Pierwsza faza dotyczyła 4 301 pacjentów, podczas gdy druga faza dotyczyła 4 804 pacjentów. Pod względem czasu, badania te zostały przeprowadzone w 1989 r. do 1991 r. dla fazy I oraz w 1992 r. do 1994 r.")
  
  #instrukcja
  output$instr <- renderText(expr = "Aplikacja składa się z czterech zakładek. Pierwsza stanowi część informacyjną o całym badaniu. Umożliwia  import danych oraz ich wyświetlenie. Druga zakładka dotyczy eksploracyjnej analizy danych. W trzeciej zakładce jest możliwość wizualizacji danych pod kątem ich porównania względem wybranych cech. Czwarta zakładka dotyczy modeli uczenia maszynowego - po wyborze danej architektury, dokonywana jest jego ocena na danych testowych.")
  
  
  uploadedData <- reactive({
    req(input$file1)
    inFile <- input$file1
    
    validTypes <- c("text/csv", "text/comma-separated-values")
    if (!inFile$type %in% validTypes) {
      stop("Nieprawidłowy typ pliku! Prześlij plik CSV.")
    }
    
    if(str_detect(as.character(input$file1$name), pattern = "support2")==F){
      stop("Nie przesłałeś odpowiedniego pliku z danymi!")
    }
    
    tryCatch({
      read.csv(inFile$datapath)
    }, error = function(e) {
      stop("Wystąpił błąd podczas odczytu pliku: ", e$message)
    })
  })
  
  output$dane <- renderDataTable({
    uploadedData()
    req(input$file1)
    inFile <- input$file1
    x <- read.csv(inFile$datapath, sep = ",")
    x
  }, options = list(scrollX=T, server=F, pageLength=7, scrollY=T))
  
  
  output$wymiary <- renderText({
    tryCatch({
      c("- dane mają ", dim(uploadedData())[1], "wierszy oraz ", dim(uploadedData())[2], "kolumn")
    }, error = function(e) {
      stop("Wczytaj plik z danymi, aby zobaczyć ich opis.")
    })
  })
  
  output$zmienne <- renderText({
    tryCatch({
      c("- zmienne w tym zbiorze danych to: ",
        str_c(unlist(colnames(uploadedData()))[1], unlist(colnames(uploadedData()))[2], unlist(colnames(uploadedData()))[3], unlist(colnames(uploadedData()))[4], unlist(colnames(uploadedData()))[5], unlist(colnames(uploadedData()))[6], unlist(colnames(uploadedData()))[7], unlist(colnames(uploadedData()))[8], unlist(colnames(uploadedData()))[9], unlist(colnames(uploadedData()))[10], unlist(colnames(uploadedData()))[11], unlist(colnames(uploadedData()))[12], unlist(colnames(uploadedData()))[13], unlist(colnames(uploadedData()))[14], unlist(colnames(uploadedData()))[15], unlist(colnames(uploadedData()))[16], unlist(colnames(uploadedData()))[17], unlist(colnames(uploadedData()))[18], unlist(colnames(uploadedData()))[19], unlist(colnames(uploadedData()))[20], unlist(colnames(uploadedData()))[21], unlist(colnames(uploadedData()))[22], unlist(colnames(uploadedData()))[23], unlist(colnames(uploadedData()))[24], unlist(colnames(uploadedData()))[25], unlist(colnames(uploadedData()))[26], unlist(colnames(uploadedData()))[27], unlist(colnames(uploadedData()))[28], unlist(colnames(uploadedData()))[29], unlist(colnames(uploadedData()))[30], unlist(colnames(uploadedData()))[31], unlist(colnames(uploadedData()))[32], unlist(colnames(uploadedData()))[33], unlist(colnames(uploadedData()))[34], unlist(colnames(uploadedData()))[35], unlist(colnames(uploadedData()))[36], unlist(colnames(uploadedData()))[37], unlist(colnames(uploadedData()))[38], unlist(colnames(uploadedData()))[39], unlist(colnames(uploadedData()))[40], unlist(colnames(uploadedData()))[41], unlist(colnames(uploadedData()))[42], unlist(colnames(uploadedData()))[43], unlist(colnames(uploadedData()))[44], unlist(colnames(uploadedData()))[45], unlist(colnames(uploadedData()))[46], unlist(colnames(uploadedData()))[47], sep = ", ")
      )}, error = function(e) {
        stop("Wczytaj plik z danymi, aby zobaczyć ich opis.")
      })
  })
  
  output$bd <- renderText({
    tryCatch({
      c("- liczba braków danych w tym zbiorze wynosi ", sum(is.na(uploadedData())))
    }, error = function(e) {
      stop("Wczytaj plik z danymi, aby zobaczyć ich opis.")
    })
  })
  
  output$statystyki_num <- renderDataTable({
    uploadedData()
    req(input$file1)
    inFile <- input$file1
    tryCatch({
      read.csv(inFile$datapath, sep = ",") |> select(input$var_num) |> summary()
    }, error = function(e) {
      stop("Wczytaj najpierw plik z danymi.")
    })
  }, options = list(server=F, searching=F, paging=F, row.names=F, ordering=F, columnDefs = list(list(targets = c(1,2), visible = FALSE))))
  
  output$rozklad_num <- renderPlot({
    zmienna <- reactive(input$var_num)
    tryCatch({
      ggplot(data = uploadedData(), aes(x = uploadedData()[,zmienna()]))+
        geom_histogram(color = "white", fill = "#6BA8F5")+
        theme(axis.title.x = element_blank())
    }, error = function(e){
      stop("Wczytaj najpierw plik z danymi.")
    })
  })
  
  output$statystyki_fct <- renderTable({
    tryCatch({
      summary(as.factor(read.csv(input$file1$datapath, sep = ",")[,input$var_fct]))
    }, error = function(e){
      stop("Wczytaj najpierw plik z danymi.")
    })
  }, digits = 4, striped = T, rownames = T, colnames = F)
  
  output$rozklad_fct <- renderPlot({
    zmienna <- reactive(input$var_fct)
    tryCatch({
      ggplot(data = uploadedData(), aes(x = uploadedData()[,zmienna()]))+
        geom_bar(fill = "#CC9999")+
        theme(axis.title.x = element_blank())
    }, error = function(e){
      stop("Wczytaj najpierw plik z danymi.")
    })
  })
  
  output$bx_num <- renderPlot({
    zmienna <- reactive(input$var_num2)
    tryCatch({
      ggplot(data = uploadedData(), aes(x = uploadedData()[,zmienna()])) + 
        geom_boxplot(fill = "#779D68")+
        theme(axis.title.x = element_blank())
    }, error = function(e){
      stop("Wczytaj najpierw plik z danymi.")
    })
  })
  
  output$cor <- renderTable({
    zm1 <- reactive(input$var_num2)
    zm2 <- reactive(input$var_num3)
    tryCatch({
      cor(uploadedData()[,zm1()], uploadedData()[,zm2()], use = "complete.obs") #są NA - zastanowić się
    }, error = function(e){
      stop("Wczytaj najpierw plik z danymi.")
    })
  }, colnames = F)
  
  #dzclass
  output$gest1 <- renderPlot({
    zm1 <- reactive(input$zmienna1)
    tryCatch({
      ggplot(data = read.csv(input$file1$datapath, sep=","), aes(x = uploadedData()[,zm1()], fill=uploadedData()[,"dzclass"]))+
        geom_density(alpha=0.5)+
        theme(axis.title = element_blank(), legend.title = element_blank())
    }, error = function(e) {
      stop("Musisz najpierw wczytać plik z danymi, aby zobaczyć wykres.")
    })
  })
  
  #ca
  output$gest2 <- renderPlot({
    zm2 <- reactive(input$zmienna2)
    tryCatch({
      ggplot(data = read.csv(input$file1$datapath, sep=","), aes(x = uploadedData()[,zm2()], fill=uploadedData()[,"ca"]))+
        geom_density(alpha=0.5)+
        scale_fill_brewer(palette="Set2")+
        theme(axis.title = element_blank(), legend.title = element_blank())
    }, error = function(e) {
      stop("Musisz najpierw wczytać plik z danymi, aby zobaczyć wykres.")
    })
  })
  
  #diabetes
  output$gest3 <- renderPlot({
    zm3 <- reactive(input$zmienna3)
    tryCatch({
      ggplot(data = read.csv(input$file1$datapath, sep=","), aes(x = uploadedData()[,zm3()], fill=as.factor(uploadedData()[,"diabetes"])))+
        geom_density(alpha=0.5)+
        scale_fill_brewer(palette="Set3")+
        theme(axis.title = element_blank(), legend.title = element_blank())
    }, error = function(e) {
      stop("Musisz najpierw wczytać plik z danymi, aby zobaczyć wykres.")
    })
  })
  
  #dementia
  output$gest4 <- renderPlot({
    zm4 <- reactive(input$zmienna4)
    tryCatch({
      ggplot(data = read.csv(input$file1$datapath, sep=","), aes(x = uploadedData()[,zm4()], fill=as.factor(uploadedData()[,"dementia"])))+
        geom_density(alpha=0.5)+
        theme(axis.title = element_blank(), legend.title = element_blank())
    }, error = function(e) {
      stop("Musisz najpierw wczytać plik z danymi, aby zobaczyć wykres.")
    })
  })
  
  output$history <- renderPlot({
    mod <- reactive(input$model)
    tryCatch({
      uploadedData()
      if(str_equal(mod(), "Model 1")==T) return(plot(h1))
      if(str_equal(mod(), "Model 2")==T) return(plot(h2))
      if(str_equal(mod(), "Model 3")==T) return(plot(h3))
      if(str_equal(mod(), "Model 4")==T) return(plot(h5))
    }, error = function(e){
      stop("Wczytaj plik z danymi, aby zobaczyć wyniki.")
    })
  })
  
  output$architektura <- renderText({
    mod <- reactive(input$model)
    if(str_equal(mod(), "Model 1")==T){
      return("Warstwa wejściowa ma wymiar 51. Warstwa ukryta, z której składają się warstwy wyjściowe zbudowana jest z 8 neuronów. Architekturę modelu zwieńczają dwa wyjścia binarne oraz trzecie wyjście wieloklasowe.")
    }
    if(str_equal(mod(), "Model 2")==T){
      return("Warstwa wejściowa ma wymiar 51. Warstwa ukryta, z której składają się warstwy wyjściowe zbudowana jest z 8 neuronów. Po warstwie wejściowej następuje warstwa flatten, a następnie dwie warstwy gęste odpowiednio z 64 oraz 32 neuronami. Architekturę modelu zwieńczają dwa wyjścia binarne oraz trzecie wyjście wieloklasowe.")
    }
    if(str_equal(mod(), "Model 3")==T){
      return("Warstwa wejściowa ma wymiar 51. Warstwa ukryta, z której składają się warstwy wyjściowe zbudowana jest z 8 neuronów. Po warstwie wejściowej następuje warstwa flatten, a następnie 4 serie naprzemiennych warstw: warstwy gęstej oraz warstwy dropout. Architekturę modelu zwieńczają dwa wyjścia binarne oraz trzecie wyjście wieloklasowe.")
    }
    if(str_equal(mod(), "Model 4")==T){
      return("Warstwa wejściowa ma wymiar 51. Warstwa ukryta, z której składają się warstwy wyjściowe zbudowana jest z 8 neuronów. Po warstwie wejściowej następują dwie warstwy sieci gęstej odpowiednio z 4 oraz 8 neuronami. Architekturę modelu zwieńczają dwa wyjścia binarne oraz trzecie wyjście wieloklasowe.")
    }
  })
  
  output$metryki <- renderTable({
    mod <- reactive(input$model)
    tryCatch({
      uploadedData()
      if(str_equal(mod(), "Model 1")==T){
        p <- m1 %>% 
          evaluate(as.matrix(test_features), list(test_targets[, "death"],test_targets[, "hospdead"],test_targets[, grep("^sfdm2", colnames(test_targets))]))
        return(p)
      }
      if(str_equal(mod(), "Model 2")==T){
        p <- m2 %>% 
          evaluate(as.matrix(test_features), list(test_targets[, "death"],test_targets[, "hospdead"],test_targets[, grep("^sfdm2", colnames(test_targets))]))
        return(p)
      }
      if(str_equal(mod(), "Model 3")==T){
        p <- m3 %>% 
          evaluate(as.matrix(test_features), list(test_targets[, "death"],test_targets[, "hospdead"],test_targets[, grep("^sfdm2", colnames(test_targets))]))
        return(p)
      }
      if(str_equal(mod(), "Model 4")==T){
        p <- m5 %>% 
          evaluate(as.matrix(test_features), list(test_targets[, "death"],test_targets[, "hospdead"],test_targets[, grep("^sfdm2", colnames(test_targets))]))
        return(p)
      }
    }, error = function(e){
      stop("Wczytaj plik z danymi, aby zobaczyć wyniki.")
    })
    
  }, striped = T, colnames = F, digits = 4, rownames = T)
  
  #death
  output$cm1 <- renderPlot({
    mod <- reactive(input$model)
    tryCatch({
      uploadedData()
      if(str_equal(mod(), "Model 1")==T){
        x <- m1 %>% predict(as.matrix(test_features))
        cm <- data.frame(x[[1]] |> k_argmax() |> as.vector(), df_test$death)
        colnames(cm) <- c("pred", "truth")
        cm$pred <- factor(cm$pred, levels = c(0:1))
        return(cm |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
      if(str_equal(mod(), "Model 2")==T){
        x <- m2 %>% predict(as.matrix(test_features))
        cm <- data.frame(x[[1]] |> k_argmax() |> as.vector(), df_test$death)
        colnames(cm) <- c("pred", "truth")
        cm$pred <- factor(cm$pred, levels = c(0:1))
        return(cm |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
      if(str_equal(mod(), "Model 3")==T){
        x <- m3 %>% predict(as.matrix(test_features))
        cm <- data.frame(x[[1]] |> k_argmax() |> as.vector(), df_test$death)
        colnames(cm) <- c("pred", "truth")
        cm$pred <- factor(cm$pred, levels = c(0:1))
        return(cm |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
      if(str_equal(mod(), "Model 4")==T){
        x <- m5 %>% predict(as.matrix(test_features))
        cm <- data.frame(x[[1]] |> k_argmax() |> as.vector(), df_test$death)
        colnames(cm) <- c("pred", "truth")
        cm$pred <- factor(cm$pred, levels = c(0:1))
        return(cm |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
    }, error = function(e){
      stop("Wczytaj plik z danymi, aby zobaczyć wyniki.")
    })
  })
  
  #hospdead
  output$cm2 <- renderPlot({
    mod <- reactive(input$model)
    tryCatch({
      uploadedData()
      if(str_equal(mod(), "Model 1")==T){
        x <- m1 %>% predict(as.matrix(test_features))
        cm2 <- data.frame(x[[2]] |> k_argmax() |> as.vector(), df_test$hospdead)
        colnames(cm2) <- c("pred", "truth")
        cm2$pred <- factor(cm2$pred, levels = c(0:1))
        return(cm2 |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
      if(str_equal(mod(), "Model 2")==T){
        x <- m2 %>% predict(as.matrix(test_features))
        cm2 <- data.frame(x[[2]] |> k_argmax() |> as.vector(), df_test$hospdead)
        colnames(cm2) <- c("pred", "truth")
        cm2$pred <- factor(cm2$pred, levels = c(0:1))
        return(cm2 |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
      if(str_equal(mod(), "Model 3")==T){
        x <- m3 %>% predict(as.matrix(test_features))
        cm2 <- data.frame(x[[2]] |> k_argmax() |> as.vector(), df_test$hospdead)
        colnames(cm2) <- c("pred", "truth")
        cm2$pred <- factor(cm2$pred, levels = c(0:1))
        return(cm2 |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
      if(str_equal(mod(), "Model 4")==T){
        x <- m5 %>% predict(as.matrix(test_features))
        cm2 <- data.frame(x[[2]] |> k_argmax() |> as.vector(), df_test$hospdead)
        colnames(cm2) <- c("pred", "truth")
        cm2$pred <- factor(cm2$pred, levels = c(0:1))
        return(cm2 |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
    }, error = function(e){
      stop("Wczytaj plik z danymi, aby zobaczyć wyniki.")
    })
  })
  
  #sfdm2
  output$cm3 <- renderPlot({
    mod <- reactive(input$model)
    tryCatch({
      uploadedData()
      if(str_equal(mod(), "Model 1")==T){
        x <- m1 %>% predict(as.matrix(test_features))
        cm3 <- data.frame(x[[3]] |> k_argmax() |> as.vector(), df_test$sfdm2)
        colnames(cm3) <- c("pred", "truth")
        cm3$pred <- factor(cm3$pred, levels = c(0:4))
        return(cm3 |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
      if(str_equal(mod(), "Model 2")==T){
        x <- m2 %>% predict(as.matrix(test_features))
        cm3 <- data.frame(x[[3]] |> k_argmax() |> as.vector(), df_test$sfdm2)
        colnames(cm3) <- c("pred", "truth")
        cm3$pred <- factor(cm3$pred, levels = c(0:4))
        return(cm3 |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
      if(str_equal(mod(), "Model 3")==T){
        x <- m3 %>% predict(as.matrix(test_features))
        cm3 <- data.frame(x[[3]] |> k_argmax() |> as.vector(), df_test$sfdm2)
        colnames(cm3) <- c("pred", "truth")
        cm3$pred <- factor(cm3$pred, levels = c(0:4))
        return(cm3 |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
      if(str_equal(mod(), "Model 4")==T){
        x <- m5 %>% predict(as.matrix(test_features))
        cm3 <- data.frame(x[[3]] |> k_argmax() |> as.vector(), df_test$sfdm2)
        colnames(cm3) <- c("pred", "truth")
        cm3$pred <- factor(cm3$pred, levels = c(0:4))
        return(cm3 |> conf_mat(truth = truth, estimate = pred) |> 
                 autoplot(type = "heatmap"))
      }
    }, error = function(e){
      stop("Wczytaj plik z danymi, aby zobaczyć wyniki.")
    })
  })
}


shinyApp(ui = ui, server = server)
