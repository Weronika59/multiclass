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

num_vars <- c("age","slos", "d.time", "num.co", "edu", "scoma", "charges", "totcst", "totmcst", "avtisst", "hday", "meanbp", "wblc", "hrt", "resp", "temp", "pafi", "alb", "bili", "crea", "sod", "ph", "glucose", "bun", "urine", "adlp", "adls", "adlsc")
fct_vars <- c("death",    "sex" ,     "hospdead", "dzgroup",  "dzclass" , "income",   "race" ,    "diabetes", "dementia", "ca",       "dnr","sfdm2")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"), #united, flatly, darkly
  #tytuł całej aplikacji
  titlePanel("SUPPORT2"),
  tabsetPanel(
    #zakładki:
    #wstęp
    tabPanel("Wstęp", 
             sidebarLayout(
               sidebarPanel(
                 #info o samym badaniu, celu
                 textOutput(outputId = "opis_badania"),
                 #instrukcja obsługi, opis co gdzie jest w aplikacji
                 textOutput(outputId = "instr"),
                 #wczytanie pliku
                 fileInput("file1", "Wybierz plik CSV:",accept = c(".csv"), buttonLabel = "Szukaj...", placeholder = "Nie wybano pliku")
               ),
               #wyświetlenie danych
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
                    textOutput(outputId = "typy_zm"),
                    textOutput(outputId = "bd")
               ),
               selectInput(inputId = "var_num", label = "Wybierz zmienną numeryczną", choices = num_vars),
               navset_card_underline(
                 title = "Eksploracja zmiennych numerycznych",
                 nav_panel(title = "Rozkład", plotOutput(outputId = "rozklad_num")),
                 nav_panel(title = "Statystyki", tableOutput(outputId = "statystyki_num"))
               ),
               selectInput(inputId = "var_fct", label = "Wybierz zmienną kategoryczną", choices = fct_vars),
               navset_card_underline(
                 title = "Eksploracja zmiennych kategorycznych",
                 nav_panel(title = "Rozkład", plotOutput(outputId = "rozklad_fct")),
                 nav_panel(title = "Liczebność", tableOutput(outputId = "statystyki_fct"))
               ),
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
               ),
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
                   #textOutput(outputId = "opis1"),
                   card(height = 100, "dzclass oznacza kategorię choroby pacjenta: ostra niewydolność nerek/niewydolność wielonarządowa - zakodowane jako 1, przewlekła obturacyjna choroba płuc/zastoinowa niewydolność serca/marskość wątroby - zakodowane jako 2, rak - zakodowane jako 3, śpiączka - zakodowane jako 4."),
                   plotOutput(outputId = "gest1")
                 ),
                 accordion_panel(
                   "Wykres gęstości dla wybranej zmiennej w podziale na ca",
                   selectInput(inputId = "zmienna2", label = "Wybierz zmienną", choices = num_vars),
                   #textOutput(outputId = "opis2"),
                   card(height = 50, "ca określa, czy pacjent ma raka - zakodowane jako 1, czy rak się rozprzestrzenił - zakodowane jako 2 lub czy jest zdrowy - zakodowane jako 0."),
                   plotOutput(outputId = "gest2")
                 ),
                 accordion_panel(
                   "Wykres gęstości dla wybranej zmiennej w podziale na diabetes",
                   selectInput(inputId = "zmienna3", label = "Wybierz zmienną", choices = num_vars),
                   #textOutput(outputId = "opis3"),
                   card(height = 50, "diabetes informuje, czy pacjent choruje na cukrzycę - 0 jako nie oraz 1 jako tak."),
                   plotOutput(outputId = "gest3")
                 ),
                 accordion_panel(
                   "Wykres gęstości dla wybranej zmiennej w podziale na dementia",
                   selectInput(inputId = "zmienna4", label = "Wybierz zmienną", choices = num_vars),
                   #textOutput(outputId = "opis4"),
                   card(height = 50, "dementia informuje, czy pacjent choruje na demencję - 0 jako nie oraz 1 jako tak."),
                   plotOutput(outputId = "gest4")
                 )
               ),
               
             )
    ),
    #modele i pred
    tabPanel("Modele i wyniki",
             mainPanel(
               selectInput(inputId = "model", label = "Wybierz model", choices = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")),
               textOutput(outputId = "info"),
               navset_card_underline(
                 title = "Wyniki i ewaluacja",
                 nav_panel(title = "Historia uczenia", plotOutput(outputId = "history")),
                 nav_panel(title = "Metryki", tableOutput(outputId = "metryki"))
               ),
               navset_card_underline(
                 title = "Wyniki predykcji",
                 nav_panel(title = "cm dla death", plotOutput(outputId = "cm1")),
                 nav_panel(title = "cm dla hospdead", plotOutput(outputId = "cm2")),
                 nav_panel(title = "cm dla sfdm2", plotOutput(outputId = "cm3"))
               )
             )
    )
  )
)

#######################
dane <- read.csv("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\support2.csv", na.strings = c("NA",""))
dane$death <- as.factor(dane$death) 
dane$hospdead <- as.factor(dane$hospdead) 
dane$diabetes <- as.factor(dane$diabetes) 
dane$dementia <- as.factor(dane$dementia) 
dane$sex <- ifelse(dane$sex=="male", 0, 1)
dane$sex <- as.factor(dane$sex) 
dane$dzgroup <- ifelse(dane$dzgroup=="Lung Cancer", 1, ifelse(dane$dzgroup=="Cirrhosis", 2, ifelse(dane$dzgroup=="Coma", 3, ifelse(dane$dzgroup=="CHF", 4, ifelse(dane$dzgroup=="Colon Cancer", 5, ifelse(dane$dzgroup=="COPD", 6, ifelse(dane$dzgroup=="MOSF w/Malig", 7, 8))))))) 
dane$dzgroup <- as.factor(dane$dzgroup) 
dane$dzclass <- ifelse(dane$dzclass=="Cancer", 1, ifelse(dane$dzclass=="ARF/MOSF", 2, ifelse(dane$dzclass=="Coma", 3, 4))) 
dane$dzclass <- as.factor(dane$dzclass) 
dane$income <- ifelse(dane$income=="under $11k", 1, ifelse(dane$income=="$11-$25k", 2, ifelse(dane$income=="$25-$50k", 3, ifelse(dane$income==">$50k", 4, NA)))) 
dane$income <- as.factor(dane$income) 
dane$race <- ifelse(dane$race=="white", 1, ifelse(dane$race=="black", 2, ifelse(dane$race=="asian", 3, ifelse(dane$race=="hispanic", 4, ifelse(dane$race=="other", 5, NA))))) 
dane$race <- as.factor(dane$race) 
dane$ca <- ifelse(dane$ca=="no", 0, ifelse(dane$ca=="yes", 1, 2)) 
dane$ca <- as.factor(dane$ca) 
dane$sfdm2 <- ifelse(dane$sfdm2=="<2 mo. follow-up", 0, ifelse(dane$sfdm2=="no(M2 and SIP pres)", 1, ifelse(dane$sfdm2=="SIP>=30", 2, ifelse(dane$sfdm2=="adl>=4 (>=5 if sur)", 3, ifelse(dane$sfdm2=="Coma or Intub", 4, NA))))) 
dane$sfdm2 <- as.factor(dane$sfdm2)
dane$dnr <- ifelse(dane$dnr=="no dnr", 0, 1)
dane$dnr <- as.factor(dane$dnr)
str(dane)
f <- c()
n <- c()
for(i in c(1:ncol(dane))){
  ifelse(class(dane[,i])=="factor", f[i] <- colnames(dane)[i], n[i] <- colnames(dane)[i])
}
n[!is.na(n)]
f[!is.na(f)]

h1 <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\history1_v2.rds")
h2 <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\history2_v2.rds")
h3 <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\history3_v2.rds")
h4 <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\history4_v2.rds")
h5 <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\history5_v2.rds")

m1 <- load_model_hdf5("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\model1_v2.h5")
m2 <- load_model_hdf5("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\model2_v2.h5")
m3 <- load_model_hdf5("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\model3_v2.h5")
m4 <- load_model_hdf5("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\model4_v2.h5")
m5 <- load_model_hdf5("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\model5_v2.h5")

train_features <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\train_features.rds")
train_targets <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\train_targets.rds")
test_features <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\test_features.rds")
test_targets <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\test_targets.rds")

df_test <- readRDS("C:\\Users\\wnadw\\Desktop\\Pliki studia\\Rok 4\\II stopień\\semestr 1\\Zaawansowane metody uczenia maszynowego\\projekt\\dane UCI\\multiclass\\support2\\multiclass\\df_test.rds")

#######################

server <- function(input, output) {
  #wstęp
  #opis badania
  output$opis_badania <- renderText(expr = "Projekt SUPPORT składał się z dwuletniego prospektywnego badania obserwacyjnego (Faza I), po którym nastąpiło dwuletnie kontrolowane badanie kliniczne (Faza II). Jego celem była poprawa procesu decyzyjnego, aby odpowiedzieć na rosnące krajowe obawy dotyczące utraty kontroli przez pacjentów pod koniec życia i zmniejszyć częstotliwość mechanicznego, bolesnego i długotrwałego procesu umierania. Wszyscy pacjenci w pięciu ośrodkach medycznych w Stanach Zjednoczonych spełnili kryteria włączenia i wykluczenia dla dziewięciu kategorii chorób: ostra niewydolność oddechowa, przewlekła obturacyjna choroba płuc, zastoinowa niewydolność serca, choroba wątroby, śpiączka, rak okrężnicy, rak płuc, niewydolność wielonarządowa z nowotworem złośliwym i niewydolność wielonarządowa z posocznicą. SUPPORT to połączenie pacjentów z dwóch badań, z których każde trwało 2 lata. Pierwsza faza dotyczyła 4 301 pacjentów, podczas gdy druga faza dotyczyła 4 804 pacjentów. Pod względem czasu, badania te zostały przeprowadzone w 1989 r. do 1991 r. dla fazy I oraz w 1992 r. do 1994 r.")
  
  #instrukcja
  output$instr <- renderText(expr = "Aplikacja składa się z czterech zakładek. Pierwsza stanowi część informacyjną o całym badaniu, zawiera instrukcję użytkowania oraz ma możliwość przesłania danych i ich wyświetlenia. Druga zakładka dotyczy eksploracyjnej analizy danych. W trzeciej zakładce jest możliwość wizualizacji danych pod kątem ich porównania względem wybranych cech. Czwarta zakładka dotyczy modeli uczenia maszynowego - po wyborze danej architektury, dokonywana jest jego ocena na danych testowych.")
  
  # Reaktywna ekspresja do przetwarzania przesłanych plików
  uploadedData <- reactive({
    # Wymaga, aby użytkownik przesłał plik
    req(input$file1)
    
    # Odczytanie danych z pliku
    inFile <- input$file1
    
    # Walidacja typu pliku - akceptujemy tylko pliki CSV
    validTypes <- c("text/csv", "text/comma-separated-values")
    if (!inFile$type %in% validTypes) {
      stop("Nieprawidłowy typ pliku! Prześlij plik CSV.")
    }
    
    if(str_detect(as.character(input$file1$name), pattern = "support2")==F){
      stop("Nie przesłałeś odpowiedniego pliku z danymi!")
    }
    
    # Przetwarzanie pliku - przykład odczytu pliku CSV
    tryCatch({
      read.csv(inFile$datapath)
    }, error = function(e) {
      stop("Wystąpił błąd podczas odczytu pliku: ", e$message)
    })
    
    
  })
  
  output$dane <- renderDataTable({
    uploadedData()
    
    req(input$file1)
    
    # Odczytanie danych z pliku
    inFile <- input$file1
    
    # Odczytanie danych z przesłanego pliku
    x <- read.csv(inFile$datapath, sep = ",")# |> head(4)
    x
  }, options = list(scrollX=T, server=F, pageLength=7, scrollY=T))
  
  
  output$wymiary <- renderText({
    tryCatch({
      c("Dane mają ", dim(uploadedData())[1], "wierszy oraz ", dim(uploadedData())[2], "kolumn.")
    }, error = function(e) {
      stop("Wczytaj plik z danymi, aby zobaczyć ich opis.")
    })
  })
  
  output$typy_zm <- renderText({
    tryCatch({
      c("Zmienne jakościowe w tym zbiorze danych to: ", 
        str_c(unlist(f[!is.na(f)])[1], unlist(f[!is.na(f)])[2], unlist(f[!is.na(f)])[3], unlist(f[!is.na(f)])[4], unlist(f[!is.na(f)])[5], unlist(f[!is.na(f)])[6], unlist(f[!is.na(f)])[7], unlist(f[!is.na(f)])[8], unlist(f[!is.na(f)])[9], unlist(f[!is.na(f)])[10], unlist(f[!is.na(f)])[11], unlist(f[!is.na(f)])[12], sep = ", "),
        ", a ilościowe to: ",
        str_sub(str_c(unlist(n[!is.na(n)])[1], unlist(n[!is.na(n)])[2], unlist(n[!is.na(n)])[3], unlist(n[!is.na(n)])[4], unlist(n[!is.na(n)])[5], unlist(n[!is.na(n)])[6], unlist(n[!is.na(n)])[7], unlist(n[!is.na(n)])[8], unlist(n[!is.na(n)])[9], unlist(n[!is.na(n)])[10], unlist(n[!is.na(n)])[11], unlist(n[!is.na(n)])[12], unlist(n[!is.na(n)])[13], unlist(n[!is.na(n)])[14], unlist(n[!is.na(n)])[15], unlist(n[!is.na(n)])[16], unlist(n[!is.na(n)])[17], unlist(n[!is.na(n)])[18], unlist(n[!is.na(n)])[19], unlist(n[!is.na(n)])[20], unlist(n[!is.na(n)])[21], unlist(n[!is.na(n)])[22], unlist(n[!is.na(n)])[23], unlist(n[!is.na(n)])[24], unlist(n[!is.na(n)])[25], unlist(n[!is.na(n)])[26], unlist(n[!is.na(n)])[27], unlist(n[!is.na(n)])[28], unlist(n[!is.na(n)])[29], unlist(n[!is.na(n)])[30], unlist(n[!is.na(n)])[31], unlist(n[!is.na(n)])[32], unlist(n[!is.na(n)])[33], unlist(n[!is.na(n)])[34], unlist(n[!is.na(n)])[35], sep = ", "), end = -1))
        }, error = function(e) {
      stop("Wczytaj plik z danymi, aby zobaczyć ich opis.")
    })
  })
  
  output$bd <- renderText({
    tryCatch({
      c("Liczba braków danych w tym zbiorze wynosi ", sum(is.na(uploadedData())), ".")
    }, error = function(e) {
      stop("Wczytaj plik z danymi, aby zobaczyć ich opis.")
    })
  })

output$statystyki_num <- renderTable({
  head(dane[,input$var_num])
}, digits = 4, striped = F, rownames = T, colnames = F)

output$rozklad_num <- renderPlot({
  zmienna <- reactive(input$var_num)
  #hist(as.numeric(dane |> pull(zmienna())))
  ggplot(data = dane, aes(x = dane[,zmienna()]))+
    geom_histogram(color = "white", fill = "#6BA8F5")+
    theme(axis.title.x = element_blank())
})

output$statystyki_fct <- renderTable({
  summary(dane[,input$var_fct])
}, digits = 4, striped = F, rownames = T, colnames = F)

output$rozklad_fct <- renderPlot({
  zmienna <- reactive(input$var_fct)
  #hist(as.numeric(dane[,as.character(zmienna())]))
  ggplot(data = dane, aes(x = dane[,zmienna()]))+
    geom_bar(fill = "#DD99EB")+
    theme(axis.title.x = element_blank())
})

output$bx_num <- renderPlot({
  zmienna <- reactive(input$var_num2)
  #hist(as.numeric(dane[,as.character(zmienna())]))
  ggplot(data = dane, aes(x = dane[,zmienna()])) + 
    geom_boxplot(fill = "#779D68")+
    theme(axis.title.x = element_blank())
})

output$cor <- renderTable({
  zm1 <- reactive(input$var_num2)
  zm2 <- reactive(input$var_num3)
  cor(dane[,zm1()], dane[,zm2()]) #są NA - zastanowić się
}, colnames = F)

#dzclass
output$gest1 <- renderPlot({
  zm1 <- reactive(input$zmienna1)
  tryCatch({
    ggplot(data = read.csv(input$file1$datapath, sep=","), aes(x = dane[,zm1()], fill=dane[,"dzclass"]))+
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
    ggplot(data = as.data.frame(read.csv(uploadedData(), sep=",")), aes(x = dane[,zm2()], fill=dane[,"ca"]))+
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
    ggplot(data = as.data.frame(read.csv(uploadedData(), sep=",")), aes(x = dane[,zm3()], fill=dane[,"diabetes"]))+
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
    ggplot(data = as.data.frame(read.csv(uploadedData(), sep=",")), aes(x = dane[,zm4()], fill=dane[,"dementia"]))+
      geom_density(alpha=0.5)+
      theme(axis.title = element_blank(), legend.title = element_blank())
  }, error = function(e) {
    stop("Musisz najpierw wczytać plik z danymi, aby zobaczyć wykres.")
  })
})

# output$opis1 <- renderText(expr = "dzclass oznacza kategorię choroby pacjenta: ostra niewydolność nerek/niewydolność wielonarządowa - zakodowane jako 1, przewlekła obturacyjna choroba płuc/zastoinowa niewydolność serca/marskość wątroby - zakodowane jako 2, rak - zakodowane jako 3, śpiączka - zakodowane jako 4.")
#  output$opis2 <- renderText(expr = "ca określa, czy pacjent ma raka - zakodowane jako 1, czy rak się rozprzestrzenił - zakodowane jako 2 lub czy jest zdrowy - zakodowane jako 0.")
#  output$opis3 <- renderText(expr = "diabetes informuje, czy pacjent choruje na cukrzycę - 0 jako nie oraz 1 jako tak.")
#  output$opis4 <- renderText(expr = "dementia informuje, czy pacjent choruje na demencję - 0 jako nie oraz 1 jako tak.")

output$info <- renderText(expr = "Każdy z modeli jest uczony na 40 epokach. Zbiór danych jest podzielony na uczący oraz testowy w proporcji 7:3. Zbiór walidacyjny stanowi 20% zbioru uczącego.")

output$history <- renderPlot({
  mod <- reactive(input$model)
  if(str_equal(mod(), "Model 1")==T) return(plot(h1))
  if(str_equal(mod(), "Model 2")==T) return(plot(h2))
  if(str_equal(mod(), "Model 3")==T) return(plot(h3))
  if(str_equal(mod(), "Model 4")==T) return(plot(h4))
  if(str_equal(mod(), "Model 5")==T) return(plot(h5))
})

output$metryki <- renderTable({
  mod <- reactive(input$model)
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
    p <- m4 %>% 
      evaluate(as.matrix(test_features), list(test_targets[, "death"],test_targets[, "hospdead"],test_targets[, grep("^sfdm2", colnames(test_targets))]))
    return(p)
  }
  if(str_equal(mod(), "Model 5")==T){
    p <- m5 %>% 
      evaluate(as.matrix(test_features), list(test_targets[, "death"],test_targets[, "hospdead"],test_targets[, grep("^sfdm2", colnames(test_targets))]))
    return(p)
  }
}, striped = T, colnames = F, digits = 4, rownames = T)

#death
output$cm1 <- renderPlot({
  mod <- reactive(input$model)
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
    x <- m4 %>% predict(as.matrix(test_features))
    cm <- data.frame(x[[1]] |> k_argmax() |> as.vector(), df_test$death)
    colnames(cm) <- c("pred", "truth")
    cm$pred <- factor(cm$pred, levels = c(0:1))
    return(cm |> conf_mat(truth = truth, estimate = pred) |> 
             autoplot(type = "heatmap"))
  }
  if(str_equal(mod(), "Model 5")==T){
    x <- m5 %>% predict(as.matrix(test_features))
    cm <- data.frame(x[[1]] |> k_argmax() |> as.vector(), df_test$death)
    colnames(cm) <- c("pred", "truth")
    cm$pred <- factor(cm$pred, levels = c(0:1))
    return(cm |> conf_mat(truth = truth, estimate = pred) |> 
             autoplot(type = "heatmap"))
  }
})

#hospdead
output$cm2 <- renderPlot({
  mod <- reactive(input$model)
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
    x <- m4 %>% predict(as.matrix(test_features))
    cm2 <- data.frame(x[[2]] |> k_argmax() |> as.vector(), df_test$hospdead)
    colnames(cm2) <- c("pred", "truth")
    cm2$pred <- factor(cm2$pred, levels = c(0:1))
    return(cm2 |> conf_mat(truth = truth, estimate = pred) |> 
             autoplot(type = "heatmap"))
  }
  if(str_equal(mod(), "Model 5")==T){
    x <- m5 %>% predict(as.matrix(test_features))
    cm2 <- data.frame(x[[2]] |> k_argmax() |> as.vector(), df_test$hospdead)
    colnames(cm2) <- c("pred", "truth")
    cm2$pred <- factor(cm2$pred, levels = c(0:1))
    return(cm2 |> conf_mat(truth = truth, estimate = pred) |> 
             autoplot(type = "heatmap"))
  }
})

#sfdm2
output$cm3 <- renderPlot({
  mod <- reactive(input$model)
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
    x <- m4 %>% predict(as.matrix(test_features))
    cm3 <- data.frame(x[[3]] |> k_argmax() |> as.vector(), df_test$sfdm2)
    colnames(cm3) <- c("pred", "truth")
    cm3$pred <- factor(cm3$pred, levels = c(0:4))
    return(cm3 |> conf_mat(truth = truth, estimate = pred) |> 
             autoplot(type = "heatmap"))
  }
  if(str_equal(mod(), "Model 5")==T){
    x <- m5 %>% predict(as.matrix(test_features))
    cm3 <- data.frame(x[[3]] |> k_argmax() |> as.vector(), df_test$sfdm2)
    colnames(cm3) <- c("pred", "truth")
    cm3$pred <- factor(cm3$pred, levels = c(0:4))
    return(cm3 |> conf_mat(truth = truth, estimate = pred) |> 
             autoplot(type = "heatmap"))
  }
})
}


shinyApp(ui = ui, server = server)

