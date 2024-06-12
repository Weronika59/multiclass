library(shiny)
library(tidyverse)
library(stringr)
library(ggplot2)
library(DT)
library(bslib)

num_vars <- c("age","slos", "d.time", "num.co", "edu", "scoma", "charges", "totcst", "totmcst", "avtisst", "hday", "meanbp", "wblc", "hrt", "resp", "temp", "pafi", "alb", "bili", "crea", "sod", "ph", "glucose", "bun", "urine", "adlp", "adls", "adlsc")
fct_vars <- c("death",    "sex" ,     "hospdead", "dzgroup",  "dzclass" , "income",   "race" ,    "diabetes", "dementia", "ca",       "dnr","sfdm2")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
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
               fileInput("file1", "Wybierz plik CSV:",accept = c(".csv"))
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
             textOutput(outputId = "wymiary"),
             textOutput(outputId = "typy_zm"),
             textOutput(outputId = "bd"),
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
             selectInput(inputId = "var_num2", label = "Wybierz zmienną numeryczną", choices = num_vars),
             selectInput(inputId = "var_num3", label = "Wybierz drugą zmienną do korelacji", choices = num_vars),
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
             fluidRow(
               column(6,
                      selectInput(inputId = "zmienna1", label = "Wybierz zmienną", choices = num_vars),
                      textOutput(outputId = "opis1"),
                      navset_card_underline(title = "Wykres gęstości w podziale na dzclass",
                                            nav_panel(title = "Wykres", plotOutput(outputId = "gest1"))
                      )
                    ),
               column(6,
                      selectInput(inputId = "zmienna2", label = "Wybierz zmienną", choices = num_vars),
                      textOutput(outputId = "opis2"),
                      navset_card_underline(title = "Wykres gęstości w podziale na ca", 
                                            nav_panel(title = "Wykres", plotOutput(outputId = "gest2"))
                      )
                    )
             ),
              fluidRow(
                column(6,
                       selectInput(inputId = "zmienna3", label = "Wybierz zmienną", choices = num_vars),
                       textOutput(outputId = "opis3"),
                       navset_card_underline(title = "Wykres gęstości w podziale na diabetes", 
                                             nav_panel(title = "Wykres", plotOutput(outputId = "gest3"))
                       )
                       ),
                column(6,
                       selectInput(inputId = "zmienna4", label = "Wybierz zmienną", choices = num_vars),
                        textOutput(outputId = "opis4"),
                        navset_card_underline(title = "Wykres gęstości w podziale na dementia", 
                        nav_panel(title = "Wykres", plotOutput(outputId = "gest4"))
                        )
                       )
              )
           )
      ),
    #modele i pred
    tabPanel("Modele i wyniki",
           
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

#######################

server <- function(input, output) {
  #wstęp
  #opis badania
  output$opis_badania <- renderText(expr = "Projekt SUPPORT składał się z dwuletniego prospektywnego badania obserwacyjnego (Faza I), po którym nastąpiło dwuletnie kontrolowane badanie kliniczne (Faza II). Jego celem była poprawa procesu decyzyjnego, aby odpowiedzieć na rosnące krajowe obawy dotyczące utraty kontroli przez pacjentów pod koniec życia i zmniejszyć częstotliwość mechanicznego, bolesnego i długotrwałego procesu umierania. W fazie I badania SUPPORT zebrano dane od pacjentów przyjętych w latach 1989-1991 w celu scharakteryzowania opieki, preferencji dotyczących leczenia i wzorców podejmowania decyzji wśród krytycznie chorych pacjentów. Interwencja została wdrożona w II fazie badania SUPPORT, do której pacjenci byli przyjmowani w latach 1992-1994. Interwencja fazy II dostarczyła lekarzom dokładnych informacji prognostycznych na temat przyszłej zdolności funkcjonalnej, prawdopodobieństwa przeżycia do sześciu miesięcy oraz preferencji pacjentów dotyczących opieki u schyłku życia. Wszyscy pacjenci w pięciu ośrodkach medycznych w Stanach Zjednoczonych spełnili kryteria włączenia i wykluczenia dla dziewięciu kategorii chorób: ostra niewydolność oddechowa, przewlekła obturacyjna choroba płuc, zastoinowa niewydolność serca, choroba wątroby, śpiączka, rak okrężnicy, rak płuc, niewydolność wielonarządowa z nowotworem złośliwym i niewydolność wielonarządowa z posocznicą. SUPPORT to połączenie pacjentów z dwóch badań, z których każde trwało 2 lata. Pierwsza faza dotyczyła 4 301 pacjentów, podczas gdy druga faza dotyczyła 4 804 pacjentów. Pod względem czasu, badania te zostały przeprowadzone w 1989 r. (12 czerwca) do 1991 r. (11 czerwca) dla fazy I oraz w 1992 r. (7 stycznia) do 1994 r. (24 stycznia).")
  #przerwa
  output$przerwa <- renderText(expr = "           ")
  #instrukcja
  output$instr <- renderText(expr = "Aplikacja składa się z czterech zakładek. Pierwsza stanowi część informacyjną o całym badaniu oraz zawiera instrukcję użytkowania. Druga zakładka prezentuje oryginalne dane poprzez podgląd oryginalnego pliku, wizualizacje rozkładów zmiennych oraz krótką ich charakterystykę. W trzeciej zakładce jest możliwość wizualizacji danych pod kątem ich porównania względem wybranych cech. Czwarta zakładka dotyczy modeli uczenia maszynowego - jest możliwość wyboru jednego z kilku i jego oceny na danych testowych.")
  
  
  output$dane <- renderDataTable({
    # Sprawdzenie, czy plik został przesłany
    req(input$file1)
    
    # Odczytanie pliku
    inFile <- input$file1
    
    # Odczytanie danych z przesłanego pliku
    x <- read.csv(inFile$datapath, sep = ",")# |> head(4)
    x
  }, server = FALSE)
  
  # Reaktywna ekspresja do przetwarzania przesłanych plików
  uploadedData <- reactive({
    # Wymaga, aby użytkownik przesłał plik
    req(input$file1)
    
    # Odczytanie danych z pliku
    inFile <- input$file1
    
    # Walidacja typu pliku - akceptujemy tylko pliki CSV
    validTypes <- c("text/csv", "text/comma-separated-values")
    if (!inFile$type %in% validTypes) {
      stop("Nieprawidłowy typ pliku. Proszę przesłać plik CSV.")
    }
    
    # Przetwarzanie pliku - przykład odczytu pliku CSV
    tryCatch({
      read.csv(inFile$datapath)
    }, error = function(e) {
      stop("Wystąpił błąd podczas odczytu pliku: ", e$message)
    })
  })
  
  output$wymiary <- renderText(expr = c("Dane mają ", dim(read.csv(uploadedData(), sep = ","))[1], "wierszy oraz ", dim(read.csv(uploadedData(), sep = ","))[2], "kolumn."))
  output$typy_zm <- renderText(expr = c("Zmienne jakościowe w tym zbiorze danych to: ", str_c(unlist(f[!is.na(f)]), sep = ","), ", a ilościowe to: ", unlist(n[!is.na(n)]), "."))
  output$bd <- renderText(expr = c("Liczba braków danych w tym zbiorze wynosi ", sum(is.na(uploadedData())), "."))
  
  output$statystyki_num <- renderTable({
    head(dane[,input$var_num])
  }, digits = 4, striped = F, rownames = T, colnames = F)
  
  output$rozklad_num <- renderPlot({
    zmienna <- reactive(input$var_num)
    #hist(as.numeric(dane |> pull(zmienna())))
    ggplot(data = dane, aes(x = dane[,zmienna()]))+
      geom_histogram(color = "white", fill = "#6BA8F5")
  })
  
  output$statystyki_fct <- renderTable({
    summary(dane[,input$var_fct])
  }, digits = 4, striped = F, rownames = T, colnames = F)
  
  output$rozklad_fct <- renderPlot({
    zmienna <- reactive(input$var_fct)
    #hist(as.numeric(dane[,as.character(zmienna())]))
    ggplot(data = dane, aes(x = dane[,zmienna()]))+
      geom_bar(fill = "#DD99EB")
  })
  
  output$bx_num <- renderPlot({
    zmienna <- reactive(input$var_num2)
    #hist(as.numeric(dane[,as.character(zmienna())]))
    ggplot(data = dane, aes(x = dane[,zmienna()])) + 
      geom_boxplot(fill = "#779D68")
  })
  
  output$cor <- renderTable({
    zm1 <- reactive(input$var_num2)
    zm2 <- reactive(input$var_num3)
    cor(dane[,zm1()], dane[,zm2()]) #są NA - zastanowić się
  }, colnames = F)
  
  #dzclass
  output$gest1 <- renderPlot({
    zm1 <- reactive(input$zmienna1)
    ggplot(data = dane, aes(x = dane[,zm1()], fill=dane[,"dzclass"]))+
      geom_density(alpha=0.5)
  })
  
  #ca
  output$gest2 <- renderPlot({
    zm2 <- reactive(input$zmienna2)
    ggplot(data = dane, aes(x = dane[,zm2()], fill=dane[,"ca"]))+
      geom_density(alpha=0.5)+
      scale_fill_brewer(palette="Set2")
  })
  
  #diabetes
  output$gest3 <- renderPlot({
    zm3 <- reactive(input$zmienna3)
    ggplot(data = dane, aes(x = dane[,zm3()], fill=dane[,"diabetes"]))+
      geom_density(alpha=0.5)+
      scale_fill_brewer(palette="Set3")
  })
  
  #dementia
  output$gest4 <- renderPlot({
    zm4 <- reactive(input$zmienna4)
    ggplot(data = dane, aes(x = dane[,zm4()], fill=dane[,"dementia"]))+
      geom_density(alpha=0.5)
  })
  
  output$opis1 <- renderText(expr = "dzclass oznacza kategorię choroby pacjenta: ostra niewydolność nerek/niewydolność wielonarządowa - zakodowane jako 1, przewlekła obturacyjna choroba płuc/zastoinowa niewydolność serca/marskość wątroby - zakodowane jako 2, rak - zakodowane jako 3, śpiączka - zakodowane jako 4.")
  output$opis2 <- renderText(expr = "ca określa, czy pacjent ma raka - zakodowane jako 1, czy rak się rozprzestrzenił - zakodowane jako 2 lub czy jest zdrowy - zakodowane jako 0.")
  output$opis3 <- renderText(expr = "diabetes informuje, czy pacjent choruje na cukrzycę - 0 jako nie oraz 1 jako tak.")
  output$opis4 <- renderText(expr = "dementia informuje, czy pacjent choruje na demencję - 0 jako nie oraz 1 jako tak.")
  
}


shinyApp(ui = ui, server = server)

