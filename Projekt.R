# Kacper Urbański

# Import potrzebnych bibliotek
library(shiny)      # biblioteka do tworzenia aplikacji shiny
library(tidyverse)  # zbiór pakietów do przetwarzania, manipulacji i wizualizacji danych
library(forecast)   # biblioteka służąca do modelowania szeregów czasowych
library(ggplot2)    # tworzenie zaawansowanych wykresów
library(seasonal)   # analiza i usuwanie sezonowości z szeregów czasowych
library(e1071)      # pakiet zawierający kurtozę


# Ścieżka do pliku danych csv
file_path <- "dane1.csv"

# Interfejs Użytkownika
ui <- fluidPage(
  titlePanel("Analiza danych walutowych"), # tytuł aplikacji
  sidebarLayout(  
    sidebarPanel( # Dynamiczne selektory ( elementy interaktywne )
      uiOutput("currency_selector"), 
      conditionalPanel(
        condition = "input.tabs == 'Korelacja'", # Widoczność tylko w zakładce "korelacja"
        uiOutput("currency_correlation_selector")
      ),
      conditionalPanel(
        condition = "input.tabs != 'Analiza czasowa'", # Ukrycie selektora roku w zakładce "analiza czasowa"
        uiOutput("year_selector")
      )
    )
    
# Panel główny, wyświetlana treść aplikacji  
    ,
    mainPanel(
      tabsetPanel( # Panel z zakładkami
        id = "tabs", # Identyfikator umożliwiający śledzenie aktywnej zakładki
        tabPanel("Analiza opisowa", tableOutput("summary")),
        tabPanel("Analiza czasowa", plotOutput("time_analysis")),
        tabPanel("Wykres pudełkowy z wąsem", plotOutput("currency_comparison")),
        tabPanel("Analiza zmienności", plotOutput("volatility_analysis")),
        tabPanel("Korelacja", uiOutput("correlation_analysis")),
        tabPanel("Prognozowanie", plotOutput("forecast_analysis")),
        tabPanel("Sezonowość", plotOutput("seasonality_analysis")),
        tabPanel("Słownik", tableOutput("dictionary"))
      )
      

  )))


# Logika serwera
server <- function(input, output, session) {
  
# Wczytywanie danych z pliku i ich przekształcanie
  data <- reactive({
    req(file.exists(file_path)) # Sprawdzenie, czy plik istnieje
    data_read <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE) %>%
      pivot_longer( # Przekształcenie danych do formatu długiego
        cols = starts_with("styczeń"):starts_with("grudzień"),
        names_to = "Miesiac",
        values_to = "Kurs_sredni"
      ) %>%
      mutate(
        Miesiac = case_when( # Zmiana nazw miesięcy na format numeryczny
          Miesiac == "styczeń" ~ "01",
          Miesiac == "luty" ~ "02",
          Miesiac == "marzec" ~ "03",
          Miesiac == "kwiecień" ~ "04",
          Miesiac == "maj" ~ "05",
          Miesiac == "czerwiec" ~ "06",
          Miesiac == "lipiec" ~ "07",
          Miesiac == "sierpień" ~ "08",
          Miesiac == "wrzesień" ~ "09",
          Miesiac == "październik" ~ "10",
          Miesiac == "listopad" ~ "11",
          Miesiac == "grudzień" ~ "12",
          TRUE ~ Miesiac
        ),
        Kurs_sredni = as.numeric(Kurs_sredni)
      )
    
    return(data_read) # Zwracanie przekształconych danych
  })
  
# Dynamiczne generowanie selektora walut na podstawie danych    
  output$currency_selector <- renderUI({
    req(data()) # Sprawdzenie, czy dane zostały załadowane
    
    if (input$tabs == "Korelacja" || input$tabs == "Słownik") {
      return(NULL) # Brak selektora waluty w zakładce Korelacja i Słownik
    }
    
# W zakładce "Analiza czasowa" usuń możliwość wyboru "Wszystkie waluty"
    if (input$tabs == "Analiza czasowa" || input$tabs == "Analiza zmienności" ||  input$tabs == "Prognozowanie" ||  input$tabs == "Sezonowość") {
      selectInput(
        "currency",
        "Wybierz walutę:",
        choices = unique(data()$kod),
        selected = unique(data()$kod)[1] # Domyślny wybór pierwszej pozycji
      )
    } else {
# W innych zakładkach możliwość wyboru "Wszystkie waluty" pozostaje
      selectInput(
        "currency",
        "Wybierz walutę:",
        choices = c("Wszystkie waluty", unique(data()$kod)),
        selected = "Wszystkie waluty"
      )
    }
  })
  
  
# Generowanie selektora roku
    output$year_selector <- renderUI({
    req(data()) # Sprawdzenie, czy dane zostały załadowane
    
    
    
# Sprawdzenie, czy zakładka nie jest jedną z tych, w której selektor roku jest ukryty
    if (input$tabs != "Analiza zmienności" & input$tabs != "Korelacja" & input$tabs != "Prognozowanie"  & input$tabs != "Sezonowość" & input$tabs != "Słownik"  ) {
      selectInput(
        "year",
        "Wybierz rok:",
        choices = c("Wszystkie lata", unique(data()$Rok)),
        selected = "Wszystkie lata" # Domyślne ustawienie "Wszystkie lata"
      )
    } else {
# Gdy aktywna jest ktoraś z wyżej wymienionych zakładek, nie wyświetla się selektor
      NULL
    }
  })
  
  
# Generowanie selektora walut do analizy korelacji
  output$currency_correlation_selector <- renderUI({
    req(data()) 
    selectInput("currency_correlation", "Wybierz waluty do korelacji:", choices = unique(data()$kod), multiple = TRUE) # Możliwość wyboru wielu walut
  })
  
# ANALIZA OPISOWA: Tabela z podstawowymi statystykami opisowymi
  output$summary <- renderTable({
    req(data(), input$currency, input$year)  # Sprawdzenie, czy potrzebne dane zostały załadowane
    
    summary_data <- data()
 
# Filtrowanie danych po walucie, jeśli została wybrana   
    if (input$currency != "Wszystkie waluty") {
      summary_data <- summary_data %>% filter(kod == input$currency)
    }
# Filtrowanie danych po roku, jeśli został wybrany   
    if (input$year != "Wszystkie lata") {
      summary_data <- summary_data %>% filter(Rok == input$year)
    }
# Obliczanie statystyk opisowych    
    summary_data %>%
      group_by(kod, Rok) %>%
      summarise(
        Sredni_kurs = mean(Kurs_sredni, na.rm = TRUE),
        Mediana = median(Kurs_sredni, na.rm = TRUE),
        Odchylenie_std = sd(Kurs_sredni, na.rm = TRUE),
        Min = min(Kurs_sredni, na.rm = TRUE),
        Max = max(Kurs_sredni, na.rm = TRUE),
        Rozstep = Max - Min,
        Kurtoza = kurtosis(Kurs_sredni, na.rm = TRUE),
        .groups = 'drop'
      )
  })
  
# ANALIZA CZASOWA
  
  output$time_analysis <- renderPlot({
    req(data(), input$currency)
# Filtr danych dla wybranej waluty i utworzenie daty na podstawie roku i miesiąca
    currency_data <- data() %>%
      filter(kod == input$currency | input$currency == "Wszystkie waluty") %>%
      mutate(Data = as.Date(paste(Rok, Miesiac, "01", sep = "-"), format = "%Y-%m-%d"))

# Generowanie wykresu 
    ggplot(currency_data, aes(x = Data, y = Kurs_sredni)) +
      geom_line() +
      labs(title = paste("Przebieg kursu w czasie:", input$currency), x = "Data", y = "Kurs średni") +
      theme_minimal()
  })
  
# WYKRES PUDEŁKOWY Z WĄSEM
    output$currency_comparison <- renderPlot({
    req(data(), input$currency, input$year) 
    
# Filtrowanie danych na podstawie roku
    comparison_data <- data() %>%
      filter(Rok == input$year | input$year == "Wszystkie lata")
    
# Filtrowanie danych na podstawie wybranej waluty, jeżeli taką wybrano
    if (input$currency != "Wszystkie waluty") {
      comparison_data <- comparison_data %>% filter(kod == input$currency)
    }
    
# Generowanie wykresu
    ggplot(comparison_data, aes(x = kod, y = Kurs_sredni)) +
      geom_boxplot() +
      labs(
        title = ifelse(input$currency == "Wszystkie waluty", 
                       paste("Porównanie kursów walut w roku", input$year), 
                       paste("Porównanie kursu waluty:", input$currency, "w roku", input$year)),
        x = "Waluta", 
        y = "Kurs średni"
      ) +
      theme_minimal()
  })
  
# ANALIZA ZMIENNOŚCI
  output$volatility_analysis <- renderPlot({
    req(data(), input$currency)
    currency_data <- data() %>%
      filter(kod == input$currency | input$currency == "Wszystkie waluty")

# generowanie wykresu podobnie jak w przypadku analizy czasowej. Dodanie jedynie krzywej wygładzonej,
# co pomaga uchwycić trend
    ggplot(currency_data, aes(x = as.Date(paste(Rok, Miesiac, "01", sep = "-")), y = Kurs_sredni)) +
      geom_line(color = "blue") +
      geom_smooth(method = "loess", color = "red") +
      labs(title = paste("Analiza zmienności kursu:", input$currency), x = "Data", y = "Kurs średni") +
      theme_minimal()
  })
  
# KORELACJA
  output$correlation_analysis <- renderUI({
    req(data())
# Sprawdzenie, czy co najmniej dwie waluty są wybrane. Jeśli nie, zostaje wyświetlony komunikat
    if (is.null(input$currency_correlation) || length(input$currency_correlation) < 2) {
      return(HTML("<p>Wybierz co najmniej dwie waluty.</p>"))
    } else {
# Wyświetlanie wykresu, w przeciwnym wypadku
      plotOutput("correlation_plot")
    }
  })
  
# Generowanie wykresu korelacji
  output$correlation_plot <- renderPlot({
    req(input$currency_correlation)
    if (length(input$currency_correlation) < 2) {
      return(NULL)
    }
    
# Filtrowanie danych, aby uzyskać tylko te wybrane, a następnie przekształcenie
# ich na szeroki format
    correlation_data <- data() %>%
      filter(kod %in% input$currency_correlation) %>%
      select(Rok, Miesiac, kod, Kurs_sredni) %>%
      pivot_wider(names_from = kod, values_from = Kurs_sredni) %>%
      select(-Rok, -Miesiac)
    
    correlation_data <- na.omit(correlation_data)
    
    numeric_data <- correlation_data %>%
      mutate(across(everything(), as.numeric))
# Obliczenie macierzy korelacji dla wybranych walut
    correlation_matrix <- cor(numeric_data, use = "complete.obs")
    
# Generowanie wykresu
    ggplot(as.data.frame(as.table(correlation_matrix)), aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
      labs(title = "Macierz korelacji kursów walut.\n W celu usunięcia wybranych walut użyj klawisza backspace", x = "Waluta", y = "Waluta") +
      theme_minimal()
  })
  
  
# PROGNOZOWANIE
  
  output$forecast_analysis <- renderPlot({
    req(data(), input$currency)
    currency_data <- data() %>%
      filter(kod == input$currency) %>%
      group_by(Rok, Miesiac) %>%
      summarise(Kurs_sredni = mean(Kurs_sredni, na.rm = TRUE), .groups = 'drop') %>%
      arrange(Rok, Miesiac)
# Użycie funkcji ARIMA do dopasowania modelu prognozowania na podstawie danych kursów walut   
    ts_data <- ts(currency_data$Kurs_sredni, frequency = 12, start = c(min(currency_data$Rok), 1))
    fit <- auto.arima(ts_data)
# Prognoza następnych 12 miesięcy   
    forecast_data <- forecast(fit, h = 12)
# Generowanie wykresu
    autoplot(forecast_data) +
      labs(title = paste("Prognoza kursu:", input$currency), x = "Czas", y = "Prognozowany kurs") +
      theme_minimal()
  })
  
# SEZONOWOŚĆ

# instalacja pakietu gridExtra, jeżeli nie jest on zainstalowany
  if (!require(gridExtra)) {
    install.packages("gridExtra")
  }
# Ładowanie pakietu
  library(gridExtra)
  output$seasonality_analysis <- renderPlot({
    req(data(), input$currency)
# Filtrowanie danych
    currency_data <- data() %>% 
      filter(kod == input$currency | input$currency == "Wszystkie waluty") %>%
      mutate(Data = as.Date(paste(Rok, Miesiac, "01", sep = "-"), format = "%Y-%m-%d"))
    
# Tworzenie obiektu szeregu czasowego z dostępnych danych
    ts_data <- ts(currency_data$Kurs_sredni, frequency = 12, start = c(min(currency_data$Rok), 1))
    
# Przeprowadzenie dekompozycji sezonowej
    decomposed <- stl(ts_data, s.window = "periodic")
    
# Wydzielenie komponentów dekompozycji
    trend_component <- decomposed$time.series[, "trend"]
    seasonal_component <- decomposed$time.series[, "seasonal"]
    residual_component <- decomposed$time.series[, "remainder"]
    
# Zrobienie wykresów bez prognoz na przyszłość
    plot_data <- data.frame(
      Data = time(ts_data),
      Trend = trend_component,
      Seasonal = seasonal_component,
      Residual = residual_component,
      Actual = currency_data$Kurs_sredni  # Dodajemy rzeczywiste dane waluty
    )
    
#  Generowanie wykresów
    p1 <- ggplot(plot_data, aes(x = Data)) +
      geom_line(aes(y = Trend), color = "blue") +
      labs(title = paste("Trend komponentu - Analiza sezonowości:", input$currency), x = "Czas", y = "Kurs średni") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::comma) +
      xlim(c(min(plot_data$Data), max(plot_data$Data)))
    
    # Drugi wykres: Sezonowość
    p2 <- ggplot(plot_data, aes(x = Data)) +
      geom_line(aes(y = Seasonal), color = "green") +
      labs(title = paste("Komponent sezonowy - Analiza sezonowości:", input$currency), x = "Czas", y = "Kurs średni") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::comma) +
      xlim(c(min(plot_data$Data), max(plot_data$Data))) 
    
    # Trzeci wykres: Reszty
    p3 <- ggplot(plot_data, aes(x = Data)) +
      geom_line(aes(y = Residual), color = "red") +
      labs(title = paste("Reszty komponentu - Analiza sezonowości:", input$currency), x = "Czas", y = "Kurs średni") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::comma) +
      xlim(c(min(plot_data$Data), max(plot_data$Data)))
    
    # Czwarty wykres: Rzeczywiste dane waluty
    p4 <- ggplot(plot_data, aes(x = Data)) +
      geom_line(aes(y = Actual), color = "purple") +
      labs(title = paste("Rzeczywisty kurs waluty:", input$currency), x = "Czas", y = "Kurs średni") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::comma) +
      xlim(c(min(plot_data$Data), max(plot_data$Data)))
    
    # Połączenie wykresów i wspólne wyświetlenie
    grid.arrange(p1, p2, p3, p4, ncol = 1)
  })
  
# SŁOWNIK WALUTOWY
  output$dictionary <- renderTable({
    data.frame(
      Kod = c("AUD","BGN","BRL","CAD","CHF","CLP","CNY","CZK","DKK","EUR","GBP","HKD","HUF","IDR","ILS","INR","ISK","JPY","KRW","MXN","MYR","NOK","NZD","PHP","RON","SEK","SGD","THB","TRY","UAH","USD","XDR","ZAR"),
      Waluta = c("Dolar australijski", "Lew bułgarski", "Real brazylijski", "Dolar kanadyjski", "Frank szwajcarski", "Peso chilijskie", "Juan chiński", "Korona czeska", "Korona duńska", "Euro", "Funt brytyjski", "Dolar Hongkongu", "Forint węgierski", "Rupia indonezyjska", "Szekel izraelski", "Rupia indyjska", "Korona islandzka", "Jen japoński", "Won południowokoreański", "Peso meksykańskie", "Ringgit malezyjski", "Korona norweska", "Dolar nowozelandzki", "Peso filipińskie", "Lej rumuński", "Korona szwedzka", "Dolar singapurski", "Baht tajski", "Lira turecka", "Hrywna ukraińska", "Dolar amerykański", "Specjalne prawo ciągnienia", "Rand południowoafrykański"
      )
    )
  })
  
  
}

# Uruchomienie aplikacji shiny
shinyApp(ui = ui, server = server)



