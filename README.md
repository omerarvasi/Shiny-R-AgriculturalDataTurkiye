# RshinyAgriculturalData
Bu R Shiny uygulaması ile Türkiye'nin 2023 yılına ait bazı tarim verilerini şehirlere göre harita üzerinde görüntüleyebilir, harita olarak ya da excel şeklide indirebilirsiniz.
Kodlar aşağıda yer almaktadır.

#Aşağıda yer alan tüm kodları  RStudio'da bir sayda açarak kopyalayıp yapıştırın.
#Aşağıda yazan kütüphanelerin yüklenmesi gerekir.
#Yüklü değilse install.packages(c("shiny", "sf", "tmap","readxl", "dplyr", "DT", "openxlsx")) kmutu ile yüklenebilir.
library(shiny)
library(sf)
library(tmap)
library(readxl)
library(dplyr)
library(DT)
library(openxlsx)

# 1. Shapefile'ı yükleyin (Türkiye şehir sınırları)
#https://www.geoboundaries.org/countryDownloads.html adresinden Turkey TUR ADM1 dosyası indirilip zip’ten çıkarılır.
#Harita dosyasının yeri (geoBoundaries-TUR-ADM1.shp) bulunarak ... olan kısım dosya yolu olarak yazılır.... yazan yer
turkiye_shp <- st_read("C:/.../.../geoBoundaries-TUR-ADM1-all/geoBoundaries-TUR-ADM1.shp ")

# 2. Excel dosyasını yükleyin (şehir isimlerine göre)
#Veriler dosyası indirilir ve ... olan kısım dosya yolu olarak yazılır.... yazan yer 
data <- read_excel("C:/.../veriler.xlsx")

# 3. Veriyi shapefile ile birleştirin
turkiye_data <- turkiye_shp %>%
  left_join(data, by = c("shapeName" = "sehir"))

# 4. Shiny UI kısmı
ui <- fluidPage(
  titlePanel("2023 Yılı Tarım Verileri"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Görselleştirmek istediğiniz değişkeni seçin:", 
                  choices = colnames(data)[-1]),  # Sadece sayısal kolonları seçenek olarak sunar
      h4("Seçilen veriye göre şehirler renklendirilecektir."),
      downloadButton("downloadMap", "Haritayı İndir (PNG)"),
      downloadButton("downloadTable", "Tabloyu İndir (Excel veya CSV)")
    ),
    mainPanel(
      tmapOutput("map"),    # Harita çıktısı
      br(),
      DTOutput("table")     # Tablo çıktısı
    )
  )
)

# 5. Shiny Server kısmı
server <- function(input, output, session) {
  
  # Dinamik harita oluşturma
  output$map <- renderTmap({
    # Şehir ismi ve değişkenin iki satır halinde gösterimi için yeni bir sütun oluştur (binlik ayıracı ile)
    turkiye_data <- turkiye_data %>%
      mutate(label = paste0(format(.data[[input$variable]], big.mark = ".", decimal.mark = ",")))  # Binlik ayıracı
    
    tm_shape(turkiye_data) +
      tm_polygons(input$variable, palette = "YlOrRd", title = input$variable, style = "quantile") +  # Renk paleti ve lejant
      tm_borders("black", lwd = 1) +  # Sınırların belirgin olması için siyah kenarlık
      tm_text("label", size = 0.8, col = "black", shadow = TRUE, just = "center") +  # Şehir ismi ve değer etiketleri
      tm_layout(
        legend.outside = TRUE,
        legend.format = list(big.mark = ".", decimal.mark = ",", text.separator = " - ")  # Lejantta binlik ayıracı ve "-" kullanımı
      )
  })
  
  # Dinamik tablo oluşturma
  output$table <- renderDT({
    turkiye_data %>%
      st_set_geometry(NULL) %>%  # Geometrik verileri tablodan çıkartıyoruz
      select(shapeName, input$variable) %>%  # Şehir ismi ve seçilen veri
      rename("sehir" = shapeName, "Deger" = input$variable) %>%
      mutate(Deger = format(Deger, big.mark = ".", decimal.mark = ","))  # Tabloda da binlik ayıracı ekleniyor
  }, options = list(pageLength = 10, autoWidth = TRUE))  # Tablo seçenekleri (örneğin, 10 satır gösterme)
  
  # Haritayı PNG olarak indirme
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste("turkiye_haritasi_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Şehir ismi ve değişkeni içeren etiket sütununu oluştur (binlik ayıracı ile)
      turkiye_data <- turkiye_data %>%
        mutate(label = paste0(format(.data[[input$variable]], big.mark = ".", decimal.mark = ",")))  # Etiketler
      
      # Haritayı png olarak kaydetmek
      tmap_save(
        tm_shape(turkiye_data) +
          tm_polygons(input$variable, palette = "YlOrRd", title = input$variable, style = "quantile") +
          tm_borders("black", lwd = 1) +
          tm_text("label", size = 0.5, col = "black", shadow = FALSE, just = "center") +  # Şehir ismi ve değer etiketleri
          tm_layout(
            legend.outside = TRUE,
            legend.format = list(big.mark = ".", decimal.mark = ",", text.separator = " - ")  # Lejantta binlik ayıracı ve "-" kullanımı
          ),
        file = file
      )
    }
  )
  
  # Tabloyu Excel veya CSV olarak indirme
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("turkiye_sehir_verileri_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Tablodaki verileri al
      df <- turkiye_data %>%
        st_set_geometry(NULL) %>%  # Geometrik verileri tablodan çıkartıyoruz
        select(shapeName, input$variable) %>%  # Şehir ismi ve seçilen veri
        rename("sehir" = shapeName, "Deger" = input$variable) %>%
        mutate(Deger = format(Deger, big.mark = ".", decimal.mark = ","))  # Excel veya CSV'de binlik ayıracı
      
      # Excel veya CSV olarak kaydet
      if (grepl("\\.xlsx$", file)) {
        write.xlsx(df, file)  # Excel formatında kaydet
      } else {
        write.csv(df, file)   # CSV formatında kaydet
      }
    }
  )
}

# 6. Shiny uygulamasını başlatma
shinyApp(ui = ui, server = server)
