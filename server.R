
options(scipen = 9999)
shinyServer(function(input,output,session){
  showNotification("Selamat datang, Stay Safe dan Tetap Patuhi Protokol Kesehatan yaaa", duration = NULL, type = "message",closeButton = TRUE)
# personal tab -----------------------------------------------------------------

  output$datainformation <-  renderUI(
    div(
      style ="text-align : justify",
      HTML("
        <style>
a:link {
  color: White;
  background-color: transparent;
  text-decoration: none;
}

a:visited {
  color: pink;
  background-color: transparent;
  text-decoration: none;
}

a:hover {
  color: red;
  background-color: transparent;
  text-decoration: underline;
}

a:active {
  color: yellow;
  background-color: transparent;
  text-decoration: underline;
}
        </style>

<h2> Data </h2>
<br>
<p><a href='#shiny-tab-side3'data-toggle='tab' data-value='side3' aria-expanded='true' tabindex='0' aria-selected='true'>Data </a> yang digunakan didapatkan dari <a href='https://www.kaggle.com/'>kaggle.com</a></p>
<p> last update : 28 Maret 2021 </p>
<p> Data berisi informasi berupa Kolom Date , Provinsi, Total Case, Total Death, Total Recovered, Total Active Case dan Population</p>
<p> Created by <a href=https://github.com/dedygs26>Dedy Sianipar</a> 5 Maret 2021 </p>


")))


  output$definition <- renderUI(
    div(
      style ="text-align : justify",
      HTML(
        "
        <style>
h2{text-align: center;font-family:verdana;}

h3{text-align: center;font-family:verdana;}

div.p{text-indent :50px;}

img{style='vertical-align:middle'}

p.aligncenter{text-align : center;}

a:link {
  color: White;
  background-color: transparent;
  text-decoration: none;
}

a:visited {
  color: pink;
  background-color: transparent;
  text-decoration: none;
}

a:hover {
  color: red;
  background-color: transparent;
  text-decoration: underline;
}

a:active {
  color: yellow;
  background-color: transparent;
  text-decoration: underline;
}
        </style>

      <TITLE> <CoronaVirus Disease-2019.> </TITLE>
          <h2> Corona Virus Disease-2019 </h2>

<p class='aligncenter'>
  <img src='https://iain-surakarta.ac.id/wp-content/uploads/2020/03/banner.png' alt='covid' height='100' width = '200' style='vertical-align:middle'>
</p>
        <br>
        <div class='p'>
<p> Coronavirus (CoV) adalah keluarga besar virus yang menyebabkan penyakit mulai dari gejala ringan sampai berat.
Ada setidaknya dua jenis coronavirus yang diketahui menyebabkan penyakit yang dapat menimbulkan gejala berat seperti <b>Middle East Respiratory Syndrome (MERSCoV)</b> dan <b>Severe Acute Respiratory Syndrome (SARS-CoV)</b>.
Virus corona adalah zoonosis (ditularkan antara hewan dan manusia).
Penelitian menyebutkan bahwa SARS-CoV ditransmisikan dari kucing luwak (civetcats) kemanusia dan MERS-CoV dari unta ke manusia.</p>
        </div>
        <hr>
        "
      )
    )
  )

  c19f<- reactive({
    req(input$provi)
    covid19fix %>%
      filter(Provinsi %in% c(input$provi)) %>%
      select(Total.Cases, Total.Recovered, Total.Deaths, Total.Active.Cases,Date,Provinsi) %>%
      pivot_longer(cols = c("Total.Cases","Total.Deaths","Total.Recovered","Total.Active.Cases"))
    })

  data_c19f <- reactive({
    req(input$boxcategory)
    c19f() %>%
      filter(name %in% input$boxcategory)
  })

  output$lineprovinsi <- renderPlotly({
    temp <- data_c19f() %>%
      # covid19fix %>%
      # filter(Provinsi %in% c(input$provi)) %>%
      # select(Total.Cases, Total.Recovered, Total.Deaths, Total.Active.Cases,Date,Provinsi) %>%
      # pivot_longer(cols = c("Total.Cases","Total.Deaths","Total.Recovered","Total.Active.Cases")) %>%
      # filter(name %in% input$boxcategory) %>%
      mutate(label = glue("
                      Provinsi: {Provinsi}
                      Date: {Date}
                      Jumlah: {value}")) %>%
      ggplot(aes(x= Date, y = value,fill = Provinsi, text=label))+
      geom_area(aes(group= name), alpha=0.8)+
      facet_wrap(~name, scales = "free_y", labeller = as_labeller(name),ncol = 2)+
      theme_dark()+
      labs(x="",y="")+
      theme(plot.background = element_rect(fill = '#000000',color = "red"))+
      theme(panel.background = element_rect(fill = '#000000', colour = 'red'))+
      theme(axis.text.x = element_text(color = "white"),
            axis.text.y = element_text(color = "white" ),
            axis.title.x = element_text(color = "white"),
            axis.title.y = element_text(color = "white"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())

    ggplotly(temp ,tooltip = "label") %>%
      layout(legend=list(orientation = "h", x =0.27,y =-0.2))
  })


# side 2------------------------------------------------------------------------
  p <- covid19fix %>%
    filter(Date == max(Date))

  labels <- paste0(p$Provinsi,
                  "<br> Total Cases : " , p$Total.Cases,
                  "<br> TotalDeaths : " , p$Total.Deaths,
                  "<br> Total Recovered: ",p$Total.Recovered,
                  "<br> Total Active Cases: ",p$Total.Active.Cases)

  # output$leaflet <- renderLeaflet({
  #   p %>%
  #     leaflet() %>%
  #     addTiles() %>%
  #     addCircleMarkers(
  #       lng = p$Longitude,
  #       lat =p$Latitude,
  #       popup = labels,
  #       fill = TRUE,
  #       color = "#03F",
  #       radius = 20,
  #       opacity = 0.5)
  # })

  output$mapindo <- renderHighchart({
    hcmap(
      map = "countries/id/id-all",
      data = map_indo,
      value = "kasus",
      name = "Confirmed Cases",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "black",
      borderWidth = 0.1,
      tooltip = list(valueDecimals = 0)
    ) %>%
      hc_add_theme(hc_theme_db()) %>%
      hc_colorAxis(minColor = "white",
                   maxColor = "red",
                   type = "logarithmic") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_title(text = "Mapping Cumulative Confirmed Cases by Country") %>%
      hc_subtitle(text = paste("Last Updated: 28 Maret 2021")) %>%
      hc_mapNavigation(enabled = TRUE)
  })

  output$mapindoA <- renderHighchart({
    hcmap(
      map = "countries/id/id-all",
      data = map_indoA,
      value = "kasus",
      name = "Kasus Aktif",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "black",
      borderWidth = 0.1,
      tooltip = list(valueDecimals = 0)
    ) %>%
      hc_add_theme(hc_theme_db()) %>%
      hc_colorAxis(minColor = "white",
                   maxColor = "blue",
                   type = "logarithmic") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_title(text = "Mapping Cumulative Active Cases by Country") %>%
      hc_subtitle(text = paste("Last Updated: 28 Maret 2021")) %>%
      hc_mapNavigation(enabled = TRUE)
  })

  output$mapindoR <- renderHighchart({
    hcmap(
      map = "countries/id/id-all",
      data = map_indoR,
      value = "kasus",
      name = "Selamat",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "black",
      borderWidth = 0.1,
      tooltip = list(valueDecimals = 0)
    ) %>%
      hc_add_theme(hc_theme_db()) %>%
      hc_colorAxis(minColor = "white",
                   maxColor = "green",
                   type = "logarithmic") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_title(text = "Mapping Cumulative Recovered Cases by Country") %>%
      hc_subtitle(text = paste("Last Updated: 28 Maret 2021")) %>%
      hc_mapNavigation(enabled = TRUE)
  })

  output$mapindoD <- renderHighchart({
    hcmap(
      map = "countries/id/id-all",
      data = map_indoD,
      value = "kasus",
      name = "Kematian",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "black",
      borderWidth = 0.1,
      tooltip = list(valueDecimals = 0)
    ) %>%
      hc_add_theme(hc_theme_db()) %>%
      hc_colorAxis(minColor = "white",
                   maxColor = "black",
                   type = "logarithmic") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_title(text = "Mapping Cumulative Death Cases by Country") %>%
      hc_subtitle(text = paste("Last Updated: 28 Maret 2021")) %>%
      hc_mapNavigation(enabled = TRUE)
  })
# side 3------------------------------------------------------------------------
output$covid19f <- renderDataTable({
  covid19fix %>%
    arrange(desc(Date)) %>%
    select(!c(Location.Level,Latitude,Longitude,Population.Density))
})

# output$Informasi <- renderUI(
#   div(HTML(
#     "
#     <p> Kolom Provinsi berisi informasi nama-nama provinsi di indonesia </p>
#     <p> Kolom </p>
#
#     "
#   ))
# )
})
















