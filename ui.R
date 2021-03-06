# header -----------------------------------------------------------------------
# header definition
header <-  dashboardHeader(
  title = "Capstone: Covid 19 Indonesia ",
  titleWidth = 300
)


# sidebar ----------------------------------------------------------------------
#sidebar definition
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Covid",
        tabName = "side1",
        badgeLabel = "demographics",
        badgeColor = "green",
        icon = icon("gears")
      ),

      menuItem(
        text = "Peta Persebaran",
        tabName = "side2",
        badgeLabel = "mapping",
        badgeColor = "blue",
        icon = icon("map-marked-alt")
      ),

      menuItem(
        text = "Data",
        tabName = "side3",
        badgeLabel = "raw data",
        badgeColor = "red",
        icon = icon("book-reader")
        ),
      menuItem(
        "About Me",
        icon = icon("user-circle"),
        #badgeLabel = "Profile",
        #badgeColor = "#f5ec42",
        menuSubItem("linkedin",
                    icon = icon("linkedin"),
                    href = "https://www.linkedin.com/in/dedy-sianipar/"),
        menuSubItem("Github",
                    icon = icon("github"),
                    href = "https://github.com/dedygs26"),
        menuSubItem("Rpubs",
                    icon = icon("r-project"),
                    href = "https://rpubs.com/dedygs26")
      )
  )

)
# body -------------------------------------------------------------------------
# body definition
body <- dashboardBody(
  shinyDashboardThemes(
    theme = "grey_dark"
  ),
  tabItems(
    tabItem(
      tabName = "side1",
      fluidPage(
      box(width = 12,
        tabsetPanel(
        type ="tabs",
        tabPanel("Tentang", box(htmlOutput("definition"),width = 6), box(htmlOutput("datainformation"))),
        tabPanel("Overview",
                 br(),
                 infoBox(width = 3,
                                 value = tags$p(style = "font-size: 14px;",comma(sum(
                                   Persen %>%
                                     pull(Total.Cases), na.rm = T))),
                                 title = tags$p(style = "font-size: 20px; text-transform: capitalize;", "Cases"),
                                 icon = icon("user-check"),
                                 color = "red",
                                 fill = TRUE
        ),
        infoBox(width = 3,
                value = tags$p(style = "font-size: 15px;",paste(comma(sum(
                  Persen %>%pull(Total.Active.Cases), na.rm = T)),
                  persenter(PTAC))),
                title = tags$p(style = "font-size: 19px; text-transform: capitalize;", "Active Cases"),
                icon = icon("user-tag"),
                color = "blue",
                fill = TRUE),
        infoBox(width = 3,
                value = tags$p(style = "font-size: 13px;",
                               paste(comma(sum(
                                 Persen %>%
                                   pull(Total.Recovered), na.rm = T)),
                                 persenter(PTR))),
                title = tags$p(style = "font-size: 20px; text-transform: capitalize;", "Recovered"),
                icon = icon("user-shield"),
                color = "green",
                fill = TRUE
        ),
        infoBox(width = 3,
                value = tags$p(style = "font-size: 15px;",comma(sum(
                  Persen %>%
                    pull(Total.Deaths), na.rm = T)),persenter(PTD)),
                title = tags$p(style = "font-size: 20px; text-transform: capitalize;", "Death"),
                icon = icon("user-slash"),
                color = "black",
                fill = TRUE
        ),
        box(

          selectInput("provi",
                      label = "Select Province",
                      multiple = TRUE,
                      choices = unique(covid19fix$Provinsi),
                      selected = c("DKI Jakarta","Sumatera Utara","Jawa Timur","Jawa Barat")
          ),

          title = 'Overview',
          width = 12,
          #background = "",

          checkboxGroupInput(inputId = "boxcategory",
                             label="Select Category",
                             choices = covid19fix %>%
                               select(Total.Cases,Total.Recovered,Total.Deaths,Total.Active.Cases) %>%
                               colnames(),
                             selected = "Total.Recovered",
                             inline = TRUE),
          plotlyOutput("lineprovinsi")

        )
        )
      )
      ))),
    tabItem(
      tabName = "side2",
      fluidRow(
        box(
          title = "Peta Kumulatif Covid di Indonesia",
          width = 12,

          # leafletOutput("leaflet"),
          highchartOutput(outputId = "mapindo", height = "600px"),
          highchartOutput(outputId = "mapindoA", height = "600px"),
          highchartOutput(outputId = "mapindoR", height = "600px"),
          highchartOutput(outputId = "mapindoD", height = "600px")
      )
      )
    ),
    tabItem(
      tabName = "side3",
      fluidRow(
        box(
          title = "Data Covid di Indonesia",
          width = 12,
          dataTableOutput("covid19f")
        )#,
        #box(width = 4,htmlOutput("Informasi"))
      )
    )
  )
)


#




# page definition
dashboardPage(
  header = header,
  body = body,
  sidebar = sidebar
  #skin = "black"
)







