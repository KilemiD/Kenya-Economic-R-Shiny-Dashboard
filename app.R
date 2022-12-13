#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(purrr)
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(shinyWidgets)
library(scales)
library(tidyverse)
library(countrycode)
library(ggimage)

library(tidyr)
library(DBI)
library(RMySQL)
library(shiny)

PARS <- list(
    debug = FALSE,
    classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
    sparkline_color = "#333333",
    font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

options(
    highcharter.google_fonts = FALSE,
    highcharter.debug = PARS$debug,
    # shiny.launch.browser = PARS$debug,
    highcharter.theme = 
        hc_theme_smpl(
            title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
            subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
            chart = list(
                backgroundColor = "transparent",
                style = list(fontFamily = PARS$font, fontSize = "1.0em")
            ),
            plotOptions = list(
                series = list(
                    dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
                    animation = list(duration = 3000)
                )
            ),
            legend = list(
                itemStyle =  list(
                    fontWeight = "normal"
                )
            )
        )
)

dropdownButtonp <- purrr::partial(
    dropdownButton,
    status = "customstatus",
    size = "sm",
    right = TRUE,
    status = "info",
    width = "400px",
    inline = TRUE,
)
#spark


#theme
hc_theme_sparkline_vb <- function(...) {
    
    theme <- list(
        chart = list(
            backgroundColor = NULL,
            margins = c(0, 0, 0, 0),
            spacingTop = 0,
            spacingRight = 0,
            spacingBottom = 0,
            spacingLeft = 0,
            plotBorderWidth = 0,
            borderWidth = 0,
            style = list(overflow = "visible")
        ),
        xAxis = list(
            visible = F, 
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        yAxis = list(
            visible = F,
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        tooltip = list(
            outside = FALSE,
            shadow = FALSE,
            borderColor = "transparent",
            botderWidth = 0,
            backgroundColor = "transparent",
            style = list(textOutline = "5px white")
        ),
        plotOptions = list(
            series = list(
                marker = list(enabled = FALSE),
                lineWidth = 2,
                shadow = FALSE,
                fillOpacity = 0.0,
                color = "#FFFFFFBF",
                fillColor = list(
                    linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
                    stops = list(
                        list(0.00, "#FFFFFF00"),
                        list(0.50, "#FFFFFF7F"),
                        list(1.00, "#FFFFFFFF")
                    )
                )
            )
        ),
        credits = list(
            enabled = FALSE,
            text = ""
        )
    )
    theme <- structure(theme, class = "hc_theme")
    
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(
            theme,
            hc_theme(...)
        )
    }
    
    theme
}

valueBoxSpark <- function(value, subtitle, icon = NULL, color = "aqua", 
                          width = 4, href = NULL, spark = NULL, height_spark = "150px",minititle = NULL) {
    
    shinydashboard:::validateColor(color)
    
    if (!is.null(icon)) 
        shinydashboard:::tagAssert(icon, type = "i")
    
    boxContent <- div(
        class = paste0("small-box bg-", color),
        div(
            class = "inner",
            if(!is.null(minititle)) tags$small(minititle),
            h3(value),
            # tags$span(style = paste0("height:", height_spark), hc_size(spark, height = "100vh")),
            tags$span(hc_size(spark, height = height_spark)),
            if (!is.null(subtitle)) p(subtitle)
        ),
        if (!is.null(icon)) div(class = "icon-large", icon)
    )
    
    if (!is.null(href)) 
        boxContent <- a(href = href, boxContent)
    
    div(class = if (!is.null(width)) 
        paste0("col-sm-", width), boxContent)
}
#gdp
sector_data=read.csv("sector.csv")
industry_data=read.csv("industry.csv",header = T,
                       check.names=F,stringsAsFactors = T)

industry=industry_data%>%
    pivot_longer(!Industry, names_to = "year", values_to = "gdp")

industry$year=as.numeric(industry$year)

#gdp contribution
gdp_contribution=read.csv("gdp contribution.csv",header = T,
                          check.names=F,stringsAsFactors = T)
contribution=gdp_contribution%>%
    pivot_longer(!Industry, names_to = "year", values_to = "gdp_contribution")
contribution$year=as.numeric(contribution$year)

#growth
gdp_growth=read.csv("gdp growth.csv",header = T,
                          check.names=F,stringsAsFactors = T)
growth=gdp_growth%>%
    pivot_longer(!Industry, names_to = "year", values_to = "gdp_growth")
growth$year=as.numeric(growth$year)


gdp_africa2=read_csv("africas gdp2.csv")

gdp_africa2$Value_lbl2=paste0(" ",round(gdp_africa2$value/1e9))


gdp_africa2$Value_lbl=round(gdp_africa2$Value_lbl,digits = 0)

gdp_africa2$country_name <-  gsub(", Arab Rep.", "", gdp_africa2$country_name)
gdp_africa2$country_name <-  gsub("Congo, Dem. Rep.", "DRC", gdp_africa2$country_name)


#the
eastafrica=readxl::read_xlsx("east africa gdp.xlsx")

eac=eastafrica%>%
    pivot_longer(!date, names_to = "Country", values_to = "gdp")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(
        title = "KENYA ECONOMIC DASHBOARD", titleWidth = 500,
        #subtitle = "GOVERNMENT OF KENYA", subtitleWidth = 500,
        tags$li(actionLink("LinkedIn", 
                           label = "", 
                           icon = icon("twitter"),
                           onclick = "window.open('https://twitter.com/dankilemi')"),
                class = "dropdown"),
        tags$li(actionLink("Facebook", 
                           label = "", 
                           icon = icon("linkedin"),
                           onclick = "window.open('https://www.linkedin.com/in/kilemi-dan-a7b259201/')"),
                class = "dropdown")
 
    ), #disable = TRUE
    dashboardSidebar(
        disable = TRUE
        
    ),
    
    
    dashboardBody(
        #tags$head(
        #    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
        #),
        #tags$div(
        #    style = "position: absolute; width: 100%; left: 0; z-index: 0; height: 90vh; position:fixed",
        #    mapwrdl
        #),
        
        #fluidRow(
         #   column(
         #       width = 12,
        #        class = PARS$classcol,
        #        tags$h2(class = "specialfont", "THE GOVERNMENT OF KENYA"),
        #        tags$em("KENYA ECONOMIC DASHBOARD")
        #    ),
        #),
        
 #       tabItem(
 #           tabName = "Overview",
       
        
    fluidRow(
        tabBox(
            title = NULL, width = 12,
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", height = "250px",
            tabPanel("Economy",
                     
                     fluidRow(
                         class = "top-buffer",
                         column(
                             width  = 10,
                             class = "col-lg-offset-4 col-lg-4 col-md-offset-2 col-md-8 col-sm-offset-1 col-sm-10",
                             selectInput(
                                 inputId = "Industry",
                                 label = "Select Economy Sector",
                                 choices =c(
                                     "Overview"="Overview",
                                     "Agriculture"="Agriculture",
                                     "Mining"="Mining",
                                     "Manufacturing"="Manufacturing",
                                     "Electricity"="Electricity",
                                     "Water"="Water",
                                     "Construction"="Construction",
                                     "Trade"="Trade",
                                     "Transport"="Transport",
                                     "Accomodation and Food"="Accomodation_and_Food",
                                     "ICT"="ICT",
                                     "Finance"="Finance",
                                     "Real Estate"="Real Estate",
                                     "Education"="Education",
                                     "Health"="Health"
                                     
                                 ),selected = "Overview"
                                 
                             ) 
                         ),
                         column(
                             width = 2,
                             dropdownButtonp(
                                 tags$h4("About this app"),
                                 "This is an app developed by Kilemi Daniel",
                                 icon = icon("info"),
                                 tooltip = tooltipOptions(title = "About this app")
                             )
                         )
                     ),
                     
                     fluidRow(
                         #class = "top-buffer",
                         column(
                             # offset = 2,
                             width = 12,
                             #class = PARS$classcol,
                             valueBoxOutput("vb_gdp", 4),
                             valueBoxOutput("vb_population", 4),
                             valueBoxOutput("vb_growth", 4),
                             #imageOutput("img1", 3)
                         )
                         
                     ),
                     fluidRow(
                         #class = "top-buffer",
                         column(
                             width = 6,
                             #class = PARS$classcol,
                             highchartOutput("prntrs", height = "500px",width = "700px")
                             #valueBoxOutput("vb_expdiv", 4)
                         ),
                         column(
                             width = 6,
                             #class = PARS$classcol,
                             highchartOutput("prntrs2", height = "500px",width = "700px")
                             #valueBoxOutput("vb_expdiv", 4)
                         )
                         
                     )
                     
                     
                     #fluidRow(
                     #class = "top-buffer",
                     
                     
                     #)
            )
        )
   )


    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$tabset1Selected <- renderInfoBox({
        infoBox("Selected Tab", input$tabset1, icon = icon("info-circle"))
    })
    

    output$vb_gdp <- renderValueBox({
        
        d <- industry %>%
            filter(Industry==input$Industry)%>%
            select(year, gdp) %>% 
            mutate(gdp_value = round(gdp/1e3, 2)) %>% 
            select(x = year, y = gdp_value)
        
        lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0("Ksh ", .," B") 
        
        hc <- hchart(d, "areaspline") %>% #, color = PARS$sparkline_color
            hc_add_theme(hc_theme_sparkline_vb()) %>% 
            hc_tooltip(valuePrefix = "Ksh ", valueSuffix = " B") 
        
        valueBoxSpark(
            value = lbl,
            subtitle = "GDP",
            color = "teal",
            spark = hc,
            minititle = "Kenya's GDP in 2022"
        )
        
    })

    
    output$vb_population <- renderValueBox({
        population=read.csv("population.csv")
        d <- population %>%
            
            select(Year, Population) %>% 
            #mutate(gdp_value = round(gdp_contribution/1e0, 2)) %>% 
            select(x = Year, y = Population)
        
        lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0(.,"")
        
        hc <- hchart(d, "areaspline") %>% #, color = PARS$sparkline_color 
            hc_add_theme(hc_theme_sparkline_vb()) %>% 
            hc_tooltip(valuePrefix = " ", valueSuffix = "")
        
        valueBoxSpark(
            value = lbl,
            subtitle = "Kenya's Population",
            color = "blue",
            spark = hc,
            minititle = "Population 2022"
        )
        
    })
    
    output$vb_growth <- renderValueBox({
        
        d <- growth %>%
            filter(Industry==input$Industry)%>%
            select(year, gdp_growth) %>% 
            #mutate(gdp_value = round(gdp_contribution/1e0, 2)) %>% 
            select(x = year, y = gdp_growth)
        
        lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0(.," %")
        
        hc <- hchart(d, "spline") %>% #, color = PARS$sparkline_color
            hc_add_theme(hc_theme_sparkline_vb())%>%
            hc_tooltip(valuePrefix = " ", valueSuffix = "") 
        
        valueBoxSpark(
            value = lbl,
            subtitle = "GDP Growth",
            color = "yellow",
            spark = hc,
            minititle = "Growth Percentage"
        )
        
    })
    
    output$prntrs <- renderHighchart({

        hc <- hchart(
            eac %>% select(x = date, group = Country, y = gdp), "line",
            hcaes(x, y, group = group),
            #lineWidth = pull(dpars, lineWidth),
            #visible   = pull(dpars, visible),
            #symbol    = pull(dpars,  symbol),
            #color     = pull(dpars,  color),
        ) %>% 
            hc_title(text = "The GDP of East African Countries from 1960-2022") %>% 
            hc_subtitle(text = "GDP in USD 'Billions", useHTML = TRUE) %>% 
            hc_tooltip(table = TRUE, sort = TRUE) %>% 
            hc_yAxis(
                # type = "logarithmic"
                title = list(text = "GDP in Billions USD")
            ) %>% 
            hc_xAxis(
                crosshair = list(label = list(enabled = TRUE)),
                title = list(text = "Year")
            ) %>% 
            hc_plotOptions(
                series = list(
                    marker = list(enabled = FALSE)
                )
            ) %>% 
            hc_legend(layout = "proximate", align = "right")
        
        hc$x$hc_opts$series <- hc$x$hc_opts$series %>% 
            map(function(x){
                
                # x <- hc$x$hc_opts$series %>% sample(1) %>% first()
                x$marker <- list(symbol = x$symbol)
                
                x
                
            })
        
        hc
        
        
    })
    
    output$prntrs2 <- renderHighchart({
        
        hc <- hchart(
            gdp_africa2 %>% select(x = year, group = country, y = Value_lbl), "line",
            hcaes(x, y, group = group),
            #lineWidth = pull(dpars, lineWidth),
            #visible   = pull(dpars, visible),
            #symbol    = pull(dpars,  symbol),
            #color     = pull(dpars,  color),
        ) %>% 
            hc_title(text = "The GDP of African Countries 1990-2022") %>% 
            hc_subtitle(text = "GDP in USD 'Billions", useHTML = TRUE) %>% 
            hc_tooltip(table = TRUE, sort = TRUE) %>% 
            hc_yAxis(
                # type = "logarithmic"
                title = list(text = "GDP in Billions USD")
            ) %>% 
            hc_xAxis(
                crosshair = list(label = list(enabled = TRUE)),
                title = list(text = "Year")
            ) %>% 
            hc_plotOptions(
                series = list(
                    marker = list(enabled = FALSE)
                )
            ) %>% 
            hc_legend(layout = "proximate", align = "right")
        
        hc$x$hc_opts$series <- hc$x$hc_opts$series %>% 
            map(function(x){
                
                # x <- hc$x$hc_opts$series %>% sample(1) %>% first()
                x$marker <- list(symbol = x$symbol)
                
                x
                
            })
        
        hc
        
        
    })
    
}

#expectancy charts

# Run the application 
shinyApp(ui = ui, server = server)
