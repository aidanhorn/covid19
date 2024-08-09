# Shiny app at https://www.aidanhorn.co.za/blog/computing/shiny/covid-19

library(nloptr)
library(zoo)
library(lpirfs)
library(lubridate)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(scales)

# Define UI for application
ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("
         
         h1 { font-size: 160% }
         
         h2 {
            font-size: 116%;
         }
        "))
    ),
    
    # Application title
    headerPanel("Covid-19 trends"),
    
    # Sidebar
    fluidRow(
        column(5,
               sidebarPanel(
                   # https://stackoverflow.com/a/48500479/4585384
                   pickerInput(
                       "country",
                       "Country",
                       choices = c(
                           "Argentina",
                           "Australia",
                           "Belgium",
                           "Brazil",
                           "Bulgaria",
                           "Canada",
                           "Chile",
                           "China",
                           "Colombia",
                           "Czechia",
                           "Ecuador",
                           "France",
                           "Germany",
                           "Greece",
                           "Guatemala",
                           "Hungary",
                           "India",
                           "Indonesia",
                           "Iran",
                           "Italy",
                           "Japan",
                           "Malaysia",
                           "Mexico",
                           "New Zealand",
                           "Pakistan",
                           "Peru",
                           "Philippines",
                           "Poland",
                           "Portugal",
                           "Romania",
                           "Russia",
                           "Slovakia",
                           "South Africa",
                           "South Korea"="Korea, South",
                           "Spain",
                           "Thailand",
                           "Turkey",
                           "Ukraine",
                           "United Kingdom",
                           "United States"="US",
                           "Vietnam"
                       ),
                       selected = "South Africa",
                       choicesOpt = list(
                           style = c(
                               NA, NA, NA,
                               "font-weight: bold;",
                               NA, NA, NA,
                               "font-weight: bold;", # China
                               NA, NA, NA,
                               "font-weight: bold;",
                               "font-weight: bold;",
                               NA, NA, NA,
                               "font-weight: bold;", # India
                               NA, NA,
                               "font-weight: bold;",
                               rep(NA, 10),
                               "font-weight: bold;",
                               NA, NA,
                               "font-weight: bold;",
                               "font-weight: bold;",
                               NA, NA, NA,
                               "font-weight: bold;",
                               "font-weight: bold;",
                               NA
                           )
                       )
                   ),
                   uiOutput("region"),
                   selectInput(
                       "deaths",
                       "Cases / deaths",
                       choices=c("Cases", "Deaths")
                   ),
                   uiOutput("logscale"),
                   width=12
               )
        ),
        
        # Show a plot of the generated distribution
        column(7,
               plotOutput("distPlot", height=480)
        )
    ),
    
    uiOutput("data_info"),
    fluidRow(
        column(12,
            HTML('<p>App developer: <a href="https://www.aidanhorn.co.za/blog/computing/shiny/covid-19" target="_blank">Aidan J. Horn</a> &nbsp;&nbsp;&nbsp;&nbsp; ➔ <a href="https://paypal.me/aidanhorn" target="_blank">buy me lunch 🥗</a></p>')   
        )
    )
    
)

# Data
# Global Cases

who <- read_csv("https://srhdpeuwpubsa.blob.core.windows.net/whdh/COVID/WHO-COVID-19-global-data.csv")

globalcovid <- read_csv('https://www.dropbox.com/s/uuevalkvgks0yyf/globalcovid.csv?raw=1')
globaldeaths <- read_csv('https://www.dropbox.com/s/50i7pxgcjtclkgg/globaldeaths.csv?raw=1')
coronavirus <- read_csv('https://www.dropbox.com/s/dkc5rwiunbnica8/sa_covid.csv?raw=1')
deaths <- read_csv('https://www.dropbox.com/s/tdg68mcijnga14l/sa_deaths.csv?raw=1')

last_date_globalcovid  <- globalcovid[order(globalcovid$Country, globalcovid$date),]$date[nrow(globalcovid)-35]
last_date_globaldeaths <- globaldeaths[order(globaldeaths$Country, globaldeaths$date),]$date[nrow(globaldeaths)-35]


# Define server logic
server <- function(input, output) {
    
    output$region <- renderUI({
        if (input$country == "South Africa") {
            selectInput(
                "province",
                "Province",
                choices = c("Total"="South Africa", "Gauteng", "Kwa-Zulu Natal", "Eastern Cape", "Western Cape", "Free State", "Mpumalanga", "North West", "Limpopo", "Northern Cape")
            )
        }
    })
    
    output$data_info <- renderUI({
        if (input$country!="South Africa") {
            fluidRow(
                column(12,
                       HTML('<p></p><p>Data source: <a href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series" target="_blank">https://github.com/CSSEGISandData/COVID-19</a></p>')
                )
            )
        } else if (!is.null(need(input$province, "Province option"))) {
            # Removes the flashed error message
        } else if (input$country == "South Africa" & input$province != "South Africa") {
            fluidRow(
                column(12,
                       HTML('<p></p><p>Data source: <a href="https://www.nicd.ac.za/media/alerts/">https://www.nicd.ac.za/media/alerts/</a> which gets scraped into <a href="https://github.com/dsfsi/covid19za/blob/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv" target="_blank">https://github.com/dsfsi/covid19za</a></p>
                   <p><a href="https://www.dropbox.com/s/c8256hvcwt1q7sg/All_provinces.png?raw=1" target="_blank">Cases by province image</a>; <a href="https://www.dropbox.com/s/bndw35ob8s6333y/All_provinces_logscale.png?raw=1" target="_blank">Log scale image</a>; <a href="https://www.dropbox.com/s/7cngv1kbwr349of/Deaths_by_province.png?raw=1" target="_blank">Deaths by province image</a>; <a href="https://www.dropbox.com/s/whj8urn8u630wdc/WC.png?raw=1" target="_blank">Western Cape graph</a>.</p>')
                )
            )
        } else if (input$country == "South Africa" & input$province == "South Africa") {
            fluidRow(
                column(12,
                       HTML('<p></p><p>Data source: <a href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series" target="_blank">https://github.com/CSSEGISandData/COVID-19</a></p>')
                )
            )
        }
    
    })
    
    output$logscale <- renderUI({
        if (!is.null(need(input$province, "Province option"))) {
            # Removes the flashed error message
        } else if (input$deaths=="Cases" & input$province!="South Africa") {
            selectInput(
                "log",
                "Log scale y axis",
                choices=c("Level scale", "Log scale")
            )
        } else if (input$deaths=="Deaths") {} else {}
        
    })
    
    output$distPlot <- renderPlot({
        
        if (!is.null(need(input$province, "Province option"))) {
            # Removes the flashed error message
        } else {
        
            province_graph_1 <- list(
                theme_minimal(),
                theme(
                    axis.text.x=element_text(angle=90, hjust=1, size=16),
                    axis.text.y=element_text(size=14),
                    plot.title=element_text(size=18),
                    plot.title.position="plot",
                    plot.subtitle=element_text(margin=margin(b=17), size=17),
                    panel.grid.major.x=element_line(linetype="solid", size=0.9, color="#c2ba80C0"),
                    panel.grid.major.y=element_line(linetype="solid", size=0.5, color="#c2b89bEA"),
                    panel.grid.minor.x=element_line(linetype="solid", size=0.7, color="#c2ba804A"),
                    axis.title.y=element_text(size=17),
                    axis.title.x=element_text(size=15, margin=margin(t=5, b=2)),
                    plot.caption=element_text(size=13, hjust=0, margin=margin(t=5), 
                                              color="darkgray"),
                    plot.caption.position="plot",
                    panel.spacing.y=unit(1.1, "lines"), # unit(0.15, "lines"),
                    panel.spacing.x=unit(0.9, "lines"),
                    axis.line.x=element_blank(),
                    axis.line.y=element_blank(),
                    legend.position="top", # c(0.5, 1.05),
                    # legend.position=c(0.73, 1.122),
                    legend.direction="horizontal",
                    legend.title=element_blank(),
                    legend.text=element_text(size=12),
                    legend.box.margin=margin(b=-14, t=-40, l=10, r=0),
                    strip.text=element_blank()
                    # strip.text=element_text(vjust=-1)
                ),
                scale_x_date(
                    breaks=seq(ymd("2021-04-01"), ymd("2024-02-01"), by="3 months"),
                    # date_breaks="2 months",
                    date_labels="%b",
                    minor_breaks=seq(ymd("2021-04-01"), ymd("2024-04-01"), by="1 month"),
                    # date_minor_breaks="1 month",
                    expand=expansion(
                        mult=c(
                            0,
                            0  # 0.1   # 0.12
                        )
                    )
                ),
                scale_colour_manual(
                    values=c("#e0b9d38A", "#bcdbfbFA", "#79b3edD0", "darkblue", "#9ea629BB"),
                    breaks = c("Year 1", "Year 2", "Year 3", "Year 4", "Forecast"),
                    # values=c("darkblue", "#b1c8cc"),
                    # breaks = c("Trend (HP filter)", "Reported")
                    guide = guide_legend(
                        direction = "horizontal",
                        nrow = 2
                    )
                )
            )
            
            global_cases <- ggplot(
                data = globalcovid %>%
                    filter(Country==input$country),
                mapping=aes(x=date)
            ) +
                province_graph_1 +
                geom_line(
                    mapping=aes(x=date + years(3), y=hpf_change, color="Year 1"),
                    lwd=0.8
                ) +
                geom_line(
                    mapping=aes(x=date + years(2), y=hpf_change, color="Year 2"),
                    lwd=1
                ) +
                geom_line(
                    data = globalcovid %>%
                        filter(Country==input$country & date <= last_date_globalcovid),
                    mapping=aes(x=date + years(1), y=hpf_change, color="Year 3"),
                    lwd=1.2
                ) +
                # geom_line(
                #     mapping=aes(x=date, y=change),
                #     color="#8D5F3B",
                #     alpha=0.5,
                #     lwd=0.25
                # ) +
                geom_line(
                    data = globalcovid %>%
                      filter(Country==input$country & date <= last_date_globalcovid),
                    mapping=aes(x=date, y=hpf_change, color="Year 4"),
                    lwd=1.4
                ) +
                geom_line(
                    data = globalcovid %>%
                        filter(
                            date  > last_date_globalcovid &
                                date <= globalcovid$date[nrow(globalcovid)-14] &
                                Country == input$country
                        ),
                    mapping=aes(x=date + years(1), y=hpf_change, color="Forecast"),
                    lwd=1
                ) +
                geom_line(
                    data = globalcovid %>%
                        filter(
                            date  > last_date_globalcovid &
                                date <= globalcovid$date[nrow(globalcovid)-14] &
                                Country == input$country
                        ),
                    mapping=aes(x=date, y=hpf_change, color="Forecast"),
                    lwd=1
                )
            
            
            province_cases_1 <- ggplot(
                data=coronavirus %>%
                    filter(Province_text==input$province),
                mapping=aes(x=date)
            ) +
                province_graph_1 +
                geom_line(
                    mapping=aes(x=date + years(2), y=hpf_change, color="Year 1"),
                    lwd=1.2
                ) +
                geom_line(
                    mapping=aes(x=date + years(1), y=hpf_change, color="Year 2"),
                    lwd=1.2
                ) +
                geom_line(
                    mapping=aes(x=date, y=change),
                    color="#8D5F3B",
                    alpha=0.5,
                    lwd=0.25
                ) +
                geom_line(
                    mapping=aes(x=date, y=hpf_change, color="Year 3"),
                    lwd=1.4
                )
            
            
            levelyscale_1 <- province_cases_1 +
                theme(
                    panel.grid.minor.y=element_blank() # element_line(linetype="solid", size=0.45, color="#f2eddfF0"),
                ) +
                labs(
                    title="Daily new confirmed cases of covid-19",
                    subtitle=paste(input$province),
                    y="Daily cases",
                    caption=paste0("Last date: ", coronavirus$date[nrow(coronavirus)], "   (trend ", round(as.numeric(with(coronavirus %>% filter(Province_text==input$province) %>% filter(date==coronavirus$date[nrow(coronavirus)]), hpf_change)), -1), " daily cases)\nAidan Horn <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total."),
                    x="One year"
                ) +
                scale_y_continuous(
                    breaks=seq(0, 20000, 2000),
                    expand=expansion(mult=c(0.01, 0.02))
                ) +
                coord_cartesian(
                    xlim=c(ymd("2022-03-15"), ymd("2023-03-14")), # c(ymd("2020-03-30"), coronavirus$date[nrow(coronavirus)]),
                    ylim=c(0, 10000)
                )
            
            breaks <- 10^(-10:10)
            minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
            
            logyscale_global <- global_cases +
                scale_y_log10(
                    breaks=breaks,
                    # minor_breaks=minor_breaks,
                    expand=expansion(mult=c(0.006, 0.006)),
                    labels=label_comma(big.mark=" ")
                ) +
                coord_cartesian(
                    xlim=c(ymd("2023-02-26"), ymd("2024-02-25")), # coronavirus$date[nrow(coronavirus)]),
                    ylim=c(10, 10^6+(10^4)*3)
                ) +
                theme(
                    panel.grid.minor.y=element_blank() # element_line(linetype="solid", size=0.3, color="#f2eddfF0")
                ) +
                labs(
                    title="Daily new confirmed cases of covid-19",
                    subtitle=paste(input$country),
                    y="Daily cases (log scale)",
                    caption=paste0("Last date: ", last_date_globalcovid, "   (trend ", round(as.numeric(with(globalcovid %>% filter(Country==input$country) %>% filter(date==last_date_globalcovid), hpf_change)), -1), " daily cases)\nAidan Horn <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total."),
                    x="One year"
                )
            
            logyscale <- province_cases_1 +
                scale_y_log10(
                    breaks=breaks,
                    minor_breaks=minor_breaks,
                    expand=expansion(mult=c(0.006, 0.006)),
                ) +
                coord_cartesian(
                    xlim=c(ymd("2022-03-15"), ymd("2023-03-14")), # coronavirus$date[nrow(coronavirus)]),
                    ylim=c(10, 21000)
                ) +
                theme(
                    panel.grid.minor.y=element_line(linetype="solid", size=0.3, color="#f2eddfF0")
                ) +
                labs(
                    title="Daily new confirmed cases of covid-19",
                    subtitle=paste(input$province),
                    y="Daily cases (log scale)",
                    caption=paste0("Last date: ", coronavirus$date[nrow(coronavirus)], "   (trend ", round(as.numeric(with(coronavirus %>% filter(Province_text==input$province) %>% filter(date==coronavirus$date[nrow(coronavirus)]), hpf_change)), -1), " daily cases)\nAidan Horn <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total."),
                    x="One year"
                )
            
            
            deathsplot_global <- ggplot(
                data = globaldeaths %>%
                    filter(Country==input$country),
                mapping=aes(x=date)
            ) +
                province_graph_1 +
                geom_line(
                    mapping=aes(x=date + years(3), y=hpf_change, color="Year 1"),
                    lwd=0.8
                ) +
                geom_line(
                    mapping=aes(x=date + years(2), y=hpf_change, color="Year 2"),
                    lwd=1
                ) +
                geom_line(
                    data = globaldeaths %>%
                        filter(Country==input$country & date<=last_date_globaldeaths),
                    mapping=aes(x=date + years(1), y=hpf_change, color="Year 3"),
                    lwd=1.2
                ) +
                # geom_line(
                #     mapping=aes(x=date, y=change),
                #     color="#8D5F3B",
                #     lwd=0.25,
                #     alpha=0.4
                # ) +
                geom_line(
                    data = globaldeaths %>%
                      filter(Country==input$country & date<=last_date_globaldeaths),
                    mapping=aes(x=date, y=hpf_change, color="Year 4"),
                    lwd=1.4
                ) +
                geom_line(
                    data = globaldeaths %>%
                      filter(
                        date  > last_date_globaldeaths &
                           date <= globaldeaths$date[nrow(globaldeaths)-14] &
                           Country == input$country
                    ),
                    mapping=aes(x=date + years(1), y=hpf_change, color="Forecast"),
                    lwd=1
                ) +
                geom_line(
                    data = globaldeaths %>%
                        filter(
                            date  > last_date_globaldeaths &
                                date <= globaldeaths$date[nrow(globaldeaths)-14] &
                                Country == input$country
                        ),
                    mapping=aes(x=date, y=hpf_change, color="Forecast"),
                    lwd=1
                ) +
                theme(
                    panel.grid.minor.y = element_blank() # element_line(linetype="solid", size=0.45, color="#f2eddf80"),
                    # axis.text.y=element_text(size=9)
                ) +
                labs(
                    title = "Daily new confirmed covid-19 deaths",
                    subtitle = paste(input$country),
                    y = "Daily deaths",
                    caption = paste0("Last date: ", last_date_globaldeaths, "   (trend ", round(as.numeric(with(globaldeaths %>% filter(Country==input$country) %>% filter(date==last_date_globaldeaths), hpf_change)), -1), " daily deaths)\nAidan Horn <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total."),
                    x="One year"
                ) +
                scale_y_continuous(
                    breaks=seq(0, 5000, 500),
                    minor_breaks=seq(0, 5000, 500),
                    expand=expansion(mult=c(0.01, 0.01))
                ) +
                coord_cartesian(
                    xlim=c(ymd("2023-02-26"), ymd("2024-02-25")), # coronavirus$date[nrow(coronavirus)]),
                    ylim=c(0, 2030) # 167)
                )
            
            
            deathsplot <- ggplot(
                data=deaths %>%
                    filter(Province_text==input$province),
                mapping=aes(x=date)
            ) +
                province_graph_1 +
                geom_line(
                    mapping=aes(x=date + years(2), y=hpf_change, color="Year 1"),
                    lwd=1.2
                ) +
                geom_line(
                    mapping=aes(x=date + years(1), y=hpf_change, color="Year 2"),
                    lwd=1.2
                ) +
                geom_line(
                    mapping=aes(x=date, y=change),
                    color="#8D5F3B",
                    lwd=0.25,
                    alpha=0.4
                ) +
                geom_line(
                    mapping=aes(x=date, y=hpf_change, color="Year 3"),
                    lwd=1.4
                ) +
                theme(
                    panel.grid.minor.y=element_blank()  # element_line(linetype="solid", size=0.45, color="#f2eddfB0"),
                    # axis.text.y=element_text(size=9)
                ) +
                labs(
                    title = "Daily new confirmed covid-19 deaths",
                    subtitle = paste(input$province),
                    y = "Daily deaths",
                    caption = paste0("Last date: ", deaths$date[nrow(deaths)], "   (trend ", round(as.numeric(with(deaths %>% filter(Province_text==input$province) %>% filter(date==deaths$date[nrow(deaths)]), hpf_change)), 0), " daily deaths)\nAidan Horn <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total."),
                    x="One year"
                ) +
                scale_y_continuous(
                    breaks=seq(0, 200, 50),
                    minor_breaks=seq(0, 200, 10),
                    expand=expansion(mult=c(0.01, 0.01))
                ) +
                coord_cartesian(
                    xlim=c(ymd("2022-03-15"), ymd("2023-03-14")), # coronavirus$date[nrow(coronavirus)]),
                    ylim=c(0, 150) # 167)
                )
            
            if (input$deaths=="Cases") {
                
                if (input$country=="South Africa" & input$province!="South Africa") {
                    if (!is.null(need(input$log, "Log scale option"))) {
                        # Removes the flashed error message
                    } else if (input$log=="Level scale") {
                        levelyscale_1
                    } else if (input$log=="Log scale") {
                        logyscale
                    }
                } else logyscale_global
                
            } else if (input$deaths=="Deaths") {
                if (input$country=="South Africa" & input$province!="South Africa") {
                    deathsplot
                } else deathsplot_global
            }
            
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
