
# Aidan Horn

names(sessionInfo()$otherPkgs)
# List of required external packages
packages <- c(
   "forecast",
   "scales",
   "ggplot2",
   "zoo",
   "tidyverse",
   "lubridate",
   "locpol",
   "lpirfs",
   "astsa",
   "latex2exp",
   "showtext"
)
# Installs packages that need to be installed
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, INSTALL_opts = '--no-lock')
# Loads required packages into the library
invisible(lapply(packages, library, c=T))
# List of loaded external packages
names(sessionInfo()$otherPkgs)


if (Sys.info()[1]=="Windows") {
   setwd(paste0(
      Sys.getenv('USERPROFILE'),
      "/Dropbox/Admin/Public information/covid-19/App"
   ))
}
# setwd() presents an error on Linux CLI.
if (Sys.info()[1]=="Linux") home <- '~'


# Data
# Global Cases
globalcovid_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  rename(Country=`Country/Region`)

globalcovid <- globalcovid_raw %>%
   filter(
      Country %in% c(
         # https://covid19.who.int/table (sorted by deaths)
         "US",
         "Brazil",
         "India",
         "Russia",
         "Mexico",
         "Peru",
         "United Kingdom",
         "Italy",
         "Indonesia",
         "France",
         "Iran",
         "Colombia",
         "Germany",
         "Argentina",
         "Poland",
         "Ukraine",
         "Spain",
         "South Africa",
         "Turkey",
         "Romania",
         "Philippines",
         "Chile",
         "Hungary",
         "Vietnam",
         "Czechia",
         "Canada",
         "Bulgaria",
         "Ecuador",
         "Malaysia",
         "Belgium",
         "Pakistan",
         "Japan",
         "Greece",
         "Thailand",
         
         # Newly reported deaths in last 7 days
         "Korea, South",
         "Australia",
         "China",
         "Portugal",
         "New Zealand",
         "Guatemala",
         "Slovakia"
         
      )
   ) %>%
   group_by(Country) %>%
   summarize(
      across(!c(`Province/State`, Lat, Long), sum)
   ) %>%
   pivot_longer(
      cols = c(-Country),
      names_to = "date",
      values_to = "total_cases"
   ) %>%
   mutate(
      date = mdy(date),
#      date_future=date + years(1),
#      date_future2 = date + years(2),
#      date_future3 = date + years(3),
      total_cases=ifelse(total_cases==0, 1, total_cases),
      change = total_cases-lag(total_cases),
      change = ifelse(change<=0 | is.na(change), 1, change),
      hpf=NA,
      
      total_cases = ifelse(   # Country-wide jump at 2021-11-23, causing decreasing HP filter before
         Country=="South Africa" & date > ymd("2021-11-01") & date < ymd("2021-11-25"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 2
         Country=="South Africa" & date >= ymd("2021-11-01") & date <= ymd("2021-11-25"),
         na.approx(total_cases),
         total_cases
      ),
      total_cases = ifelse(   # Step 3
         Country=="South Africa" & date > ymd("2021-11-20") & date < ymd("2021-12-10"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 4
         Country=="South Africa" & date >= ymd("2021-11-20") & date <= ymd("2021-12-10"),
         na.approx(total_cases),
         total_cases
      ),
      
      total_cases = ifelse(
         Country=="China" & date > ymd("2020-02-10") & date < ymd("2020-03-30"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 2
         Country=="China" & date >= ymd("2020-02-10") & date <= ymd("2020-03-30"),
         na.approx(total_cases),
         total_cases
      ),
      total_cases = ifelse(   # Step 3
         Country=="China" & date > ymd("2020-03-10") & date < ymd("2020-05-24"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 4
         Country=="China" & date >= ymd("2020-03-10") & date <= ymd("2020-05-24"),
         na.approx(total_cases),
         total_cases
      ),

      total_cases = ifelse(
         Country=="China" & date > ymd("2021-01-20") & date < ymd("2021-02-20"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 2
         Country=="China" & date >= ymd("2021-01-20") & date <= ymd("2021-02-20"),
         na.approx(total_cases),
         total_cases
      ),
      
      total_cases = ifelse(
         Country=="China" & date > ymd("2021-12-10") & date < ymd("2022-01-26"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 2
         Country=="China" & date >= ymd("2021-12-10") & date <= ymd("2022-01-26"),
         na.approx(total_cases),
         total_cases
      ),
      total_cases = ifelse(   # Step 3
         Country=="China" & date > ymd("2022-01-05") & date < ymd("2022-02-26"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 4
         Country=="China" & date >= ymd("2022-01-05") & date <= ymd("2022-02-26"),
         na.approx(total_cases),
         total_cases
      ),
      
      total_cases = ifelse(
         Country=="China" & date > ymd("2022-05-17") & date < ymd("2022-06-17"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 2
         Country=="China" & date >= ymd("2022-05-17") & date <= ymd("2022-06-17"),
         na.approx(total_cases),
         total_cases
      ),
      
      
      total_cases = ifelse(
         Country=="France" & date > ymd("2021-04-09") & date < ymd("2021-07-20"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 2
         Country=="France" & date >= ymd("2021-04-09") & date <= ymd("2021-07-20"),
         na.approx(total_cases),
         total_cases
      ),
      
      total_cases = ifelse(
         Country=="India" & date > ymd("2021-12-18") & date < ymd("2022-01-09"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 2
         Country=="India" & date >= ymd("2021-12-18") & date <= ymd("2022-01-09"),
         na.approx(total_cases),
         total_cases
      ),
      
      total_cases = ifelse(
         Country=="India" & date > ymd("2022-02-04") & date < ymd("2022-03-15"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 2
         Country=="India" & date >= ymd("2022-02-04") & date <= ymd("2022-03-15"),
         na.approx(total_cases),
         total_cases
      ),
      total_cases = ifelse(   # Step 3
         Country=="India" & date > ymd("2022-03-06") & date < ymd("2022-05-01"),
         NA,
         total_cases
      ),
      total_cases = ifelse(   # Step 4
         Country=="India" & date >= ymd("2022-03-06") & date <= ymd("2022-05-01"),
         na.approx(total_cases),
         total_cases
      )
      
   ) %>%
   ungroup()

# globalcovid$change[globalcovid$date==ymd("2020-01-22")] <- 0

last_date = globalcovid$date[nrow(globalcovid)]

not_forecasted = c()

for (country in unique(globalcovid$Country)) {
   tryCatch(
      expr = {
         insert <- data.frame(matrix(nrow=35, ncol=dim(globalcovid)[2]))
         names(insert) <- names(globalcovid)
         insert$date = seq(last_date + days(1), last_date + days(35), 1)
         insert$Country = rep(country, 35)
         
         if (country %in% c(
               "Brazil",
               "China",
               "South Africa",
               "Ukraine",
               "US",
               "Philippines"
            )) {
            insert$change = exp((sarima.for(
               with(globalcovid %>% filter(Country==country), log(change)),
               n.ahead=35,
               1, 1, 1, 1, 1, 1, 7
            ))$pred)

         } else {
            insert$change = exp((sarima.for(
               with(globalcovid %>% filter(Country==country), log(change)),
               n.ahead=35,
               3, 1, 3, 1, 1, 1, 7
            ))$pred)
            
         }
      },
      error = function(e){
         print(paste(country, " cases not forecasted."))
         not_forecasted <- append(not_forecasted, country)
         
         insert$change = exp((sarima.for(
             with(globalcovid %>% filter(Country==country), log(change)),
             n.ahead=35,
             1, 1, 1
         ))$pred)
      },
      
      finally = {
         globalcovid <- globalcovid %>%
            rbind(insert) %>%
            group_by(Country) %>%
            mutate(
               change_orig = total_cases - lag(total_cases),
               change_orig = ifelse(is.na(change_orig), change, change_orig),
               total_cases = ifelse(is.na(total_cases), cumsum(change_orig), total_cases)
            ) %>% ungroup()
      }
      
   )
}



for (country in unique(globalcovid$Country)) {
   globalcovid <- mutate(
      .data=globalcovid %>%
          filter(Country==country & !is.na(total_cases)) %>%
          select(total_cases, date)
      ,
      hpf_temp=hp_filter(
         x=as.matrix(log(.data$total_cases)),
         lambda=4000     # 1600
      )[[2]]
   ) %>%
      select(-total_cases) %>%
      left_join(
         x=globalcovid,
         y=.,
         by="date"
      ) %>%
      mutate(
         hpf = replace(
            x = hpf,
            list = is.na(hpf) & Country == country,
            values = exp(hpf_temp[Country == country])
         )
      ) %>%
      select(-hpf_temp)
   
   print(paste(Sys.time(), "  ", country))
   
}
globalcovid <- globalcovid %>%
   group_by(Country) %>%
   mutate(
      hpf = ifelse(hpf<0, 0, hpf),
      hpf_change = hpf - lag(hpf)
      # hpf_change = ifelse(hpf_change<0, 0, hpf_change)
   )



# Global Deaths
globaldeaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  rename(Country=`Country/Region`)

globaldeaths <- globaldeaths_raw %>%
  filter(
    Country %in% c(
      # https://covid19.who.int/table (sorted by deaths)
      "US",
      "Brazil",
      "India",
      "Russia",
      "Mexico",
      "Peru",
      "United Kingdom",
      "Italy",
      "Indonesia",
      "France",
      "Iran",
      "Colombia",
      "Germany",
      "Argentina",
      "Poland",
      "Ukraine",
      "Spain",
      "South Africa",
      "Turkey",
      "Romania",
      "Philippines",
      "Chile",
      "Hungary",
      "Vietnam",
      "Czechia",
      "Canada",
      "Bulgaria",
      "Ecuador",
      "Malaysia",
      "Belgium",
      "Pakistan",
      "Japan",
      "Greece",
      "Thailand",
      
      # Newly reported deaths in last 7 days
      "Korea, South",
      "Australia",
      "China",
      "Portugal",
      "New Zealand",
      "Guatemala",
      "Slovakia"
      
    )
  ) %>%
  group_by(Country) %>%
  summarize(
    across(!c(`Province/State`, Lat, Long), sum)
  ) %>%
  pivot_longer(
    cols = c(-Country),
    names_to = "date",
    values_to = "total_deaths"
  ) %>%
  mutate(
    date = mdy(date),
#    date_future=date + years(1),
#    date_future2 = date + years(2),
#    date_future3 = date + years(3),
    total_deaths=ifelse(total_deaths==0, 1, total_deaths),
    change = total_deaths-lag(total_deaths),
    change = ifelse(change<=0 | is.na(change), 1, change),
    hpf=NA
  ) %>%
  ungroup()

# globaldeaths$change[globaldeaths$date==ymd("2020-01-22")] <- 0

not_forecasted_deaths = c()
print(" ")
print("Deaths:")

for (country in unique(globaldeaths$Country)) {
   tryCatch(
      expr = {
         insert <- data.frame(matrix(nrow=35, ncol=dim(globaldeaths)[2]))
         names(insert) <- names(globaldeaths)
         insert$date = seq(last_date + days(1), last_date + days(35), 1)
         insert$Country = rep(country, 35)
         
         if (country %in% c()) {
            insert$change = exp((sarima.for(
               with(globaldeaths %>% filter(Country==country), log(change)),
               n.ahead=35,
               1, 1, 1, 1, 1, 1, 7
            ))$pred)
            
         } else {
            insert$change = exp((sarima.for(
               with(globaldeaths %>% filter(Country==country), log(change)),
               n.ahead=35,
               2, 1, 2, 1, 1, 1, 7
            ))$pred)
            
         }
      },
      error = function(e){
         print(paste(country, " deaths not forecasted."))
         not_forecasted_deaths <- append(not_forecasted_deaths, country)
         
         insert$change = exp((sarima.for(
             with(globaldeaths %>% filter(Country==country), log(change)),
             n.ahead=35,
             1, 1, 1
         ))$pred)
      },
      
      finally = {
         globaldeaths <- globaldeaths %>%
            rbind(insert) %>%
            group_by(Country) %>%
            mutate(
               change_orig = total_deaths - lag(total_deaths),
               change_orig = ifelse(is.na(change_orig), change, change_orig),
               total_deaths = ifelse(is.na(total_deaths), cumsum(change_orig), total_deaths)
            ) %>% ungroup()
      }
      
   )
}

for (country in unique(globaldeaths$Country)) {
  globaldeaths <- mutate(
    .data=globaldeaths %>%
        filter(Country==country & !is.na(total_deaths)) %>%
        select(total_deaths, date)
    ,
    hpf_temp=hp_filter(
      x=as.matrix(log(.data$total_deaths)),
      lambda=1600     # 1600
    )[[2]]
  ) %>%
    select(-total_deaths) %>%
    left_join(
      x=globaldeaths,
      y=.,
      by="date"
    ) %>%
    mutate(
      hpf = replace(
        x = hpf,
        list = is.na(hpf) & Country == country,
        values = exp(hpf_temp[Country == country])
      )
    ) %>%
    select(-hpf_temp)
  
  print(paste(Sys.time(), "  ", country))
  
}
globaldeaths <- globaldeaths %>%
  group_by(Country) %>%
  mutate(
    hpf = ifelse(hpf<0, 0, hpf),
    hpf_change = hpf - lag(hpf)
    # hpf_change = ifelse(hpf_change<0, 0, hpf_change)
  )


# South Africa

coronavirus <- read_csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv") %>%
  pivot_longer(
    cols=c(EC, FS, GP, KZN, LP, MP, NC, NW, WC),
    names_to="Province",
    values_to="total_cases"
  ) %>%
  group_by(Province) %>%
  mutate(
    date=dmy(date),
#    date_future=date + years(1),
#    date_future2 = date + years(2),
    YYYYMMDD=ymd(YYYYMMDD),
    total_cases=ifelse(total_cases==0, 1, total_cases),
    total_cases=ifelse(     # linear imputation
      date==ymd("2020-03-27"),
      round((total_cases[date==ymd("2020-03-28")] +
               total_cases[date==ymd("2020-03-26")]
      )/2),
      total_cases
    ),
    total_cases=ifelse(     # linear imputation
      date==ymd("2020-04-07"),
      round((total_cases[date==ymd("2020-04-08")] +
               total_cases[date==ymd("2020-04-06")]
      )/2),
      total_cases
    ),
    total_cases = ifelse(   # Country-wide jump at 2021-11-23, causing decreasing HP filter before
      date > ymd("2021-11-01") & date < ymd("2021-11-25"),
      NA,
      total_cases
    ),
    total_cases = ifelse(   # Step 2
      date >= ymd("2021-11-01") & date <= ymd("2021-11-25"),
      na.approx(total_cases),
      total_cases
    ),
    total_cases = ifelse(   # Step 3
      date > ymd("2021-11-20") & date < ymd("2021-12-10"),
      NA,
      total_cases
    ),
    total_cases = ifelse(   # Step 4
      date >= ymd("2021-11-20") & date <= ymd("2021-12-10"),
      na.approx(total_cases),
      total_cases
    ),
    change=total_cases-lag(total_cases),
    change_roll=rollapply(change, 14, mean, na.rm=TRUE, by=1, partial=TRUE, fill=NA),
    Province_text=factor(
      Province,
      levels=c("GP", "KZN", "EC", "WC", "FS", "MP", "NW", "LP", "NC"),
      labels = c("Gauteng", "Kwa-Zulu Natal", "Eastern Cape", "Western Cape", "Free State", "Mpumalanga", "North West", "Limpopo", "Northern Cape")
    ),
    hpf=NA
  ) %>%
  ungroup()

arrange(summarize(coronavirus, max=max(change_roll)), desc(max))
tail(select(coronavirus, date, YYYYMMDD, Province, change, change_roll))
sum(is.na(coronavirus$change_roll))

coronavirus$change[coronavirus$date==ymd("2020-03-05")] <- 0

for (prov in unique(coronavirus$Province)) {
  coronavirus <- mutate(
    .data=coronavirus %>% filter(Province==prov) %>% select(total_cases, YYYYMMDD),
    hpf_temp=hp_filter(
      x=as.matrix(log(.data$total_cases)),
      lambda=1600     # 1600
    )[[2]]
  ) %>%
    select(-total_cases) %>%
    left_join(
      x=coronavirus,
      y=.,
      by="YYYYMMDD"
    ) %>%
    mutate(hpf=replace(
      x=hpf,
      list=is.na(hpf) & Province==prov,
      values=exp(hpf_temp[Province==prov])
    )) %>%
    select(-hpf_temp)
  
}
coronavirus <- coronavirus %>%
  group_by(Province) %>%
  mutate(
    hpf=ifelse(hpf<0, 0, hpf),
    hpf_change = hpf - lag(hpf)
    # hpf_change = ifelse(hpf_change<0, 0, hpf_change)
  )

for (prov in unique(coronavirus$Province)) {
  changelast <- coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-8] - coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-9]
  coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-7] <- coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-8] + changelast
  coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-6] <- coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-7] + changelast
  coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-5] <- coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-6] + changelast
  coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-4] <- coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-5] + changelast
  coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-3] <- coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-4] + changelast
  coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-2] <- coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-3] + changelast
  coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-1] <- coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-2] + changelast
  coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9] <- coronavirus$hpf_change[coronavirus$Province==prov][nrow(coronavirus)/9-1] + changelast
}

deaths <- read_csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv") %>%
  pivot_longer(
    cols=c(EC, FS, GP, KZN, LP, MP, NC, NW, WC),
    names_to="Province",
    values_to="total_cases"
  ) %>%
  group_by(Province) %>%
  mutate(
    date=dmy(date),
#    date_future=date + years(1),
#    date_future2 = date + years(2),
    YYYYMMDD=ymd(YYYYMMDD),
    total_cases=ifelse(total_cases==0, 1, total_cases),
    change=total_cases-lag(total_cases),
    change_roll=rollapply(change, 14, mean, na.rm=TRUE, by=1, partial=TRUE, fill=NA),
    Province_text=factor(
      Province,
      levels=c("GP", "KZN", "EC", "WC", "FS", "MP", "NW", "LP", "NC"),
      labels = c("Gauteng", "Kwa-Zulu Natal", "Eastern Cape", "Western Cape", "Free State", "Mpumalanga", "North West", "Limpopo", "Northern Cape")
    ),
    hpf=NA
  ) %>%
  ungroup()

arrange(summarize(deaths, max=max(change_roll)), desc(max))
tail(select(deaths, date, YYYYMMDD, Province, change, change_roll))
sum(is.na(deaths$change_roll))


deaths$change[deaths$date==ymd("2020-03-27")] <- 0

for (prov in unique(deaths$Province)) {
  deaths <- mutate(
    .data=deaths %>% filter(Province==prov) %>% select(total_cases, YYYYMMDD),
    hpf_temp=hp_filter(
      x=as.matrix(log(.data$total_cases)),
      lambda=5000     # 5000
    )[[2]]
  ) %>%
    select(-total_cases) %>%
    left_join(
      x=deaths,
      y=.,
      by="YYYYMMDD"
    ) %>%
    mutate(hpf=replace(
      x=hpf,
      list=is.na(hpf) & Province==prov,
      values=exp(hpf_temp[Province==prov])
    )) %>%
    select(-hpf_temp)
}
deaths <- deaths %>%
  group_by(Province) %>%
  mutate(
    hpf=ifelse(hpf<0, 0, hpf),
    hpf_change = hpf - lag(hpf)
  )


write_csv(
    select(globalcovid, -c(total_cases, change, change_orig, hpf)),
    "globalcovid.csv"
)
write_csv(
    select(globaldeaths, -c(total_deaths, change, change_orig, hpf)),
    "globaldeaths.csv"
)
write_csv(
    select(
        coronavirus,
        -c(YYYYMMDD, UNKNOWN, total, source, total_cases, change_roll, hpf)
    ),
    "sa_covid.csv"
)
write_csv(
    select(
        deaths,
        -c(YYYYMMDD, UNKNOWN, total, source, total_cases, change_roll, hpf)
    ),
    'sa_deaths.csv'
)



# Graphs --------------------------------

# https://stackoverflow.com/a/51906008/4585384
font_families()
font_add_google('Nunito Sans')
font_families()
showtext_auto()


Prov.base.size =22 #9.6


Aidangraph <- list(
   scale_x_date(
      breaks=seq(ymd("2020-04-01"), ymd("2024-04-01"), by="3 months"),
      # date_breaks="2 months",
      date_labels="\n%b",
      minor_breaks=seq(ymd("2020-04-01"), ymd("2024-04-01"), by="1 month"),
      # date_minor_breaks="1 month",
      expand=expansion(mult=c(0, 0) ),  # c(0.02, 0.07)),
      guide=guide_axis(angle=90)
   ),
   theme(
      text = element_text(family='Nunito Sans'), # https://stackoverflow.com/a/51906008/4585384
      # axis.title.x=element_blank(),
      plot.title=element_text(face="bold"),
      axis.text.x=element_text(margin=margin(t=-0.3, b=4)),
      plot.title.position="plot",
      plot.subtitle=element_text(margin=margin(b=Prov.base.size*1.4)),
      panel.grid.major.x=element_line(linetype="solid"),
      panel.grid.minor.x=element_line(linetype="solid", color="#7cb87040"),  # alpha was 28
      # panel.grid.minor.x=element_blank(),
      panel.grid.major.y=element_line(linetype="solid", color="#c2b89bC0"),
      panel.grid.minor.y=element_line(linetype="solid", color="#f2eddfEE"),
      plot.caption=element_text(hjust=0, margin=margin(t=-1),
                                color="#9ca6a0"),
      # axis.title.y=element_text(size=10),
      plot.caption.position="plot",
      #, axis.text.y=element_text(size=7)
      legend.position = c(0.51, 1.06), #c(0.42, 1.043),
   #   legend.box.margin=margin(b=-20, t=-60, l=10, r=0),
      legend.title=element_blank()
   ),
   labs(
      y="Daily cases",
      x="A year",
      subtitle="Daily new confirmed cases of covid-19, over time"
   )
)


AidanProvgraph <- list(
   theme_minimal(
      base_size=Prov.base.size,
      base_line_size=Prov.base.size/19
   ),
   Aidangraph,
   theme( 
      axis.text.x=element_text(size=Prov.base.size*1),
      plot.caption=element_text(size=Prov.base.size/1.6),
      panel.grid.major.x=element_line(linewidth=0.048*Prov.base.size, color="#7cb870A0"),
      panel.grid.minor.x=element_line(linewidth=0.035*Prov.base.size),
      panel.grid.major.y=element_line(linewidth=0.043*Prov.base.size),
      panel.grid.minor.y=element_line(linewidth=0.02*Prov.base.size),
      axis.title.x = element_text(
         margin=margin(b=Prov.base.size/2, t=-Prov.base.size/8)
      ),
      legend.box.margin=margin(t=0, b=0)
   ),
   labs(
      caption=paste("Last date:", last_date, "\nAnalysis by Aidan Horn  <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total.")
      # , subtitle="Two-week rolling average"
   ),
   coord_cartesian( xlim=c(ymd("2020-03-30"), NA) )
)


ZA_graph <- ggplot() +
   AidanProvgraph +
   theme(
      # panel.grid.minor.y=element_blank(),
   #   legend.position="top",
   #   legend.margin=margin(t=2, r=5, b=1, l=-35, unit="pt"),
      legend.text = element_text(size=Prov.base.size*0.7),
      legend.spacing.y = unit(-0.2, "lines"),
      legend.spacing.x = unit(0.15, "lines")
   ) +
   # geom_line(
   #    data = globalcovid %>%
   #       filter( date<=last_date & Country=="South Africa")
   #    ,
   #    mapping=aes(x=date, y=change),
   #    color="#b1c8cc",
   #    alpha=0.5,
   #    lwd=Prov.base.size/36
   # ) +
   geom_line(
      data = globalcovid %>% filter(Country=="South Africa"),
      mapping=aes(x=date + years(3), y=hpf_change, color="Year 1   "),
      lwd=Prov.base.size/19
   ) +
   geom_line(
      data = globalcovid %>%
         filter( date<=last_date & Country=="South Africa")
      ,
      mapping=aes(x=date + years(2), y=hpf_change, color="Year 2   "),
      lwd=Prov.base.size/18
   ) +
   geom_line(
      data = globalcovid %>%
         filter(
            date  > last_date &
               date <= globalcovid$date[nrow(globalcovid)-14] &
               Country == "South Africa"
         ),
      mapping=aes(x=date, y=hpf_change, color="Forecast  "),
      # alpha=0.7,
      lwd=Prov.base.size/21
      # linetype='dashed'
   ) +
   geom_line(
      data = globalcovid %>%
         filter( date<=last_date & Country=="South Africa")
      ,
      mapping=aes(x=date + years(1), y=hpf_change, color="Year 3   "),
      lwd=Prov.base.size/17
   ) +
   geom_line(
      data = globalcovid %>%
         filter(Country=="South Africa" & date <= last_date),
      mapping=aes(x=date, y=hpf_change, color="Year 4   "),
      lwd=Prov.base.size/16
   ) +
   scale_colour_manual(
      values=c("#e0b9d38A", "#bcdbfbFA", "#79b3edD0", "darkblue", "#9ea629BB"),
      breaks = c("Year 1   ", "Year 2   ", "Year 3   ", "Year 4   ", "Forecast  "),
      guide = guide_legend(
         direction = "horizontal",
         nrow = 1,
         byrow = T
      )
   ) +
   # geom_line(
   #     data=filter(coronavirus, date>ymd("2020-03-26")),
   #     mapping=aes(x=date, y=WCchange_roll),
   #     color="darkblue",
   #     lwd=0.9
   # ) +
   labs( title="South Africa") +
   scale_y_log10(
      breaks = 10^(-10:10),
      minor_breaks = rep(1:9, 21)*(10^rep(-10:10, each=9)),
      expand=expansion(mult=c(0.006, 0.006)),
      labels=label_comma(big.mark=" ")
   ) +
   coord_cartesian(
      xlim=c(ymd("2023-03-10"), ymd("2024-03-09")), # coronavirus$date[nrow(coronavirus)]),
      ylim=c(80, 10^4*2 + 10^3*6)
   ) +
   labs(
      caption = paste(
         "Last date:", last_date,
         "   (trend", round(as.numeric(with(globalcovid %>%
               filter(Country=="South Africa" & date==last_date),
               hpf_change)), -1),
         "daily cases)\nAnalysis by Aidan Horn  <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total.\nThree-week forecast: SARIMA(1, 1, 1)(1, 1, 1)7 on log daily change."
      ),
      y = "Daily cases (log scale)"
   )

ZA_graph

png(
   filename="../Graphs/South Africa.png",
   width=760, height=760,
   res = Prov.base.size*5.68 #24
)
ZA_graph
dev.off()