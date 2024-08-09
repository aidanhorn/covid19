# Charts showing daily covid-19 cases in South African provinces, over time
# Aidan Horn
# September 2020

# Currently loaded external packages
names(sessionInfo()$otherPkgs)
# List of required external packages
packages <- c(
    "forecast",
    "ggplot2", 
    "zoo", 
    "tidyverse", 
    "lubridate", 
    "locpol", 
    "lpirfs", 
    "astsa",
    "latex2exp"
)
# Installs packages that need to be installed
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Loads required packages into the library
invisible(lapply(packages, library, c=T))
# List of loaded external packages
names(sessionInfo()$otherPkgs)

if (Sys.info()[1]=="Windows") home <- Sys.getenv('USERPROFILE')
if (Sys.info()[1]=="Linux") home <- path.expand('~')
# Folder on user's computer
setwd(paste0(home, "/Dropbox/Admin/Public information/covid-19"))

# Pulls data from the web
coronavirus <- read_csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv") %>%    
   mutate(
      date=dmy(date), 
      date_past = date - years(1),
      date_future=date + years(1),
      date_future2= date + years(2),
      YYYYMMDD=ymd(YYYYMMDD), 
      WC=ifelse(WC==0, 1, WC),
      GP=ifelse(GP==0, 1, GP),
      WC=ifelse(
         date==ymd("2020-03-27"), 
         (WC[date==ymd("2020-03-26")] + WC[date==ymd("2020-03-28")])/2, 
         WC),
      GP=ifelse(
         date==ymd("2020-03-27"), 
         (GP[date==ymd("2020-03-26")] + GP[date==ymd("2020-03-28")])/2, 
         GP),
      KZN=ifelse(KZN==0, 1, KZN),
      KZN=ifelse(
         date==ymd("2020-03-27"), 
         (KZN[date==ymd("2020-03-26")] + KZN[date==ymd("2020-03-28")])/2, 
         KZN),
      WC=ifelse(
         date==ymd("2020-04-07"), 
         (WC[date==ymd("2020-04-06")] + WC[date==ymd("2020-04-08")])/2, 
         WC),
      KZN=ifelse(
         date==ymd("2020-04-07"), 
         (KZN[date==ymd("2020-04-06")] + KZN[date==ymd("2020-04-08")])/2, 
         KZN),
      GP=ifelse(
         date==ymd("2020-04-07"), 
         (GP[date==ymd("2020-04-06")] + GP[date==ymd("2020-04-08")])/2, 
         GP),
      
            GP = ifelse(   # Country-wide jump at 2021-11-23, causing decreasing HP filter before
               date > ymd("2021-11-01") & date < ymd("2021-11-25"),
               NA,
               GP
            ),
            GP = ifelse(   # Step 2
              date >= ymd("2021-11-01") & date <= ymd("2021-11-25"),
              na.approx(GP),
              GP
            ),
            GP = ifelse(   # Step 3
              date > ymd("2021-11-20") & date < ymd("2021-12-10"),
              NA,
              GP
            ),
            GP = ifelse(   # Step 4
              date >= ymd("2021-11-20") & date <= ymd("2021-12-10"),
              na.approx(GP),
              GP
            ),
      
      WCchange=WC-lag(WC),
      GPchange=GP-lag(GP),
      KZNchange=KZN-lag(KZN),
      WCchange_roll=rollapply(WCchange, 14, mean, na.rm=TRUE, by=1, partial=TRUE, fill=NA),
      GPchange_roll=rollapply(GPchange, 14, mean, na.rm=TRUE, by=1, partial=TRUE, fill=NA),
      KZNchange_roll=rollapply(KZNchange, 14, mean, na.rm=TRUE, by=1, partial=TRUE, fill=NA)
   )
tail(select(coronavirus, date, YYYYMMDD, WC, WCchange, WCchange_roll, KZNchange, KZNchange_roll), n=15L)
sum(is.na(coronavirus$WCchange_roll))

WCarima <- sarima.for(
    with(coronavirus %>%
            mutate(
                WCchange = ifelse(WCchange<=0 | is.na(WCchange), 1, WCchange),
                WCchange = ifelse(date==ymd('2021-11-23'), 50, WCchange),
                WCchange = ifelse(date==ymd('2021-12-15'), 5400, WCchange),
                WCchange = ifelse(date==ymd('2021-12-17'), 3655, WCchange)
            ),
        log(WCchange)), 
    n.ahead=35, 
    8, 1, 4, 1, 1, 1, 7
)

KZNarima <- sarima.for(
    with(coronavirus %>%
            mutate(
                KZNchange = ifelse(KZNchange<=0 | is.na(KZNchange), 1, KZNchange),
                KZNchange = ifelse(date==ymd('2021-11-23'), 50, KZNchange)
            ),
        log(KZNchange)), 
    n.ahead=35, 
    7, 1, 3, 1, 1, 1, 7
)

GParima <- sarima.for(
   with(coronavirus %>%
           mutate(
              GPchange = ifelse(GPchange<=0 | is.na(GPchange), 1, GPchange)
           ),
    log(GPchange)), 
    n.ahead=35, 
    7, 1, 3, 1, 1, 1, 7
)

WCchange <- append(
    coronavirus$WCchange,
    exp(WCarima$pred)
)
KZNchange <- append(
    coronavirus$KZNchange,
    exp(KZNarima$pred)
)
GPchange <- append(
    coronavirus$GPchange,
    exp(GParima$pred)
)
last_date <- coronavirus$date[nrow(coronavirus)]
dates <- append(coronavirus$date, as.Date(as.integer(last_date) + seq(1, 35)))
insert <- data.frame(matrix(nrow=35, ncol=dim(coronavirus)[2]))
names(insert) <- names(coronavirus)
coronavirus <- rbind(coronavirus, insert)
coronavirus$WCchange <- WCchange
coronavirus$KZNchange <- KZNchange
coronavirus$GPchange <- GPchange
coronavirus$date <- dates

coronavirus$WCchange[1] <- 0
coronavirus$KZNchange[1] <- 0
coronavirus$GPchange[1] <- 0

coronavirus <- coronavirus %>%
    mutate(
        WC = ifelse(is.na(WC), cumsum(WCchange), WC),
        KZN = ifelse(is.na(KZN), cumsum(KZNchange), KZN),
        GP = ifelse(is.na(GP), cumsum(GPchange), GP),
        
        KZN = ifelse(   # Country-wide jump at 2021-11-23, causing decreasing HP filter before
           date > ymd("2021-11-01") & date < ymd("2021-11-25"),
           NA,
           KZN
        ),
        KZN = ifelse(   # Step 2
           date >= ymd("2021-11-01") & date <= ymd("2021-11-25"),
           na.approx(KZN),
           KZN
        ),
        KZN = ifelse(   # Step 3
           date > ymd("2021-11-20") & date < ymd("2021-12-10"),
           NA,
           KZN
        ),
        KZN = ifelse(   # Step 4
           date >= ymd("2021-11-20") & date <= ymd("2021-12-10"),
           na.approx(KZN),
           KZN
        )
    )

WChp <- hp_filter(x=as.matrix(log(coronavirus$WC)), lambda=1000) # 1600 is customary
GPhp <- hp_filter(x=as.matrix(log(coronavirus$GP)), lambda=1000) # 1600 is customary
KZNhp <- hp_filter(x=as.matrix(log(coronavirus$KZN)), lambda=1000) # 1600 is customary
coronavirus <- coronavirus %>%
  mutate(
      WCtotaltrend=exp(WChp[[2]]),
      WCtrend = WCtotaltrend - lag(WCtotaltrend),
      GPtotaltrend=exp(GPhp[[2]]),
      GPtrend = GPtotaltrend - lag(GPtotaltrend),
      KZNtotaltrend=exp(KZNhp[[2]]),
      KZNtrend = KZNtotaltrend - lag(KZNtotaltrend)
  )

ggplot(data=coronavirus %>% filter(date<=last_date & date>last_date - days(60))) +
   theme_minimal() +
   geom_line(aes(x=date, y=WCtotaltrend)) +
   geom_point(aes(x=date, y=WC))

ggplot(data=tail(coronavirus, n=20)) +
    theme_minimal() +
    geom_line(aes(x=date, y=GPtotaltrend)) +
    geom_point(aes(x=date, y=GP))

# WCgrad <- coronavirus$WCtrend[nrow(coronavirus)-8] - coronavirus$WCtrend[nrow(coronavirus)-9]
# coronavirus$WCtrend[nrow(coronavirus)-7] <- coronavirus$WCtrend[nrow(coronavirus)-8] + WCgrad
# coronavirus$WCtrend[nrow(coronavirus)-6] <- coronavirus$WCtrend[nrow(coronavirus)-7] + WCgrad
# coronavirus$WCtrend[nrow(coronavirus)-5] <- coronavirus$WCtrend[nrow(coronavirus)-6] + WCgrad
# coronavirus$WCtrend[nrow(coronavirus)-4] <- coronavirus$WCtrend[nrow(coronavirus)-5] + WCgrad
# coronavirus$WCtrend[nrow(coronavirus)-3] <- coronavirus$WCtrend[nrow(coronavirus)-4] + WCgrad
# coronavirus$WCtrend[nrow(coronavirus)-2] <- coronavirus$WCtrend[nrow(coronavirus)-3] + WCgrad
# coronavirus$WCtrend[nrow(coronavirus)-1] <- coronavirus$WCtrend[nrow(coronavirus)-2] + WCgrad
# coronavirus$WCtrend[nrow(coronavirus)] <- coronavirus$WCtrend[nrow(coronavirus)-1] + WCgrad

plot(coronavirus$WCtrend)

coronavirus <- coronavirus %>% mutate(
   WCtrend_accel = WCtrend - lag(WCtrend)
)

# KZNgrad <- coronavirus$KZNtrend[nrow(coronavirus)-4] - coronavirus$KZNtrend[nrow(coronavirus)-5]
# coronavirus$KZNtrend[nrow(coronavirus)-3] <- coronavirus$KZNtrend[nrow(coronavirus)-4] + KZNgrad
# coronavirus$KZNtrend[nrow(coronavirus)-2] <- coronavirus$KZNtrend[nrow(coronavirus)-3] + KZNgrad
# coronavirus$KZNtrend[nrow(coronavirus)-1] <- coronavirus$KZNtrend[nrow(coronavirus)-2] + KZNgrad
# coronavirus$KZNtrend[nrow(coronavirus)] <- coronavirus$KZNtrend[nrow(coronavirus)-1] + KZNgrad


Aidangraph <- list( 
    scale_x_date(
        breaks=seq(ymd("2020-04-01"), ymd("2023-04-01"), by="3 months"),
        # date_breaks="2 months", 
        date_labels="\n%b",
        minor_breaks=seq(ymd("2020-04-01"), ymd("2023-04-01"), by="1 month"),
        # date_minor_breaks="1 month",
        expand=expansion(mult=c(0, 0) ),  # c(0.02, 0.07)),
        guide=guide_axis(angle=90)
    ),
    theme(
        # axis.title.x=element_blank(),
        plot.title=element_text(face="bold"),
        axis.text.x=element_text(margin=margin(t=-0.3, b=4)),
        plot.title.position="plot",
        plot.subtitle=element_text(margin=margin(b=7)),
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
        legend.position="top",
        legend.title=element_blank()
    ),
    labs(
        y="Daily cases",
        x="A year",
        subtitle="Daily new confirmed cases of covid-19, over time"
    )
)

Prov.base.size=9.6

AidanProvgraph <- list(
    theme_minimal(
        base_size=Prov.base.size,
        base_line_size=Prov.base.size/15
    ),
    Aidangraph,
    theme( 
        axis.text.x=element_text(size=Prov.base.size*1),
        plot.caption=element_text(size=Prov.base.size/1.6),
        panel.grid.major.x=element_line(size=0.06*Prov.base.size, color="#7cb870A0"),
        panel.grid.minor.x=element_line(size=0.04*Prov.base.size),
        panel.grid.major.y=element_line(size=0.05*Prov.base.size),
        panel.grid.minor.y=element_line(size=0.047*Prov.base.size),
        axis.title.x=element_text(margin=margin(t=-Prov.base.size/5)),
        legend.box.margin=margin(t=-1/Prov.base.size*80, b=-1/Prov.base.size*80)
    ),
    labs(
        caption=paste("Last date:", last_date, "\nAnalysis by Aidan Horn  <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total.")
        # , subtitle="Two-week rolling average"
    ),
    coord_cartesian( xlim=c(ymd("2020-03-30"), NA) )
)


AidanWCgraph <- ggplot() +
    AidanProvgraph + 
    theme(
        # panel.grid.minor.y=element_blank(),
        legend.position="top",
        legend.margin=margin(t=2, r=5, b=1, l=-35, unit="pt"),
        legend.text=element_text(size=7)
    ) +
    geom_line(
      data=coronavirus %>% filter(date<=last_date),
      mapping=aes(x=date, y=WCchange),
      color="#b1c8cc",
      alpha=0.5,
      lwd=Prov.base.size/36
    ) +
    geom_line(
        data=coronavirus,
        mapping=aes(x=date_future2, y=WCtrend, color="Year 1"),
        lwd=Prov.base.size/15
    ) +
    geom_line(
      data=coronavirus %>% filter(date<=last_date),
      mapping=aes(x=date_future, y=WCtrend, color="Year 2"),
      lwd=Prov.base.size/15
    ) +
    geom_line(
        data=coronavirus %>%
            filter(
                date>last_date &
                date<=coronavirus$date[nrow(coronavirus)-21]
            ),
        mapping=aes(x=date, y=WCtrend, color="Forecast"),
        # alpha=0.7,
        lwd=Prov.base.size/20
        # linetype='dashed'
    ) +
    geom_line(
      data=coronavirus %>% filter(date<=last_date),
      mapping=aes(x=date, y=WCtrend, color="Year 3"),
      lwd=Prov.base.size/15
    ) +
    scale_colour_manual( 
      values=c("#ced8deBB", "#79b3edD3", # Year 2 was: "#64a1de"
        "darkblue", 
        "#9ea629BB"
        ), 
      breaks = c("Year 1", "Year 2", 
        "Year 3", 
        "Forecast"
        ) 
    ) +
    # geom_line( 
    #     data=filter(coronavirus, date>ymd("2020-03-26")),
    #     mapping=aes(x=date, y=WCchange_roll),
    #     color="darkblue",
    #     lwd=0.9
    # ) +
    labs( title="Western Cape") +
    scale_y_continuous(
        breaks=seq(0, 5000, by=1000), 
        minor_breaks=seq(0, 5000, by=500),
        expand=expansion(mult=c(0.01, 0.02))
    ) +
    coord_cartesian(
        xlim=c(ymd("2022-03-15"), ymd("2023-03-14")),
        ylim=c(0, 3600)
    ) +
    labs(
        caption=paste("Last date:", last_date, "\nAnalysis by Aidan Horn  <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total.\nTwo-week forecast: SARIMA(8, 1, 4)(1, 1, 1)7 on log daily change."
        )
    )

png(
    filename="Graphs/WC.png",
    width=760, height=760,
    res=Prov.base.size*24
)
AidanWCgraph
dev.off()



AidanWCgraph_accel <- ggplot() +
    AidanProvgraph + 
    theme(
       legend.position=c(0.63, 1.18),
       legend.box.margin=margin(b=-6, t=-6),
       legend.direction="horizontal"
    ) +
    geom_hline(yintercept=0, color="black") +
    geom_line(
        data = filter(coronavirus),
        mapping = aes(x=date_future2, y=WCtrend_accel, color="Year 1"),
        lwd = Prov.base.size/13,
        alpha = 0.8
    ) +
    geom_line(
      data = filter(coronavirus, date<=last_date),
      mapping = aes(x=date_future, y=WCtrend_accel, color="Year 2"),
      lwd = Prov.base.size/12,
      alpha = 0.8
    ) +
    geom_line(
      data=filter(coronavirus, date<=last_date),
      mapping=aes(x=date, y=WCtrend_accel, color="Year 3"),
      lwd=Prov.base.size/12
    ) +
    scale_colour_manual( 
      values=c("#ced8deBB", "#79b3edD3", "darkblue"), 
      breaks = c("Year 1", "Year 2", "Year 3") 
    ) +
    # geom_line( 
    #     data=filter(coronavirus, date>ymd("2020-03-26")),
    #     mapping=aes(x=date, y=WCchange_roll),
    #     color="darkblue",
    #     lwd=0.9
    # ) +
    labs( 
       title="Western Cape",
       subtitle="Acceleration of total cases (second derivative)",
       y="Extra daily cases per day"
    ) +
    scale_y_continuous(
        breaks=seq(-200, 200, by=40), 
        # minor_breaks=seq(0, 5000, by=500),
        expand=expansion(mult=c(0.01, 0.01))
    ) +
    coord_cartesian(
        xlim=c(ymd("2022-03-15"), ymd("2023-03-14")),
        ylim=c(-120, 120)
    )
AidanWCgraph_accel

png(
    filename="Graphs/WCaccel.png",
    width=760, height=760,
    res=Prov.base.size*24
)
AidanWCgraph_accel
dev.off()


# Deaths
deaths <- read_csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv") %>%
    pivot_longer(
        cols=c(EC, FS, GP, KZN, LP, MP, NC, NW, WC), 
        names_to="Province", 
        values_to="total_cases"
    ) %>%
    group_by(Province) %>%
    mutate(
        date=dmy(date), 
        date_future=date + years(1),
        date_future2 = date + years(2),
        YYYYMMDD=ymd(YYYYMMDD), 
        total_cases = ifelse(total_cases==0, 1, total_cases),
        total_cases = ifelse(
            Province=="EC" & date==ymd("2022-02-15"), 
            16430, total_cases
        ),
        total_cases = ifelse(
            Province=="LP" & date==ymd("2022-01-18"), 
            4179, total_cases
        ),
        change=total_cases-lag(total_cases),
        change_roll=rollapply(change, 14, mean, na.rm=TRUE, by=1, partial=TRUE, fill=NA),
        hpf=NA
    ) %>%
    ungroup()

arrange(summarize(deaths, max=max(change_roll)), desc(max))
tail(select(deaths, date, YYYYMMDD, Province, change, change_roll))
sum(is.na(deaths$change_roll))



last_date <- deaths$date[nrow(deaths)]
insert <- data.frame(matrix(nrow=35, ncol=dim(deaths)[2]))
names(insert) <- names(deaths)

deaths$change[deaths$date==ymd("2020-03-27")] <- 0

# for (prov in unique(deaths$Province)) {
prov="WC"
    print(prov)
    deaths.sarima.for <- sarima.for(
        (deaths %>% filter(Province==prov) %>% select(change) + 0.5) %>% log(), 
        n.ahead=35, 
        7, 1, 3, 1, 1, 1, 7
    )
    deathschange <- append(
        deaths$change,
        as.vector(exp(deaths.sarima.for$pred))
    )
    deathsdate <- append(deaths$date, as.Date(as.integer(last_date) + seq(1, 35)))
    deaths <- rbind(deaths, insert)
    deaths$change <- deathschange
    deaths$date <- deathsdate
    deaths$Province <- ifelse(is.na(deaths$Province), prov, deaths$Province)
    
    deaths <- mutate(
        .data = deaths %>% 
            filter(Province==prov) %>% 
            select(total_cases, date, change, Province),
        total_cases = ifelse(is.na(total_cases), cumsum(change), total_cases),
        hpf_temp=as.vector(hp_filter(
            x=as.matrix(log(.data$total_cases)),
            lambda=5000     # 1600 is customary.
        )[[2]])
    ) %>%
        select(-total_cases, -change) %>%
        left_join(
            x=deaths,
            y=.,
            by=c("date", "Province")
        ) %>%
        mutate(
            hpf=replace(
                x=hpf, 
                list=is.na(hpf) & Province==prov, 
                values=exp(hpf_temp[Province==prov])
            )
        ) %>%
        select(-hpf_temp)
    
# }

deaths <- deaths %>%
  group_by(Province) %>%
  mutate(
    Province=factor(
        Province, 
        levels=c("GP", "KZN", "EC", "WC", "FS", "MP", "NW", "LP", "NC"),
        labels = c("Gauteng", "Kwa-Zulu Natal", "Eastern Cape", "Western Cape", "Free State", "Mpumalanga", "North West", "Limpopo", "Northern Cape")
    ),
    hpf=ifelse(hpf<0, 0, hpf),
    hpf_change = hpf - lag(hpf)
  )

AidanWCdeathgraph <- ggplot() +
    AidanProvgraph + 
    geom_line(
        data=filter(deaths, Province=="Western Cape"),
        mapping=aes(x=date_future2, y=hpf_change, color="Year 1"),
        lwd=Prov.base.size/15,
        alpha=0.8
    ) +    
    geom_line(
        data=filter(deaths, Province=="Western Cape"),
        mapping=aes(x=date_future, y=hpf_change, color="Year 2"),
        lwd=Prov.base.size/15,
        alpha=0.8
    ) +
    geom_line(
        data=deaths %>%
            filter(
                date>last_date &
                date<=deaths$date[nrow(deaths)-14] &
                Province == "Western Cape"
            ),
        mapping=aes(x=date, y=hpf_change, color="Forecast"),
        # alpha=0.7,
        lwd=Prov.base.size/20
        # linetype='dashed'
    ) +
    geom_line(
        data=filter(deaths, Province=="Western Cape" & date <= last_date),
        mapping=aes(x=date, y=change),
        color="#b1c8cc",
        alpha=0.7,
        lwd=Prov.base.size/36
    ) +
    geom_line(
      data=filter(deaths, Province=="Western Cape", date<=last_date),
      mapping=aes(x=date, y=hpf_change, color="Year 3"),
      lwd=Prov.base.size/15
    ) +
    scale_colour_manual( 
      values=c("#ced8deBB", "#79b3edD3",
        "darkblue", 
        "#9ea629BB"
      ), 
      breaks = c("Year 1", "Year 2", "Year 3", "Forecast") 
    ) +
    # geom_line( 
    #     data=filter(coronavirus, date>ymd("2020-03-26")),
    #     mapping=aes(x=date, y=WCchange_roll),
    #     color="darkblue",
    #     lwd=0.9
    # ) +
    labs( 
      title="Western Cape",
      subtitle="Daily new confirmed covid-19 deaths, over time",
      y="Daily deaths",
      caption=paste("Last date:", last_date, "\nAnalysis by Aidan Horn  <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total deaths\nThree-week forecast: SARIMA(7, 1, 3)(1, 1, 1)7 on log daily change..")
    ) +
    scale_y_continuous(
        breaks=seq(0, 200, by=25), 
        minor_breaks=seq(0, 200, by=25),
        expand=expansion(mult=c(0.01, 0.01))
    ) +
    coord_cartesian(
        xlim=c(ymd("2022-03-15"), ymd("2023-03-14")),
        ylim=c(0, 134)
    ) +
    theme(
        legend.margin=margin(t=2, r=5, b=1, l=-35, unit="pt"),
        panel.grid.minor.y=element_blank()
    )

png(
    filename="Graphs/WCdeaths.png",
    width=760, height=760,
    res=Prov.base.size*24
)
AidanWCdeathgraph
dev.off()


AidanKZNgraph <- ggplot() +
    AidanProvgraph + 
    geom_line(
        data=filter(coronavirus, date>ymd("2020-03-25") & date<=last_date), # was 2020-03-26 before I used coord_cartesian(xlim=c(ymd("2020-03-27"), NA))
        mapping=aes(x=date, y=KZNchange),
        color="#b1c8cc",
        alpha=0.7,
        lwd=Prov.base.size/36
    ) +
    geom_line(
      data=coronavirus,
      mapping=aes(x=date_future2, y=KZNtrend, color="Year 1"),
      lwd=Prov.base.size/15
    ) +
    geom_line(
      data=coronavirus,
      mapping=aes(x=date_future, y=KZNtrend, color="Year 2"),
      lwd=Prov.base.size/15
    ) +
    geom_line(
        data=coronavirus %>% 
            filter(
                date>last_date & 
                date<=coronavirus$date[nrow(coronavirus)-21]
            ),
        mapping=aes(x=date, y=KZNtrend, color="Forecast"),
        # alpha=0.7,
        lwd=Prov.base.size/20
        # linetype='dashed'
    ) +
    geom_line(
        data=filter(coronavirus, date<=last_date),
        mapping=aes(x=date, y=KZNtrend, color="Year 3"),
        lwd=Prov.base.size/15
    ) +
    scale_colour_manual( 
      values=c("#ced8deBB", "#79b3edD3", "darkblue", "#9ea629BB"),
      breaks = c("Year 1", "Year 2", "Year 3", "Forecast") 
    ) +
    # geom_line( 
    #     data=filter(coronavirus, date>ymd("2020-03-26")),
    #     mapping=aes(x=date, y=KZNchange_roll),
    #     color="darkblue",
    #     lwd=0.9
    # ) +
    labs( title="Kwa-Zulu Natal" ) +
    scale_y_continuous(
        breaks=seq(0, 10000, by=1000), 
        minor_breaks=seq(0, 10000, by=500),
        expand=expansion(mult=c(0.01, 0.05))
    ) +
    coord_cartesian(
        xlim=c(ymd("2022-03-15"), ymd("2023-03-14")),
        ylim=c(0, 5000)
    ) +
    theme(
      legend.margin=margin(t=2, r=5, b=1, l=-35, unit="pt"),
      panel.grid.minor.y=element_blank()
    ) +
    labs(
        caption=paste("Last date:", last_date, "\nAnalysis by Aidan Horn  <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total.\nTwo-week forecast: SARIMA(7, 1, 3)(1, 1, 1)7 on log daily change.")
    )


png(
    filename="Graphs/KZN.png",
    width=760, height=760,
    res=Prov.base.size*24
)
AidanKZNgraph
dev.off()



AidanGPgraph <- ggplot() +
   AidanProvgraph + 
   geom_line(
      data=filter(coronavirus, date>ymd("2020-03-25") & date<=last_date), # was 2020-03-26 before I used coord_cartesian(xlim=c(ymd("2020-03-27"), NA))
      mapping=aes(x=date, y=GPchange),
      color="#b1c8cc",
      alpha=0.7,
      lwd=Prov.base.size/36
   ) +
   geom_line(
      data=coronavirus,
      mapping=aes(x=date_future2, y=GPtrend, color="Year 1"),
      lwd=Prov.base.size/15
   ) +
   geom_line(
      data=coronavirus,
      mapping=aes(x=date_future, y=GPtrend, color="Year 2"),
      lwd=Prov.base.size/15
   ) +
   geom_line(
      data=coronavirus %>% 
         filter(
            date>last_date & 
               date<=coronavirus$date[nrow(coronavirus)-21]
         ),
      mapping=aes(x=date, y=GPtrend, color="Forecast"),
      # alpha=0.7,
      lwd=Prov.base.size/20
      # linetype='dashed'
   ) +
   geom_line(
      data=filter(coronavirus, date<=last_date),
      mapping=aes(x=date, y=GPtrend, color="Year 3"),
      lwd=Prov.base.size/15
   ) +
   scale_colour_manual( 
      values=c("#ced8deBB", "#79b3edD3", "darkblue", "#9ea629BB"),
      breaks = c("Year 1", "Year 2", "Year 3", "Forecast") 
   ) +
   # geom_line( 
   #     data=filter(coronavirus, date>ymd("2020-03-26")),
   #     mapping=aes(x=date, y=KZNchange_roll),
   #     color="darkblue",
   #     lwd=0.9
   # ) +
   labs( title="Gauteng" ) +
   scale_y_continuous(
      breaks=seq(0, 10000, by=2000), 
      minor_breaks=seq(0, 10000, by=1000),
      expand=expansion(mult=c(0.01, 0.02))
   ) +
   coord_cartesian(
      xlim=c(ymd("2022-03-15"), ymd("2023-03-14")),
      ylim=c(0, 10700)
   ) +
   theme(
      legend.margin=margin(t=2, r=5, b=1, l=-35, unit="pt"),
      panel.grid.minor.y=element_blank()
   ) +
   labs(
      caption=paste("Last date:", last_date, "\nAnalysis by Aidan Horn  <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function: first derivative of a stochastic filter on total.\nTwo-week forecast: SARIMA(7, 1, 3)(1, 1, 1)7 on log daily change.")
   )


png(
   filename="Graphs/GP.png",
   width=760, height=760,
   res=Prov.base.size*24
)
AidanGPgraph
dev.off()


# Cape Town ---------------------------------------------------------------


CT <- # read_csv("C:/Users/hrnaid001/Documents/GitHub/covid19za/data/district_data/provincial_wc_cumulative.csv") %>%
#  read_csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/district_data/provincial_wc_cumulative.csv") %>%
  read_csv("https://raw.githubusercontent.com/aidanhorn/covid19za/WCaidan/data/district_data/provincial_wc_cumulative.csv") %>%
    select(date, YYYYMMDD, CT) %>%
    mutate(
        date=dmy(date),
        date_future = date + years(1) ,
        YYYYMMDD=ymd(YYYYMMDD),
        CT=ifelse(is.na(CT) & date==ymd("2020-03-30"), (lead(CT) - lag(CT))/2 + lag(CT), CT),
        
        CT=ifelse(is.na(CT) & date==ymd("2020-10-09"), (lead(CT, 3) - lag(CT))/4 + lag(CT), CT),
        
        CT=ifelse(is.na(CT) & date==ymd("2020-10-23"), (lead(CT, 3) - lag(CT))/4 + lag(CT), CT),
        
        CT=ifelse(is.na(CT) & date==ymd("2020-10-28"), (lead(CT) - lag(CT))/2 + lag(CT), CT),
        
        CT=ifelse(is.na(CT) & date==ymd("2020-12-25"), (lead(CT, 4) - lag(CT))/5 + lag(CT), CT),
        CT=ifelse(is.na(CT) & date==ymd("2020-12-26"), (lead(CT, 3) - lag(CT, 2))/5 + lag(CT), CT),
        CT=ifelse(is.na(CT) & date==ymd("2020-12-27"), (lead(CT, 2) - lag(CT, 3))/5 + lag(CT), CT),
        CT=ifelse(is.na(CT) & date==ymd("2020-12-28"), (lead(CT) - lag(CT, 4))/5 + lag(CT), CT),
        
        CT=ifelse(is.na(CT) & date==ymd("2021-03-22"), (lead(CT) - lag(CT, 3))*3/4 + lag(CT, 3), CT),
        
        CT=ifelse(is.na(CT) & date==ymd("2021-04-02"), (lead(CT, 4) - lag(CT))/5 + lag(CT), CT),
        CT=ifelse(is.na(CT) & date==ymd("2021-04-05"), (lead(CT) - lag(CT, 4))*4/5 + lag(CT, 4), CT),  # Easter 2021
        
        CT=ifelse(is.na(CT) & date==ymd("2021-04-27"), (lead(CT) - lag(CT))/2 + lag(CT), CT),
        
        CT=ifelse(is.na(CT) & date==ymd("2021-05-06"), (lead(CT) - lag(CT))/2 + lag(CT), CT),
        
        CT=ifelse(is.na(CT) & date==ymd("2021-06-14"), (lead(CT) - lag(CT, 3))*3/4 + lag(CT, 3), CT),
        
        CT=ifelse(is.na(CT) & date==ymd("2021-06-16"), (lead(CT) - lag(CT))/2 + lag(CT), CT),
       
        CT=ifelse(is.na(CT) & date==ymd("2021-06-21"), (lead(CT) - lag(CT, 3))*3/4 + lag(CT, 3), CT),
        
        CT=ifelse(is.na(CT) & weekdays(date)=="Saturday", (lead(CT, 2) - lag(CT))/3 + lag(CT), CT),
        CT=ifelse(is.na(CT) & weekdays(date)=="Sunday", (lead(CT, 1) - lag(CT, 2))*2/3 + lag(CT, 2), CT),
        CTchange=CT-lag(CT), 
        CTchange_roll=rollapply(CTchange, 30, mean, na.rm=TRUE, by=1, partial=TRUE, fill=NA)
    )
tail(CT, n=15L)
sum(is.na(CT$CTchange_roll))

CT.base.size=6.3

CT$CTchange[1] <- 15
CThp <- hp_filter(
        x=as.matrix(
            filter(CT, !is.na(CTchange))$CTchange   # !is.na() should only be removing the last one or two observations, if they are on the weekend.
        ), 
        lambda=3000   # was 1000 previously     # 1600 is customary
    ) 
CT <- mutate(CT, 
   CTtrend=ifelse(!is.na(CTchange), CThp[[2]], NA), 
   
)
plot(CT$CTtrend)

CTgrad <- CT$CTtrend[nrow(CT)-4] - CT$CTtrend[nrow(CT)-5]
CT$CTtrend[nrow(CT)-3] <- CT$CTtrend[nrow(CT)-4] + CTgrad
CT$CTtrend[nrow(CT)-2] <- CT$CTtrend[nrow(CT)-3] + CTgrad
CT$CTtrend[nrow(CT)-1] <- CT$CTtrend[nrow(CT)-2] + CTgrad
CT$CTtrend[nrow(CT)] <- CT$CTtrend[nrow(CT)-1] + CTgrad


AidanCTgraph <- ggplot() +
    theme_minimal(
        base_size=CT.base.size,
        base_line_size=CT.base.size/15
    ) +
    Aidangraph +
    geom_line(
        data=filter(CT, date>ymd("2020-03-20")),
        mapping=aes(x=date, y=CTchange, #color="Reported"
            ),
        color="#a3b2b5D0",
        alpha=0.75,
        lwd=CT.base.size/44
    ) +
#    geom_vline() + geom_hline() +
#    geom_line(
#        data=filter(CT, date>ymd("2020-03-20")),
#        mapping=aes(x=date, y=CTchange_roll, color="30-day rolling average"),
#        lwd=CT.base.size/11
#    ) +
    geom_line(
        data=CT,
        mapping=aes(x=date_future, y=CTtrend, color="Year 1"),
        lwd=CT.base.size/15,
        alpha=0.8
    ) +
    geom_line(
        data=CT,
        mapping=aes(x=date, y=CTtrend, color="Year 2"),
        lwd=CT.base.size/15
    ) +
#    geom_smooth(
#        data=filter(CT, date>ymd("2020-04-01")),
#        mapping=aes(x=date, y=CTchange, color="Trend"),
#        lwd=CT.base.size/16,
#        span=0.3,
#        se=F,
#        degree=1
#    ) +
    scale_colour_manual( 
      values=c("#c7cbd6", "darkblue"), 
      breaks = c("Year 1", "Year 2") 
    ) +
    labs(
        title="Cape Town",
        caption=paste("Last date:", CT$date[nrow(filter(CT, !is.na(CTchange)))], "\nSource: https://coronavirus.westerncape.gov.za/news\nAnalysis by Aidan Horn  <www.aidanhorn.co.za/blog/computing/shiny/covid-19>\nSmoothing function used is an HP filter.")
    ) +
    scale_y_continuous(
        breaks=seq(0, 4000, by=500), 
        minor_breaks=seq(0, 4000, by=500),
        expand=expansion(mult=c(0.01, 0.01))
    ) +
    coord_cartesian(
        xlim=c(ymd("2021-03-15"), ymd("2022-03-14")),
        ylim=c(0, 2000)
    ) +
    theme(
        axis.text.x=element_text(size=CT.base.size*1),
        axis.title.x=element_text(margin=margin(t=-CT.base.size/5)),
        plot.caption=element_text(size=CT.base.size/1.6),
        legend.box.margin=margin(t=-1/CT.base.size*40, b=-1/CT.base.size*40),
        panel.grid.major.x=element_line(size=0.055*CT.base.size, color="#7cb870D0"),
        panel.grid.minor.x=element_line(size=0.05*CT.base.size),
        panel.grid.major.y=element_line(size=0.05*CT.base.size),
        panel.grid.minor.y=element_line(size=0.045*CT.base.size),
        # axis.title.x=element_text(margin=margin(t=CT.base.size))
    )
AidanCTgraph

png(
    filename="Graphs/CT.png",
    width=1000, height=1000,
    res=CT.base.size*70,
    pointsize=CT.base.size
)
AidanCTgraph + labs(x="A year")
dev.off()






ggplot(
  data = coronavirus %>% 
    select(GP, date, GPtotaltrend) %>% 
    filter(date > ymd("2021-10-10") & date < ymd("2021-12-05")), 
  mapping = aes(x=date)
) + 
  geom_point(aes(y=GP)) + 
  geom_line(aes(y=GPtotaltrend))

# GP: 2021-10-25 to 2021-11-18


ggplot(
  data = coronavirus %>% 
    select(KZN, date, KZNtotaltrend) %>% 
    filter(date > ymd("2021-10-10") & date < ymd("2021-12-05")), 
  mapping = aes(x=date)
) + 
  geom_point(aes(y=KZN)) + 
  geom_line(aes(y=KZNtotaltrend))

# KZN: 2021-10-30 to 2021-11-29


# Jump happened countrywide on 2021-11-23.





