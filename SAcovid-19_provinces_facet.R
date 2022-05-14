# Charts showing daily covid-19 cases in South African provinces, over time
# Aidan Horn
# September 2020

# Currently loaded external packages
names(sessionInfo()$otherPkgs)
# List of required external packages
packages <- c(
    "ggplot2", 
    "tidyverse", 
    "lubridate", 
    "zoo", 
    "scales",
    "latex2exp", 
    "lpirfs"
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
            date > ymd("2021-10-25") & date < ymd("2021-12-01"),
            NA,
            total_cases
        ),
        total_cases = ifelse(   # Step 2
            date >= ymd("2021-10-25") & date <= ymd("2021-12-01"),
            na.approx(total_cases),
            total_cases
        ),
        total_cases = ifelse(   # Step 3
            date > ymd("2021-11-18") & date < ymd("2021-12-12"),
            NA,
            total_cases
        ),
        total_cases = ifelse(   # Step 4
            date >= ymd("2021-11-18") & date <= ymd("2021-12-12"),
            na.approx(total_cases),
            total_cases
        ),
        total_cases = ifelse(   # Step 5
            date > ymd("2021-12-02") & date < ymd("2021-12-15"),
            NA,
            total_cases
        ),
        total_cases = ifelse(   # Step 6
            date >= ymd("2021-12-02") & date <= ymd("2021-12-15"),
            na.approx(total_cases),
            total_cases
        ),
        change=total_cases-lag(total_cases), 
        weeklychange =  change + 
                        lag(change) + 
                        lag(change, 2) +
                        lag(change, 3) +
                        lag(change, 4) +
                        lag(change, 5) +
                        lag(change, 6),
        
        change_roll=rollapply(change, 14, mean, na.rm=TRUE, by=1, partial=TRUE, fill=NA),
        Province=factor(
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
            .data=coronavirus %>% 
                filter(Province==prov # & date>=ymd("2020-03-14")
                ) %>% select(total_cases, YYYYMMDD),
            hpf_temp=hp_filter(
                x=as.matrix(log(.data$total_cases)),
                lambda=1600     # 5000      # 1600
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
            list=is.na(hpf) & Province==prov, # & date>=ymd("2020-03-14"), 
            values=exp(hpf_temp[Province==prov])
        )) %>%
        select(-hpf_temp)
}
coronavirus <- coronavirus %>% 
    group_by(Province) %>%
    mutate(
        hpf=ifelse(hpf<0, 0, hpf),
        hpf_change=hpf-lag(hpf)
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



# Check for Eastern Cape
ec <- ggplot(
  data = coronavirus %>% 
    filter(Province=="Eastern Cape" & date > ymd("2021-10-15") & date < ymd("2021-12-25")) %>%
    select(date, total_cases, hpf), 
  mapping = aes(x=date)
) + 
  geom_point(aes(y=total_cases)) + 
  geom_line(aes(y=hpf))
ec

png(
    filename="Archive/EC_imputation.png",
    width=1000, height=1000,
    res=190
)
ec
dev.off()



province_graph <- ggplot() + 
    theme_minimal() +
    theme(
        axis.text.x=element_text(angle=90, hjust=1, size=11),
        plot.title.position="plot",
        plot.subtitle=element_text(margin=margin(b=10)),
        panel.grid.major.x=element_line(linetype="solid", size=0.4, color="#c2ba80"),
        panel.grid.major.y=element_line(linetype="solid", size=0.5, color="#c2b89bB6"),
        panel.grid.minor.x=element_line(linetype="solid", size=0.4, color="#c2ba8020"),
        # axis.title.y=element_text(size=10),
        axis.title.x=element_text(margin=margin(t=6)),
        plot.caption=element_text(size=7, hjust=0, margin=margin(t=0), # margin(t=4)
            color="darkgray"),
        plot.caption.position="plot",
        panel.spacing.y=unit(1.1, "lines"), # unit(0.15, "lines"),
        panel.spacing.x=unit(0.9, "lines"),
        axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        legend.position=c(0.73, 1.05),
        # legend.position=c(0.73, 1.122),
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.box.margin=margin(b=-6, t=-6),
        strip.text=element_blank()
        # strip.text=element_text(vjust=-1)
    ) +
    scale_x_date(
        breaks=seq(ymd("2021-04-01"), ymd("2024-02-01"), by="3 months"),
       # date_breaks="2 months", 
        date_labels= # c("Apr", "Jul", "Oct", "Jan", " "),
            "%b",
        minor_breaks=seq(ymd("2021-04-01"), ymd("2024-02-01"), by="1 month"),
       # date_minor_breaks="1 month",
        expand=expansion(
            mult=c(
               0, 
               0 # 0.1   # 0.12
            )
        )
    ) +
    labs(
        x="A year"
    ) +
    facet_wrap(facets="Province") +
    scale_colour_manual( 
        values=c("#c7cbd6C8", "#79b3edD3", "darkblue"), 
        breaks = c("Year 1", "Year 2", "Year 3") 
    )


province_cases <- province_graph +
    geom_line(
        data=coronavirus,
        mapping=aes(x=date_future2, y=hpf_change, color="Year 1"),
        lwd=0.7
    ) +
    geom_line(
        data=coronavirus,
        mapping=aes(x=date_future, y=hpf_change, color="Year 2"),
        lwd=0.7
    ) +
    geom_line(
        data=coronavirus,
        mapping=aes(x=date, y=change),
        color="#8D5F3B", # "#b1c8cc",
        alpha=0.4, # 0.8, # 1,
        lwd=0.3  # 0.7
    ) +
    geom_line(
        data=coronavirus,
        mapping=aes(x=date, y=hpf_change, color="Year 3"),
        lwd=0.8
    ) +
    labs(
        caption=paste("Last date:", coronavirus$date[nrow(coronavirus)], "\nAidan Horn <www.aidanhorn.co.za/blog/graphs>\nSmoothing function: first derivative of a stochastic filter on total cases.")
    )


levelyscale <- province_cases +
    theme(
        panel.grid.minor.y=element_blank(), # element_line(linetype="solid", size=0.45, color="#f2eddfF0"),
        axis.text.y=element_text(size=8)
    ) +
    labs(
        title="Daily new confirmed covid-19 cases, over time", # "Confirmed covid-19 cases per week",
        subtitle="Across South African provinces",
        y="Daily cases"
    ) +
    scale_y_continuous(
        breaks=seq(0, 14000, 2000), # weekly: seq(0, 100000, 20000),
        expand=expansion(mult=c(0.01, 0.02))
        # labels=label_number(suffix="k", scale=1e-3)
    ) +
    coord_cartesian(
        xlim=c(ymd("2022-03-15"), ymd("2023-03-14")), # c(ymd("2021-03-30"), ymd("2022-03-29")), # coronavirus$date[nrow(coronavirus)]),
        ylim=c(0, 10000) # 66000)
    ) +
    geom_text(
    data=coronavirus,
     x=ymd("2023-02-15"), # ymd("2020-04-15"),
     y=9600, # 57000,
     aes(
       label=Province,
       hjust=1,
       vjust=1
     ),
     size=3.8,
     fontface="plain",
     check_overlap=T
   )
levelyscale

png(
    filename="Graphs/All_provinces.png",
    width=1000, height=1000,
    res=190
)
levelyscale
dev.off()


province_cases_story <- province_graph +
    geom_line(
        data=filter(coronavirus, Province!="Limpopo"),
        mapping=aes(x=date_future2, y=hpf_change, color="Year 1"),
        lwd=0.7
    ) +
    geom_line(
        data=filter(coronavirus, Province!="Limpopo"),
        mapping=aes(x=date_future, y=hpf_change, color="Year 2"),
        lwd=0.7
    ) +
    geom_line(
        data=filter(coronavirus, Province!="Limpopo"),
        mapping=aes(x=date, y=change),
        color="#8D5F3B",  # "#b1c8cc",
        alpha=0.4, # 0.8, # 1,
        lwd=0.3  # 0.7
    ) +
    geom_line(
        data=filter(coronavirus, Province!="Limpopo"),
        mapping=aes(x=date, y=hpf_change, color="Year 3"),
        lwd=0.8
    ) +
    facet_wrap(
       facets="Province",
       ncol=2
    ) +
    theme(
       legend.position = "top", # c(0.4, 1), # c(0.73, 1.05),
       # legend.box.spacing = unit(20, "pt"),
       legend.box.margin = margin(t=-13, r=40, b=-10, l=-60)  # margin(b=20, t=10)
    )

levelyscale_story <- province_cases_story +
    theme(
        panel.grid.minor.y=element_line(linetype="solid", size=0.45, color="#f2eddfF0"),
        axis.text.y=element_text(size=8),
        # plot.margin=unit(c(20, 9, 16, 12), "points"),
        plot.margin=unit(c(90, 9, 70, 12), "points"),
        plot.caption=element_text(size=9, hjust=0, margin=margin(t=3), 
            color="darkgray"),
       panel.spacing.x=unit(1, "lines")
    ) +
    labs(
        title="Daily new confirmed cases of covid-19",
        subtitle="South African provinces",
        y="Daily cases"
    ) +
    scale_y_continuous(
        breaks=seq(0, 15000, 5000),  # seq(0, 100000, 25000),
        expand=expansion(mult=c(0.01, 0.02))
        # labels=label_number(suffix="k", scale=1e-3)
    ) +
    coord_cartesian(
        xlim=c(ymd("2022-03-15"), ymd("2023-03-14")),
        ylim=c(0, 11600) # 75000)
    ) +
    geom_text(
       data=filter(coronavirus, Province!="Limpopo"),
        x=ymd("2023-02-15"),
        y=11200,  # 74000,
        aes(
          label=Province,
          hjust=1,
          vjust=1
        ),
        size=3.8,
        fontface="plain",
        check_overlap=T
    )
levelyscale_story

png(
    filename="Graphs/All_provinces_story.png",
    # width=720, height=1280,
    # res=190
    width=550, height=1200,
    res=155
)
levelyscale_story
dev.off()



### Log scale ###
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

logyscale <- province_cases +
    scale_y_log10(
        breaks=breaks, 
        minor_breaks=minor_breaks,
        expand=expansion(mult=c(0.03, 0.03)),
        labels=label_comma(big.mark=" ")
    ) +
    coord_cartesian(
        xlim=c(ymd("2022-03-15"), ymd("2023-03-14")), # coronavirus$date[nrow(coronavirus)]),
        ylim=c(10, 21000) # 100000)
    ) +
    theme(
        panel.grid.minor.y=element_line(linetype="solid", size=0.3, color="#f2eddfF0")
    ) +
    labs(
        title="Daily new confirmed cases of covid-19 (log scale)",
        subtitle="Across South African provinces",
        y="Daily cases (log scale)"
    ) +
    geom_text(
    data=coronavirus,
     x=ymd("2023-02-15"),
     y=log10(19500),  # 90000),
     aes(
       label=Province,
       hjust=1,
       vjust=1
     ),
     size=3.7,
     fontface="plain",
     check_overlap=T
   )
logyscale

png(
    filename="Graphs/All_provinces_logscale.png",
    width=1100, height=1100,
    res=190
)
logyscale
dev.off()



### Log scale story

logyscale_story_WA <- province_cases_story +
   scale_y_log10(
      breaks=breaks, 
      minor_breaks=minor_breaks,
      expand=expansion(mult=c(0.006, 0.006)),
   ) +
   coord_cartesian(
      xlim=c(ymd("2022-03-15"), ymd("2023-03-14")),
      ylim=c(10, 21200)
   ) +
   theme(
      panel.grid.minor.y = element_blank(), # element_line(linetype="solid", size=0.3, color="#f2eddfF0"),
      axis.text.y = element_text(size=8),
      plot.margin = unit(c(90, 9, 70, 12), "points"),
      plot.caption = element_text(
         size = 9, 
         hjust = 0, 
         margin = margin(t=-2),  # 8 
         color = "darkgray"
      ),
      panel.spacing.x=unit(1, "lines")
   ) +
   labs(
      title="Daily new confirmed cases of covid-19",
      subtitle="South African provinces",
      y="Daily cases (log scale)",
      caption=paste("Last date: ", coronavirus$date[nrow(coronavirus)], "\nAidan Horn <www.aidanhorn.co.za/blog/graphs>")
   ) +
   geom_text(
      data=filter(coronavirus, Province!="Limpopo"),
      x=ymd("2023-02-15"),
      y=log10(20300),
      aes(
         label=Province,
         hjust=1,
         vjust=1
      ),
      size=3.5,
      fontface="plain",
      check_overlap=T
   )
logyscale_story_WA

png(
   filename="Graphs/All_provinces_logscale_story_WA.png",
   width=550, height=1200,
   res=155
)
logyscale_story_WA
dev.off()




### Deaths ###

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
        total_cases=ifelse(total_cases==0, 1, total_cases),
        change=total_cases-lag(total_cases), 
        weeklychange =  change + 
                        lag(change) + 
                        lag(change, 2) +
                        lag(change, 3) +
                        lag(change, 4) +
                        lag(change, 5) +
                        lag(change, 6),
        
        change_roll=rollapply(change, 14, mean, na.rm=TRUE, by=1, partial=TRUE, fill=NA),
        Province=factor(
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
        .data=deaths %>% 
            filter(Province==prov # & date>=ymd("2020-04-07")
            ) %>% select(total_cases, YYYYMMDD),
        hpf_temp=hp_filter(
            x=as.matrix(log(.data$total_cases)),
            lambda=5000  # 26000
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
            list=is.na(hpf) & Province==prov, # & date>=ymd("2020-04-07"), 
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


deathsplot <- province_graph +
    geom_line(
        data=filter(deaths, date>ymd("2020-03-30")),
        mapping=aes(x=date_future2, y=hpf_change, color="Year 1"),
        lwd=0.7
    ) +
    geom_line(
        data=filter(deaths, date>ymd("2020-03-30")),
        mapping=aes(x=date_future, y=hpf_change, color="Year 2"),
        lwd=0.7
    ) +
    geom_line(
        data=filter(deaths, date>ymd("2020-03-30")),
        mapping=aes(x=date, y=change),
        color="#8D5F3B", # "#b1c8cc",
        lwd=0.28,
        alpha=0.3  # 0.64
    ) +
    geom_line(
        data=filter(deaths, date>ymd("2020-03-30")),
        mapping=aes(x=date, y=hpf_change, color="Year 3"),
        lwd=0.8
    ) +
    theme(
        panel.grid.minor.y=element_blank(), # element_line(linetype="solid", size=0.45, color="#f2eddfB0"),
        axis.text.y=element_text(size=9),
        panel.spacing.y=unit(1.3, "lines")
    ) +
    labs(
        title="Daily new confirmed covid-19 deaths, over time",
        subtitle="Across South African provinces",
        y="Daily deaths",
        caption=paste("Last date:", coronavirus$date[nrow(coronavirus)], "\nAidan Horn <www.aidanhorn.co.za/blog/graphs>\nSmoothing function: first derivative of a stochastic filter on total deaths.")
    ) +
    scale_y_continuous(
        breaks=seq(0, 250, 50),  # 20000, 500)
        minor_breaks=seq(0, 250, 10),  # seq(0, 20000, 250),
        expand=expansion(mult=c(0.01, 0.01))
    ) +
    coord_cartesian(
        xlim=c(ymd("2022-03-15"), ymd("2023-03-14")), # coronavirus$date[nrow(coronavirus)]),
        ylim=c(0, 155) # 1150) for weekly change
    ) +
    geom_text(
    data=deaths,
     x=ymd("2022-12-26"),
     y=140,  # 1150,
     aes(
       label=Province,
       hjust=1,
       vjust=1
     ),
     size=3.5,
     fontface="plain",
     check_overlap=T
   )
deathsplot

png(
    filename="Graphs/Deaths_by_province.png",
    width=1000, height=1000,
    res=190
)
deathsplot
dev.off()



