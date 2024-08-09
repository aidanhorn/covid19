# Coronavirus Trends (R Shiny App)

Includes **41 countries, cases and deaths.**

## South Africa

![South Africa covid-19 graph](https://www.dropbox.com/s/l0mq0go1fexqutw/South%20Africa.png?raw=1)

## Interpretation

The trend in cases may be out of proportion, due to increased testing. The trend in deaths is a more accurate description of the trend (this is also confirmed by [research on seropositivity tests](https://www.economist.com/briefing/2020/09/26/the-covid-19-pandemic-is-worse-than-official-figures-show)—testing blood samples from a random sample of the population). For excellent research on excess deaths globally, visit https://www.economist.com/graphic-detail/coronavirus-excess-deaths-estimates .

The development of new vaccines to counter the coronavirus pandemic will continue.

## Hodrick-Prescott filter

There is a lot of noise (variance) in the reported number of new cases daily (this could be due to testing laboratories not working on weekends, for example). Therefore, one needs to smooth out the noise, in order to get a good estimate for how the rate of infection is actually trending in reality. I use a [Hodrick-Prescott (HP) filter](https://en.wikipedia.org/wiki/Hodrick%E2%80%93Prescott_filter) in my graphs, which is a statistical function that removes the noise from a time series, leaving the underlying trend. This trend curve is smoother than a rolling average, so it is simpler to look at and understand, when observing the graphs.

Note that, understandably, the estimated HP-filter trend on the end-point can be slightly inaccurate, due to the future reported cases being unknown. I have solved this problem by forecasting the trends, then cutting off the HP filter. The HP filter is however a good estimate of the true trend in the rate of cases—in comparison, the latest daily numbers you see being reported elsewhere on the internet are inaccurate because of the high variance of that statistic. When one reads my graphs, one can simply use the y-axis to read what the latest daily rate of infections is, but overall, the focus of my graphs is more on the shapes of the trends of infection rates over time.

## Media Posts

- [Twitter](https://twitter.com/AidanHorn1/status/1525592405851914242)
- [Facebook](https://www.facebook.com/aidanjhorn.page/posts/433137861361355)
- [LinkedIn](https://www.linkedin.com/posts/aidan-horn_daily-covid-19-cases-across-provinces-in-activity-6707022105550163969-OnNn).

[News24 article: Tom Moultrie on the limitations of aggregator dashboards.](https://www.news24.com/news24/Analysis/analysis-what-can-and-cant-we-learn-from-the-official-covid-19-data-20200430)