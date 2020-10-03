# edinburgh_bike_hire

## Summary

Last updates: 4th October 2020

A mish mash of various data visualisation and analysis of City of Edinburgh's [cycle hire statistics](https://edinburghcyclehire.com/open-data). I'll add to this as I go.

## Bike share heatmap

Focusing on 2020, the impact of the 23 March lockdown on bike rentals is clear.  However, rentals pick up for some stations, possibly because of good weather, an unwillingness to use public transport, and quieter streets. Most of the stations seem to get busier as lockdown progresses, but there are clear bands of low use - see for example George Square, the heart of the old University of Edinburgh campus. 

There are two noticeable peaks in Cramond and Portobello in the same week. I don't know why, but it could be related to good weather. It might also be the case Portobello residents were using the scheme to travel to Cramond - I'll check this.

![Heatmap of bike station checkouts in time (2020), absolute weekly counts](output/Edinburgh_weekly_bike_rentals_checkouts_2020.png)


We have a better picture of peak use by normalising the colour scale to peak useage. This also allows me to add a total row without skewing the colour scale. To note the barplots are on a log10 scale.

![Heatmap of bike station checkouts in time (2020), normalised to peak weekly counts](output/Edinburgh_weekly_bike_rentals_checkouts_peak_proportion_2020.png)

