
## WindRose:

Plots wind direction and wind speed. You can choose to plot it by month, year, day, etc.

- color: wind speed bin
- paddle size: also wind speed
- direction: direction where the wind comes from
- percentage: percentage of time going in that direction at that wind speed. * It doesn't always look like it adds to 100%.

This function also calculates percentage of time when the wind speed is 0, the calm.


- remaining question: the bigger wind speed is more dominant. what happens when small winds are more dominant than large winds??


## PercentileRose:

Calculates percentile levels of a pollutant and plots them by wind direction. In this case, the binned percentile levels, are bins based on the magnitude of bin0 data.

- concentric circles : wind speed
- direction: direction where the wind comes from
- color : the bin associated with a certain wind speed and direction, where the bin is a subsection of the data, divided by percentile


It is most useful for showing the distribution of concentrations by wind direction and often can reveal different sources, e.g. those that only affect high percentile concentrations such as a chimney stack.

## summaryPlot

Shows you the time series and histograms of the different pollutants in your dataset. Can specify which pollutants you want to look at or which time range. 

## timeVariation

I think that Scott called this a diurnal plot as well. It's a collection of four plots, that shows monthly trends, weekly trends and daily trends. It will also create a plot that's hourly data, for each day of the week. This is done by averaging the data in the timeframe that you picked. It also 

- In the documentation, it says that it is useful to normalize the pollutants, if you're looking at two or more pollutants at once, in order to compare trends.
- You can pick different statistics variables as well. For example, if you pick "median", you see the median line, the 25/75th  quantile values, and the 5/95th quantile values. All of this can help you see how data is distributed. 
- If you set it equal to a variable (ie myoutput <- timeVariation(mydata...)) then you receive the data in a table format. 

## corPlot

This creates a correlation matrix, where you can see the correlation between any two pairs of data in the dataset. Conveniently, corPlot also visually represents the correlation between these pairs in the matrix, by adding an ellipse shape and a color around the numeric value in the matrix. I don't think it's stated in the openair manual which correlation function is used, but I assume Pearson's or something else standard. 

- One useful feature of this function is that there are multiple ways it allows you to see which variables tend to behave most like one another. You can use cluster analysis (ie cluster=TRUE) in order to group variables that are most similar. I'm not sure at this time if similarity is just determined by correlation value or some other factor. You can also do this with a dendogram, but I'm still a little confused by what it is actually showing.
- You can also split up your data into quarentiles or some time interval that we use (like week, season, year, etc). I think these will show as different plots. 
- Apparently, it is typical to combine the data from a lot of sites in this analysis, and the manual walks through an example of this. 

## pollutionRose

PollutionRose shows how wind direction is associated with which concentrations of a given pollutant. 


- color : pollutant bin
- paddlesize : not stated, but since it's a wrapper for windRose, I assume that it's just the same as the concentric circles.
- concentric circles : the percentage of time the concentration is in a particular range. I think this is the most difficult part of the graph to interpret. I also assume that range refers to wind direction but I'm not sure. 
- direction : direction where the wind comes from

- The manual says that if you set "statistic="prop.mean"" (which is proportional contribution to the mean) then you can use that to get a sense of which wind directions contribute most to overall concentrations. 


## polarPlot

During our IS, we decided to not use this function, because it is the least intuitive of the three plots (windPlot, percentilePlot, polarPlot) to interpret. That's because it's a plot that shows pollution concetration as a function of wind direction and wind speed, but when you look at it, it looks like pollution as a function of 2D space.

- color: pollutant concentration
- concentric circles : wind speed
- direction : wind direction 

Like other openair functions, you can apply statistics and separate the plot into quartiles or seasons. 

## Hexbin method

When you're graphing a large dataset, it can be helpful to bin the data, to produce a cleaner graph

from openair: "The benefit of hexagonal binning is that it works equally well with enormous data sets e.g. several million records."
