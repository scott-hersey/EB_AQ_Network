
## WindRose:

Plots wind direction and wind speed. You can choose to plot it by month, year, day, etc.

- color: wind speed bin
- paddle size: also wind speed
- direction: direction where the wind comes from
- percentage: percentage of time going in that direction at that wind speed. * It doesn't always look like it adds to 100%.

* again, the bigger wind speed is more dominant. what happens when small winds are more dominant than large winds??

This function also calculates percentage of time when the wind speed is 0, the calm.

## PercentileRose:

Calculates percentile levels of a pollutant and plots them by wind direction. In this case, the binned percentile levels, are bins based on the magnitude of bin0 data.

- concentric circles : wind speed
- direction: direction where the wind comes from
- color : the bin associated with a certain wind speed and direction, where the bin is a subsection of the data, divided by percentile


It is most useful for showing the distribution of concentrations by wind direction and often can reveal different sources
e.g. those that only affect high percentile concentrations such as a chimney stack.


## Hexbin method

When you're graphing a large dataset, it can be helpful to bin the data, to produce a cleaner graph

from openair: "The benefit of hexagonal binning is that it works equally well with enormous data sets e.g. several million records."
