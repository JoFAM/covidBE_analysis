# Analysis of evolution COVID in Belgium

I provide a set of scripts that download the data from Sciensano, the Belgian 
source for numbers on covid, and subsequently provide basic summary statistics.

**WORK IN PROGRESS**

## Goal and a warning

This is not a solid statistical analysis, but allows you to browse the data
in a bit more detail. The code is free to use (under the GPL-3 license), 
as long as you refer to me as a source. It comes without any warranty,
and should not be used as official communication or proof of anything.

There's enough armchair epidemiologists on social media as it is. Use this
code to give you more insight, not to confirm your own firm opinion on
how this epidemic evolves. Because frankly, the public data just isn't 
enough to give us solid answers. It can only serve as a warning.

## Organisation

This code base can be run as a shiny app or separate scripts. It's built
up as follows :

 - **scripts**: contains the scripts that take the different steps for
 the summarisation of the data.
 - **modules**: contains shiny modules that are used in the app itself.
 - **functions**: contains a set of utility functions I use throughout
 the code base.
 - **Data**: contains the data on the population by age class, obtained
 from https://bestat.statbel.fgov.be/bestat/
 - **global.R**: is the file that sets up the global environment and takes
 the necessary steps to run whatever script necessary.
 - **ui.R** and **server.R** form the actual application.
 
You can run this from within RStudio using :

```
runGitHub("JoFAM/CovidAgeDistBE")
```
## Actions

The shiny application takes the following steps:

 - download the data from https://epistat.wiv-isp.be/Covid/
 - calculate the 7-day running averages to smooth out the effect of differences between week and weekend in reporting. (This is a naive smoothing technique, but gives a better idea of the long term trends)
 - save this data with the date as a timestamp. When you rerun the code the 
 next day, the timestamp will tell the code to download the newest data.
 - summarise the data in a set of graphs I myself find informative. But you have the data and the scripts, so have a blast.
