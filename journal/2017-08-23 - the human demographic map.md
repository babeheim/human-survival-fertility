
This figure is based on Livi-Bacci's, but updated to use the best data.

The essential idea is that, under a broad range of assumptions, population growth is determined once you know

We begin with data from Max Roser's [Our World in Data](https://ourworldindata.org), which has downloadable files. We can get national life expectancy, total fertility and population growth in seperate sets.

In each case the coding is similar: national units are under the variable `Entity` with a corresponding `Country.code`, and we have the outcome variable. I will use the codes `le` for life expectancy, `r` for growth and `

We can combine all of these using the keys.

TFR, LE and growth are related by a log-linear equation

$$
	growth = a + b log(tfr) + c log(le)
$$

Empirical estimation puts this at

And, since we are missing growth for many populations, we can include our estimate from the fitted model. I call the function `growth_isocline`.

The result is, for me, one of the most beautifully informative figures ever.


We can include other informatoin as well.


Here, I draw these as regions.

Ok, now to make a series of pictures



Now merge them all together!

Ok, here's the best version of this figs so far (test 6)


We can include other informatoin as well.


Here, I draw these as regions.




Gurven and Kaplan report average life expectancy of:

- 29.23 for hunter/gatherers
- 38.80 for Horticulturalists
- 42.78 for pastoralists

They also report the following TFRs

- 5.7 for hunter-gatherers
- 6.2 for hort
- 5.6 for pastoralists

Each of these is a coordinate on the plot.

Before 1850, no human population existed. 

Between 1900 and 1955, set of human populations slide down. Followed by an explosion of growth, a second set of populations.
Arriving the sane place.
