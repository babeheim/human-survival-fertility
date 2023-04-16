
source("project_support.R")

# downloaded in 2017 (url's are dead)

# https://ourworldindata.org/grapher/natural-population-growth

# Natural growth rate
# UN population division 
# Crude birth rate minus the crude death rate. Represents the portion of population growth (or decline) determined exclusively by births and deaths.

# (for TFR, originally used https://ourworldindata.org/grapher/total-fertility-rate but link is dead
# not used: children-per-woman-UN

# https://ourworldindata.org/grapher/life-expectancy

# Life expectancy at birth
# The average number of years that a newborn could expect to live, if he or she were to pass through life exposed to the sex- and age-specific death rates prevailing at the time of his or her birth, for a specific year, in a given country, territory, or geographic area.
# source: United Nations, Department of Economic and Social Affairs, Population Division (2022). World Population Prospects 2022, Online Edition; Zijdeman et al. (2015) (via clio-infra.eu); Riley, J. C. (2005). Estimates of Regional and Global Life Expectancy, 1800-2001. Population and Development Review, 31(3), 537–543. http://www.jstor.org/stable/3401478

# https://ourworldindata.org/grapher/children-born-per-woman

# Children Born per Woman
# Fertility rate
# Total fertility rate represents the number of children that would be born to a woman if she were to live to the end of her childbearing years and bear children in accordance with age-specific fertility rates of the specified year.
# publisher: Gapminder
# Data is that of version 12 of Gapminder, the latest version as of 2019. This is the full fertility rate dataset published by Gapminder.


cat("process data\n")

cat("life expectancy at birth by country and year\n")

life <- read.csv("data/ourworldindata2023/life-expectancy.csv")

stopifnot(nrow(life) == 20449) # up from 14177 in 2017
# however, a good chunk of these are for regional aggregates and should be excluded
stopifnot(length(unique(life$Code)) == 238) # up from 199 in 2017
stopifnot(length(unique(life$Entity)) == 256) # up from 202 in 2017

stopifnot(range(life$Year) == c(1543, 2021))

life <- rename(life, e0 = `Life.expectancy.at.birth..historical.`)
life <- mutate(life, key = paste(Code, Year))



cat("population growth by country and year\n")

growth <- read.csv("data/ourworldindata2023/natural-population-growth.csv")

stopifnot(nrow(growth) == 38505) # up from 3120 in 2017, wow
stopifnot(length(unique(growth$Code)) == 238) # up from 204 in 2017
stopifnot(range(growth$Year) == c(1950, 2100))

growth <- rename(growth, gr = `Natural.growth.rate...Sex..all...Age..all...Variant..estimates`)
growth <- mutate(growth, key = paste(Code, Year))



cat("total fertility rate by country and year\n")

tfr <- read.csv("data/ourworldindata2023/children-born-per-woman.csv")

stopifnot(nrow(tfr) == 22185) # originally 60673 in 2017
stopifnot(length(unique(tfr$Code)) == 200)
stopifnot(range(tfr$Year) == c(1541, 2022)) # originally extrapolated to 2099 in 2017

tfr <- rename(tfr, tfr = `Fertility.rate..Select.Gapminder..v12...2017.`)
tfr <- mutate(tfr, key = paste(Code, Year))



cat("create analysis dataframe\n")

life |> 
  filter(Code != "") |>
  inner_join(select(tfr, tfr, key), by = "key") |>
  left_join(select(growth, gr, key), by = "key") |>
  select(-key) -> d 

stopifnot(nrow(d) == 16084) # originally 13881 in 2017
stopifnot(sum(is.na(d$gr)) == 1908) # originally 11687 in 2017

stopifnot(range(d$Year) == c(1543, 2021))
stopifnot(length(unique(d$Code)) == 197)

write.csv(d, "data/nation_year_demographics.csv", row.names = FALSE)

