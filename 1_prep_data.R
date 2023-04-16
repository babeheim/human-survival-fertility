
source("project_support.R")

if (!dir.exists("data/ourworldindata")) {

  dir_init("./data/ourworldindata")

  cat("downloading data\n")
  download_file("https://ourworldindata.org/grapher/total-fertility-rate.csv", "data/ourworldindata/total-fertility-rate.csv")
  download_file("https://ourworldindata.org/grapher/life-expectancy.csv", "data/ourworldindata/life-expectancy.csv")
  download_file("https://ourworldindata.org/grapher/natural-population-growth.csv", "data/ourworldindata/natural-population-growth.csv")

  # not used: children-per-woman-UN.csv
  # not used: population-by-country.csv

}


cat("process data\n")



cat("life expectancy at birth by country and year\n")

life <- read.csv("data/ourworldindata/life-expectancy.csv")

stopifnot(nrow(life) == 14177)
stopifnot(length(unique(life$Country.code)) == 199)
stopifnot(length(unique(life$Entity)) == 202)
# two kinds of country codes

stopifnot(range(life$Year) == c(1543, 2012))

life <- rename(life, e0 = `Life.Expectancy.at.Birth..both.genders.`)
life <- mutate(life, key = paste(Country.code, Year))



cat("population growth by country and year\n")

growth <- read.csv("data/ourworldindata/natural-population-growth.csv")

stopifnot(nrow(growth) == 3120)
stopifnot(length(unique(growth$Country.code)) == 204)
stopifnot(range(growth$Year) == c(1955, 2015))

growth <- rename(growth, gr = `Rate.of.Natural.Population.Increase...UN.2015`)
growth <- mutate(growth, key = paste(Country.code, Year))



cat("total fertility rate by country and year\n")

tfr <- read.csv("data/ourworldindata/total-fertility-rate.csv")

stopifnot(nrow(tfr) == 60673)
stopifnot(length(unique(tfr$Country.code)) == 202)
stopifnot(range(tfr$Year) == c(1541, 2099))

tfr <- rename(tfr, tfr = `Gapminder..Fertility.v6.`)
tfr <- mutate(tfr, key = paste(Country.code, Year))



cat("create analysis dataframe\n")

life |> 
  inner_join(select(tfr, tfr, key), by = "key") |>
  left_join(select(growth, gr, key), by = "key") |>
  select(-key) -> d 

stopifnot(nrow(d) == 13881)
stopifnot(sum(is.na(d$gr)) == 11687)
# we are missing growth rates for most entries but that's not limiting

stopifnot(range(d$Year) == c(1543, 2012))
stopifnot(length(unique(d$Country.code)) == 197)

write.csv(d, "data/nation_year_demographics.csv", row.names = FALSE)

