
source("project_support.R")

if (!dir.exists("data")) {

  dir_init("./data")

  cat("downloading data\n")
  download_file("https://ourworldindata.org/grapher/children-per-woman-UN.csv", "./data/ourworldindata/children-per-woman-UN.csv")
  download_file("https://ourworldindata.org/grapher/total-fertility-rate.csv", "./data/ourworldindata/total-fertility-rate.csv")
  download_file("https://ourworldindata.org/grapher/life-expectancy.csv", "./data/ourworldindata/life-expectancy.csv")
  download_file("https://ourworldindata.org/grapher/population-by-country.csv", "./data/ourworldindata/population-by-country.csv")
  download_file("https://ourworldindata.org/grapher/natural-population-growth.csv", "./data/ourworldindata/natural-population-growth.csv")

}


cat("process data\n")

life <- read.csv("./data/ourworldindata/life-expectancy.csv")

# Entity
# Year
# Country.code
# Life.Expectancy.at.Birth..both.genders.

life <- rename(life, le = `Life.Expectancy.at.Birth..both.genders.`)
life$key <- paste(life$Country.code, life$Year)

growth <- read.csv("./data/ourworldindata/natural-population-growth.csv")

# Entity
# Year
# Country.code
# Rate.of.Natural.Population.Increase...UN.2015

growth$gr <- growth$Rate.of.Natural.Population.Increase...UN.2015
growth$key <- paste(growth$Country.code, growth$Year)

tfr <- read.csv("./data/ourworldindata/total-fertility-rate.csv")

tfr$tfr <- tfr$Gapminder..Fertility.v6.
tfr$key <- paste(tfr$Country.code, tfr$Year)

# Entity
# Year
# Country.code
# Gapminder..Fertility.v6.

cat("create analysis dataframe\n")

d <- life
d$tfr <- tfr$tfr[match(d$key, tfr$key)]
d$gr <- growth$gr[match(d$key, growth$key)]

# i cna probabl estimate the relationships between these empirically...then recalculate!

#  growth = a + b log(tfr) + c log(le)

d$ln_tfr <- log(d$tfr)
d$ln_le <- log(d$le)

write.csv(d, "analysis_data.csv", row.names = FALSE)
