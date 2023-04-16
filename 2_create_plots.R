
source("project_support.R")

dir_init("./figures")

# load from polygon data
foragers <- read.csv("data/forager-polygon.csv")
pastoralists <- read.csv("data/pastoralist-polygon.csv")
hort <- read.csv("data/horticulturalist-polygon.csv")

# load country-year stats from 2017 ourworldindata
d <- read.csv("data/nation_year_demographics.csv")
d <- filter(d, Code != "") 

d$my_col <- data_gradient(d$gr, my.start=0, my.stop=3.5)

d$my_col[is.na(d$gr)] <- gray(0.8)

# together these caluclate gr exactly?

#  growth = a + b log(tfr) + c log(le)

d$ln_tfr <- log(d$tfr)
d$ln_e0 <- log(d$e0)

m1 <- lm(d$gr ~ d$ln_tfr + d$ln_e0) # pretty close! R^2 = 0.88

d$gr_pred <- coef(m1)[1] + coef(m1)[2]*d$ln_tfr + coef(m1)[3]*d$ln_e0

png("figures/growth_predicted_actual.png", res = 300, height = 6, width = 6, units = "in")
plot(d$gr_pred, d$gr, xlab = "predicted growth", ylab = "actual growth rate", main = "gr ~ ln(tfr) + ln(e0)")
abline(0, 1, lty = 2, col = "red")

text(-0.6, 3.7, paste0("R^2 = ", round(summary(m1)$r.squared, 2)))

dev.off() # uh oh....


growth_isocline <- function(x, r) exp( (coef(m1)[1] + coef(m1)["d$ln_e0"] * log(x) - r) / -coef(m1)["d$ln_tfr"] )

plot(NULL, frame.plot=FALSE, 
  ylim = c(1, 9), xlim = c(20, 85), 
  xlab = "Life expectancy at birth", 
  ylab = "Total fertility rate", las = 1, axes = FALSE,
  main = year)

polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )

for (r in 0:4) curve(growth_isocline(x, r), from = 0, to = 100, add = TRUE, lty = 2, col = gray(0.4))


# d <- read.csv("data/nation_year_demographics.csv")
# d <- filter(d, Code != "") 
# lots of categories of countries here

# r = a + b*ln_e0 + c*ln_tfr
# -c*ln_tfr + r = a + b*ln_e0 
# -c*ln_tfr = a + b*ln_e0 - r

# exp(ln_tfr) = exp(( a + b*ln_e0 - r)/(-c))

# exp(ln_tfr) = exp((-10.96 + 2.36*ln_e0 - r)/2.39)

# TFR \approx \exp\bigg(\frac{a + b \times \ln(e_0) - R}{c}\bigg)
# https://viereck.ch/latex-to-svg/




dir_init("./test")

# years: 1880, 1905, 1930, 1955, 1980, 2005

years <- sort(unique(d$Year))

list(
  list(
    Code = "SWE",
    label = "Sweden"
  ),
  list(
    Code = "GBR",
    label = "Great Britain"
  ),
  list(
    Code = "JPN",
    label = "Japan"
  ),
  list(
    Code = "FRA",
    label = "France"
  ),
  list(
    Code = "CHN",
    label = "China"
  ),
  list(
    Code = "BOL",
    label = "Bolivia"
  ),
  list(
    Code = "RUS",
    label = "Russia"
  )
) |> bind_rows() |> as.data.frame() -> named

# it would be useful to have rolling values for years there is no data...

named$tfr <- NA
named$e0 <- NA

for (i in 1:length(years)) {

  year <- years[i]

  my_filename <- paste0("test/test", year, ".png")
  png(my_filename, res = 300, units = "in", height = 6, width = 7)

  plot(NULL, frame.plot=FALSE, 
    ylim = c(1, 9), xlim = c(20, 85), 
    xlab = "Life expectancy at birth", 
    ylab = "Total fertility rate", las = 1, axes = FALSE,
    main = year)

  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )

  for (r in 0:4) curve(growth_isocline(x, r), from = 0, to = 100, add = TRUE, lty = 2, col = gray(0.4))

  text(26.19916, 4.5, label="hunter-\n gatherers", cex=0.9)

  text(37, 7.7, label="horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1, labels=FALSE)
  axis(2, at=seq(1,9, by=2), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

  text(39.70611, 2.407064, label="0% population growth", srt=(-14.5), col = gray(0.8))
  text(43.22775, 3.401599, label="1%", srt=(-14.5), col = gray(0.8))
  text(48.51021, 4.622958, label="2%", srt=(-16), col = gray(0.8))
  text(55.67089, 6.245621, label="3%", srt=(-18), col = gray(0.8))
  text(64.35761, 8.321932, label="4%", srt=(-20), col = gray(0.8))

  d_year <- filter(d, Year == year)
  
  points(d_year$e0, d_year$tfr, pch = 16, col = col.alpha("dodgerblue", 0.3))

  tar <- which(named$Code %in% d_year$Code)
  link <- match(named$Code[tar], d_year$Code)
  named$tfr[tar] <- d_year$tfr[link]
  named$e0[tar] <- d_year$e0[link]

  points(named$e0, named$tfr, pch = 20, col = col_alpha("black", 0.9))
  text(named$e0, named$tfr, label = named$label, col = "black", pos = 1)

  dev.off()

  cat(my_filename, "\n")

}


