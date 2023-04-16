
source("project_support.R")

library(tikzDevice)

dir_init("./figures")

# load from polygon data
foragers <- read.csv("data/forager-polygon.csv")
pastoralists <- read.csv("data/pastoralist-polygon.csv")
hort <- read.csv("data/horticulturalist-polygon.csv")

d <- read.csv("data/nation_year_demographics.csv")

# together these caluclate gr exactly?
# growth = a + b log(tfr) + c log(le)

d$ln_tfr <- log(d$tfr)
d$ln_e0 <- log(d$e0)

m1 <- lm(d$gr ~ d$ln_tfr + d$ln_e0) # pretty close! R^2 = 0.88

growth_isocline <- function(x, r) exp( (coef(m1)[1] + coef(m1)["d$ln_e0"] * log(x) - r) / -coef(m1)["d$ln_tfr"] )

d$gr_pred <- coef(m1)[1] + coef(m1)[2]*d$ln_tfr + coef(m1)[3]*d$ln_e0

png("figures/growth_predicted_actual.png", res = 300, height = 6, width = 6, units = "in")

plot(d$gr_pred, d$gr, xlab = "predicted growth", ylab = "actual growth rate", main = "gr ~ ln(tfr) + ln(e0)", col = col.alpha("dodgerblue", 0.8))

abline(0, 1, col = "red")

text(-0.6, 3.7, paste0("R^2 = ", round(summary(m1)$r.squared, 2)))

dev.off()



tikz("figures/production_vitals.tex", height = 5, width = 6)

plot(NULL, frame.plot=FALSE, 
  ylim = c(1, 9), xlim = c(20, 85), 
  xlab = "Life expectancy at birth", 
  ylab = "Total fertility rate", las = 1, axes = FALSE,
  main = "Vital rates by production system (Gurven \\& Kaplan 2007)")

polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4))

polygon(hort$x, hort$y, border=NA, col=col_alpha("lightblue", 0.4))

polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha("green", 0.1))

for (r in 0:4) curve(growth_isocline(x, r), from = 0, to = 100, add = TRUE, lty = 2, col = gray(0.4))

axis(2, at=seq(1,9, by=1), las=1, labels=FALSE)
axis(2, at=seq(1,9, by=2), las=1)
axis(1, at=seq(20,90, by=10), las=1)

text(39.70611, 2.407064, label="0\\% population growth", srt=(-14.5), col = gray(0.8))
text(43.22775, 3.401599, label="1\\%", srt=(-14.5), col = gray(0.8))
text(48.51021, 4.622958, label="2\\%", srt=(-16), col = gray(0.8))
text(55.67089, 6.245621, label="3\\%", srt=(-18), col = gray(0.8))
text(64.35761, 8.321932, label="4\\%", srt=(-20), col = gray(0.8))

gk <- read.csv("data/gurven-kaplan-2007/gurven-kaplan-2007-data.csv")

tar <- grep("foragers", gk$category)
points(gk$e0[tar], gk$tfr[tar], pch = 16)

tar <- grep("horticulturalists", gk$category)
points(gk$e0[tar], gk$tfr[tar], pch = 16, col = "lightblue")

tar <- grep("pastoralist", gk$category)
points(gk$e0[tar], gk$tfr[tar], pch = 16, col = "darkseagreen")

dev.off()

# r = a + b*ln_e0 + c*ln_tfr
# -c*ln_tfr + r = a + b*ln_e0 
# -c*ln_tfr = a + b*ln_e0 - r

# exp(ln_tfr) = exp(( a + b*ln_e0 - r)/(-c))

# exp(ln_tfr) = exp((-10.96 + 2.36*ln_e0 - r)/2.39)

# TFR \approx \exp\bigg(\frac{a + b \times \ln(e_0) - R}{c}\bigg)
# https://viereck.ch/latex-to-svg/




dir_init("./figures/ourworldindata")

years <- sort(unique(d$Year))
years <- years[years > 1850]

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

named$tfr <- NA
named$e0 <- NA

for (i in 1:length(years)) {

  year <- years[i]

  my_filename <- paste0("figures/ourworldindata/frame", i, ".tex")
  tikz(my_filename, height = 5, width = 6)

  plot(NULL, frame.plot=FALSE, 
    ylim = c(1, 9), xlim = c(20, 85), 
    xlab = "Life expectancy at birth", 
    ylab = "Total fertility rate", las = 1, axes = FALSE,
    main = year)

  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4))

  polygon(hort$x, hort$y, border=NA, col=col_alpha("lightblue", 0.4))

  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha("green", 0.1))

  for (r in 0:4) curve(growth_isocline(x, r), from = 0, to = 100, add = TRUE, lty = 2, col = gray(0.4))

  text(26.19916, 4.5, label="hunter-\n gatherers", cex=0.9)

  text(37, 7.7, label="horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1, labels=FALSE)
  axis(2, at=seq(1,9, by=2), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

  text(39.70611, 2.407064, label="0\\% population growth", srt=(-14.5), col = gray(0.8))
  text(43.22775, 3.401599, label="1\\%", srt=(-14.5), col = gray(0.8))
  text(48.51021, 4.622958, label="2\\%", srt=(-16), col = gray(0.8))
  text(55.67089, 6.245621, label="3\\%", srt=(-18), col = gray(0.8))
  text(64.35761, 8.321932, label="4\\%", srt=(-20), col = gray(0.8))

  d_year <- filter(d, Year == year)
  
  points(d_year$e0, d_year$tfr, pch = 16, col = col_alpha("orangered", 0.4))

  tar <- which(named$Code %in% d_year$Code)
  link <- match(named$Code[tar], d_year$Code)
  named$tfr[tar] <- d_year$tfr[link]
  named$e0[tar] <- d_year$e0[link]

  points(named$e0, named$tfr, pch = 20, col = col_alpha("black", 0.9))
  text(named$e0, named$tfr, label = named$label, col = "black", pos = 1)

  dev.off()

  cat(my_filename, "\n")

}
