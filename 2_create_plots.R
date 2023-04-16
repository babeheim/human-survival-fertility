
source("project_support.R")

dir_init("./figures")

# load from polygon data
foragers <- read.csv("data/forager-polygon.csv")
pastoralists <- read.csv("data/pastoralist-polygon.csv")
hort <- read.csv("data/horticulturalist-polygon.csv")

# load country-year stats from ourworldindata
d <- read.csv("nation_year_demographics.csv")

d$my_col <- data_gradient(d$gr, my.start=0, my.stop=3.5)

d$my_col[is.na(d$gr)] <- gray(0.8)

# together these caluclate gr exactly?

#  growth = a + b log(tfr) + c log(le)

d$ln_tfr <- log(d$tfr)
d$ln_e0 <- log(d$e0)

m1 <- lm(d$gr ~ d$ln_tfr + d$ln_e0) # pretty close! R^2 = 0.88

d$x <- coef(m1)[1] + coef(m1)[2]*d$ln_tfr + coef(m1)[3]*d$ln_e0

png("figures/growth_predicted_actual.png", res = 300, height = 6, width = 6, units = "in")
plot(d$x, d$gr, xlab = "predicted growth", ylab = "actual growth rate", main = "gr ~ ln(tfr) + ln(e0)")
abline(0, 1, lty = 2, col = "red")

text(-0.6, 3.7, paste0("R^2 = ", round(summary(m1)$r.squared, 2)))

dev.off()

# r = a + b*ln_e0 + c*ln_tfr
# -c*ln_tfr + r = a + b*ln_e0 
# -c*ln_tfr = a + b*ln_e0 - r

# exp(ln_tfr) = exp(( a + b*ln_e0 - r)/(-c))

# exp(ln_tfr) = exp((-10.96 + 2.36*ln_e0 - r)/2.39)

# TFR \approx \exp\bigg(\frac{a + b \times \ln(e_0) - R}{c}\bigg)
# https://viereck.ch/latex-to-svg/

growth_isocline <- function(x, r) exp( (coef(m1)[1] + coef(m1)["d$ln_e0"] * log(x) - r) / -coef(m1)["d$ln_tfr"] )

d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )


cat("create figures\n")


cat("first test\n")

pdf("figures/test.pdf", height=15/2.54, width=21/2.54)

tar <- 1:nrow(d)

plot(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), 
  col=col_alpha(d$my_col[tar], 0.3), 
  pch=16, frame.plot=FALSE, cex=1, ylim=c(1,9), xlim=c(20,80), 
  xlab="Life expectancy at birth", 
  ylab="Number of children per woman", las=1, axes=FALSE)
# curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)


# abline(v=gurv$e0, col="blue")

# e0 calculated from published l15 in each case using equation  -14.581 +  79.668*l15, not the best but okay

# bentley <- c(6.2, 4.2, 4.3, 4.36, 5.5, 5.8, 5.69, 6.1)

# abline(h=bentley, col="green")

# Europe
europe <- c("United Kingdom",
"France",
"Finland",
"Germany",
"Greece",
"Sweden",        
"Norway",
"Switzerland")

# commonwealth
com <- c("United States",
"Canada",
"Australia",
"India")

# east asia
asia <- c("Japan",
"China",
"Vietnam",
"South Korea")

# south america
s_america <- c("Bolivia", "Brazil")

tar <- which(d$Year == 2000)
points(d$e0[tar], d$tfr[tar], pch=20)

# text

# text(34.65842, 1.709144, label="-1%", srt=(-14.5))
text(39.70611, 2.407064, label="0% population growth", srt=(-14.5))
text(43.22775, 3.401599, label="1%", srt=(-14.5))
text(48.51021, 4.622958, label="2%", srt=(-16))
text(55.67089, 6.245621, label="3%", srt=(-18))
text(64.35761, 8.321932, label="4%", srt=(-20))


axis(2, at=seq(1,9, by=1), las=1)
axis(1, at=seq(20,90, by=10), las=1)


text(56.78387, 3.349255, "world nations,\nyear 2000", font=2)

dev.off()


cat("test5 all\n")


pdf("figures/test5all.pdf", height = 1 * 4.5, width = 2.0 * 4.5) 

par(mfrow=c(2,3))
par(mar=c(2.2,2.2,1.2,1.2))

# test5a

  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1880")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  show_all <- TRUE
  named_pch <- 1


  col_1880 <- cyan_to_rose[2]

  tar <- which(d$Year == 1880)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col.alpha(col_1880, 0.2))

  tar <- which(d$Year == 1880 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1880, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=col_1880, pos=1)

  tar <- which(d$Year == 1880 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1880, 0.5))
  text(d$e0[tar], d$tfr[tar], label="UK", col=col_1880, pos=4)

  tar <- which(d$Year == 1880 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1880, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=col_1880, pos=4)

  tar <- which(d$Year == 1880 & d$Country.code=="FRA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1880, 0.5))
  text(d$e0[tar], d$tfr[tar], label="France", col=col_1880, pos=4)


  # text

#  text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  text(39.70611, 2.407064, label="0%", srt=(-14.5))
  text(43.22775, 3.401599, label="1%", srt=(-14.5))
  text(48.51021, 4.622958, label="2%", srt=(-16))
  text(55.67089, 6.245621, label="3%", srt=(-18))
  text(64.35761, 8.321932, label="4% population growth", srt=(-30))

  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)



# test5b


  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1905")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  show_all <- TRUE
  named_pch <- 1

  col_1905 <- rose[3]

  tar <- which(d$Year == 1905)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col.alpha(col_1905, 0.2))

  tar <- which(d$Year == 1905 & d$Country.code=="USA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$e0[tar], d$tfr[tar], label="USA", col=col_1905, pos=2)

  tar <- which(d$Year == 1905 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="IND")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$e0[tar], d$tfr[tar], label="India", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=col_1905, pos=3)

  tar <- which(d$Year == 1905 & d$Country.code=="CUB")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Cuba", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$e0[tar], d$tfr[tar], label="UK", col=col_1905, pos=1)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  # text(39.70611, 2.407064, label="0%", srt=(-14.5))
  # text(43.22775, 3.401599, label="1%", srt=(-14.5))
  # text(48.51021, 4.622958, label="2%", srt=(-16))
  # text(55.67089, 6.245621, label="3%", srt=(-18))
  # text(64.35761, 8.321932, label="4% population growth", srt=(-30))

#  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

#  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)


# test5b2

  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1930")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  show_all <- TRUE
  named_pch <- 1

  col_1930 <- rose[3]

  tar <- which(d$Year == 1930)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col.alpha(col_1930, 0.2))

  tar <- which(d$Year == 1930 & d$Country.code=="USA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$e0[tar], d$tfr[tar], label="USA", col=col_1930, pos=2)

  tar <- which(d$Year == 1927 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=col_1930, pos=1)

  tar <- which(d$Year == 1931 & d$Country.code=="IND")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$e0[tar], d$tfr[tar], label="India", col=col_1930, pos=1)

  tar <- which(d$Year == 1930 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=col_1930, pos=3)

  tar <- which(d$Year == 1930 & d$Country.code=="CUB")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Cuba", col=col_1930, pos=1)

  tar <- which(d$Year == 1930 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$e0[tar], d$tfr[tar], label="UK", col=col_1930, pos=1)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  # text(39.70611, 2.407064, label="0%", srt=(-14.5))
  # text(43.22775, 3.401599, label="1%", srt=(-14.5))
  # text(48.51021, 4.622958, label="2%", srt=(-16))
  # text(55.67089, 6.245621, label="3%", srt=(-18))
  # text(64.35761, 8.321932, label="4% population growth", srt=(-30))

#  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

#  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)



# test5c

  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1955")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  show_all <- TRUE
  named_pch <- 1



  col_1955 <- aquamarine[5]

  tar <- which(d$Year == 1955)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col.alpha(col_1955, 0.2))

  tar <- which(d$Year == 1955 & d$Country.code=="USA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="USA", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="CHN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="China", col=col_1955, pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="BOL")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Bolivia", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="TZA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Tanzania", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="PER")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Peru", col=col_1955, pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="IND")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="India", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=col_1955, pos=1)

  # tar <- which(d$Year == 1955 & d$Country.code=="MAR")
  # points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  # text(d$e0[tar], d$tfr[tar], label="Morocco", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="NAM")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Namibia", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="FJI")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Fiji", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="CUB")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="Cuba", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$e0[tar], d$tfr[tar], label="UK", col=col_1955, pos=3)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  # text(39.70611, 2.407064, label="0%", srt=(-14.5))
  # text(43.22775, 3.401599, label="1%", srt=(-14.5))
  # text(48.51021, 4.622958, label="2%", srt=(-16))
  # text(55.67089, 6.245621, label="3%", srt=(-18))
  # text(64.35761, 8.321932, label="4% population growth", srt=(-30))

#  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

#  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)

dev.off()





cat("test 6\n")

d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=cyan_to_rose )

pdf("figures/test6.pdf", height = 1 * 3.5, width = 2.0 * 3.5) 

par(mfrow=c(2,3))
par(mar=c(2.2,2.2,1.2,1.2))

# test5a

  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1880")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )

  curve_col <- gray(0.4)

  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  show_all <- TRUE
  named_pch <- 16
  named_alpha <- 0.9

  tar <- which(d$Year == 1880)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.6))

  tar <- which(d$Year == 1880 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1880 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1880 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1880 & d$Country.code=="FRA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="France", col=d$my_col[tar], pos=4)


  # text

#  text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  # text(39.70611, 2.407064, label="0%", srt=(-14.5))
  # text(43.22775, 3.401599, label="1%", srt=(-14.5))
  # text(48.51021, 4.622958, label="2%", srt=(-16))
  # text(55.67089, 6.245621, label="3%", srt=(-18))
  # text(64.35761, 8.321932, label="4% population growth", srt=(-30))

  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1, labels=FALSE)
  axis(2, at=seq(1,9, by=2), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)



# test5b


  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1905")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  show_all <- TRUE

  tar <- which(d$Year == 1905)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 1905 & d$Country.code=="USA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1905 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="IND")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1905 & d$Country.code=="CUB")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=1)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  # text(39.70611, 2.407064, label="0%", srt=(-14.5))
  # text(43.22775, 3.401599, label="1%", srt=(-14.5))
  # text(48.51021, 4.622958, label="2%", srt=(-16))
  # text(55.67089, 6.245621, label="3%", srt=(-18))
  # text(64.35761, 8.321932, label="4% population growth", srt=(-30))

#  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

#  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1, labels=FALSE)
  axis(2, at=seq(1,9, by=2), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)


# test5b2

  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1930")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  show_all <- TRUE


  tar <- which(d$Year == 1930)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 1930 & d$Country.code=="USA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1927 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1931 & d$Country.code=="IND")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1930 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1930 & d$Country.code=="CUB")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1930 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=1)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  # text(39.70611, 2.407064, label="0%", srt=(-14.5))
  # text(43.22775, 3.401599, label="1%", srt=(-14.5))
  # text(48.51021, 4.622958, label="2%", srt=(-16))
  # text(55.67089, 6.245621, label="3%", srt=(-18))
  # text(64.35761, 8.321932, label="4% population growth", srt=(-30))

#  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

#  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1, labels=FALSE)
  axis(2, at=seq(1,9, by=2), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)



# test5c

  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1955")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  show_all <- TRUE


  tar <- which(d$Year == 1955)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 1955 & d$Country.code=="USA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1955 & d$Country.code=="CHN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="China", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="BOL")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Bolivia", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="TZA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Tanzania", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1955 & d$Country.code=="PER")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Peru", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="IND")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1955 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  # tar <- which(d$Year == 1955 & d$Country.code=="MAR")
  # points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  # text(d$e0[tar], d$tfr[tar], label="Morocco", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="NAM")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Namibia", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="FJI")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Fiji", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="CUB")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1955 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=1)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  # text(39.70611, 2.407064, label="0%", srt=(-14.5))
  # text(43.22775, 3.401599, label="1%", srt=(-14.5))
  # text(48.51021, 4.622958, label="2%", srt=(-16))
  # text(55.67089, 6.245621, label="3%", srt=(-18))
  # text(64.35761, 8.321932, label="4% population growth", srt=(-30))

#  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

#  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1, labels=FALSE)
  axis(2, at=seq(1,9, by=2), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)




# test5d


  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1980")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  show_all <- TRUE


  tar <- which(d$Year == 1980)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 1980 & d$Country.code=="USA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="CHN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="China", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1980 & d$Country.code=="BOL")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Bolivia", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1980 & d$Country.code=="TZA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Tanzania", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1980 & d$Country.code=="PER")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Peru", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="IND")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1980 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=1)

  # tar <- which(d$Year == 1980 & d$Country.code=="MAR")
  # points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  # text(d$e0[tar], d$tfr[tar], label="Morocco", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="NAM")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Namibia", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="FJI")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Fiji", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="CUB")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1980 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=3)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  # text(39.70611, 2.407064, label="0%", srt=(-14.5))
  # text(43.22775, 3.401599, label="1%", srt=(-14.5))
  # text(48.51021, 4.622958, label="2%", srt=(-16))
  # text(55.67089, 6.245621, label="3%", srt=(-18))
  # text(64.35761, 8.321932, label="4% population growth", srt=(-30))

#  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

#  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1, labels=FALSE)
  axis(2, at=seq(1,9, by=2), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)




# test5e


  tar <- 1:nrow(d)

  plot(d$e0, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="2005")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$e0[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  show_all <- TRUE



  tar <- which(d$Year == 2005)
  if(show_all) points(d$e0[tar], d$tfr[tar], pch=16, col=col_alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 2005 & d$Country.code=="USA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="JPN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="CHN")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="China", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="BOL")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Bolivia", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="TZA")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Tanzania", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="PER")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Peru", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="IND")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="SWE")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  # tar <- which(d$Year == 2005 & d$Country.code=="MAR")
  # points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  # text(d$e0[tar], d$tfr[tar], label="Morocco", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="NAM")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Namibia", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="FJI")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Fiji", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="CUB")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="GBR")
  points(d$e0[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$e0[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=3)



  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  text(39.70611, 2.407064, label="0%", srt=(-14.5))
  text(43.22775, 3.401599, label="1%", srt=(-14.5))
  text(48.51021, 4.622958, label="2%", srt=(-16))
  text(55.67089, 6.245621, label="3%", srt=(-18))
  text(64.35761, 8.321932, label="4% population growth", srt=(-30))

#  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

#  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1, labels=FALSE)
  axis(2, at=seq(1,9, by=2), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)


dev.off()


