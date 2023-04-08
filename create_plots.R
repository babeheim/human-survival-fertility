
rm(list = ls())

source("project_support.R")

dir_init("./figures")

d <- read.csv("analysis_data.csv")

foragers <- read.csv("./data/forager-polygon.csv")
pastoralists <- read.csv("./data/pastoralist-polygon.csv")
hort <- read.csv("./data/horticulturalist-polygon.csv")

d$my_col <- data_gradient(d$gr, my.start=0, my.stop=3.5)

d$my_col[is.na(d$gr)] <- gray(0.8)


# together these caluclate r exaactly?

m1 <- lm(d$gr ~ d$ln_tfr + d$ln_le) # pretty close!

d$x <- coef(m1)[1] + coef(m1)[2]*d$ln_tfr + coef(m1)[3]*d$ln_le

# r = a + b*ln_le + c*ln_tfr
# -c*ln_tfr + r = a + b*ln_le 
# -c*ln_tfr = a + b*ln_le - r

# exp(ln_tfr) = exp(( a + b*ln_le - r)/(-c))


# exp(ln_tfr) = exp((-10.96 + 2.36*ln_le - r)/2.39)

growth_isocline <- function(x, r) exp( (coef(m1)[1] + coef(m1)["d$ln_le"] * log(x) - r) / -coef(m1)["d$ln_tfr"] )

d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )


cat("create figures\n")

pdf('./figures/test.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), 
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
  points(d$le[tar], d$tfr[tar], pch=20)

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

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 


d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )

pdf('./figures/test2.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,80), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE)

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  tar <- 1:nrow(d)
  points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha("orange", 0.6) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha("blue", 0.6) )
  # polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha("red", 0.6) )


  tar <- which(d$Year == 2000)
  points(d$le[tar], d$tfr[tar], pch=20)

  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)



  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  text(39.70611, 2.407064, label="0% population growth", srt=(-14.5))
  text(43.22775, 3.401599, label="1%", srt=(-14.5))
  text(48.51021, 4.622958, label="2%", srt=(-16))
  text(55.67089, 6.245621, label="3%", srt=(-18))
  text(64.35761, 8.321932, label="4%", srt=(-20))


  text(26.19916, 2.804078, label="hunter-\n gatherers", col="blue")

  text(30.95021, 8.619962, label="horticulturalists", col="orange")

  axis(2, at=seq(1,9, by=1), las=1)
  axis(1, at=seq(20,90, by=10), las=1)


  text(56.78387, 3.349255, "world nations,\nyear 2000", font=2)

dev.off()

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 


d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )

pdf('./figures/test4.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="The Human Adventure")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  foragers <- read.csv("./data/forager-polygon.csv")
  pastoralists <- read.csv("./data/pastoralist-polygon.csv")
  hort <- read.csv("./data/horticulturalist-polygon.csv")

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha(gray(0.8), 0.4) )
#  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha(gray(0.8), 0.4) )


  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  show_all <- FALSE
  named_pch <- 16


  col_1855 <- cyan_to_rose[2]

  tar <- which(d$Year == 1855)
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1855, 0.2))

  tar <- which(d$Year == 1855 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1855, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1855, pos=1)

  tar <- which(d$Year == 1855 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1855, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1855, pos=4)


  col_1905 <- rose[3]

  tar <- which(d$Year == 1905)
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1905, 0.2))

  tar <- which(d$Year == 1905 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_1905, pos=2)

  tar <- which(d$Year == 1905 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1905, pos=3)

  tar <- which(d$Year == 1905 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1905, pos=1)




  col_1955 <- aquamarine[5]

  tar <- which(d$Year == 1955)
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1955, 0.2))

  tar <- which(d$Year == 1955 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="China", col=col_1955, pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Peru", col=col_1955, pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1955, pos=1)

  # tar <- which(d$Year == 1955 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1955, pos=3)


  col_2005 <- orangered[5]

  tar <- which(d$Year == 2005)
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col_alpha(col_2005, 0.2))

  tar <- which(d$Year == 2005 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_2005, pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="China", col=col_2005, pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=col_2005, pos=4)

  tar <- which(d$Year == 2005 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=col_2005, pos=4)

  tar <- which(d$Year == 2005 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Peru", col=col_2005, pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_2005, pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_2005, pos=4)

  # tar <- which(d$Year == 2005 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=col_2005, pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_2005, pos=3)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  text(39.70611, 2.407064, label="0%", srt=(-14.5))
  text(43.22775, 3.401599, label="1%", srt=(-14.5))
  text(48.51021, 4.622958, label="2%", srt=(-16))
  text(55.67089, 6.245621, label="3%", srt=(-18))
  text(64.35761, 8.321932, label="4% population growth", srt=(-30))


  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=1), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

    text(32, 3.5, label="1855", col=col_1855, cex=3)
  text(55, 2.2, label="1905", col=col_1905, cex=3)
  text(63, 7, label="1955", col=col_1955, cex=3)
  text(80.104, 3.63, label="2005", col=col_2005, cex=3)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)

dev.off()

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 

# now a series of pictures

d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )

pdf('./figures/test5a.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="The Human Adventure")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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


  col_1865 <- cyan_to_rose[2]

  tar <- which(d$Year == 1865)
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1865, 0.2))

  tar <- which(d$Year == 1865 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1865, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1865, pos=1)

  tar <- which(d$Year == 1865 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1865, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1865, pos=4)

  tar <- which(d$Year == 1865 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1865, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1865, pos=4)

  tar <- which(d$Year == 1865 & d$Country.code=="FRA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1865, 0.5))
  text(d$le[tar], d$tfr[tar], label="France", col=col_1865, pos=4)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
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

dev.off()

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 

d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )

pdf('./figures/test5b.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="The Human Adventure")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1905, 0.2))

  tar <- which(d$Year == 1905 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_1905, pos=2)

  tar <- which(d$Year == 1905 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1905, pos=3)

  tar <- which(d$Year == 1905 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1905, pos=1)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
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

dev.off()

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 

d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )

pdf('./figures/test5c.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="The Human Adventure")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  foragers <- read.csv("./data/forager-polygon.csv")
  pastoralists <- read.csv("./data/pastoralist-polygon.csv")
  hort <- read.csv("./data/horticulturalist-polygon.csv")

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1955, 0.2))

  tar <- which(d$Year == 1955 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="China", col=col_1955, pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Peru", col=col_1955, pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1955, pos=1)

  # tar <- which(d$Year == 1955 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1955, pos=3)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
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

dev.off()

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 


d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )

pdf('./figures/test5d.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="The Human Adventure")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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



  col_1985 <- aquamarine[5]

  tar <- which(d$Year == 1985)
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1985, 0.2))

  tar <- which(d$Year == 1985 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_1985, pos=1)

  tar <- which(d$Year == 1985 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1985, pos=1)

  tar <- which(d$Year == 1985 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="China", col=col_1985, pos=4)

  tar <- which(d$Year == 1985 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=col_1985, pos=3)

  tar <- which(d$Year == 1985 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=col_1985, pos=1)

  tar <- which(d$Year == 1985 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="Peru", col=col_1985, pos=4)

  tar <- which(d$Year == 1985 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_1985, pos=1)

  tar <- which(d$Year == 1985 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1985, pos=1)

  # tar <- which(d$Year == 1985 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=col_1985, pos=1)

  tar <- which(d$Year == 1985 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=col_1985, pos=1)

  tar <- which(d$Year == 1985 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=col_1985, pos=3)

  tar <- which(d$Year == 1985 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_1985, pos=3)

  tar <- which(d$Year == 1985 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1985, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1985, pos=3)


  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
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

dev.off()

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 

d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )

pdf('./figures/test5e.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="The Human Adventure")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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



  col_2005 <- orangered[5]

  tar <- which(d$Year == 2005)
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col_alpha(col_2005, 0.2))

  tar <- which(d$Year == 2005 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_2005, pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="China", col=col_2005, pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=col_2005, pos=4)

  tar <- which(d$Year == 2005 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=col_2005, pos=4)

  tar <- which(d$Year == 2005 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Peru", col=col_2005, pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_2005, pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_2005, pos=4)

  # tar <- which(d$Year == 2005 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=col_2005, pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_2005, pos=3)



  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
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

dev.off()

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 



pdf('./figures/test5all.pdf', height = 1 * 4.5, width = 2.0 * 4.5) 

par(mfrow=c(2,3))
par(mar=c(2.2,2.2,1.2,1.2))

# test5a

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1880")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1880, 0.2))

  tar <- which(d$Year == 1880 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1880, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1880, pos=1)

  tar <- which(d$Year == 1880 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1880, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1880, pos=4)

  tar <- which(d$Year == 1880 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1880, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1880, pos=4)

  tar <- which(d$Year == 1880 & d$Country.code=="FRA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1880, 0.5))
  text(d$le[tar], d$tfr[tar], label="France", col=col_1880, pos=4)


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

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1905")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1905, 0.2))

  tar <- which(d$Year == 1905 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_1905, pos=2)

  tar <- which(d$Year == 1905 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1905, pos=3)

  tar <- which(d$Year == 1905 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_1905, pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1905, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1905, pos=1)


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

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1930")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1930, 0.2))

  tar <- which(d$Year == 1930 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_1930, pos=2)

  tar <- which(d$Year == 1927 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1930, pos=1)

  tar <- which(d$Year == 1931 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_1930, pos=1)

  tar <- which(d$Year == 1930 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1930, pos=3)

  tar <- which(d$Year == 1930 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_1930, pos=1)

  tar <- which(d$Year == 1930 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1930, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1930, pos=1)


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

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1955")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1955, 0.2))

  tar <- which(d$Year == 1955 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="China", col=col_1955, pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Peru", col=col_1955, pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1955, pos=1)

  # tar <- which(d$Year == 1955 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=col_1955, pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_1955, pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1955, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1955, pos=3)


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




# test5d


  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1980")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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



  col_1980 <- aquamarine[5]

  tar <- which(d$Year == 1980)
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(col_1980, 0.2))

  tar <- which(d$Year == 1980 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_1980, pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_1980, pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="China", col=col_1980, pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=col_1980, pos=3)

  tar <- which(d$Year == 1980 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=col_1980, pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="Peru", col=col_1980, pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_1980, pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_1980, pos=1)

  # tar <- which(d$Year == 1980 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=col_1980, pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=col_1980, pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=col_1980, pos=3)

  tar <- which(d$Year == 1980 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_1980, pos=3)

  tar <- which(d$Year == 1980 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_1980, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_1980, pos=3)


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




# test5e


  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="2005")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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



  col_2005 <- orangered[5]

  tar <- which(d$Year == 2005)
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col_alpha(col_2005, 0.2))

  tar <- which(d$Year == 2005 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="USA", col=col_2005, pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Japan", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="China", col=col_2005, pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=col_2005, pos=4)

  tar <- which(d$Year == 2005 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=col_2005, pos=4)

  tar <- which(d$Year == 2005 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Peru", col=col_2005, pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="India", col=col_2005, pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=col_2005, pos=4)

  # tar <- which(d$Year == 2005 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=col_2005, pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=col_2005, pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(col_2005, 0.5))
  text(d$le[tar], d$tfr[tar], label="UK", col=col_2005, pos=3)



  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  text(39.70611, 2.407064, label="0%", srt=(-14.5))
  text(43.22775, 3.401599, label="1%", srt=(-14.5))
  text(48.51021, 4.622958, label="2%", srt=(-16))
  text(55.67089, 6.245621, label="3%", srt=(-18))
  text(64.35761, 8.321932, label="4% population growth", srt=(-30))

#  text(26.19916, 4.5, label="all hunter-\n gatherers", cex=0.9)

#  text(37, 7.7, label="all horticulturalists")

  axis(2, at=seq(1,9, by=2), las=1)
  axis(1, at=seq(20,90, by=10), las=1)

#  text(56.78387, 3.349255, "world nations,\nyear 2005", font=2)


dev.off()

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 



d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=cyan_to_rose )

pdf('./figures/test6.pdf', height = 1 * 3.5, width = 2.0 * 3.5) 

par(mfrow=c(2,3))
par(mar=c(2.2,2.2,1.2,1.2))

# test5a

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1880")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.6))

  tar <- which(d$Year == 1880 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1880 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1880 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1880 & d$Country.code=="FRA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="France", col=d$my_col[tar], pos=4)


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

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1905")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 1905 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1905 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1905 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1905 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=1)


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

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1930")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 1930 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1927 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1931 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1930 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1930 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1930 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=1)


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

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1955")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 1955 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1955 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="China", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1955 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1955 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Peru", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1955 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  # tar <- which(d$Year == 1955 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1955 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1955 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1955 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=1)


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

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="1980")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col.alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 1980 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="China", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1980 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1980 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 1980 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Peru", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1980 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=1)

  # tar <- which(d$Year == 1980 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 1980 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=d$my_col[tar], pos=4)

  tar <- which(d$Year == 1980 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 1980 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=3)


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

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,85), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE,
    main="2005")

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2, col=curve_col)

  # tar <- 1:nrow(d)
  # points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


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
  if(show_all) points(d$le[tar], d$tfr[tar], pch=16, col=col_alpha(d$my_col[tar], 0.2))

  tar <- which(d$Year == 2005 & d$Country.code=="USA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="USA", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="JPN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Japan", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="CHN")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="China", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="BOL")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Bolivia", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="TZA")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Tanzania", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="PER")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Peru", col=d$my_col[tar], pos=3)

  tar <- which(d$Year == 2005 & d$Country.code=="IND")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="India", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="SWE")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Sweden", col=d$my_col[tar], pos=4)

  # tar <- which(d$Year == 2005 & d$Country.code=="MAR")
  # points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  # text(d$le[tar], d$tfr[tar], label="Morocco", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="NAM")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Namibia", col=d$my_col[tar], pos=2)

  tar <- which(d$Year == 2005 & d$Country.code=="FJI")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Fiji", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="CUB")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="Cuba", col=d$my_col[tar], pos=1)

  tar <- which(d$Year == 2005 & d$Country.code=="GBR")
  points(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], named_alpha))
  text(d$le[tar], d$tfr[tar], label="UK", col=d$my_col[tar], pos=3)



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

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 





d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )

pdf('./figures/test2.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,80), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE)

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  tar <- 1:nrow(d)
  points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha("orange", 0.6) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha("blue", 0.6) )
  # polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha("red", 0.6) )


  tar <- which(d$Year == 2000)
  points(d$le[tar], d$tfr[tar], pch=20)

  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)



  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  text(39.70611, 2.407064, label="0% population growth", srt=(-14.5))
  text(43.22775, 3.401599, label="1%", srt=(-14.5))
  text(48.51021, 4.622958, label="2%", srt=(-16))
  text(55.67089, 6.245621, label="3%", srt=(-18))
  text(64.35761, 8.321932, label="4%", srt=(-20))


  text(26.19916, 2.804078, label="hunter-\n gatherers", col="blue")

  text(30.95021, 8.619962, label="horticulturalists", col="orange")

  axis(2, at=seq(1,9, by=1), las=1)
  axis(1, at=seq(20,90, by=10), las=1)


  text(56.78387, 3.349255, "world nations,\nyear 2000", font=2)





dev.off()



d$my_col <- data_gradient(d$x, my.start=0, my.stop=3, colors=orangered )

tfr <- seq(1, 9, by=0.1)
le <- seq(20, 85, by=0.1)
background <- expand.grid(tfr, le)
colnames(background) <- c("tfr", "le")

background$ln_tfr <- log(background$tfr)
background$ln_le <- log(background$le)

background$x <- coef(m1)[1] + coef(m1)[2]*background$ln_tfr + coef(m1)[3]*background$ln_le

background$my_col <- data_gradient(background$x, my.start=0, my.stop=3, colors=orangered )


pdf('./figures/test3.pdf', height=15/2.54, width=21/2.54)

  tar <- 1:nrow(d)

  plot(d$le, d$tfr, col=NA, frame.plot=FALSE, 
    ylim=c(1,9), xlim=c(20,80), 
    xlab="Life expectancy at birth", 
    ylab="Number of children per woman", las=1, axes=FALSE)

  points(background$le, background$tfr, col=background$my_col, pch=15)

  # curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

  tar <- 1:nrow(d)
  points(jitter(d$le[tar], amount=0.1), jitter(d$tfr[tar], amount=0.1), pch=16, col=col_alpha("gray", 0.3))


  polygon(hort$x, hort$y, border=NA, col=col_alpha("orange", 0.2) )
  polygon(foragers$x, foragers$y, border=NA, col=col_alpha("blue", 0.2) )
  polygon(pastoralists$x, pastoralists$y, border=NA, col=col_alpha("red", 0.2) )


  tar <- which(d$Year == 2000)
  points(d$le[tar], d$tfr[tar], pch=20)

  curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
  curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)




  # text

  # text(34.65842, 1.709144, label="-1%", srt=(-14.5))
  text(39.70611, 2.407064, label="0% population growth", srt=(-14.5))
  text(43.22775, 3.401599, label="1%", srt=(-14.5))
  text(48.51021, 4.622958, label="2%", srt=(-16))
  text(55.67089, 6.245621, label="3%", srt=(-18))
  text(64.35761, 8.321932, label="4%", srt=(-20))


  text(26.19916, 4.5, label="hunter-\n gatherers", col="blue")

  text(30.95021, 8.619962, label="horticulturalists", col="orange")

  axis(2, at=seq(1,9, by=1), las=1)
  axis(1, at=seq(20,90, by=10), las=1)


  text(56.78387, 3.349255, "world nations,\nyear 2000", font=2)

dev.off()

# reduce the number of nations, esp small ones that are probably throwing off
# maybe 

par(mfrow=c(2,2))


tar <- which(d$Year == 1850)

plot(d$le[tar], d$tfr[tar], pch=20, col=col_alpha(d$my_col[tar], 0.9), frame.plot=FALSE, ylim=c(1,9), xlim=c(20,80), main=1850, 
  xlab="Life expectancy at birth", 
  ylab="Number of children per woman (avg.)")
curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

points(29.23, 5.7, pch=20) # foragers average
points(38.80, 6.2, pch=20) # horticulturalists average
points(42.78, 5.6, pch=20) # pastoralists average



tar <- which(d$Year == 1900)

plot(d$le[tar], d$tfr[tar], col=col_alpha(d$my_col[tar], 0.9), pch=20, frame.plot=FALSE, ylim=c(1,9), xlim=c(20,80), main=1900, 
  xlab="Life expectancy at birth", 
  ylab="Number of children per woman (avg.)")
curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)


points(29.23, 5.7, pch=20) # foragers average
points(38.80, 6.2, pch=20) # horticulturalists average
points(42.78, 5.6, pch=20) # pastoralists average



tar <- which(d$Year == 1955)

plot(d$le[tar], d$tfr[tar], col=col_alpha(d$my_col[tar], 0.9), pch=20, frame.plot=FALSE, ylim=c(1,9), xlim=c(20,80), main=1955, 
  xlab="Life expectancy at birth", 
  ylab="Number of children per woman (avg.)")
curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

points(29.23, 5.7, pch=20) # foragers average
points(38.80, 6.2, pch=20) # horticulturalists average
points(42.78, 5.6, pch=20) # pastoralists average




tar <- which(d$Year == 2000)

plot(d$le[tar], d$tfr[tar], col=col_alpha(d$my_col[tar], 0.9), pch=20, frame.plot=FALSE, ylim=c(1,9), xlim=c(20,80), main=2000, 
  xlab="Life expectancy at birth", 
  ylab="Number of children per woman (avg.)")
curve(growth_isocline(x, -1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 0), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 1), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 2), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 3), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)
curve(growth_isocline(x, 4), from=0, to=100, ylim=c(0, 8), add=TRUE, lty=2)

points(29.23, 5.7, pch=20) # foragers average
points(38.80, 6.2, pch=20) # horticulturalists average
points(42.78, 5.6, pch=20) # pastoralists average

# even if i dont have each data point, i can create a 2-D cloud based on the invidiaul observations of TFR and of life expectancy in each figure...i can use this!

  # my_rows <- which(d$Country.code=="BRA")
  # max_growth <- max(d$x[my_rows], na.rm=TRUE)
  # my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )
  # points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))

  # my_rows <- which(d$Country.code=="CHN")
  # max_growth <- max(d$x[my_rows], na.rm=TRUE)
  # my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )
  # points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))


  # my_rows <- which(d$Country.code=="JPN")
  # max_growth <- max(d$x[my_rows], na.rm=TRUE)
  # my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )
  # points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))


  # my_rows <- which(d$Country.code=="DEU")
  # max_growth <- max(d$x[my_rows], na.rm=TRUE)
  # my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )
  # points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))


  # my_rows <- which(d$Country.code=="FRA")
  # max_growth <- max(d$x[my_rows], na.rm=TRUE)
  # my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )
  # points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))

  # my_rows <- which(d$Country.code=="AUS")
  # max_growth <- max(d$x[my_rows], na.rm=TRUE)
  # my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )
  # points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))


  # my_rows <- which(d$Country.code=="CAN")
  # max_growth <- max(d$x[my_rows], na.rm=TRUE)
  # my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )
  # points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))


  # my_rows <- which(d$Country.code=="USA")
  # max_growth <- max(d$x[my_rows], na.rm=TRUE)
  # my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )
  # points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))



  # my_rows <- which(d$Country.code=="GBA")
  # max_growth <- max(d$x[my_rows], na.rm=TRUE)
  # my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )
  # points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))


# # now plot lines by country - horrible!

# codes <- unique(d$Country.code)
# codes <- codes[-which(codes=="")]

# plot(1,1, col=NULL, pch=20, frame.plot=FALSE, ylim=c(1,9), xlim=c(20,80), 
#   xlab="Life expectancy at birth", 
#   ylab="Number of children per woman (avg.)")


# for(i in 1:length(codes)){
#   my_rows <- which(d$Country.code==codes[i])
#   max_growth <- max(d$x[my_rows], na.rm=TRUE)

#   my_col <- data_gradient(max_growth, my.start=0, my.stop=3, colors=rev(rev(orangered)) )

#   points(d$le[my_rows], d$tfr[my_rows], type="l", col=col.alpha(my_col, 0.9))
# }

# plot(d$le[tar], d$tfr[tar], pch=named_pch, col=col_alpha(d$my_col[tar], 0.5), pch=20, frame.plot=FALSE, cex=1, ylim=c(1,9), xlim=c(20,80), 
#   xlab="Life expectancy at birth", 
#   ylab="Number of children per woman (avg.)")


# }
