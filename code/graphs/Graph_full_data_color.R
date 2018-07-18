# Copyright Josef Koch (2018)
# Copyright Eva Koch (2018)

# Set working directory to source file location
library(rstudioapi)
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path # get the path of your current open file
  setwd(dirname(current_path )) # set the working directory to the source file location
  print( getwd() ) # display the directory
}
set_wd()

# Clear workspace
rm(list=ls())



x        <- c(1,2,3)  # Moment 1 und 2 und 3
epsilon  <- 0.05      # Error bar top and bottom line length (x-dimension)


# Critical items
#---------------
# Yes input - incidental
meanYesImpCrit  <- c(37.04, 54.80, 61.44) 
minCIYesImpCrit <- c(26.29, 41.90, 51.04) 
maxCIYesImpCrit <- c(49.55, 66.48, 71.38) 
colYesImpCrit   <- "darkorange2" # Linienfarbe
ltyYesImpCrit   <- 1             # Line type Yes (1 = normal)
diffYesImpCrit  <- -0.16        # Versatz

# No input - incidental
meanNoImpCrit   <- c(41.25, 41.66, 59.28) 
minCINoImpCrit  <- c(30.38, 29.97, 50.26) 
maxCINoImpCrit  <- c(52.84, 54.09, 67.86) 
colNoImpCrit    <- "darkorange2"
ltyNoImpCrit    <- 3          # Line type No (3 = gepunktet)
diffNoImpCrit   <- -0.12      # Versatz

# Yes input - explicit
meanYesExpCrit  <- c(63.52, 79.40, 76.15) 
minCIYesExpCrit <- c(51.41, 65.63, 67.38) 
maxCIYesExpCrit <- c(73.42, 87.63, 83.98) 
colYesExpCrit   <- "dodgerblue3"
ltyYesExpCrit   <- 1         # Line type Yes (1 = normal)
diffYesExpCrit  <- 0.02     # Versatz

# No input - explicit
meanNoExpCrit   <- c(62.47, 61.52, 66.03) 
minCINoExpCrit  <- c(52.35, 51.24, 56.40) 
maxCINoExpCrit  <- c(70.90, 71.03, 74.09) 
colNoExpCrit    <- "dodgerblue3"
ltyNoExpCrit    <- 3         # Line type No (3 = gepunktet)
diffNoExpCrit   <- 0.06     # Versatz


# Control items
#--------------
# Yes input - incidental
meanYesImpContr  <- c(93.15, 92.65, 86.27) 
minCIYesImpContr <- c(89.58, 89.31, 82.17) 
maxCIYesImpContr <- c(95.83, 95.24, 89.84) 
colYesImpContr    <- "darkorange2"
ltyYesImpContr   <- 1          # Line type Yes (1 = normal)
diffYesImpContr  <- -0.06     # Versatz

# No input - incidental
meanNoImpContr   <- c(91.47, 91.65, 87.91) 
minCINoImpContr  <- c(87.96, 88.93, 85.28) 
maxCINoImpContr  <- c(94.35, 93.91, 90.48) 
colNoImpContr    <- "darkorange2"
ltyNoImpContr    <- 3         # Line type No (3 = gepunktet)
diffNoImpContr  <- -0.02     # Versatz

# Yes input - explicit
meanYesExpContr  <- c(83.33, 89.34, 86.99) 
minCIYesExpContr <- c(77.60, 82.10, 81.10) 
maxCIYesExpContr <- c(89.00, 96.50, 92.90) 
colYesExpContr   <- "dodgerblue3"
ltyYesExpContr   <- 1         # Line type Yes (1 = normal)
diffYesExpContr  <- 0.12     # Versatz

# No input - explicit
meanNoExpContr   <- c(86.13, 86.18, 86.25) 
minCINoExpContr  <- c(80.70, 79.70, 80.75) 
maxCINoExpContr  <- c(91.60, 92.60, 91.75) 
colNoExpContr    <- "dodgerblue3"
ltyNoExpContr    <- 3         # Line type No (3 = gepunktet)
diffNoExpContr   <- 0.16     # Versatz


# Plot Dimensionen bestimmen (inches)
plotwidth = 8
plotheight = 6.5

# Neues Plot Fenster machen mit den korrekten Dimensionen
quartz("plotwindowW, plotwidh, plotheight")

# Plot leer
plot(x =  0,
     y =  0,
     main = "Test scores",
     xlim = c(0.5,3.5),
     ylim = c(0,100),
     xaxt = "n", # x-Achse soll nicht mit 1 und 2 nummeriert werden
     ylab = "Accuracy percentage",
     xlab = "Test moment",
     col  = colYesImpCrit) #?

## Dazu Punkte fuer die Konditionen
# Critical items
points(x + diffYesImpCrit, meanYesImpCrit, col = colYesImpCrit, pch = 24, bg = "darkorange2") # pch 24: Dreieck; bg "gray49": fuelle das Symbol mit Farbe.
points(x + diffNoImpCrit, meanNoImpCrit, col = colNoImpCrit, pch = 24, bg = "darkorange2")
points(x + diffYesExpCrit, meanYesExpCrit, col = colYesExpCrit, pch = 19, bg = "dodgerblue3") # pch 19: Kreis
points(x + diffNoExpCrit, meanNoExpCrit, col = colNoExpCrit, pch = 19, bg = "dodgerblue3")

# Control items
points(x + diffYesImpContr, meanYesImpContr, col = colYesImpContr, pch = 22, bg = "darkorange2") # pch 25: Dreieck (Spitze nach unten)
points(x + diffNoImpContr, meanNoImpContr, col = colNoImpContr, pch = 22, bg = "darkorange2")
points(x + diffYesExpContr, meanYesExpContr, col = colYesExpContr, pch = 23, bg = "dodgerblue3") # pch 22: Quadrat
points(x + diffNoExpContr, meanNoExpContr, col = colNoExpContr, pch = 23, bg = "dodgerblue3")

# x-Achse selber beschriften
par(cex = 0.9) # Fuer den naechsten Schritt die Schriftgroesse auf 90% setzen
axis(1,at=c(1,2,3),labels=c("T1 (learning task)","T2 (learning task)","T3 (explicit posttest)"))
par(cex = 1) # Fuer weitere Schritte Schriftgroesse wieder auf 100% setzen
par(lwd = 2) # Liniensdicke anpassen

## Verbindungslinien zwischen Punkten
# Criticals
# Verbindungslinien T1-T2 - incidental
segments(1 + diffYesImpCrit, meanYesImpCrit[1], 2 + diffYesImpCrit, meanYesImpCrit[2], col = colYesImpCrit, lty = ltyYesImpCrit) # Verbindungslinie Yes
segments(1 + diffNoImpCrit, meanNoImpCrit[1], 2 + diffNoImpCrit, meanNoImpCrit[2], col = colNoImpCrit, lty = ltyNoImpCrit)       # Verbindungslinie No

# Verbindungslinien T2-T3 - incidental
segments(2 + diffYesImpCrit, meanYesImpCrit[2], 3 + diffYesImpCrit, meanYesImpCrit[3], col = colYesImpCrit, lty = ltyYesImpCrit) # Verbindungslinie Yes
segments(2 + diffNoImpCrit, meanNoImpCrit[2], 3 + diffNoImpCrit, meanNoImpCrit[3], col = colNoImpCrit, lty = ltyNoImpCrit)       # Verbindungslinie No

# Verbindungslinien T1-T2 - explicit
segments(1 + diffYesExpCrit, meanYesExpCrit[1], 2 + diffYesExpCrit, meanYesExpCrit[2], col = colYesExpCrit, lty = ltyYesExpCrit) # Verbindungslinie Yes
segments(1 + diffNoExpCrit, meanNoExpCrit[1], 2 + diffNoExpCrit, meanNoExpCrit[2], col = colNoExpCrit, lty = ltyNoExpCrit)       # Verbindungslinie No

# Verbindungslinien T2-T3 - explicit
segments(2 + diffYesExpCrit, meanYesExpCrit[2], 3 + diffYesExpCrit, meanYesExpCrit[3], col = colYesExpCrit, lty = ltyYesExpCrit) # Verbindungslinie Yes
segments(2 + diffNoExpCrit, meanNoExpCrit[2], 3 + diffNoExpCrit, meanNoExpCrit[3], col = colNoExpCrit, lty = ltyNoExpCrit)       # Verbindungslinie No


# Controls
# Verbindungslinien T1-T2 - incidental
segments(1 + diffYesImpContr, meanYesImpContr[1], 2 + diffYesImpContr, meanYesImpContr[2], col = colYesImpContr, lty = ltyYesImpContr) # Verbindungslinie Yes
segments(1 + diffNoImpContr, meanNoImpContr[1], 2 + diffNoImpContr, meanNoImpContr[2], col = colNoImpContr, lty = ltyNoImpContr)       # Verbindungslinie No

# Verbindungslinien T2-T3 - incidental
segments(2 + diffYesImpContr, meanYesImpContr[2], 3 + diffYesImpContr, meanYesImpContr[3], col = colYesImpContr, lty = ltyYesImpContr) # Verbindungslinie Yes
segments(2 + diffNoImpContr, meanNoImpContr[2], 3 + diffNoImpContr, meanNoImpContr[3], col = colNoImpContr, lty = ltyNoImpContr)       # Verbindungslinie No

# Verbindungslinien T1-T2 - explicit
segments(1 + diffYesExpContr, meanYesExpContr[1], 2 + diffYesExpContr, meanYesExpContr[2], col = colYesExpContr, lty = ltyYesExpContr) # Verbindungslinie Yes
segments(1 + diffNoExpContr, meanNoExpContr[1], 2 + diffNoExpContr, meanNoExpContr[2], col = colNoExpContr, lty = ltyNoExpContr)           # Verbindungslinie No

# Verbindungslinien T2-T3 - explicit
segments(2 + diffYesExpContr, meanYesExpContr[2], 3 + diffYesExpContr, meanYesExpContr[3], col = colYesExpContr, lty = ltyYesExpContr) # Verbindungslinie Yes
segments(2 + diffNoExpContr, meanNoExpContr[2], 3 + diffNoExpContr, meanNoExpContr[3], col = colNoExpContr, lty = ltyNoExpContr)       # Verbindungslinie No


### Error bars: CI ###
## Criticals
# Yes T1 CI - imp
segments(1+diffYesImpCrit, minCIYesImpCrit[1], 1+diffYesImpCrit, maxCIYesImpCrit[1], col=colYesImpCrit, lty = ltyYesImpCrit)                   # Error bar
segments(1+diffYesImpCrit-epsilon, minCIYesImpCrit[1], 1+diffYesImpCrit+epsilon, minCIYesImpCrit[1], col = colYesImpCrit, lty = ltyYesImpCrit) # Error bar bottom
segments(1+diffYesImpCrit-epsilon, maxCIYesImpCrit[1], 1+diffYesImpCrit+epsilon, maxCIYesImpCrit[1], col = colYesImpCrit, lty = ltyYesImpCrit) # Error bar top

# Yes T2 CI - ImpCrit
segments(2+diffYesImpCrit, minCIYesImpCrit[2], 2+diffYesImpCrit, maxCIYesImpCrit[2], col=colYesImpCrit, lty = ltyYesImpCrit)                   # Error bar
segments(2+diffYesImpCrit-epsilon, minCIYesImpCrit[2], 2+diffYesImpCrit+epsilon, minCIYesImpCrit[2], col = colYesImpCrit, lty = ltyYesImpCrit) # Error bar bottom
segments(2+diffYesImpCrit-epsilon, maxCIYesImpCrit[2], 2+diffYesImpCrit+epsilon, maxCIYesImpCrit[2], col = colYesImpCrit, lty = ltyYesImpCrit) # Error bar top

# Yes T3 CI - ImpCrit
segments(3+diffYesImpCrit, minCIYesImpCrit[3], 3+diffYesImpCrit, maxCIYesImpCrit[3], col=colYesImpCrit, lty = ltyYesImpCrit)                   # Error bar
segments(3+diffYesImpCrit-epsilon, minCIYesImpCrit[3], 3+diffYesImpCrit+epsilon, minCIYesImpCrit[3], col = colYesImpCrit, lty = ltyYesImpCrit) # Error bar bottom
segments(3+diffYesImpCrit-epsilon, maxCIYesImpCrit[3], 3+diffYesImpCrit+epsilon, maxCIYesImpCrit[3], col = colYesImpCrit, lty = ltyYesImpCrit) # Error bar top


# No T1 CI - ImpCrit
segments(1+diffNoImpCrit, minCINoImpCrit[1], 1+diffNoImpCrit, maxCINoImpCrit[1], col=colNoImpCrit, lty = ltyNoImpCrit)                   # Error bar
segments(1+diffNoImpCrit-epsilon, minCINoImpCrit[1], 1+diffNoImpCrit+epsilon, minCINoImpCrit[1], col = colNoImpCrit, lty = ltyNoImpCrit) # Error bar bottom
segments(1+diffNoImpCrit-epsilon, maxCINoImpCrit[1], 1+diffNoImpCrit+epsilon, maxCINoImpCrit[1], col = colNoImpCrit, lty = ltyNoImpCrit) # Error bar top

# No T2 CI - ImpCrit
segments(2+diffNoImpCrit, minCINoImpCrit[2], 2+diffNoImpCrit,maxCINoImpCrit[2], col=colNoImpCrit, lty = ltyNoImpCrit)                    # Error bar
segments(2+diffNoImpCrit-epsilon, minCINoImpCrit[2], 2+diffNoImpCrit+epsilon, minCINoImpCrit[2], col = colNoImpCrit, lty = ltyNoImpCrit) # Error bar bottom
segments(2+diffNoImpCrit-epsilon, maxCINoImpCrit[2], 2+diffNoImpCrit+epsilon, maxCINoImpCrit[2], col = colNoImpCrit, lty = ltyNoImpCrit) # Error bar top

# No T3 CI - ImpCrit
segments(3+diffNoImpCrit, minCINoImpCrit[3], 3+diffNoImpCrit,maxCINoImpCrit[3], col=colNoImpCrit, lty = ltyNoImpCrit)                    # Error bar
segments(3+diffNoImpCrit-epsilon, minCINoImpCrit[3], 3+diffNoImpCrit+epsilon, minCINoImpCrit[3], col = colNoImpCrit, lty = ltyNoImpCrit) # Error bar bottom
segments(3+diffNoImpCrit-epsilon, maxCINoImpCrit[3], 3+diffNoImpCrit+epsilon, maxCINoImpCrit[3], col = colNoImpCrit, lty = ltyNoImpCrit) # Error bar top


# Yes T1 CI - ExpCrit
segments(1+diffYesExpCrit, minCIYesExpCrit[1], 1+diffYesExpCrit, maxCIYesExpCrit[1], col=colYesExpCrit, lty = ltyYesExpCrit)                   # Error bar
segments(1+diffYesExpCrit-epsilon, minCIYesExpCrit[1], 1+diffYesExpCrit+epsilon, minCIYesExpCrit[1], col = colYesExpCrit, lty = ltyYesExpCrit) # Error bar bottom
segments(1+diffYesExpCrit-epsilon, maxCIYesExpCrit[1], 1+diffYesExpCrit+epsilon, maxCIYesExpCrit[1], col = colYesExpCrit, lty = ltyYesExpCrit) # Error bar top

# Yes T2 CI - ExpCrit
segments(2+diffYesExpCrit, minCIYesExpCrit[2], 2+diffYesExpCrit, maxCIYesExpCrit[2], col=colYesExpCrit, lty = ltyYesExpCrit)                   # Error bar
segments(2+diffYesExpCrit-epsilon, minCIYesExpCrit[2], 2+diffYesExpCrit+epsilon, minCIYesExpCrit[2], col = colYesExpCrit, lty = ltyYesExpCrit) # Error bar bottom
segments(2+diffYesExpCrit-epsilon, maxCIYesExpCrit[2], 2+diffYesExpCrit+epsilon, maxCIYesExpCrit[2], col = colYesExpCrit, lty = ltyYesExpCrit) # Error bar top

# Yes T3 CI - ExpCrit
segments(3+diffYesExpCrit, minCIYesExpCrit[3], 3+diffYesExpCrit, maxCIYesExpCrit[3], col=colYesExpCrit, lty = ltyYesExpCrit)                   # Error bar
segments(3+diffYesExpCrit-epsilon, minCIYesExpCrit[3], 3+diffYesExpCrit+epsilon, minCIYesExpCrit[3], col = colYesExpCrit, lty = ltyYesExpCrit) # Error bar bottom
segments(3+diffYesExpCrit-epsilon, maxCIYesExpCrit[3], 3+diffYesExpCrit+epsilon, maxCIYesExpCrit[3], col = colYesExpCrit, lty = ltyYesExpCrit) # Error bar top


# No T1 CI - ExpCrit
segments(1+diffNoExpCrit, minCINoExpCrit[1], 1+diffNoExpCrit, maxCINoExpCrit[1], col=colNoExpCrit, lty = ltyNoExpCrit)                   # Error bar
segments(1+diffNoExpCrit-epsilon, minCINoExpCrit[1], 1+diffNoExpCrit+epsilon, minCINoExpCrit[1], col = colNoExpCrit, lty = ltyNoExpCrit) # Error bar bottom
segments(1+diffNoExpCrit-epsilon, maxCINoExpCrit[1], 1+diffNoExpCrit+epsilon, maxCINoExpCrit[1], col = colNoExpCrit, lty = ltyNoExpCrit) # Error bar top

# No T2 CI - ExpCrit
segments(2+diffNoExpCrit, minCINoExpCrit[2], 2+diffNoExpCrit,maxCINoExpCrit[2], col=colNoExpCrit, lty = ltyNoExpCrit)                    # Error bar
segments(2+diffNoExpCrit-epsilon, minCINoExpCrit[2], 2+diffNoExpCrit+epsilon, minCINoExpCrit[2], col = colNoExpCrit, lty = ltyNoExpCrit) # Error bar bottom
segments(2+diffNoExpCrit-epsilon, maxCINoExpCrit[2], 2+diffNoExpCrit+epsilon, maxCINoExpCrit[2], col = colNoExpCrit, lty = ltyNoExpCrit) # Error bar top

# No T3 CI - ExpCrit
segments(3+diffNoExpCrit, minCINoExpCrit[3], 3+diffNoExpCrit,maxCINoExpCrit[3], col=colNoExpCrit, lty = ltyNoExpCrit)                    # Error bar
segments(3+diffNoExpCrit-epsilon, minCINoExpCrit[3], 3+diffNoExpCrit+epsilon, minCINoExpCrit[3], col = colNoExpCrit, lty = ltyNoExpCrit) # Error bar bottom
segments(3+diffNoExpCrit-epsilon, maxCINoExpCrit[3], 3+diffNoExpCrit+epsilon, maxCINoExpCrit[3], col = colNoExpCrit, lty = ltyNoExpCrit) # Error bar top


## Controls
# Yes T1 CI - ImpContr
segments(1+diffYesImpContr, minCIYesImpContr[1], 1+diffYesImpContr, maxCIYesImpContr[1], col=colYesImpContr, lty = ltyYesImpContr)                   # Error bar
segments(1+diffYesImpContr-epsilon, minCIYesImpContr[1], 1+diffYesImpContr+epsilon, minCIYesImpContr[1], col = colYesImpContr, lty = ltyYesImpContr) # Error bar bottom
segments(1+diffYesImpContr-epsilon, maxCIYesImpContr[1], 1+diffYesImpContr+epsilon, maxCIYesImpContr[1], col = colYesImpContr, lty = ltyYesImpContr) # Error bar top

# Yes T2 CI - ImpContr
segments(2+diffYesImpContr, minCIYesImpContr[2], 2+diffYesImpContr, maxCIYesImpContr[2], col=colYesImpContr, lty = ltyYesImpContr)                   # Error bar
segments(2+diffYesImpContr-epsilon, minCIYesImpContr[2], 2+diffYesImpContr+epsilon, minCIYesImpContr[2], col = colYesImpContr, lty = ltyYesImpContr) # Error bar bottom
segments(2+diffYesImpContr-epsilon, maxCIYesImpContr[2], 2+diffYesImpContr+epsilon, maxCIYesImpContr[2], col = colYesImpContr, lty = ltyYesImpContr) # Error bar top

# Yes T3 CI - ImpContr
segments(3+diffYesImpContr, minCIYesImpContr[3], 3+diffYesImpContr, maxCIYesImpContr[3], col=colYesImpContr, lty = ltyYesImpContr)                   # Error bar
segments(3+diffYesImpContr-epsilon, minCIYesImpContr[3], 3+diffYesImpContr+epsilon, minCIYesImpContr[3], col = colYesImpContr, lty = ltyYesImpContr) # Error bar bottom
segments(3+diffYesImpContr-epsilon, maxCIYesImpContr[3], 3+diffYesImpContr+epsilon, maxCIYesImpContr[3], col = colYesImpContr, lty = ltyYesImpContr) # Error bar top


# No T1 CI - ImpContr
segments(1+diffNoImpContr, minCINoImpContr[1], 1+diffNoImpContr, maxCINoImpContr[1], col=colNoImpContr, lty = ltyNoImpContr)                   # Error bar
segments(1+diffNoImpContr-epsilon, minCINoImpContr[1], 1+diffNoImpContr+epsilon, minCINoImpContr[1], col = colNoImpContr, lty = ltyNoImpContr) # Error bar bottom
segments(1+diffNoImpContr-epsilon, maxCINoImpContr[1], 1+diffNoImpContr+epsilon, maxCINoImpContr[1], col = colNoImpContr, lty = ltyNoImpContr) # Error bar top

# No T2 CI - ImpContr
segments(2+diffNoImpContr, minCINoImpContr[2], 2+diffNoImpContr,maxCINoImpContr[2], col=colNoImpContr, lty = ltyNoImpContr)                    # Error bar
segments(2+diffNoImpContr-epsilon, minCINoImpContr[2], 2+diffNoImpContr+epsilon, minCINoImpContr[2], col = colNoImpContr, lty = ltyNoImpContr) # Error bar bottom
segments(2+diffNoImpContr-epsilon, maxCINoImpContr[2], 2+diffNoImpContr+epsilon, maxCINoImpContr[2], col = colNoImpContr, lty = ltyNoImpContr) # Error bar top

# No T3 CI - ImpContr
segments(3+diffNoImpContr, minCINoImpContr[3], 3+diffNoImpContr,maxCINoImpContr[3], col=colNoImpContr, lty = ltyNoImpContr)                    # Error bar
segments(3+diffNoImpContr-epsilon, minCINoImpContr[3], 3+diffNoImpContr+epsilon, minCINoImpContr[3], col = colNoImpContr, lty = ltyNoImpContr) # Error bar bottom
segments(3+diffNoImpContr-epsilon, maxCINoImpContr[3], 3+diffNoImpContr+epsilon, maxCINoImpContr[3], col = colNoImpContr, lty = ltyNoImpContr) # Error bar top


# Yes T1 CI - ExpContr
segments(1+diffYesExpContr, minCIYesExpContr[1], 1+diffYesExpContr, maxCIYesExpContr[1], col=colYesExpContr, lty = ltyYesExpContr)                   # Error bar
segments(1+diffYesExpContr-epsilon, minCIYesExpContr[1], 1+diffYesExpContr+epsilon, minCIYesExpContr[1], col = colYesExpContr, lty = ltyYesExpContr) # Error bar bottom
segments(1+diffYesExpContr-epsilon, maxCIYesExpContr[1], 1+diffYesExpContr+epsilon, maxCIYesExpContr[1], col = colYesExpContr, lty = ltyYesExpContr) # Error bar top

# Yes T2 CI - ExpContr
segments(2+diffYesExpContr, minCIYesExpContr[2], 2+diffYesExpContr, maxCIYesExpContr[2], col=colYesExpContr, lty = ltyYesExpContr)                   # Error bar
segments(2+diffYesExpContr-epsilon, minCIYesExpContr[2], 2+diffYesExpContr+epsilon, minCIYesExpContr[2], col = colYesExpContr, lty = ltyYesExpContr) # Error bar bottom
segments(2+diffYesExpContr-epsilon, maxCIYesExpContr[2], 2+diffYesExpContr+epsilon, maxCIYesExpContr[2], col = colYesExpContr, lty = ltyYesExpContr) # Error bar top

# Yes T3 CI - ExpContr
segments(3+diffYesExpContr, minCIYesExpContr[3], 3+diffYesExpContr, maxCIYesExpContr[3], col=colYesExpContr, lty = ltyYesExpContr)                   # Error bar
segments(3+diffYesExpContr-epsilon, minCIYesExpContr[3], 3+diffYesExpContr+epsilon, minCIYesExpContr[3], col = colYesExpContr, lty = ltyYesExpContr) # Error bar bottom
segments(3+diffYesExpContr-epsilon, maxCIYesExpContr[3], 3+diffYesExpContr+epsilon, maxCIYesExpContr[3], col = colYesExpContr, lty = ltyYesExpContr) # Error bar top


# No T1 CI - ExpContr
segments(1+diffNoExpContr, minCINoExpContr[1], 1+diffNoExpContr, maxCINoExpContr[1], col=colNoExpContr, lty = ltyNoExpContr)                   # Error bar
segments(1+diffNoExpContr-epsilon, minCINoExpContr[1], 1+diffNoExpContr+epsilon, minCINoExpContr[1], col = colNoExpContr, lty = ltyNoExpContr) # Error bar bottom
segments(1+diffNoExpContr-epsilon, maxCINoExpContr[1], 1+diffNoExpContr+epsilon, maxCINoExpContr[1], col = colNoExpContr, lty = ltyNoExpContr) # Error bar top

# No T2 CI - ExpContr
segments(2+diffNoExpContr, minCINoExpContr[2], 2+diffNoExpContr,maxCINoExpContr[2], col=colNoExpContr, lty = ltyNoExpContr)                    # Error bar
segments(2+diffNoExpContr-epsilon, minCINoExpContr[2], 2+diffNoExpContr+epsilon, minCINoExpContr[2], col = colNoExpContr, lty = ltyNoExpContr) # Error bar bottom
segments(2+diffNoExpContr-epsilon, maxCINoExpContr[2], 2+diffNoExpContr+epsilon, maxCINoExpContr[2], col = colNoExpContr, lty = ltyNoExpContr) # Error bar top

# No T3 CI - ExpContr
segments(3+diffNoExpContr, minCINoExpContr[3], 3+diffNoExpContr,maxCINoExpContr[3], col=colNoExpContr, lty = ltyNoExpContr)                    # Error bar
segments(3+diffNoExpContr-epsilon, minCINoExpContr[3], 3+diffNoExpContr+epsilon, minCINoExpContr[3], col = colNoExpContr, lty = ltyNoExpContr) # Error bar bottom
segments(3+diffNoExpContr-epsilon, maxCINoExpContr[3], 3+diffNoExpContr+epsilon, maxCINoExpContr[3], col = colNoExpContr, lty = ltyNoExpContr) # Error bar top




# Legende
par(cex = 0.8)  # Fuer den naechsten Schritt die Schriftgroesse auf 80% setzen
legend("bottomright",
       c("Incidental: critical items with input", "Incidental: critical items without input",
         "Explicit: critical items with input", "Explicit: critical items without input",
         "Incidental: control items with input", "Incidental: control items without input",
         "Explicit: control items with input", "Explicit: control items without input"),
       col = c(colYesImpCrit, colNoImpCrit, colYesExpCrit, colYesExpCrit, colYesImpContr, colNoImpContr, colYesExpContr, colYesExpContr),
       pch = c(24,24,19,19,22,22,23,23),
       pt.bg = c("darkorange2", "darkorange2", "dodgerblue3","dodgerblue3", "darkorange2", "darkorange2", "dodgerblue3", "dodgerblue3"),
       lty = c(1, 3, 1, 3, 1, 3, 1, 3),
       inset = 0.02)
par(cex = 1) #  Fuer weitere Schritte Schriftgroesse wieder auf 100% setzen



# Save plot information into record
PlotRecord<-recordPlot()

# Plot speichern (mit Speicherort, Weite, Hoehe, Aufloesung)
png("../plots/plot_full_data.png", width=plotwidth, height=plotheight,units="in",res=300)
replayPlot(PlotRecord)
dev.off()