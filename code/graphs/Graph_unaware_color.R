# Copyright Josef Koch (2018)
# Copyright Eva Koch (2018)

# Clear workspace
rm(list=ls())

# Set working directory to source file location
library(rstudioapi)
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path # get the path of your current open file
  setwd(dirname(current_path )) # set the working directory to the source file location
  print( getwd() ) # display the directory
}
set_wd()



x        <- c(1,2,3)  # Moment 1 und 2 und 3
epsilon  <- 0.05      # Error bar top and bottom line length (x-dimension)


# Critical items
#---------------
# Yes input - unaware
meanYesUnawareCrit  <- c(05.71,07.80,26.37) 
minCIYesUnawareCrit <- c(02.22,02.22,10.97) 
maxCIYesUnawareCrit <- c(09.13,12.10,45.72) 
colYesUnawareCrit   <- "darkorange2" # Linienfarbe
ltyYesUnawareCrit   <- 1             # Line type Yes (1 = normal)
diffYesUnawareCrit  <- -0.045        # Versatz

# No input - unaware
meanNoUnawareCrit   <- c(02.08,02.23,21.30) 
minCINoUnawareCrit  <- c(00.00,00.00,05.56) 
maxCINoUnawareCrit  <- c(04.17,04.61,46.95) 
colNoUnawareCrit    <- "darkorange2"
ltyNoUnawareCrit    <- 3          # Line type No (3 = gepunktet)
diffNoUnawareCrit   <- 0.045      # Versatz


# Control items
#--------------
# Yes input - unaware
meanYesUnawareContr  <- c(100.00, 98.96, 89.58) 
minCIYesUnawareContr <- c(100.00, 94.79, 63.54) 
maxCIYesUnawareContr <- c(100.00, 100.00, 96.88) 
colYesUnawareContr    <- "dodgerblue3"
ltyYesUnawareContr   <- 1          # Line type Yes (1 = normal)
diffYesUnawareContr  <- -0.045     # Versatz

# No input - unaware
meanNoUnawareContr   <- c(98.81, 96.66, 88.88) 
minCINoUnawareContr  <- c(94.05, 93.61, 71.10) 
maxCINoUnawareContr  <- c(100.00, 98.96, 95.55) 
colNoUnawareContr    <- "dodgerblue3"
ltyNoUnawareContr    <- 3         # Line type No (3 = gepunktet)
diffNoUnawareContr  <- 0.045     # Versatz


# Plot Dimensionen bestimmen (inches)
plotwidth = 8
plotheight = 6.5

# Neues Plot Fenster machen mit den korrekten Dimensionen
quartz("plotwindowW, plotwidh, plotheight")

# Plot leer
plot(x =  0,
     y =  0,
     main = "Test scores: Unaware group",
     xlim = c(0.5,3.5),
     ylim = c(0,100),
     xaxt = "n", # x-Achse soll nicht mit 1 und 2 nummeriert werden
     ylab = "Accuracy percentage",
     xlab = "Test moment",
     col  = colYesUnawareCrit) #?

## Dazu Punkte fuer die Konditionen
# Critical items
points(x + diffYesUnawareCrit, meanYesUnawareCrit, col = colYesUnawareCrit, pch = 19, bg = "darkorange2") # pch 19: Kreis; bg "dodgerblue3": fuelle das Symbol mit Farbe.
points(x + diffNoUnawareCrit, meanNoUnawareCrit, col = colNoUnawareCrit, pch = 19, bg = "darkorange2")

# Control items
points(x + diffYesUnawareContr, meanYesUnawareContr, col = colYesUnawareContr, pch = 22, bg = "dodgerblue3") # pch 22: Quadrat
points(x + diffNoUnawareContr, meanNoUnawareContr, col = colNoUnawareContr, pch = 22, bg = "dodgerblue3")

# x-Achse selber beschriften
par(cex = 0.9) # Fuer den naechsten Schritt die Schriftgroesse auf 90% setzen
axis(1,at=c(1,2,3),labels=c("T1 (learning task)","T2 (learning task)","T3 (explicit posttest)"))
par(cex = 1) # Fuer weitere Schritte Schriftgroesse wieder auf 100% setzen
par(lwd = 2) # Liniensdicke anpassen

## Verbindungslinien zwischen Punkten
# Criticals
# Verbindungslinien T1-T2 - unaware
segments(1 + diffYesUnawareCrit, meanYesUnawareCrit[1], 2 + diffYesUnawareCrit, meanYesUnawareCrit[2], col = colYesUnawareCrit, lty = ltyYesUnawareCrit) # Verbindungslinie Yes
segments(1 + diffNoUnawareCrit, meanNoUnawareCrit[1], 2 + diffNoUnawareCrit, meanNoUnawareCrit[2], col = colNoUnawareCrit, lty = ltyNoUnawareCrit)       # Verbindungslinie No

# Verbindungslinien T2-T3 - unaware
segments(2 + diffYesUnawareCrit, meanYesUnawareCrit[2], 3 + diffYesUnawareCrit, meanYesUnawareCrit[3], col = colYesUnawareCrit, lty = ltyYesUnawareCrit) # Verbindungslinie Yes
segments(2 + diffNoUnawareCrit, meanNoUnawareCrit[2], 3 + diffNoUnawareCrit, meanNoUnawareCrit[3], col = colNoUnawareCrit, lty = ltyNoUnawareCrit)       # Verbindungslinie No


# Controls
# Verbindungslinien T1-T2 - unaware
segments(1 + diffYesUnawareContr, meanYesUnawareContr[1], 2 + diffYesUnawareContr, meanYesUnawareContr[2], col = colYesUnawareContr, lty = ltyYesUnawareContr) # Verbindungslinie Yes
segments(1 + diffNoUnawareContr, meanNoUnawareContr[1], 2 + diffNoUnawareContr, meanNoUnawareContr[2], col = colNoUnawareContr, lty = ltyNoUnawareContr)       # Verbindungslinie No

# Verbindungslinien T2-T3 - unaware
segments(2 + diffYesUnawareContr, meanYesUnawareContr[2], 3 + diffYesUnawareContr, meanYesUnawareContr[3], col = colYesUnawareContr, lty = ltyYesUnawareContr) # Verbindungslinie Yes
segments(2 + diffNoUnawareContr, meanNoUnawareContr[2], 3 + diffNoUnawareContr, meanNoUnawareContr[3], col = colNoUnawareContr, lty = ltyNoUnawareContr)       # Verbindungslinie No


### Error bars: CI ###
## Criticals
# Yes T1 CI - UnawareCrit
segments(1+diffYesUnawareCrit, minCIYesUnawareCrit[1], 1+diffYesUnawareCrit, maxCIYesUnawareCrit[1], col=colYesUnawareCrit, lty = ltyYesUnawareCrit)                   # Error bar
segments(1+diffYesUnawareCrit-epsilon, minCIYesUnawareCrit[1], 1+diffYesUnawareCrit+epsilon, minCIYesUnawareCrit[1], col = colYesUnawareCrit, lty = ltyYesUnawareCrit) # Error bar bottom
segments(1+diffYesUnawareCrit-epsilon, maxCIYesUnawareCrit[1], 1+diffYesUnawareCrit+epsilon, maxCIYesUnawareCrit[1], col = colYesUnawareCrit, lty = ltyYesUnawareCrit) # Error bar top

# Yes T2 CI - UnawareCrit
segments(2+diffYesUnawareCrit, minCIYesUnawareCrit[2], 2+diffYesUnawareCrit, maxCIYesUnawareCrit[2], col=colYesUnawareCrit, lty = ltyYesUnawareCrit)                   # Error bar
segments(2+diffYesUnawareCrit-epsilon, minCIYesUnawareCrit[2], 2+diffYesUnawareCrit+epsilon, minCIYesUnawareCrit[2], col = colYesUnawareCrit, lty = ltyYesUnawareCrit) # Error bar bottom
segments(2+diffYesUnawareCrit-epsilon, maxCIYesUnawareCrit[2], 2+diffYesUnawareCrit+epsilon, maxCIYesUnawareCrit[2], col = colYesUnawareCrit, lty = ltyYesUnawareCrit) # Error bar top

# Yes T3 CI - UnawareCrit
segments(3+diffYesUnawareCrit, minCIYesUnawareCrit[3], 3+diffYesUnawareCrit, maxCIYesUnawareCrit[3], col=colYesUnawareCrit, lty = ltyYesUnawareCrit)                   # Error bar
segments(3+diffYesUnawareCrit-epsilon, minCIYesUnawareCrit[3], 3+diffYesUnawareCrit+epsilon, minCIYesUnawareCrit[3], col = colYesUnawareCrit, lty = ltyYesUnawareCrit) # Error bar bottom
segments(3+diffYesUnawareCrit-epsilon, maxCIYesUnawareCrit[3], 3+diffYesUnawareCrit+epsilon, maxCIYesUnawareCrit[3], col = colYesUnawareCrit, lty = ltyYesUnawareCrit) # Error bar top


# No T1 CI - UnawareCrit
segments(1+diffNoUnawareCrit, minCINoUnawareCrit[1], 1+diffNoUnawareCrit, maxCINoUnawareCrit[1], col=colNoUnawareCrit, lty = ltyNoUnawareCrit)                   # Error bar
segments(1+diffNoUnawareCrit-epsilon, minCINoUnawareCrit[1], 1+diffNoUnawareCrit+epsilon, minCINoUnawareCrit[1], col = colNoUnawareCrit, lty = ltyNoUnawareCrit) # Error bar bottom
segments(1+diffNoUnawareCrit-epsilon, maxCINoUnawareCrit[1], 1+diffNoUnawareCrit+epsilon, maxCINoUnawareCrit[1], col = colNoUnawareCrit, lty = ltyNoUnawareCrit) # Error bar top

# No T2 CI - UnawareCrit
segments(2+diffNoUnawareCrit, minCINoUnawareCrit[2], 2+diffNoUnawareCrit,maxCINoUnawareCrit[2], col=colNoUnawareCrit, lty = ltyNoUnawareCrit)                    # Error bar
segments(2+diffNoUnawareCrit-epsilon, minCINoUnawareCrit[2], 2+diffNoUnawareCrit+epsilon, minCINoUnawareCrit[2], col = colNoUnawareCrit, lty = ltyNoUnawareCrit) # Error bar bottom
segments(2+diffNoUnawareCrit-epsilon, maxCINoUnawareCrit[2], 2+diffNoUnawareCrit+epsilon, maxCINoUnawareCrit[2], col = colNoUnawareCrit, lty = ltyNoUnawareCrit) # Error bar top

# No T3 CI - UnawareCrit
segments(3+diffNoUnawareCrit, minCINoUnawareCrit[3], 3+diffNoUnawareCrit,maxCINoUnawareCrit[3], col=colNoUnawareCrit, lty = ltyNoUnawareCrit)                    # Error bar
segments(3+diffNoUnawareCrit-epsilon, minCINoUnawareCrit[3], 3+diffNoUnawareCrit+epsilon, minCINoUnawareCrit[3], col = colNoUnawareCrit, lty = ltyNoUnawareCrit) # Error bar bottom
segments(3+diffNoUnawareCrit-epsilon, maxCINoUnawareCrit[3], 3+diffNoUnawareCrit+epsilon, maxCINoUnawareCrit[3], col = colNoUnawareCrit, lty = ltyNoUnawareCrit) # Error bar top


## Controls
# Yes T1 CI - UnawareContr
segments(1+diffYesUnawareContr, minCIYesUnawareContr[1], 1+diffYesUnawareContr, maxCIYesUnawareContr[1], col=colYesUnawareContr, lty = ltyYesUnawareContr)                   # Error bar
segments(1+diffYesUnawareContr-epsilon, minCIYesUnawareContr[1], 1+diffYesUnawareContr+epsilon, minCIYesUnawareContr[1], col = colYesUnawareContr, lty = ltyYesUnawareContr) # Error bar bottom
segments(1+diffYesUnawareContr-epsilon, maxCIYesUnawareContr[1], 1+diffYesUnawareContr+epsilon, maxCIYesUnawareContr[1], col = colYesUnawareContr, lty = ltyYesUnawareContr) # Error bar top

# Yes T2 CI - UnawareContr
segments(2+diffYesUnawareContr, minCIYesUnawareContr[2], 2+diffYesUnawareContr, maxCIYesUnawareContr[2], col=colYesUnawareContr, lty = ltyYesUnawareContr)                   # Error bar
segments(2+diffYesUnawareContr-epsilon, minCIYesUnawareContr[2], 2+diffYesUnawareContr+epsilon, minCIYesUnawareContr[2], col = colYesUnawareContr, lty = ltyYesUnawareContr) # Error bar bottom
segments(2+diffYesUnawareContr-epsilon, maxCIYesUnawareContr[2], 2+diffYesUnawareContr+epsilon, maxCIYesUnawareContr[2], col = colYesUnawareContr, lty = ltyYesUnawareContr) # Error bar top

# Yes T3 CI - UnawareContr
segments(3+diffYesUnawareContr, minCIYesUnawareContr[3], 3+diffYesUnawareContr, maxCIYesUnawareContr[3], col=colYesUnawareContr, lty = ltyYesUnawareContr)                   # Error bar
segments(3+diffYesUnawareContr-epsilon, minCIYesUnawareContr[3], 3+diffYesUnawareContr+epsilon, minCIYesUnawareContr[3], col = colYesUnawareContr, lty = ltyYesUnawareContr) # Error bar bottom
segments(3+diffYesUnawareContr-epsilon, maxCIYesUnawareContr[3], 3+diffYesUnawareContr+epsilon, maxCIYesUnawareContr[3], col = colYesUnawareContr, lty = ltyYesUnawareContr) # Error bar top


# No T1 CI - UnawareContr
segments(1+diffNoUnawareContr, minCINoUnawareContr[1], 1+diffNoUnawareContr, maxCINoUnawareContr[1], col=colNoUnawareContr, lty = ltyNoUnawareContr)                   # Error bar
segments(1+diffNoUnawareContr-epsilon, minCINoUnawareContr[1], 1+diffNoUnawareContr+epsilon, minCINoUnawareContr[1], col = colNoUnawareContr, lty = ltyNoUnawareContr) # Error bar bottom
segments(1+diffNoUnawareContr-epsilon, maxCINoUnawareContr[1], 1+diffNoUnawareContr+epsilon, maxCINoUnawareContr[1], col = colNoUnawareContr, lty = ltyNoUnawareContr) # Error bar top

# No T2 CI - UnawareContr
segments(2+diffNoUnawareContr, minCINoUnawareContr[2], 2+diffNoUnawareContr,maxCINoUnawareContr[2], col=colNoUnawareContr, lty = ltyNoUnawareContr)                    # Error bar
segments(2+diffNoUnawareContr-epsilon, minCINoUnawareContr[2], 2+diffNoUnawareContr+epsilon, minCINoUnawareContr[2], col = colNoUnawareContr, lty = ltyNoUnawareContr) # Error bar bottom
segments(2+diffNoUnawareContr-epsilon, maxCINoUnawareContr[2], 2+diffNoUnawareContr+epsilon, maxCINoUnawareContr[2], col = colNoUnawareContr, lty = ltyNoUnawareContr) # Error bar top

# No T3 CI - UnawareContr
segments(3+diffNoUnawareContr, minCINoUnawareContr[3], 3+diffNoUnawareContr,maxCINoUnawareContr[3], col=colNoUnawareContr, lty = ltyNoUnawareContr)                    # Error bar
segments(3+diffNoUnawareContr-epsilon, minCINoUnawareContr[3], 3+diffNoUnawareContr+epsilon, minCINoUnawareContr[3], col = colNoUnawareContr, lty = ltyNoUnawareContr) # Error bar bottom
segments(3+diffNoUnawareContr-epsilon, maxCINoUnawareContr[3], 3+diffNoUnawareContr+epsilon, maxCINoUnawareContr[3], col = colNoUnawareContr, lty = ltyNoUnawareContr) # Error bar top


# Legende
par(cex = 0.8)  # Fuer den naechsten Schritt die Schriftgroesse auf 80% setzen
legend("left",
       c("Critical items with input", "Critical items without input",
         "Control items with input", "Control items without input"),
       col = c(colYesUnawareCrit, colNoUnawareCrit, colYesUnawareContr, colNoUnawareContr),
       pch = c(19,19,22,22),
       pt.bg = c("darkorange2", "darkorange2", "dodgerblue3", "dodgerblue3"),
       lty = c(1, 3, 1, 3),
       inset = 0.02)
par(cex = 1) #  Fuer weitere Schritte Schriftgroesse wieder auf 100% setzen



# Save plot information into record
PlotRecord<-recordPlot()

# Plot speichern (mit Speicherort, Weite, Hoehe, Aufloesung)
png("../plots/plot_unaware_color.png", width=plotwidth, height=plotheight,units="in",res=300)
replayPlot(PlotRecord)
dev.off()