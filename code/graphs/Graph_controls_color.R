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

#----------------------------
# CONTROL ITEMS - T1-T2-T3
#----------------------------

x        <- c(1,2,3)  # Test moment 1 und 2 und 3
epsilon  <- 0.05      # Error bar top and bottom line length (x-dimension)

# Yes input - incidental
meanYesImp  <- c(93.15, 92.65, 86.27) 
minCIYesImp <- c(89.58, 89.31, 82.17) 
maxCIYesImp <- c(95.83, 95.24, 89.84) 
colYesImp   <- "darkorange2"
ltyYesImp   <- 1          # Line type Yes (1 = normal)
diffYesImp  <- -0.065     # Versatz

# No input - incidental
meanNoImp   <- c(91.47, 91.65, 87.91) 
minCINoImp  <- c(87.96, 88.93, 85.28) 
maxCINoImp  <- c(94.35, 93.91, 90.48) 
colNoImp    <- "darkorange2"
ltyNoImp    <- 3         # Line type No (3 = gepunktet)
diffNoImp  <- -0.025     # Versatz

# Yes input - explicit
meanYesExp  <- c(83.33, 89.34, 86.99) 
minCIYesExp <- c(77.60, 82.10, 81.10) 
maxCIYesExp <- c(89.00, 96.50, 92.90) 
colYesExp   <- "dodgerblue3"
ltyYesExp   <- 1         # Line type Yes (1 = normal)
diffYesExp  <- 0.025     # Versatz

# No input - explicit
meanNoExp   <- c(86.13, 86.18, 86.25) 
minCINoExp  <- c(80.70, 79.70, 80.75) 
maxCINoExp  <- c(91.60, 92.60, 91.75) 
colNoExp    <- "dodgerblue3"
ltyNoExp    <- 3         # Line type No (3 = gepunktet)
diffNoExp   <- 0.065     # Versatz


# Plot Dimensionen bestimmen (inches)
plotwidth = 8
plotheight = 6.5

# Neues Plot Fenster machen mit den korrekten Dimensionen
quartz("plotwindowW, plotwidh, plotheight")

# Plot leer
plot(x =  0,
     y =  0,
     main = "Control items",
     xlim = c(0.5,3.5),
     ylim = c(0,100), # c(min(minCIYes,minCINo),max(maxCIYes,maxCINo)), <- Alternative
     xaxt = "n", # x-Achse soll nicht mit 1 und 2 nummeriert werden
     ylab = "Accuracy percentage",
     xlab = "Test moment",
     col  = colYesImp)

# Dazu Punkte fuer die Konditionen
points(x + diffYesImp, meanYesImp, col = colYesImp, pch = 24, bg = "darkorange2") # pch 24: Dreieck; bg "darkorange2": fuelle das Symbol mit Farbe.
points(x + diffNoImp, meanNoImp, col = colNoImp, pch = 24, bg = "darkorange2")
points(x + diffYesExp, meanYesExp, col = colYesExp, pch = 19, bg = "dodgerblue3") # pch 19: Kreis
points(x + diffNoExp, meanNoExp, col = colNoExp, pch = 19, bg = "dodgerblue3")

# x-Achse selber beschriften
par(cex = 0.9) # Fuer den naechsten Schritt die Schriftgroesse auf 90% setzen
axis(1,at=c(1,2,3),labels=c("T1 (learning task)","T2 (learning task)","T3 (explicit posttest)"))
par(cex = 1) # Fuer weitere Schritte Schriftgroesse wieder auf 100% setzen
par(lwd = 2) # Liniensdicke anpassen

# Verbindungslinien T1-T2 - incidental
segments(1 + diffYesImp, meanYesImp[1], 2 + diffYesImp, meanYesImp[2], col = colYesImp, lty = ltyYesImp) # Verbindungslinie Yes
segments(1 + diffNoImp, meanNoImp[1], 2 + diffNoImp, meanNoImp[2], col = colNoImp, lty = ltyNoImp)       # Verbindungslinie No

# Verbindungslinien T2-T3 - incidental
segments(2 + diffYesImp, meanYesImp[2], 3 + diffYesImp, meanYesImp[3], col = colYesImp, lty = ltyYesImp) # Verbindungslinie Yes
segments(2 + diffNoImp, meanNoImp[2], 3 + diffNoImp, meanNoImp[3], col = colNoImp, lty = ltyNoImp)       # Verbindungslinie No

# Verbindungslinien T1-T2 - explicit
segments(1 + diffYesExp, meanYesExp[1], 2 + diffYesExp, meanYesExp[2], col = colYesExp, lty = ltyYesExp) # Verbindungslinie Yes
segments(1 + diffNoExp, meanNoExp[1], 2 + diffNoExp, meanNoExp[2], col = colNoExp, lty = ltyNoExp)       # Verbindungslinie No

# Verbindungslinien T2-T3 - explicit
segments(2 + diffYesExp, meanYesExp[2], 3 + diffYesExp, meanYesExp[3], col = colYesExp, lty = ltyYesExp) # Verbindungslinie Yes
segments(2 + diffNoExp, meanNoExp[2], 3 + diffNoExp, meanNoExp[3], col = colNoExp, lty = ltyNoExp)       # Verbindungslinie No


### Error bars: CI ###

# Yes T1 CI - imp
segments(1+diffYesImp, minCIYesImp[1], 1+diffYesImp, maxCIYesImp[1], col=colYesImp, lty = ltyYesImp)                   # Error bar
segments(1+diffYesImp-epsilon, minCIYesImp[1], 1+diffYesImp+epsilon, minCIYesImp[1], col = colYesImp, lty = ltyYesImp) # Error bar bottom
segments(1+diffYesImp-epsilon, maxCIYesImp[1], 1+diffYesImp+epsilon, maxCIYesImp[1], col = colYesImp, lty = ltyYesImp) # Error bar top

# Yes T2 CI - imp
segments(2+diffYesImp, minCIYesImp[2], 2+diffYesImp, maxCIYesImp[2], col=colYesImp, lty = ltyYesImp)                   # Error bar
segments(2+diffYesImp-epsilon, minCIYesImp[2], 2+diffYesImp+epsilon, minCIYesImp[2], col = colYesImp, lty = ltyYesImp) # Error bar bottom
segments(2+diffYesImp-epsilon, maxCIYesImp[2], 2+diffYesImp+epsilon, maxCIYesImp[2], col = colYesImp, lty = ltyYesImp) # Error bar top

# Yes T3 CI - imp
segments(3+diffYesImp, minCIYesImp[3], 3+diffYesImp, maxCIYesImp[3], col=colYesImp, lty = ltyYesImp)                   # Error bar
segments(3+diffYesImp-epsilon, minCIYesImp[3], 3+diffYesImp+epsilon, minCIYesImp[3], col = colYesImp, lty = ltyYesImp) # Error bar bottom
segments(3+diffYesImp-epsilon, maxCIYesImp[3], 3+diffYesImp+epsilon, maxCIYesImp[3], col = colYesImp, lty = ltyYesImp) # Error bar top


# No T1 CI - imp
segments(1+diffNoImp, minCINoImp[1], 1+diffNoImp, maxCINoImp[1], col=colNoImp, lty = ltyNoImp)                   # Error bar
segments(1+diffNoImp-epsilon, minCINoImp[1], 1+diffNoImp+epsilon, minCINoImp[1], col = colNoImp, lty = ltyNoImp) # Error bar bottom
segments(1+diffNoImp-epsilon, maxCINoImp[1], 1+diffNoImp+epsilon, maxCINoImp[1], col = colNoImp, lty = ltyNoImp) # Error bar top

# No T2 CI - imp
segments(2+diffNoImp, minCINoImp[2], 2+diffNoImp,maxCINoImp[2], col=colNoImp, lty = ltyNoImp)                    # Error bar
segments(2+diffNoImp-epsilon, minCINoImp[2], 2+diffNoImp+epsilon, minCINoImp[2], col = colNoImp, lty = ltyNoImp) # Error bar bottom
segments(2+diffNoImp-epsilon, maxCINoImp[2], 2+diffNoImp+epsilon, maxCINoImp[2], col = colNoImp, lty = ltyNoImp) # Error bar top

# No T3 CI - imp
segments(3+diffNoImp, minCINoImp[3], 3+diffNoImp,maxCINoImp[3], col=colNoImp, lty = ltyNoImp)                    # Error bar
segments(3+diffNoImp-epsilon, minCINoImp[3], 3+diffNoImp+epsilon, minCINoImp[3], col = colNoImp, lty = ltyNoImp) # Error bar bottom
segments(3+diffNoImp-epsilon, maxCINoImp[3], 3+diffNoImp+epsilon, maxCINoImp[3], col = colNoImp, lty = ltyNoImp) # Error bar top


# Yes T1 CI - exp
segments(1+diffYesExp, minCIYesExp[1], 1+diffYesExp, maxCIYesExp[1], col=colYesExp, lty = ltyYesExp)                   # Error bar
segments(1+diffYesExp-epsilon, minCIYesExp[1], 1+diffYesExp+epsilon, minCIYesExp[1], col = colYesExp, lty = ltyYesExp) # Error bar bottom
segments(1+diffYesExp-epsilon, maxCIYesExp[1], 1+diffYesExp+epsilon, maxCIYesExp[1], col = colYesExp, lty = ltyYesExp) # Error bar top

# Yes T2 CI - exp
segments(2+diffYesExp, minCIYesExp[2], 2+diffYesExp, maxCIYesExp[2], col=colYesExp, lty = ltyYesExp)                   # Error bar
segments(2+diffYesExp-epsilon, minCIYesExp[2], 2+diffYesExp+epsilon, minCIYesExp[2], col = colYesExp, lty = ltyYesExp) # Error bar bottom
segments(2+diffYesExp-epsilon, maxCIYesExp[2], 2+diffYesExp+epsilon, maxCIYesExp[2], col = colYesExp, lty = ltyYesExp) # Error bar top

# Yes T3 CI - exp
segments(3+diffYesExp, minCIYesExp[3], 3+diffYesExp, maxCIYesExp[3], col=colYesExp, lty = ltyYesExp)                   # Error bar
segments(3+diffYesExp-epsilon, minCIYesExp[3], 3+diffYesExp+epsilon, minCIYesExp[3], col = colYesExp, lty = ltyYesExp) # Error bar bottom
segments(3+diffYesExp-epsilon, maxCIYesExp[3], 3+diffYesExp+epsilon, maxCIYesExp[3], col = colYesExp, lty = ltyYesExp) # Error bar top


# No T1 CI - exp
segments(1+diffNoExp, minCINoExp[1], 1+diffNoExp, maxCINoExp[1], col=colNoExp, lty = ltyNoExp)                   # Error bar
segments(1+diffNoExp-epsilon, minCINoExp[1], 1+diffNoExp+epsilon, minCINoExp[1], col = colNoExp, lty = ltyNoExp) # Error bar bottom
segments(1+diffNoExp-epsilon, maxCINoExp[1], 1+diffNoExp+epsilon, maxCINoExp[1], col = colNoExp, lty = ltyNoExp) # Error bar top

# No T2 CI - exp
segments(2+diffNoExp, minCINoExp[2], 2+diffNoExp,maxCINoExp[2], col=colNoExp, lty = ltyNoExp)                    # Error bar
segments(2+diffNoExp-epsilon, minCINoExp[2], 2+diffNoExp+epsilon, minCINoExp[2], col = colNoExp, lty = ltyNoExp) # Error bar bottom
segments(2+diffNoExp-epsilon, maxCINoExp[2], 2+diffNoExp+epsilon, maxCINoExp[2], col = colNoExp, lty = ltyNoExp) # Error bar top

# No T3 CI - exp
segments(3+diffNoExp, minCINoExp[3], 3+diffNoExp,maxCINoExp[3], col=colNoExp, lty = ltyNoExp)                    # Error bar
segments(3+diffNoExp-epsilon, minCINoExp[3], 3+diffNoExp+epsilon, minCINoExp[3], col = colNoExp, lty = ltyNoExp) # Error bar bottom
segments(3+diffNoExp-epsilon, maxCINoExp[3], 3+diffNoExp+epsilon, maxCINoExp[3], col = colNoExp, lty = ltyNoExp) # Error bar top


# Legende
par(cex = 0.9)  # Fuer den naechsten Schritt die Schriftgroesse auf 80% setzen
legend("bottomright", title = ("Conditions"),
       c("Incidental - with input", "Incidental - without input", "Explicit - with input", "Explicit - without input"),
       col = c(colYesImp, colNoImp, colYesExp, colYesExp), pch = c(24, 24, 19, 19),
       pt.bg = c("darkorange2", "darkorange2", "dodgerblue3", "dodgerblue3"), lty = c(1, 3, 1, 3),
       inset = 0.02)
par(cex = 1) #  Fuer weitere Schritte Schriftgroesse wieder auf 100% setzen

# Save plot information into record
PlotRecord<-recordPlot()

# Plot speichern (mit Speicherort, Weite, Hoehe, Aufloesung)
png("../plots/plot_controls.png", width=plotwidth, height=plotheight,units="in",res=300)
replayPlot(PlotRecord)
dev.off()





