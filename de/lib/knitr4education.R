### knitr4education - v0.0.6
### https://www.github.com/niebert/knitr4education

### Include this file in your KnitR code by placing the following code in your 
### code chunk:
# source("knitr4education.R")
### If you store the library in a subdirectory 'lib/' use the following comamnd
# source("lib/knitr4education.R")



load_csv <- function(pFilename) {
   retData <- read.csv(pFilename, header=TRUE, stringsAsFactors=FALSE)
   ### Rückgabe retData ist ein Dataframe (Tabelle)
   return(retData)
}

save_csv <- function(pFilename, pData) {
  write.csv(pData, pFilename, row.names = FALSE)
}

copy4vec <- function (pVec) {
  return <- rep(0,length(pVec))
  for (i in length(pVec)) {
    return[i] <- pVec[i]
  }
  ## return cloned vector
  return
}

echo4vec <- function (pVarname,pVec) {
  print(paste(pVarname,"=(",paste(pVec,collapse=","),")"),sep="")
}

hello <- function(name) {
  print(paste("Hallo ",name,"! Mache keinen Mist",sep=""))
}

loesung_ausgeben <- function(hilfe,name,solution) {
  hilfe_ausgabe <-  str_replace(hilfetext,"SOLUTION",solution)
  hilfe_ausgabe <-  str_replace(hilfe_ausgabe,"NAME",name)
  ## Rückgabe der ersetzen Hilfe
  hilfe_ausgabe
}


find_min4error <- function (pError, pGrad, pa, px_D, py_D, alpha=1, evalcount=100) {
  ## Parameter
  ## pError: Fehlerfunktion
  ## pGrad:  Gradient der Fehlerfunktion
  ## px_D: x-Vektoren der Daten für den Definitionsbereich
  ## py_D: y-Sollwerte für die Fehlerfunktion
  return <- rep(0,2) ### return <- c(0,0);
  ## erste Komponente von return ist der minimale lambda-Wert
  ## zweite Komponente von return ist der minimale Fehler
  s4a <- (-evalcount:evalcount)/evalcount
  #s4a <- 2*runif(2*evalcount+1,-1,1)
  E4a <-  rep(0,2*evalcount+1) ## +1 wegen x4a=0
  ## smin - Skalar für Gradient für das Minimum der berechneten Fehler
  scalar4min <- s4a[1] ### ersten smin-Wert setzen - hier -1
  ### Fehler in pa berechnen als Startwert
  error4min <- pError( pa, px_D, py_D )
  grad4a      <- alpha * pGrad( pa, px_D, py_D )
  ## in Gradientenrichtung auswerten zwischen -1*pGrad und +1*Grad
  ## die Fehlerfunktion auswerten und über die x4a-Liste iterieren
  for (k in 1:length(s4a)) {
    ## Fehler für den um x4a[k] skalierten Gradienten pGrad
    ## und die Daten px_D, py_D berechnen
    E4a[k] <- pError(pa + s4a[k]*grad4a, px_D, py_D)
    ## Überprüfen, ob der Fehler kleiner ist als der
    ## bisher berechnete minimale Fehler errormin
    if (E4a[k] < error4min) {
      error4min  <-E4a[k]
      scalar4min <-s4a[k]
    }
  }
  # plot(s4a,E4a)
  return <- c(scalar4min,error4min)
  ## Rückgabewert des Skalars für den Gradienten und dem minimalen Fehler
  return
}

fuzzy_nand <- function (pf1,pf2) {
  return <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    return[i] <- 1 - min(pf1[i],pf2[2])
  }
  ### Rückgabewert ist das komponentenweise Minimum der beiden Vektoren pf1 und pf2
  return
}

fuzzy_and <- function (pf1,pf2) {
  return <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    return[i] <- min(pf1[i],pf2[2])
  }
  ### Rückgabewert ist das komponentenweise Minimum der beiden Vektoren pf1 und pf2
  return
}

fuzzy_or <- function (pf1,pf2) {
  return <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    return[i] <- max(pf1[i],pf2[i])
  }
  ### Rückgabewert ist das komponentenweise Maximum der beiden Vektoren pf1 und pf2
  return
}

fuzzy_not <- function (pf1) {
  return <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    return[i] <- 1 - pf1[i]
  }
  ### Rückgabewert ist das komponentenweise Minimum der beiden Vektoren pf1 und pf2
  return
}

fuzzy2_nand <- function (pf1,pf2) {
  return <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    return[i] <- 1-(pf1[i] * pf2[i])
  }
  ### Rückgabewert ist das komponentenweise Minimum der beiden Vektoren pf1 und pf2
  return
}

fuzzy2_and <- function (pf1,pf2) {
  return <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    return[i] <- pf1[i] * pf2[i]
  }
  ### Rückgabewert ist das komponentenweise Minimum der beiden Vektoren pf1 und pf2
  return
}

fuzzy2_or <- function (pf1,pf2) {
  return <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    return[i] <- 1 - (1-pf1[i]) * (1-pf2[i])
  }
  ### Rückgabewert ist das komponentenweise "Oder" der beiden Vektoren pf1 und pf2
  return
}

fuzzy2_implication <- function (pf1,pf2) {
  notf1 <-  fuzzy2_not(pf1)
  return <- fuzzy2_or(notf1,pf2)
  ### Rückgabewert ist das komponentenweise "Oder" der beiden Vektoren pf1 und pf2
  return
}

fuzzy2_implication <- function (pf1,pf2) {
  notf1 <-  fuzzy2_not(pf1)
  return <- fuzzy2_or(notf1,pf2)
  ### Rückgabewert ist das komponentenweise "Oder" der beiden Vektoren pf1 und pf2
  return
}


fuzzy_indicator <- function(pf1,pgrenze,pvalue0,pvalue1) {
  ret <- rep(pvalue0,length(pf1))
  for (i in 1:length(pf1)) {
    if (pf1[i] >= pgrenze) {
      ret[i] <- pvalue1 
    }
  }
  ### Rückgabewert ist ein Vektor, der komponentenweise als Indikator funktioniert
  ####  pvalue0 wenn die Grenze pgrenze NICHT überschritten wird
  ####  pvalue1 wenn die Grenze pgrenze überschritten wird
  ret
}

fuzzify_points <- function(pData,pColNames) {
  spaltenzahl <- length(pColNames)
  for (i in 1:spaltenzahl) {
      ### colname1 ist die Bezeichnung der Spalte
      ### mit den Rohdaten z.B. "aufgabe1"
      colname1 <- pColName[i]
      ### colvec1 wird als numerischer Vektor gelesen
      colvec1  <- as.numeric(pData[[colname1]])
      
      ### colname2 ist die Bezeichnung der Spalte
      ### mit den fuzzifzierten Rohdaten z.B. "fuzzy1"
      colname2 <- paste("fuzzy",i,sep="")
      ### Alter colname2-Definition
      # colname2 <- paste(colname1,"fuzzy",sep="")
      ### Fuzzyspalte berechnen
      colvec2 <- colvec1 * 1/colvec[1]
      ### pData um Fuzzyspalte erweitern
      pData[,colname2] <- colvec2
  }
  ### Rueckgabe der erweiterten Daten 
  return(pData)
}


koaktivitaetsmatrix <- function (pVecX,pVecY) {
  colx <- length(pVecX)
  mat <- matrix(pVecY , ncol=1) %*% matrix(pVecX,ncol=colx) 
  
  # return Kooktivitaetsmatrix mat als Rueckgabewert
  mat
}

### AUFRUF koaktivitaetmatrix()
# x <- c(1,0,1)
# y <- c(0,1) 
# koaktivitaetsmatrix(x,y)

### NOTENBERECHNUNG

notengrenzen_calc <- function(pPunkteMax, pBestGrenze,pBestIndex,pNotenSkala) {
     ## Parameter
     ## pPunkteMax: maximale Anzahl der Punkte in der Klausur 
     ## pBestGrenze: Bestehensgrenze
     ## pNotenSkala: Vektor mit Noten z.B. c("sehr gut","gut","befriedigend",ausreichend","mangelhaft","ungenügend")
     ## pBestIndex: Bestehensindex z.B. 4 -> verweist auf "ausreichend als bestanden.
     ## siehe 
     grenze = rep(0, length(pNotenSkala))
     imax <- length(pNotenSkala)
     schritt <- pBestGrenze/(imax-pBestIndex)
     ## Index für 6 bis zur 4 berechnen 
     gi <- imax
     ## Grenze unterhalb der Bestehensgrenze
     g <- 0
     while (gi > pBestIndex) {
        # Füge die Grenze in den Vektor ein
        g <- g + schritt
        grenze[gi] <- g
     
        # Vermindere den Index
        gi <- gi - 1 
     }
     ## Grenze oberhalb der Bestehensgrenze
     schritt <- (pPunkteMax - pBestGrenze)/(pBestIndex)
     ## Index für 4 und besser 
     while (gi > 0) {
        g <- g + schritt
        grenze[gi] <- g
        # Vermindere den Index
        gi <- gi - 1 
     }
     grenze <- round(grenze * 10)/10
     ## Rückgabewert Vektor "grenze"
     grenze
}

tabelle4grenzen <- function (pNotenSkala,pGrenzen) {
  ## Erzeuge einen Dataframe mit der Notenskala in der 1. Spalte 
  return4df <- data.frame(Notenskala=pNotenSkala)  
  max_i <- length(pNotenSkala) 

  Notengrenze <- rep("-",max_i)
  ## Berechne die Notengrenzen z.B. "22-28" 
  for (i in 1:(max_i-1)) {
    Notengrenze[i] <- paste(pGrenzen[i],"-",pGrenzen[i+1])
  }
  ## Setze die Punktegrenzen für schlechteste Note
  Notengrenze[max_i] <- paste(pGrenzen[max_i],"-",0)
  return4df$Notengrenze <-Notengrenze  
  
  ## return Dataframe mit Punktegrenzen
  return4df
}

note_zuordnen <- function(pPunkte,pGrenzen,pNotenskala) {
     ## Vektor mit der Länge der Punkte erstellen
     ## in dem Vektor wird die Note eingetragen
     anzahl_klausuren <- length(pPunkte)
     
     anzahl_noten <- length(pNotenskala)
     #print(paste("anzahl_noten =",anzahl_noten))
     
     note4punkte <- rep(0,anzahl_klausuren)
     for (i in 1:anzahl_klausuren) {
       ## Notenindex auf schlechteste Note setzen
       gi <- anzahl_noten
       ## Noten auf schlechtesten Index setzen
       note4punkte[i] <- pNotenskala[gi]
       ## so lange die Note verbessern bis Note jeweilge Notengrenze kleiner als Punkte
       while ((gi > 1) & (pPunkte[i] >= pGrenzen[gi]))  {
          ## Index für die Grenze auf bessere Note setzen
          gi <- gi - 1 
          ## Notenbezeichnung setzen
          if (gi >= 1) {
             note4punkte[i] <- pNotenskala[gi]
          }
       } 
     }
     ### Rückgabewert der Noten fuer alle Punkte
     note4punkte        
}
