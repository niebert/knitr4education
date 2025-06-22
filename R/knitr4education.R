
#' Funktion komma2punkt(pWert)
#'
#' Diese Funktion "komma2punkt()" ersetzt ein deutsches Komma ","
#' in einer reellen Zahl (z.B. "3,5") durch einen Dezimalpunkt,
#' damit man mit den Werten in R rechnen kann (z.B. mit 3.5 ).
#'
#' @param pWert ist als Wert eine Zeichenkette, Vektor oder Spalte in einem Dataframe
#'
#' @return Die reelle Zahl die ggf. Komma durch eine R-lesbaren Dezimalpunkt
#'
#' @examples
#' Direkt eine Zeichenkette in die entsprechende Dezimalzahl umwandel
#'   komma2punkt("3,7")
#' gibt dann die reelle Zahl 3.7 zurück
#'
#' Variable definieren und umwandeln
#'   wert <- "123,71"
#'   komma2punkt(wert)
#' gibt dann die reelle Zahl 123.71 zurück

#' Vektor definieren und umwandeln
#'   vec <- c("123,71","3,7",1.2, 51)
#'   komma2punkt(wert)
#' gibt dann den folgenden Vektor von reellen Zahlen c(123.71 , 3.7 , 1.2 , 51)  zurück

komma2punkt <- function(pWert) {
  pWert <- as.character(pWert)
  pWert <- gsub(",", ".", pWert)
  pWert <- as.numeric(pWert)
  return(pWert)
}


#' Funktion load_csv(pFilename)
#'
#' @param pFilename Der Dateipfad der CSV-Datei
#'
#' @return Ein Dataframe mit den Daten aus der CSV-Datei
#'
#' @examples
#' df <- load_csv("meine_datei.csv")
#' studentdata <- load_csv("data/klausur.csv")
#'
load_csv <- function(pFilename) {
  retData <- read.csv(pFilename, header=TRUE, stringsAsFactors=FALSE)
  ### Rückgabe retData ist ein Dataframe (Tabelle)
  return(retData)
}

#' Funktion save_csv(pFilename,pData)
#'
#' @param pFilename Der Dateipfad der CSV-Datei
#' @param pData der Dataframe mit Spaltenbezeichnung colnames
#'
#' @return NULL
#'
#' @examples
#' save_csv("meine_datei.csv", meine_data)
save_csv <- function(pFilename, pData) {
  write.csv(pData, pFilename, row.names = FALSE)
}


#' Funktion load_inout_csv()
#'
#' Diese Ladefunktion wird für die Auswahl von bestimmten Ein- und Ausgabespalten verwendet,
#' wobei nur die genannten Eingabe- und Ausgabespalten aus CSV-Datei extrahiert werden
#' Das die Datenspalten für numerische Berechnungen verwendet werden, werden
#' die Datenspalten ggf. in numerische Werte konvertiert.
#' ferner wird bei Rohdaten mit deutschem Komma in der Dezimalzahl "," durch "." ersetzt.
#'
#' @param pFilename  Dateipfad der CSV-Datei
#' @param pInCols  Namen der Eingabespalten als Vektor mit Spaltenbezeichnungen
#' @param pOutCols Namen der Ausgabespalten als Vektor mit Spaltenbezeichnungen
#'
#' @return Ein Dataframe mit den ausgewählten Eingabespalten und Ausgabespalten
#'
#' @examples
#' incols <- c("Spalte1", "Spalte2","Spalte3")
#' outcols <- c("Spalte4", "Spalte5")
#'
#' Die obigen Spaltenbezeichnung sollten als Spalten in der CSV-Datei existieren.
#' mydata <- load_inout_csv("klausur.csv", incols , outcols )
#'

load_inout_csv <- function(pFilename,pInCols,pOutCols) {
  data <- read.csv(pFilename, header=TRUE, stringsAsFactors=FALSE)
  ### Spalten für Eingabe bzw. Ausgabe in Zeichen konvertieren,
  ### ggf. deutsches Komma in Dezimalzahlen durch Punkt
  for (i in 1:length(pInCols)) {
    col4str <- unlist(data[pInCols[i]])
    data[pInCols[i]] <- komma2punkt(col4str)
  }
  for (i in 1:length(pOutCols)) {
    col4str <- unlist(data[pOutCols[i]])
    data[pOutCols[i]] <- komma2punkt(col4str)
  }
  ### Spalten mit den Bezeichnung pColNames extrahieren
  data4cols <-  list(
    xin  = data[pInCols],
    yout = data[pOutCols]
  )
  ### Rueckgabe der extrahieren numerischen Datenspalten
  return(data4cols)
}


#' Funktion cols4sum
#'
#' @param pData Ein Dataframe
#' @param pColname4Sum Der Name der neuen Spalte
#' @param pInCols Die Namen der Spalten, deren Summe berechnet werden soll
#'
#' @return Ein Dataframe mit der Summe der Spalten
#'
#' @examples
#'
#' datensumme <- cols4sum(studentdata, "summe", c("aufgabe1", "aufgabe2", "aufgabe2"))
#'
#' Spaltenbezeichnung in einem Vektor
#' colnames4data <-  c("aufgabe1", "aufgabe2", "aufgabe2")
#' datensumme <- cols4sum(studentdata, "summe", colnames4data)
#'
cols4sum <- function(pData, pColname4Sum, pInCols) {
  # Überprüfe, ob pData ein Dataframe ist
  if (!is.data.frame(pData)) {
    stop("pData muss ein Dataframe sein")
  }

  # Überprüfe, ob pInCols ein Vektor von Spaltennamen ist
  if (!is.character(pInCols) || length(pInCols) == 0) {
    stop("pInCols muss ein Vektor von Spaltennamen sein")
  }

  # Überprüfe, ob pColname4Sum ein eindeutiger Spaltenname ist
  if (pColname4Sum %in% names(pData)) {
    stop("pColname4Sum ist bereits ein Spaltenname")
  }

  # Überprüfe, ob alle Spaltennamen in pInCols in pData existieren
  if (any(!pInCols %in% names(pData))) {
    stop("Einige Spaltennamen in pInCols existieren nicht in pData")
  }

  # Überprüfe, ob alle Spaltennamen in pInCols numerische Variablen sind
  if (any(!sapply(pData[, pInCols], is.numeric))) {
    stop("Einige Spaltennamen in pInCols sind keine numerischen Variablen")
  }

  # Erstelle eine neue Spalte in pData mit dem Namen in pColname4Sum
  pData[[pColname4Sum]] <- rowSums(pData[, pInCols])

  # Rückgabe des Dataframes
  return(pData)
}

#' Funktion copy4vec(pVec)
#'
#' kopiert mit einer For-Schleife die Werte aus dem gegebenen Vektor pVec
#' in einen neuen Vektor und gibt diesen zurueck,
#'
#' @param pVec Ein Vektor
#'
#' @return Der kopierte Vektor mit gleichem Inhalten in den Komponenten
#'
#' @examples
#' copy4vec(c(1, 2, 3))
#'
copy4vec <- function (pVec) {
  ret <- rep(0,length(pVec))
  for (i in length(pVec)) {
    ret[i] <- pVec[i]
  }
  ## return cloned vector
  return(vec)
}

#' Funktion echo4vec(pVarname,pVec)
#'
#' @param pVarname Der Name der Variablen als Zeichenkette
#' @param pVec Ein Vektor
#'
#' @return Zeichenkette, die mit Print ausgegeben wurde
#'
echo4vec <- function (pVarname,pVec) {
  output <- paste(pVarname,"=(",paste(pVec,collapse=","),")",sep="")
  print(output)
  return(output)
}

#' Funktion hello(name)
#'
#' @param pName Name der Person, die den Text erhalten soll
#'
#' @return NULL
#'
#' @examples
#' hello("Hans")
hello <- function(pName) {
  print(paste("Hallo ",pName,", alles Gute bei Bearbeitung der Aufgaben.",sep=""))
}

#' Funktion loesung_ausgeben(pHilfe,pName,pSolution)
#'
#' Die Funktion erhält eine Hilfe als Zeichenkette. Die Zeichenkette beinhaltet
#' Textstellen mit "NAME" und "SOLUTION".
#' Die Textstelle "NAME" wird durch den Inhalt des Parameters "pName" ersetzt.
#' Die Textstelle "SOLUTION" wird durch den Inhalt des Parameters "pSolution" ersetzt.
#'
#' @param hilfe String/Zeichenkette mit einer Hilfe
#' @param name Der Name
#' @param solution Die Lösung
#'
#' @return Die Hilfe mit ersetztem Name und ersetzter Lösung
#'
#' @examples
#' hilfe <- "Hinweis für NAME : Die Lösung ist SOLUTION"
#' loesung_ausgeben(hilfe, "Hans", "zerlege 8 in 3+5 und rechne 7+8=(7+3)+5 ")
#'
loesung_ausgeben <- function(hilfe,name,solution) {
  hilfe_ausgabe <-  str_replace(hilfetext,"SOLUTION",solution)
  hilfe_ausgabe <-  str_replace(hilfe_ausgabe,"NAME",name)
  ## Rückgabe der ersetzen Hilfe
  return(hilfe_ausgabe)
}

#' Funktion fuzzy_nand(pf1,pf2)
#'
#' berechnet die Negation des fuzzylogischen UND ("not and" bzw. "nand")
#' von zwei Vektoren pf1 und pf2 von Fuzzywerte. Die Komponenten der Vektoren
#' sind reelle Zahlen zwischen 0 und 1.
#'
#' @param pf1 Zahl oder Vektor mit Fuzzywerten
#' @param pf2 Zahl oder Vektor mit Fuzzywerten
#'
#' @return Ein Vektor mit Fuzzywerten
#'
fuzzy_nand <- function (pf1,pf2) {
  ret <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    ret[i] <- 1 - min(pf1[i],pf2[2])
  }
  ### Rückgabewert ist das komponentenweise Minimum der beiden Vektoren pf1 und pf2
  ret
}

#' Funktion fuzzy_and(pf1,pf2)
#'
#' berechnet das fuzzylogische UND  mit dem Minimum von pf1 und pf2
#' von zwei Vektoren pf1 und pf2 von Fuzzywerte. Die Komponenten der Vektoren
#' sind reelle Zahlen zwischen 0 und 1.
#'
#' @param pf1 Zahl oder Vektor mit Fuzzywerten
#' @param pf2 Zahl oder Vektor mit Fuzzywerten
#'
#' @return Ein Vektor mit Fuzzywerten
#'
fuzzy_and <- function (pf1,pf2) {
  ret <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    ret[i] <- min(pf1[i],pf2[2])
  }
  ### Rückgabewert ist das komponentenweise Minimum der beiden Vektoren pf1 und pf2
  ret
}

#' Funktion fuzzy_or(pf1,pf2)
#'
#' berechnet das fuzzylogische ODER mit dem Maximum von pf1 und pf2
#' von zwei Vektoren pf1 und pf2 von Fuzzywerte. Die Komponenten der Vektoren
#' sind reelle Zahlen zwischen 0 und 1.
#'
#' @param pf1 Zahl oder Vektor mit Fuzzywerten
#' @param pf2 Zahl oder Vektor mit Fuzzywerten
#'
#' @return Zahl oder Vektor mit Fuzzywerten
#'
fuzzy_or <- function (pf1,pf2) {
  ret <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    ret[i] <- max(pf1[i],pf2[i])
  }
  ### Rückgabewert ist das komponentenweise Maximum der beiden Vektoren pf1 und pf2
  return(ret)
}

#' Funktion fuzzy_not(pf1)
#'
#' berechnet das fuzzylogische NICHT mit 1-pf1
#' von einem Vektoren pf1 und pf2 von Fuzzywerte. Die Komponenten der Vektoren
#' sind reelle Zahlen zwischen 0 und 1.
#'
#' @param pf1 Zahl oder Vektor mit Fuzzywerten
#'
#' @return Zahl oder Vektor mit Fuzzywerten
#'
fuzzy_not <- function (pf1) {
  ret <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    ret[i] <- 1 - pf1[i]
  }
  ### Rückgabewert ist das komponentenweise Minimum der beiden Vektoren pf1 und pf2
  return(ret)
}


#' Funktion fuzzy_implication(pf1,pf2)
#' berechnet das fuzzylogische IMPLICATION mit der NICHT pf1 ODER pf2
#' von zwei Vektoren pf1 und pf2 von Fuzzywerte. Die Komponenten der Vektoren
#' sind reelle Zahlen zwischen 0 und 1. Analog zur Logik p->q <=> nicht p oder q
#'
#' @param pf1 Zahl oder Vektor mit Fuzzywerten
#' @param pf2 Zahl oder Vektor mit Fuzzywerten
#'
#' @return Zahl oder Vektor mit Fuzzywerten
#'
fuzzy_implication <- function (pf1,pf2) {
  notf1 <-  fuzzy1_not(pf1)
  ret <- fuzzy1_or(notf1,pf2)
  ### Rückgabewert ist das komponentenweise Implikation der beiden Vektoren pf1 und pf2
  return(ret)
}

#' Funktion fuzzy2_nand(pf1,pf2)
#'
#' berechnet die Negation des fuzzylogischen UND ("not and" bzw. "nand")
#' von zwei Vektoren pf1 und pf2 von Fuzzywerte. Die Komponenten der Vektoren
#' sind reelle Zahlen zwischen 0 und 1.
#' Grundlage für die fuzzy2_...() ist hier multiplikative Verknüpfung für UND
#' Dies ist eine strenges Fuzzy-UND im Vergleich zu der fuzzy_und() als  Minimum
#'
#' @param pf1 Zahl oder Vektor mit Fuzzywerten
#' @param pf2 Zahl oder Vektor mit Fuzzywerten
#'
#' @return Ein Vektor mit Fuzzywerten
#'
fuzzy2_nand <- function (pf1,pf2) {
  ret <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    ret[i] <- 1-(pf1[i] * pf2[i])
  }
  ### Rückgabewert ist die Negation der multiplikative UND-Verknüpfung "NAND" der beiden Vektoren pf1 und pf2
  return(ret)
}

#' Funktion fuzzy2_and(pf1,pf2)
#'
#' berechnet das fuzzylogischen UND
#' von zwei Vektoren pf1 und pf2 von Fuzzywerte. Die Komponenten der Vektoren
#' sind reelle Zahlen zwischen 0 und 1.
#' Grundlage für die fuzzy2_...() ist hier multiplikative Verknüpfung für UND
#' Dies ist eine strenges Fuzzy-UND im Vergleich zu der fuzzy_and() als  Minimum
#'
#' @param pf1 Zahl oder Vektor mit Fuzzywerten
#' @param pf2 Zahl oder Vektor mit Fuzzywerten
#'
#' @return Ein Vektor mit Fuzzywerten
#'
fuzzy2_and <- function (pf1,pf2) {
  ret <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    ret[i] <- pf1[i] * pf2[i]
  }
  ### Rückgabewert ist das komponentenweise Minimum der beiden Vektoren pf1 und pf2
  return(ret)
}

#' Funktion fuzzy2_or(pf1,pf2)
#' berechnet das fuzzylogische ODER mit dem Maximum von pf1 und pf2
#' von zwei Vektoren pf1 und pf2 von Fuzzywerte. Die Komponenten der Vektoren
#' sind reelle Zahlen zwischen 0 und 1.
#' Grundlage für die fuzzy2_or() ist hier Negation der multiplikativen Verknüpfung für UND
#' Dies liefert ein schwächeres Fuzzy-ODER im Vergleich zu der fuzzy_or() als Maximum
#'
#' @param pf1 Zahl oder Vektor mit Fuzzywerten
#' @param pf2 Zahl oder Vektor mit Fuzzywerten
#'
#' @return Zahl oder Vektor mit Fuzzywerten
#'
fuzzy2_or <- function (pf1,pf2) {
  ret <- rep(0,length(pf1))
  for (i in 1:length(pf1)) {
    ret[i] <- 1 - (1-pf1[i]) * (1-pf2[i])
  }
  ### Rückgabewert ist das komponentenweise "Oder" der beiden Vektoren pf1 und pf2
  return(ret)
}

#' Funktion fuzzy2_implication(pf1,pf2)
#'
#' berechnet das fuzzylogische IMPLICATION mit der NICHT pf1 ODER pf2
#' von zwei Vektoren pf1 und pf2 von Fuzzywerte. Die Komponenten der Vektoren
#' sind reelle Zahlen zwischen 0 und 1. Analog zur Logik p->q <=> nicht p oder q
#' Grundlage für die fuzzy2_impcation() ist hier Negation der multiplikativen Verknüpfung für UND
#' Dies liefert eine schwächere Fuzzy-IMPLIKATION im Vergleich zu der fuzzy_implication() als Maximum
#' bzw. dem verwendeten fuzzy_or()
#'
#' @param pf1 Zahl oder Vektor mit Fuzzywerten
#' @param pf2 Zahl oder Vektor mit Fuzzywerten
#'
#' @return Zahl oder Vektor mit Fuzzywerten
#'
fuzzy2_implication <- function (pf1,pf2) {
  notf1 <-  fuzzy2_not(pf1)
  ret <- fuzzy2_or(notf1,pf2)
  ### Rückgabewert ist das komponentenweise "Oder" der beiden Vektoren pf1 und pf2
  return(ret)
}


#' Funktion fuzzy_indicator(pf1,pgrenze,pvalue0,pvalue1)
#'
#' @param pf1 Vektor mit Fuzzywerten zwischen 0 und 1
#' @param pgrenze die Grenze, ab der der Fuzzy-Indikator von 0 auf 1 gesetzt wird.
#' @param pvalue0 der Ausgabewert, der bei dem Indikator 0 ausgegeben wird,
#' @param pvalue1 der Ausgabewert, der bei dem Indikator 1 ausgegeben wird
#'
#' @return Ein Vektor
#'
#' @examples
#' Vektor mit Fuzzywerten
#' fvec <- c(0.1, 0.9, 0.5, 0.7)
#'
#' fuzzy_indicator(fvec, 0.5, 0, 1)
#' ### Ausgabevektor (0,1,1,1)
#'
#' fuzzy_indicator(fvec, 0.8, 0, 1)
#' ### Ausgabevektor (0,1,0,0)
#'
#' fuzzy_indicator(fvec, 0.5, "schlecht", "gut")
#' ### Ausgabevektor ("schlecht","gut","gut","gut")
#'
#' fuzzy_indicator(fvec, 0.8, "schlecht", "gut")
#' ### Ausgabevektor ("schlecht","gut","schlecht","schlecht")
#'
fuzzy_indicator <- function(pf1,pgrenze,pvalue0=0,pvalue1=1) {
  ret <- rep(pvalue0,length(pf1))
  for (i in 1:length(pf1)) {
    if (pf1[i] >= pgrenze) {
      ret[i] <- pvalue1
    }
  }
  ### Rückgabewert ist ein Vektor, der komponentenweise als Indikator funktioniert
  ####  pvalue0 wenn die Grenze pgrenze NICHT überschritten wird
  ####  pvalue1 wenn die Grenze pgrenze überschritten wird
  return(ret)
}

#' Funktion fuzzify_points(pData,pColNames)
#'
#' Fuzzifziert Datenspalten in dem Dataframe pData.
#' Die Spaltenbezeichnungen werden in dem Vektor pColNames angegeben.
#' Die Vektoren in den Spalten erwarten in dem ersten Eintrag die maximal
#' möglichen Punkte in einer Aufgabe
#'
#' @param pData Ein Dataframe
#' @param pColNames Vektor mit Namen/Bezeichnungen der fuzzifizierten Spalten
#'
#' @return Ein Dataframe mit den fuzzifizierten Spalten
#'
#' @examples
#' ## Spaltenbezeichnung des Dataframes, die als Spalten fuzzifiziert werden sollen.
#' name <- c("MAXPKT","Anna","Bert","Charlie")
#' aufgabe1 <- c(10,1,4,9)
#' aufgabe2 <- c(20,10,5,15)
#' studentdata <- data.frame(name,aufgabe1,aufgabe2)
#' cols4fuzzy <- c("aufgabe1", "aufgabe2")
#' df <- fuzzify_points(studentdata, cols4fuzzy)
#' ### Dataframe df enthält nun die Spalten "aufgabe1fuzzy" "aufgabe2fuzzy"
#' ### aufgabe1fuzzy <- c(1.0, 0.1, 0.4, 0.9)
#' ### aufgabe2fuzzy <- c(1.0, 0.5, 0.25, 0.75)
#'
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

#' Funktion koaktivitaetsmatrix
#'

#' @param pVecX Ein Vektor
#' @param pVecY Ein Vektor
#'
#' @return Ein Matrix
#'
#' @examples
#' koaktivitaetsmatrix(c(1, 2, 3), c(4, 5, 6))
#'koaktivitaetmatrix()
#' x <- c(1,0,1)
#' y <- c(0,1)
#' koaktivitaetsmatrix(x,y)
#' ## Matrix hat die Gestalt
#' ##       [,1] [,2] [,3]
#' ## [1,]    0    0    0
#' ## [2,]    1    0    1
#'
koaktivitaetsmatrix <- function (pVecX,pVecY) {
  colx <- length(pVecX)
  mat <- matrix(pVecY , ncol=1) %*% matrix(pVecX,ncol=colx)

  # return Kooktivitaetsmatrix mat als Rueckgabewert
  mat
}

#' Funktion notengrenzen_calc
#'
#' Mit pPunkteMax als Parameter wird die maximale Anzahl der Punkte in der Klausur festgelegt.
#' Jede Klausur hat eine Bestehensgrenze, mit der man gerade noch bestanden hat. Diese wird durch
#' den Parameter pBestGrenze angegeben. Gleichzeitig bekommt die Funktion auch eine Notenskala, die als
#' Grundlage für die später Bewertung bzw. Notenzuordnung verwendet wird.
#' Ein Beispiel für eine Notenskala ist z.B. der Vektor mit Noten
#'  c("sehr gut","gut","befriedigend",ausreichend","mangelhaft","ungenügend")
#' Dazu wird ein Bestehensindex mit pBestIndex als Parameter übergeben. Der Bestehensindex ist im
#' obigen Beispiel  4 und verweist damit auf die Note "ausreichend" mit der man gerade noch bestanden hat.
#' Notenskalen können mit dem zugehörigen Bestehensindex auf wie folgt aussehen.
#' notenskala <- c("1+","1","1-","2+","2","2-","3+","3","3-","4+","4","4-","5+","5","5-","6")
#' bestindex <- 11 verweist als Index für Note "4" und mit "4-" hat man nicht mehr bestanden.
#' notenskala <- c("1+","1","1-","2+","2","2-","3+","3","3-","4+","4","4-","5+","5","5-","6")
#' bestindex <- 12 verweist als Index für Note "4-"  mit der nun noch bestanden hat.
#' Notenskalen können auch Zahlenwerte enthalten. Die kann von Vorteil sein, wenn man später zugeordnet Noten
#' auf ganze Noten runden möchte.
#' notenskala <- c(0.7,1.0, 1.3, 1.7, 2.0, 2.3, 3.3, 3.7, 4.0, 4.3, 4.7, 5.0, 5.3, 6.0)
#' Die Funktion notengrenzen_calc() wird zusammen mit der Funktion noten_zuordnen() verwendet, die die Notengrenze
#' als Eingabe benötigt, um die Noten zuordnen zu können.

#' @param pPunkteMax maximale Anzahl der Punkte in der Klausur
#' @param pBestGrenze Bestehensgrenze, ab der die Klausur bestanden ist
#' @param pBestIndex  Bestehensindex, ist der Index in der Notenskala, bei dem die Klausur gerade noch bestanden ist
#' @param pNotenSkala Notenskala als Vektor der bestandenen Werte
#'
#' @return ein Vektor, der jeweils die Notengrenzen für die jeweiligen Noten angibt (von guten zu schlechteren Noten)
#'
#' @examples
#' notenskala <- c("sehr gut", "gut", "befriedigend", "ausreichend", "mangelhaft", "ungenügend")
#' notengrenzen_calc(100, 50, 4, notenskala)
#' ## Ausgabe: [1] 100.0  87.5  75.0  62.5  50.0  25.0
#' ## Bestanden haette man mit "ausreichend". Der Bestehensindex ist also in diesem Fall 4
#'
notengrenzen_calc <- function(pPunkteMax, pBestGrenze,pBestIndex,pNotenSkala) {
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


#' Funktion find_min4error(pError, pGrad, pa, px_D, py_D, alpha=1, evalcount=100)
#'
#' Die Funktion ist eine Hilfsfunktion für die Minimierung einer Fehlerfunktion. Die Funktion gibt
#' zwei Zahlenwerte in einem Vektor zurück, wobei
#' @param pError Die Fehlerfunktion
#' @param pGrad Der (normierte) Gradient der Fehlerfunktion
#' @param pa Startwert der Fehlerfunktion, die negativer Gradientenrichtung verändert werden sol
#' @param px_D Ein Vektor mit den x-Werten als gegebene Eingabedaten einer Funktion
#' @param py_D Ein Vektor mit den y-Werten als gegebene Eingabedaten einer Funktion
#' @param alpha Lernschrittweite in Gradientenrichtung. Gib an, wie stark der Vektor a in Gradientenrichtung verändert werden kann.
#' @param evalcount gibt die Anzahl der Evaluierungen in Gradientenrichtung an, um den minimalen Wert der Fehlerfunktion zu finden.
#'
#' @return Vektor mit zwei reellen Zahlen, die 1. Zahl gibt die skalare Veränderung in Gradientenrichtung an. Die 2. Zahl der neue minimale Fehler
#'
#' @examples
#' a = c(1, 2, 3) ein Vektor oder A eine Matrix
#' # x_D ist in diesem Fall eine Dataframe mit 3 Spalten von Eingabedaten
#' # y_D ist in diesem Fall eine Dataframe mit einer Spalte von Ausgabedaten
#' # Schrittweite ist 1 und es werden 100 Evaluation in Gradientenrichtung für die Minimierung verwendet
#' # evalcount höher zu setzen verbessert die Optimierung, erhöht aber auch die Rechnenzeit
#' # da pro Optimierungsschritt mehr Evalulationen durchgeführt werden.
#' find_min4error(E_LR, Grad_LR, A , x_D, y_D, 1, 100)
find_min4error <- function (pError, pGrad, pa, px_D, py_D, alpha=1, evalcount=100) {
  ## Parameter
  ## pError: Fehlerfunktion
  ## pGrad:  Gradient der Fehlerfunktion
  ## px_D: x-Vektoren der Daten für den Definitionsbereich
  ## py_D: y-Sollwerte für die Fehlerfunktion
  ret <- rep(0,2) ### ret <- c(0,0);
  ## erste Komponente von ret ist der minimale lambda-Wert
  ## zweite Komponente von ret ist der minimale Fehler
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
  ret <- c(scalar4min,error4min)
  ## Rückgabewert des Skalars für den Gradienten und dem minimalen Fehler
  ret
}
