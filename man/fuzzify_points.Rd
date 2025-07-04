% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knitr4education.R
\name{fuzzify_points}
\alias{fuzzify_points}
\title{Funktion fuzzify_points(pData,pColNames)}
\usage{
fuzzify_points(pData, pColNames)
}
\arguments{
\item{pData}{Ein Dataframe}

\item{pColNames}{Vektor mit Namen/Bezeichnungen der fuzzifizierten Spalten}
}
\value{
Ein Dataframe mit den fuzzifizierten Spalten
}
\description{
Fuzzifziert Datenspalten in dem Dataframe pData.
Die Spaltenbezeichnungen werden in dem Vektor pColNames angegeben.
Die Vektoren in den Spalten erwarten in dem ersten Eintrag die maximal
möglichen Punkte in einer Aufgabe
}
\examples{
## Spaltenbezeichnung des Dataframes, die als Spalten fuzzifiziert werden sollen.
name <- c("MAXPKT","Anna","Bert","Charlie")
aufgabe1 <- c(10,1,4,9)
aufgabe2 <- c(20,10,5,15)
studentdata <- data.frame(name,aufgabe1,aufgabe2)
cols4fuzzy <- c("aufgabe1", "aufgabe2")
df <- fuzzify_points(studentdata, cols4fuzzy)
### Dataframe df enthält nun die Spalten "aufgabe1fuzzy" "aufgabe2fuzzy"
### aufgabe1fuzzy <- c(1.0, 0.1, 0.4, 0.9)
### aufgabe2fuzzy <- c(1.0, 0.5, 0.25, 0.75)

}
