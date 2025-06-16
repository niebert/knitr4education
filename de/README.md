## Erläuterung zu den Demo-Dateien
Die Demodateien wurden für Wikiversity-Lernumgebungen erzeugt, die Studierende dabei unterstützen sollen mit Daten und [dynamischer Textgenerierung](https://de.wikiversity.org/wiki/KnitR) in [R]([https://GNU_R](https://de.wikibooks.org/wiki/GNU_R)) (Python, Octave, ...) zu arbeiten. KnitR ist dabei das R-Paket, das die Realisation von dynamischer Dokumentengenerierung ermöglicht.
* **Demo 1:** Zeigt die prinzipielle Verwendung von Codechunks, mathematischen Formeln und der Verwendnug von Variableninhalten im Text.

### Verzeichnisse 
* **(Daten - `/data`)** Das Verzeichnis [`/data`](data) erhält Demodaten, die man in der Lernresource [KnitR in Wikiversity](https://de.wikiversity.org/wiki/KnitR). Weitere Daten zu Testzwecken findet man in dem Repository https://github.com/guru99-edu/R-Programming von [guru99-edu](https://github.com/guru99-edu).
* **(Bilder - `/img`)** Das Verzeichnis [`/img`](img) erhält Demobilder, die man in der Lernresource [KnitR in Wikiversity](https://de.wikiversity.org/wiki/KnitR) für die Einbindung der Bilder in [KnitR-Dokumente](https://de.wikiversity.org/wiki/KnitR) verwendet.
* **(Funktionsammlung - Library `/lib`)** In der Programmierung ist es üblich, oft verwendete Funktionen für die erneute Nutzung in Bibliotheken in der zusammenzufassen. Diese findet man in dem Unterverzeichnis [`/lib`](lib) Diese Bibliotheken im Kontext der [KnitR-Lernumgebung](https://de.wikiversity.org/wiki/KnitR) erstellt wurden, werden in dem Unterverzeichnis `lib/` abgespeichert.
* **(Ausgaben Demo - `/out4knitr`)** Das Verzeichnis [`/out4knitr`](out4knitr) erhält Ausgabedateien von [KnitR](https://de.wikiversity.org/wiki/KnitR), die man sich hier ansehen kann. 

### Datensätze 
Beispieldatensätze sind erstellt worden, um mit diesen die Funktionsweise von bestimmten Funktionen in R zu erlernen.
Diese finden Sie in dem Unterverzeichnis [`data/`](data) oder bei [`R-Programming`](https://github.com/guru99-edu/R-Programming).

### Funktionssammlungen / Bibliotheken
Bibliotheken können in R bzw. in KnitR-Code-Chunks mit `source("mylib.R")` eingebunden werden. Durch die Verwendung von Bibliotheken kann man Funktionen für mehrere R-Markdowndateien mit [KnitR](https://de.wikiversity.org/wiki/KnitR) verfügbar machen. 
* `[knitr4education.R](lib/knitr4education.R)` ist ein Bibliothek, die die wesentlichen im dem Kurs behandelten Funktionen enthält. Einbindung erfolgt mit `source("lib/knitr4education.R")`  
