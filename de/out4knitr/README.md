## Output für KnitR
Diese Verzeichnis enthält Output-Dateien für [KnitR](https://de.wikiversity.org/wiki/KnitR). Im Kopf der R-Markdown-Datei kann man das Ausgabeformat festlegen.
* **(HTML)** `html_document` erzeugt eine HTML-Datei aus dem R-Markdown-Dokument.
*  **(LibreOffice)** `odt_document` erzeugt eine ODT-Datei aus dem R-Markdown-Dokument, die man mit [LibreOffice-Writer](https://www.libreoffice.org/download/download-libreoffice/)   weiter bearbeiten kann.
*  **(Latex)** `latex_document` erzeugt eine LaTeX-Datei aus dem R-Markdown-Dokument.
*  **(PDF)** `pdf_document` erzeugt eine PDF-Datei direkt aus dem R-Markdown-Dokument. Dabei sollte LaTeX auf Ihrem Rechner installiert sein:
   - `Linux` - ist in Ihrem Packagemanager auf Linux enthalten (starten Sie dazu Ihren Paketmanager auf Ihrem Linux
   - `Windows` - es gibt z.B. die Latex-Installation von [MikTex](https://miktex.org/) und [Tex Live](https://www.tug.org/texlive/)
   - `MacOSX` - es gibt [MacTeX](https://www.tug.org/mactex/) als Installation für den Mac.
   - Alternativ kann man natürlich auch aus der `odt_document`-Ausgabe ein PDF-Export in LibreOffice durchführen. Das macht insbesondere dann Sinn, wenn ohnehin vorher noch in dem Dokument editiert wird.
* Im Kopf der R-Markdown-Datei kann man das Ausgabeformat festlegen.
```ỳaml
---
title: "ODT Arbeitsblatt - Learning Analytics "
author: "Vorname Nachname"
date: "`r Sys.Date()`"
output: odt_document
---
```

### PDF-Dateien für die Demodateien
Wenn man eine Datei mit KnitR erzeugt, kann man bei installierten LaTeX auch PDF-Dateien erzeugen. Damit Sie in der [Wikiversity-Lerneinheit](https://de.wikiversity.org/wiki/KnitR) auch die generierten Dateien ansehen können, finden Sie in diesem Ordner auch die erzeugten PDF.
