# UdderHealthMonitor

UdderHealtMonitor is a package for [R](https://www.r-project.org/) that contains 
functions to analyse the udder health of dairy cow herds using somatic cell count 
data from Dairy Herd Improvement Tests. Its main function `monitor_SCCUdderHealth` 
allows even absolute R-newbies to produce pretty reports 


/home/veit/Dokumente/Zim-Notizbuecher/StatistikSoftware/eRkenntnisse/aRbeit/R-Ext.pdf
http://www.utf8-chartable.de/
https://cran.rstudio.com/web/packages/roxygen2/vignettes/formatting.html
http://kbroman.org/pkg_primer/pages/vignettes.html
http://kbroman.org/pkg_primer/pages/docs.html



This package contains functions to analyse the udder health of dairy
    cow herds using somatic cell count data from Dairy Herd Improvement Tests. With
    the main function 'monitor_SCCUdderHealth()' you can import the
    ADIS/ADED coded data from PCstart or PCBerater files
    (<http://www.vit.de/index.php?id=datenbereitstellung>) and obtain the most
    important indicators for the udder health as data frames in R - or
    automatically generate a nice report. Further information about monitoring
    the udder health is available from
    <http://www.milchqplus.de/kennzahlen_1.html> and V. Zoche, W. Heuwieser, and
    V. Krömker, "Risk-based monitoring of udder health. A review.",
    Tierärztliche Praxis. Ausgabe G, Grosstiere/Nutztiere, vol. 39, no. 2,
    pp. 88–94, 2011. (<http://www.schattauer.de/t3page/1214.html?manuscript=16040>)