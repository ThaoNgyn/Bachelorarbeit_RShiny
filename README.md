# Bachelorarbeit_RShiny

This R Shiny App was developed as part of a bachelor thesis at the University of Applied Sciences Berlin (HTW) 
and in cooperation with the Robert-Koch-Institute (RKI).\
It is a framework for visualizing quality control metrics to monitor performance and quality of Illumina sequencing systems.\
Only compatible with **SQL** database and **HiSeq** or **MiSeq systems**.\
Implemented metrics: **corrected intensities**, **quality metrics**.

# Usage
* requires R and RStudio https://rstudio-education.github.io/hopr/starting.html
* for usage with own database and data
  1. insert database name
  2. choose table by table index
  2. replace example_data with run_data
```
    db <- dbConnect(SQLite(), "your_database.db")
    run_data <- get_table(db, table_index)
    data <- run_data
```
 
