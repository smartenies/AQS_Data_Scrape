# AQS_Data_Scrape
Scraping and kriging monitoring data from the US EPA's Air Quality System

### For Wande (11/15/18)

- I've generated estimates for ozone and PM2.5 at the grid points using both inverse-distance
weighting and ordinary kriging. Sometimes the model has a difficult time fitting
a semivarigram to the data, and kriged estimates aren't available.
    - All estiamtes for ozone (44201) are in ppb
    - All estimates for PM2.5 (88101) are in ug/m3

- You'll have to take a look at the distribution of each daily estimate in order
to see if you want to use the kriged data. IDW estimates may be better given some of
the limitations (time resolution, spatial coverage of monitors)

- Due to storage issues on my U:/ drive, the output files are currently here:
https://drive.google.com/open?id=1-omBeF4YsEzJOFT12N9fvQXp2UqfnUIR
    - "8hour" is the daily 8hour max concentration (not necessarily contemporaneous)
    - "Daily"" is the daily mean concentraton

- Once we have the R: drive up and running (very soon!) I'll be updating this code

- One major limitation is that the grid extends father east than where we have monitors.
This will generate errors in the predictions

- An alternative approach may be to average these concentrations over weeks or 
months to get better estimates of concentrations, but that will likely depend on your
analysis. It's better to average at the monitors and re-krige. If this is something you
want, let me know!

### Notes on the kriging analysis:
- See Li and Heap for a nice explanation of kriging diagnostics
https://pdfs.semanticscholar.org/686c/29a81eab59d7f6b7e2c4b060b1184323a122.pdf

- If the monitoring data weren't normal, the code attempts to use log-transformed
estimates. We need to apply a correction to get estimates back in the original
scale (See Oliver and Webster 2007, PAGE 185)
https://books.google.com/books?hl=en&lr=&id=WBwSyvIvNY8C&oi=fnd&pg=PR5&ots=CCLmSNqK1c&sig=lFZanxv2eVSKec6nPdESzuIFrA4#v=onepage&q&f=false
- Note: A back-transformed variance estimate for OK cannot be calculated because the mean is not known (Oliver and Webster 2007, page 185)

