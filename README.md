# sampling
RATSTATS replications

Background: 
This folder consists of a series of functions meant to replicate the sampling features provided in the RATSTATS program. RATSTATS is a statistical software developed by the Health and Human Services Office of Inspector General in the 1970s. More information about the software can be found here: https://oig.hhs.gov/compliance/rat-stats/index.asp#:~:text=RAT%2DSTATS%20is%20a%20free,OIG's%20Office%20of%20Audit%20Services.

Files:
1. simplerat.r - This file creates a function that replicates the simple random sample module in the RATSTATS software. Due to an undocument correction in the RATSTATS software, this function has trouble replicating RATSTATS sample sizes for small populations.
2. ratstrat.r - This file creates a function that replicates the stratified sample module in the RATSTATS software. The function is designed to take in text files or R objects.
2. ratstrat shiny.r - This file is currently in progress to develop a user friendly Shiny app to help with the stratified sampling function created in the ratstrat.r script.

