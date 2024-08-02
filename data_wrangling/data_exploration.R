# preliminary data exploration for bobcat gps data
# margaret mercer
# july 30, 2024
# I put this together by looking at the code from Noonan 2019 and Noonan 2021 simultaneously
    # and using what applied to me
    # see: https://static-content.springer.com/esm/art%3A10.1186%2Fs40462-019-0177-1/MediaObjects/40462_2019_177_MOESM2_ESM.pdf
    # and: https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf

# load packages
library (ctmm)

# loading in gps data as csv
data <- read.csv("data/bobcat_locs_all.csv")


# pulling out one bobcat. a beautiful bobcat. a talented bobcat. a brilliant bobcat.
# a bobcat
# named
# Margaret 
# :)
margaret_gps <- data[data$individual.local.identifier == "BC #12 Margaret", ]
summary(margaret_gps)

# making margaret a telemetry object so ctmm recognizes it
margaret <- as.telemetry(margaret_gps)
summary(margaret)

# plotting!
# plot(margaret ,
      # error = 2 ,
      # level.UD = 0.50) # this doesn't work great because we don't know the error yet. thats ok.

# ID outliers
outliers <- outlie(margaret)
# plot(outliers)

# get rid of outliers in margaret
outlier_t <- outliers$t[outliers$speed >= .40]
margaret <- margaret[!margaret$t %in% outlier_t, ]

# replot data
# plot(margaret,
       # error = 2 ,
       # level.UD = 0.50)
outliers <- outlie(margaret)
# plot(outliers)

# make a variogram
vg <- variogram(margaret)

# Guesstimate the model to obtain initial parameter values
guess <- ctmm.guess(margaret,
                        variogram = vg,
                        interactive = FALSE)
guess$error <- TRUE

fits <- ctmm.select(margaret, CTMM = guess) # let this run for a while! it takes like 24 hours!
# run this on the hpc!!

# return a summary of the fitted models
summary(fits)

# plot variogram and model
# plot(vg, CTMM = fits)

# THEN do other stuff...

save(fits, file = "margaret_ctmm.Rda")



