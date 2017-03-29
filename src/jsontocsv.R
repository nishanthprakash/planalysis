library(jsonlite)
library(data.table)

setwd("~/Projects/Plan Composition/planalysis/data")
dir.create(file.path('../jmeasures'), showWarnings = FALSE)

mfs = list.files(path = "./json-anf/")
unique(lapply(mfs, function (i) strsplit(i, "[.]")[[1]][1]))

js = read_json('10-1_1.arr')
dt = data.table(t(sapply(js, unlist)))
dt[, c(1, 5:7):=NULL]
write.table(format(dt, scientific=FALSE), file = "../jmeasures/dat2.csv", sep=",", row.names = FALSE , col.names = FALSE)

