setwd("~/Projects/Plan Composition/pyret-starter/planalysis/data")

mfs = list.files(path = "./measures/")
mfs1 = unique(lapply(mfs, function (i) strsplit(i, "f")[[1]]))
mfns <<- unique(lapply(mfs1, function (i) i[1]))
mfs2 = unique(lapply(mfs1, function (i) strsplit(i[2], "_")[[1]]))
msubs <<- unique(lapply(mfs2, function (i) i[1]))
mtcs <<- unique(lapply(mfs2, function (i) strsplit(i[2], "[.]")[[1]][1]))

for (mfn in mfns){
  if (mfn > 1) 
    break
  for (tc in mtcs){
    disj = list()
    maxints = 0
    for (sub in msubs){
      fn = list()
      sfs = read.csv(paste("./measures/", mfn, "f", sub, "_", tc, ".csv", sep=""), header=FALSE)
      for(i in 1:length(sfs[, 1])){
        fint = fn[[as.character(sfs[i, 3])]]
        if(is.null(fint) || ((sfs[i, 2] - sfs[i, 1]) > (fint[2] - fint[1]))) {
          fn[[as.character(sfs[i, 3])]] = c(sfs[i, 1], sfs[i, 2])
        }
      }
      
      # note that the data is already sorted by start pos of functions
      for (i in 1:length(fn)){ fn[[i]][3] = TRUE}
      #fn[[1]][3] = TRUE
      for (i in 1:length(fn)){
        for (j in i:length(fn)){
          if (j > i){
            if ((fn[[j]][1] < fn[[i]][2]) && (fn[[j]][1] > fn[[i]][1])){
              fn[[j]][3] = FALSE
            }
          }
        }
      }
      
      disj[[sub]] = Filter(function(x) x[3], fn)
      
      max = sfs[[2]][which.max(abs(as.numeric(sfs[[2]])))]
      len = length(disj[[sub]])
      maxints = if (maxints < len) len else maxints
    }
    
  }
}