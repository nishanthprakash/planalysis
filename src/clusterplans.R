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
      for (i in 1:length(sfs[, 1])){ sfs[i,4] = TRUE}
      for(i in 1:length(sfs[, 1])){
        for(j in i:length(sfs[, 1])){
          if (j > i){
            if ((sfs[j,1] < sfs[i,2]) && (sfs[j,1] > sfs[i,1])){
              sfs[j,4] = FALSE
            }
          }
        }
      }
      
      disj[[sub]] = subset(sfs, V4)[, 1:3]
      
      max = sfs[[2]][which.max(abs(as.numeric(sfs[[2]])))]
      len = length(disj[[sub]])
      #maxints = if (maxints < len) len else maxints
    }
    m = NULL
    maxints = 0
    for(i in 1:length(disj)){ 
      maxints = if (maxints < length(disj[[i]][1:2][,1])) length(disj[[i]][1:2][,1]) else maxints
    }
    for(i in 1:length(disj)){
      disjel = as.vector(t(disj[[i]][1:2])) - 1
      disjel = disjel/tail(disjel, n=1)
      m = rbind(m, c(disjel, rep(1, 2*maxints - length(disjel))))
    }
    #png(paste("./plots/planclusters/", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    plot(hclust(dist(m)))
    #dev.off()
  }
}