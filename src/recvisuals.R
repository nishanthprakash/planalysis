setwd("~/Projects/Plan Composition/planalysis/data")

dir.create(file.path('.', 'plots2'), showWarnings = FALSE)

mfs = list.files(path = "./measures/")
mfs1 = unique(lapply(mfs, function (i) strsplit(i, "f")[[1]]))
mfns <<- unique(lapply(mfs1, function (i) i[1]))
mfs2 = unique(lapply(mfs1, function (i) strsplit(i[2], "_")[[1]]))
msubs <<- unique(lapply(mfs2, function (i) i[1]))
mtcs <<- unique(lapply(mfs2, function (i) strsplit(i[2], "[.]")[[1]][1]))

unlink(file.path('.', 'plots2/order'), recursive = TRUE, force = FALSE)
dir.create(file.path('.', 'plots2/order'), showWarnings = FALSE)

for (mfn in mfns){
  for (tc in mtcs){
    disj = list()
    maxints = 0
    for (sub in msubs){
      studfnscsv = read.csv(paste("./measures/", mfn, "f", sub, "_", tc, ".csv", sep=""), header=FALSE)
      studfns = data.frame(lapply(studfnscsv, function(x) {gsub(":_", ":", x)}))
      c3 = as.vector(studfns[3])
      fs = unique(c3)
      fs = data.frame(fs)
      for (i in 1:nrow(fs)){ fs[i,2] = FALSE}
      
      rownames(fs) = fs[, 1]
      fs[, 1] = NULL
      
      studfns = studfns[order(as.character(studfns$V3), as.numeric(as.character(studfns$V1))), ]
      
      prevfn = ""
      prevstart = 0
      prevend = 0
      rec = FALSE
      for(i in 1:nrow(studfns)){
        if(as.character(studfns[i,3]) == prevfn && !rec){
          if (prevstart < as.numeric(as.character(studfns[i,1])) && as.numeric(as.character(studfns[i,2])) < prevend){
            rec = TRUE
          }
        } else {
          if (as.character(studfns[i, 3]) != prevfn && prevfn != ""){
            fs[as.character(prevfn), 1] = rec
            prevfn = ""
            prevstart = 0
            prevend = 0
            rec = FALSE
          }
        }
        prevfn = as.character(studfns[i, 3])
        prevstart = as.numeric(as.character(studfns[i,1]))
        prevend = as.numeric(as.character(studfns[i,2]))
      }
      
      studfns = studfns[order(as.numeric(as.character(studfns$V1))), ]
      
      c13 = cbind(studfns[, c(1, 3)], 1:nrow(studfns[, c(1, 3)]))
      c23 = cbind(studfns[, c(2, 3)], 1:nrow(studfns[, c(1, 3)]))
      names(c13) = names(c23)
      fnpos = rbind(c13, c23)
      fnpos = fnpos[order(as.numeric(as.character(fnpos[,1]))), ]

      for(i in 1:nrow(fnpos)){
        if (!fs[as.character(fnpos[i, 2]), 1]){
          fnpos[-(1:i), 1] = as.numeric(as.character(fnpos[-(1:i), 1])) - 1
        }
      }
      recinds = fs[as.character(fnpos[, 2]), 1]
      recfns = fnpos[recinds, ]
      
      for(i in 1:nrow(recfns)){
        recfns[i, 4] = as.numeric(as.character(recfns[recfns[, 3] == recfns[i, 3], ][2, 1]))
      }
      finalfns = (recfns[recfns[, 1] != recfns[, 4], ])[, c(1, 4, 2)]
      finalfns = finalfns[order(as.numeric(as.character(finalfns[,1]))), ]
      
      png(paste("./plots2/order/", mfn, "f", sub, "_", tc,".png", sep=""), width=12,height=12,units="in", res=1000)
      par(cex=0.5, mar=c(12,3,3,5))
      c13 = finalfns[, c(1, 3)]
      c23 = finalfns[, c(2, 3)]
      names(c13) = names(c23)
      i = rbind(c13, c23)
      i = i[order(as.numeric(as.character(i[,1]))), ]
      i1 = as.vector(i[, 1])
      i2 = as.vector(i[, 2])
      
      c1 = as.vector(finalfns[, 1])
      c2 = as.vector(as.numeric(finalfns[, 2]))
      c3 = as.vector(finalfns[, 3])
      j = sort(c(as.numeric(c1), c2))
      
      fs = unique(c3)
      
      fcols = rainbow(length(fs))
      
      plot.new()
      plot.window(xlim = range(j), ylim = range(j))
      axis(1, at=i1,labels=i2, pos=-1, las=2)
      axis(2, at=j,labels=j, pos=-1, las=2)
      
      par(xpd=TRUE, cex=0.8)
      legend("top",legend=lapply(fs, function(x) as.character(x)), col=fcols, pch=15, pt.cex=5,  horiz=TRUE)
      
      clip(-1, length(j), -1, length(j))
      abline(h=j, v=j, col="darkgray", lty=1, lwd=0.2)
      
      for (ind in 1:nrow(finalfns)){
        fin = match(c(as.character(finalfns[ind,3])), fs)
        x = c(as.numeric(as.vector(finalfns[ind, 1])), as.numeric(as.vector(finalfns[ind, 1])), finalfns[ind, 2], finalfns[ind, 2])
        y = c(as.numeric(as.vector(finalfns[ind, 1])), finalfns[ind, 2], finalfns[ind, 2], as.numeric(as.vector(finalfns[ind, 1])))
        polygon(x, y, col=fcols[fin])
      }
      
      dev.off()
    }
  }
  break
}