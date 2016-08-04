#Write the parametersTable

estNames <-list('mle_SF','mle_SS')#case s=0?
nList <- list(10,20, 50, 100, 200)
xiList <-list(0.2,0.5, 1, 2)
etaList <-list(0, 0.2,0.5, 1, 2, 3.14)
sList <- list(0, 0.2, 1, 10)
parTable <- NULL
for (n in nList){ 
  for (xi in xiList){ 
    for (eta in etaList){ 
      for (s in sList){
        for (est in estNames){
          parTable = rbind(parTable,c(est,n,xi,eta,s))
        }
      }
    }
  }
}

write.table(parTable, file = "parametersTable")
