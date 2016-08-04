# Read what is already in results
# Update parameters Table
library(gsubfn)

getvalue <- function(filename,parname){
  switch(parname,
         'n'={pattern = "^.*_n(.*)_xi.*$"},
         'xi'={pattern = "^.*_xi(.*)_eta.*$"},
         'eta'={pattern = "^.*_eta(.*)_.*$"},
         's'={pattern = '^.*_s(.*)$'},
{print('No name of parameter given')}
  )
val = strapplyc(filename, pattern, simplify = TRUE)
return(as.numeric(val))
}

t <- read.table("parametersTable", stringsAsFactors = FALSE)
filesList = list.files(path = './Results/')

for (file in filesList) {
  est = strapplyc(file, '^(.*)_n.*$', simplify = TRUE)
  n = getvalue(file, 'n')
  xi = getvalue(file, 'xi')
  eta = getvalue(file, 'eta')
  s = getvalue(file, 's')
  for (i in Reduce(intersect, list(which(t[,1] == est),
                                   which(t[,2]==n),
                                   which(t[,3]==xi), 
                                   which(t[,4]==eta),
                                   which(t[,5]==s)))){
    t <- t[-i,]
  }
}

write.table(t, file = "parametersTable2")

