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

getdistributionFromFile <- function(estimate, filename){
  filename <- paste0('./Results/',filename)
  distributions <- readRDS(filename)
  switch(estimate,
         'q0'={data = distributions$distributions[['q0']]},
         'phi0'={data = distributions$distributions[['phi0']]},
         's'={data = distributions$distributions[['s']]},
         'xi'={data=distributions$distributions[['xi']]},
         'eta'={data = distributions$distributions[['eta']]},
{print('ERROR: No variable given.')}
  )
return(data)
}

getdistributionFromSim <- function(estimate, estname, sim){
  n= sim$n
  xi = sim$xi
  eta = sim$eta
  s = sim$s
  filename <- paste0('./Results/',estname,"_n",n,"_xi",xi,"_eta",eta,"_s",s)
  distributions <- readRDS(filename)
  switch(estimate,
         'q0'={data = distributions$distributions[['q0']]},
         'phi0'={data = distributions$distributions[['phi0']]},
         's'={data = distributions$distributions[['s']]},
         'xi'={data=distributions$distributions[['xi']]},
         'eta'={data = distributions$distributions[['eta']]},
{print('ERROR: No variable given.')}
  )
return(data)
}

#allparList is a List of fixed parameters, 
#eg seulement les files for n=10, eta=0 etc.

getfilesList<- function(estname,fixedparList){
  # La liste fixedparList doit etre triee dans l'ordre n xi eta s
  globpattern = NULL
  for (parname in names(fixedparList)){
    globpattern = paste0(globpattern,"*",parname,fixedparList[[parname]],"_*")
  }
  globpattern = paste0(estname,globpattern)
  pat = glob2rx(globpattern)
  
  if (any(unlist(names(fixedparList))=='s')){ 
    #quand le pattern doit finir par s0.1, on enleve _.* et $
    pat= substr(pat, 1, nchar(pat)-1)
    pat = paste0(pat,"$")
   #print(pat)
    
  }
  else { #on enleve juste $ ajoute par glob2rx
    ##pat = substr(pat, 1, nchar(pat)-1)
   # print(pat)
  }
  filesList = list.files(path = './Results/', pattern = pat )
  return(filesList)
}