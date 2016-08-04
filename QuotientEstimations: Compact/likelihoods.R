babyllf_delta <- function(v,x){
  y0 <- v[1]
  phi0 <- v[2]
  sigma <- v[3]
  if ((sigma==0)||is.nan(sigma)||is.nan(y0)||is.nan(phi0)){ L=1e+4} #Case L=Inf-Inf
  else{L= sqdist(x,y0,phi0)/(2*sigma^2)+log(2*pi*sigma^2)}
  return(L)
}

llf_delta <- function(v,sim) {
  obs=sim$Data
  L=apply(obs,1, function(x) babyllf_delta(v,x))
  return(sum(L))
}

babyllf_gaussian <- function(v,x) {
  y0 <- v[1]
  phi0 <- v[2]
  sigma <- v[3]
  eta <- v[4]
  # L = 1e+4
  # cat('Avant funs: phi0=',phi0,'eta=',eta,'\n')
  #cat('sigma=',sigma,'\n')
  if ((sigma == 0)||is.nan(sigma)||is.nan(y0)||is.nan(phi0)||is.nan(eta)) { 
    #### CAS DIRAC
    #cat('Erreur: one current estimate is Nan, or sigmaest=0')
    probaQ <-function(phi) 1e-10
  }
  else{
    in_expQ <- function (phi) -(1e-10)*(sqdist(x,y0,phi))/(2*sigma^2)
    probaQ <- function(phi) exp(in_expQ(phi))^(1e+10)/(2*pi*sigma)
    #cat('ProbaQ non 0: \n')
    #cat('probaQ(-pi)=',probaQ(-pi),', probaQ(0)=',probaQ(0),', probaQ(1)=',probaQ(1),'\n')
  }
  if ((eta==0)||is.nan(sigma)||is.nan(y0)||is.nan(phi0)||is.nan(eta)){ 
    #### CAS DIRAC
    #cat('Erreur: one current estimate is Nan, or etaest=0')
    probaO <-function(phi) 1e-10
  }
  else{
    in_expO <- function(phi) -(1e-5)*(phi-phi0)^2/(2*eta^2)
    probaO <-function(phi) exp(in_expO(phi))^(1e+5)/(sqrt(2*pi)*eta) 
#     cat('eta=',eta,', phi0=',phi0,'\n')
#     cat('probaO(-pi)=',probaO(-pi),'\n') #,', probaO(0)=',probaO(0),', probaO(1)=',probaO(1),'\n')
  }  
  
  # cat("For x=",x[1],',',x[2],":\n")
  #cat("ProbaQ(1)=",probaQ(1),"and ProbaO(0)=",probaO(0),"\n")
  #  plot(Vectorize(probaO),-pi+phi0,phi0+pi)
  #  plot(Vectorize(probaQ),-pi+phi0,phi0+pi)
  #plot(Vectorize(probaO),-pi+phi0,phi0+pi)
  
  integrand <- function(phi) (1e+40)*probaQ(phi)*probaO(phi)
  #   plot(Vectorize(integrand),-pi+phi0,pi+phi0)
  # #   res = integrate(Vectorize(integrand),-pi+phi0,pi+phi0)
  #   preL=res$value
  #   cat('preL=res$value=', res$value,'\n')
  
  #cat('integrand(1)=',integrand(1),'\n')
  preL = 1e-10
  tryCatch({res = integrate(Vectorize(integrand),-pi+phi0,pi+phi0);preL=(1e-40)*res$value},
           error=function(cond){message("Attention Erreur: ", cond)
#                                 plot(Vectorize(in_expQ),-pi+phi0,phi0+pi);
#                                 plot(Vectorize(in_expO),-pi+phi0,phi0+pi);
#                                 plot(Vectorize(probaO),-pi+phi0,phi0+pi);
#                                 plot(Vectorize(probaQ),-pi+phi0,phi0+pi);
#                                 plot(Vectorize(integrand),-pi+phi0,pi+phi0)
           },
           finally={NA}) 
  # cat("L=",L,'\n')
  #  cat('res=',res,'\n')
  #cat('in trycatch: preL=',preL,'\n')
  # res= integrate(Vectorize(integrand),-pi+phi0,pi+phi0)
  # L=res$value
  #cat('Out of trycatch: preL=',preL,'\n')
  #if (preL < 1e-10) {preL=1e-10}
  #L = -log(preL)
  if (preL==0) {preL=1e-10 
                #cat('Warning: integration has given 0.\n')
                #                 plot(Vectorize(in_expQ),-pi+phi0,phi0+pi)
                #                 plot(Vectorize(probaO),-pi+phi0,phi0+pi)
                #                 plot(Vectorize(probaQ),-pi+phi0,phi0+pi)
                #                 plot(Vectorize(integrand),-pi+phi0,pi+phi0)
  }
  L = -log(preL)
  #cat('L=',L,'\n')
  return(L)# to have a MINIMIZATION procedure
  
}

llf_gaussian <- function(v,sim) {
  obs=sim$Data
  babyL=apply(obs,1, function(x) babyllf_gaussian(v,x))
  L = sum(babyL)
  #cat('L=',L,'\n')
  return(L)
}


babyllf_uniform <- function(v,x) {
  y0 <- v[1]
  phi0 <- v[2]
  sigma <- v[3]
  # L = 1e+4
  # cat('Avant funs: phi0=',phi0,'eta=',eta,'\n')
  #cat('sigma=',sigma,'\n')
  if ((sigma == 0)||is.nan(sigma)||is.nan(y0)||is.nan(phi0)||is.nan(eta)) { 
    #### CAS DIRAC
    #cat('Erreur: one current estimate is Nan, or sigmaest=0')
    probaQ <-function(phi) 1e-10
  }
  else{
    in_expQ <- function (phi) -(1e-10)*(sqdist(x,y0,phi))/(2*sigma^2)
    probaQ <- function(phi) exp(in_expQ(phi))^(1e+10)/(2*pi*sigma)
    #cat('ProbaQ non 0: \n')
    #cat('probaQ(-pi)=',probaQ(-pi),', probaQ(0)=',probaQ(0),', probaQ(1)=',probaQ(1),'\n')
  }
  
  # cat("For x=",x[1],',',x[2],":\n")
  #cat("ProbaQ(1)=",probaQ(1),"and ProbaO(0)=",probaO(0),"\n")
  #  plot(Vectorize(probaO),-pi+phi0,phi0+pi)
  #  plot(Vectorize(probaQ),-pi+phi0,phi0+pi)
  #plot(Vectorize(probaO),-pi+phi0,phi0+pi)
  
  integrand <- function(phi) (1e+40)*probaQ(phi)/(2*pi)
  #   plot(Vectorize(integrand),-pi+phi0,pi+phi0)
  # #   res = integrate(Vectorize(integrand),-pi+phi0,pi+phi0)
  #   preL=res$value
  #   cat('preL=res$value=', res$value,'\n')
  
  #cat('integrand(1)=',integrand(1),'\n')
  preL = 1e-10
  tryCatch({res = integrate(Vectorize(integrand),-pi+phi0,pi+phi0);preL=(1e-40)*res$value},
           error=function(cond){message("Attention Erreur: ", cond);  
                                #                                 plot(Vectorize(in_expQ),-pi+phi0,phi0+pi)
                                #                                 plot(Vectorize(probaO),-pi+phi0,phi0+pi);
                                #                                 plot(Vectorize(probaQ),-pi+phi0,phi0+pi);
                                #                                 plot(Vectorize(integrand),-pi+phi0,pi+phi0)
           },
           finally={NA}) 
  # cat("L=",L,'\n')
  #  cat('res=',res,'\n')
  #cat('in trycatch: preL=',preL,'\n')
  # res= integrate(Vectorize(integrand),-pi+phi0,pi+phi0)
  # L=res$value
  #cat('Out of trycatch: preL=',preL,'\n')
  #if (preL < 1e-10) {preL=1e-10}
  #L = -log(preL)
  if (preL==0) {preL=1e-10 
                cat('Warning: integration has given 0.\n')
                #                 plot(Vectorize(in_expQ),-pi+phi0,phi0+pi)
                #                 plot(Vectorize(probaO),-pi+phi0,phi0+pi)
                #                 plot(Vectorize(probaQ),-pi+phi0,phi0+pi)
                #                 plot(Vectorize(integrand),-pi+phi0,pi+phi0)
  }
  L = -log(preL)
  #cat('L=',L,'\n')
  return(L)# to have a MINIMIZATION procedure
  
}


llf_uniform <- function(v,sim) {
  obs=sim$Data
  babyL=apply(obs,1, function(x) babyllf_uniform(v,x))
  L = sum(babyL)
  #cat('L=',L,'\n')
  return(L)
}
