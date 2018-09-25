
# r2 = 1 - exp[-2/n(Lfull - Lnull)]
# where n is the sample size, Lfull is the likelihood of the fitted model, and 
# Lnull is the likelihood of the null model (Nagelkerke 1991).
#1 - exp((-2/2815)*((sem.nb1.5.w$LL) - (sem.nb1.5.w$logLik_lm.model)))

SARr2 <- function(Lfull, Lnull, N){
  r2 <- 1 - exp((-2/N)*((Lfull) - (Lnull)))
  return(r2)
}

#SARr2(Lfull = sem.nb1.5.w$LL, Lnull = sem.nb1.5.w$logLik_lm.model, N = 2815)

AICcalc <- function(Lfull, Lnull, LNullNull){
    if ( ! ("geiger" %in% installed.packages())) {install.packages("geiger", dependencies = T)}
  AICs <- c(AIC(Lfull), AIC(Lnull), AIC(LNullNull))
  AICw <- geiger::aicw(AICs)
  return(AICw)
}
#AICcalc(Lfull = sem.nb1.5.w, Lnull = sem.nb1.5.w$logLik_lm.model, LNullNull = sem.nb1.5.w$LLNullLlm)



