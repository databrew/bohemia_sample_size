hayes_rates <- function( leff, # rate in exp. group
                         lcont, # rate in contr. group
                         alpha, beta, # significance levels
                         CV, # kappa value
                         clustersize, # average number of subjects per cluster
                         followup # follow-up time, same unit as rates
) {
  zalpha <- qnorm(alpha/2, lower.tail=F)
  zbeta <- qnorm(beta)
  #y <- (zalpha+zbeta)^2*(leff+lcont)/(leff-lcont)^2
  y <- clustersize*followup
  return( 1+(zalpha+zbeta)^2 * ((leff+lcont)/y + CV^2*(leff^2+lcont^2))/(lcont-leff)^2 )
}
