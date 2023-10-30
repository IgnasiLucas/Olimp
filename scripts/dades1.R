NumEnzims <- 7
QuantsConstitutius <- 2
QuantsVariables <- NumEnzims - QuantsConstitutius
TipusEnzim <- c(rep('Constitutiu', QuantsConstitutius),
                rep('Variable', QuantsVariables))[sample(1:NumEnzims, NumEnzims)]
QuinEnzim <- sample(which(TipusEnzim == 'Variable'), 1)
NumMostres <- 10
MostresLlista <- lapply(1:NumMostres, function(x, mean1 = 1.5, sd1=0.02) {
   dades <- data.frame(Muestra = sprintf("M%02i", x),
                       Enzima  = sprintf("E%02i", 1:NumEnzims),
                       Actividad = sapply(TipusEnzim, function(y) {
                          ifelse(y == 'Constitutiu', rnorm(1, mean1, sd1), rexp(1, mean1 * 2))
                       }))
   dades$Clorpirifos <- 10 * exp(-dades[QuinEnzim, 'Actividad']) + rnorm(1)
   return(dades)
})
Mostres <- do.call(rbind, MostresLlista)
write.table(Mostres, file = 'Datos1.txt', quote = FALSE, sep = '\t', row.names = FALSE)
