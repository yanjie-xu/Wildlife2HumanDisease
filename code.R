setwd('D:/Postdoc-Helsinki/project/review2/Model3_change')
library(MCMCglmm)
setwd('D:/Postdoc-Helsinki/project/review2/Model2_host/code/riskmap5/')
load("D:/Postdoc-Helsinki/project/review2/Model2_host/code/riskmap5/all4predict.Rdata")
df = read.table('data/Birds Breeding/Birds Breeding/Accipiter badius.txt')

i = pathogen[1]
df = preva_trait[preva_trait$Pathogen == i,]

model = MCMCglmm(cbind(Npositive, Ntested-Npositive) ~ Temp + log(Prec) +
                   Clutch_MEAN + Maximum.longevity +
                   Mass + Migration + Habitat, 
                 random=~animal, 
                 pedigree = phy,
                 family ="multinomial2",
                 data=preva_trait,
                 nitt=133000, 
                 burnin=3000, 
                 thin=100)

predict.MCMCglmm(model, newdata=predictw)

predictwresult = predictw[0,]
predictwresult['predict.preva'] = double()

#for i in 1:length(files){}
##Add _ between species names!!!!!!







