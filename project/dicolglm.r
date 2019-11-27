### just a bit of code that runs a dicol in glm:



gvec=c(hgoalsofar,agoalsofar)
avec=c(hat1,aat1)
havec=c(rep(1,length(hat1)),rep(0,length(hat1)))
dvec=c(aat1,hat1)+length(unteam)
mod=glm(gvec~factor(avec)+factor(dvec)+havec,family=poisson)
attcoef=coef(mod)[1:20]
defcoef=c(0,coef(mod)[21:39])
cbind(unteam,attcoef,defcoef,attcoef-defcoef)


mod = glm(scored ~ factor(team) + factor(oppteam) + isHome, family = poisson, data = resultdf %>% filter(season==1718))

attcoef = coef(mod)[grep('factor\\(team\\)', names(coef(mod)))]
names(attcoef) = gsub('factor\\(team\\)', '', names(attcoef))
defcoef = coef(mod)[grep('factor\\(oppteam\\)', names(coef(mod)))]
names(defcoef) = gsub('factor\\(oppteam\\)', '', names(defcoef))

coefdf = tibble(team = names(attcoef),
				attcoef = as.numeric(attcoef),
				defcoef = as.numeric(defcoef)) %>%
				mutate(overallcoef = attcoef - defcoef) %>%
				arrange(overallcoef)
				