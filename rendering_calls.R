library(rmarkdown)
library(knitr)

# Render RMDs to .html and .md and convert .RMD to .R

## Study 1

render(input = "Study 1/code/Analysis_SCIEEFF.Rmd",
       envir = new.env())

purl(input="Study 1/code/Analysis_SCIEEFF.Rmd",
     output="Study 1/code/Analysis_SCIEEFF.R",
     documentation = 2)

render(input = "Study 1/code/Analysis_INTBRSCI.Rmd",
       envir = new.env())

purl(input="Study 1/code/Analysis_INTBRSCI.Rmd",
     output="Study 1/code/Analysis_INTBRSCI.R",
     documentation = 2)

render(input = "Study 1/code/Analysis_JOYSCIE.Rmd",
       envir = new.env())

purl(input="Study 1/code/Analysis_JOYSCIE.Rmd",
     output="Study 1/code/Analysis_JOYSCIE.R",
     documentation = 2)

## Study 2

render(input = "Study 2/code/Analysis_altruism.Rmd",
       envir = new.env())

purl(input="Study 2/code/Analysis_altruism.Rmd",
     output="Study 2/code/Analysis_altruism.R",
     documentation = 2)

render(input = "Study 2/code/Analysis_trust.Rmd",
       envir = new.env())

purl(input="Study 2/code/Analysis_trust.Rmd",
     output="Study 2/code/Analysis_trust.R",
     documentation = 2)

render(input = "Study 2/code/Analysis_posrecip.Rmd",
       envir = new.env())

purl(input="Study 2/code/Analysis_posrecip.Rmd",
     output="Study 2/code/Analysis_posrecip.R",
     documentation = 2)

render(input = "Study 2/code/Analysis_negrecip.Rmd",
       envir = new.env())

purl(input="Study 2/code/Analysis_negrecip.Rmd",
     output="Study 2/code/Analysis_negrecip.R",
     documentation = 2)

render(input = "Study 2/code/Analysis_risktaking.Rmd",
       envir = new.env())

purl(input="Study 2/code/Analysis_risktaking.Rmd",
     output="Study 2/code/Analysis_risktaking.R",
     documentation = 2)

render(input = "Study 2/code/Analysis_patience.Rmd",
       envir = new.env())

purl(input="Study 2/code/Analysis_patience.Rmd",
     output="Study 2/code/Analysis_patience.R",
     documentation = 2)




## Study 3

### Multivariate

render(input = "Study 3/code/Multivariate_Analysis.Rmd",
       envir = new.env())

purl(input="Study 3/code/Multivariate_Analysis.Rmd",
     output="Study 3/code/Multivariate_Analysis.R",
     documentation = 2)

### Univariate

render(input = "Study 3/code/Neuroticism_Analysis.Rmd",
       envir = new.env())

purl(input="Study 3/code/Neuroticism_Analysis.Rmd",
     output="Study 3/code/Neuroticism_Analysis.R",
     documentation = 2)

render(input = "Study 3/code/Extraversion_Analysis.Rmd",
       envir = new.env())

purl(input="Study 3/code/Extraversion_Analysis.Rmd",
     output="Study 3/code/Extraversion_Analysis.R",
     documentation = 2)

render(input = "Study 3/code/Openness_Analysis.Rmd",
       envir = new.env())

purl(input="Study 3/code/Openness_Analysis.Rmd",
     output="Study 3/code/Openness_Analysis.R",
     documentation = 2)

render(input = "Study 3/code/Agreeableness_Analysis.Rmd",
       envir = new.env())

purl(input="Study 3/code/Agreeableness_Analysis.Rmd",
     output="Study 3/code/Agreeableness_Analysis.R",
     documentation = 2)

render(input = "Study 3/code/Conscientiousness_Analysis.Rmd",
       envir = new.env())

purl(input="Study 3/code/Conscientiousness_Analysis.Rmd",
     output="Study 3/code/Conscientiousness_Analysis.R",
     documentation = 2)
