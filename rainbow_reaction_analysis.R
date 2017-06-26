library(gmodels) 

### ACCESS REPORTS FROM FACEBOOK USERS
### Availability (did they report being able to use Facebook's Rainbow Reaction): Yes, No
### Seen (did they report having seen the feature): Yes, No
### City (which city are they in): (30 cities, in the format CityState)
### CityWide: Did we determine, based on responses, that the rollout was citywide
###           For this determination, we looked at the propotion of non-LGBT people
###           Who reported access to the Rainbow Reaction
### Election: What candidate received more votes in the 2016 presidential election

fbreports <- read.csv("reactions_survey_data_anonymized.csv")

## First, remove responses that were made by
## people outside our targeted Facebook ads, and for whom we
## do not have any location or Facebook algorithm information
fbreports = subset(fbreports, City!="")

fbreports$available = as.integer(fbreports$Availability=="Yes")
fbreports$seen = as.integer(fbreports$Seen=="Yes")

### SUMMARY STATISTICS
summary(fbreports$Availability)

CrossTable(fbreports$available,  fbreports$CityWide,
           prop.t=FALSE, prop.r=TRUE, prop.c=FALSE,  expected=FALSE, prop.chisq=FALSE)

CrossTable(subset(fbreports, LGBTInterest==0)$City,  subset(fbreports, LGBTInterest==0)$Availability,
           prop.t=FALSE, prop.r=TRUE, prop.c=FALSE,  expected=FALSE, prop.chisq=FALSE)

CrossTable(subset(fbreports, LGBTInterest==1)$City,  subset(fbreports, LGBTInterest==1)$Availability,
           prop.t=FALSE, prop.r=TRUE, prop.c=FALSE,  expected=FALSE, prop.chisq=FALSE)

nrow(subset(fbreports, CityWide==0))
length(unique(subset(fbreports, CityWide==0)$City))

### TEST THE HYPOTHESIS IF A DIFFERENCE BETWEEN 
### LGBT-INTERESTED FACEBOOK USERS AND NON-LGBT-INTERESTED USERS
### ADJUSTING FOR CITY

s2s <- summary(s2 <- glm(Availability ~ 
                           LGBTInterest + City, 
                         family=binomial, data=subset(fbreports, CityWide==0)))



baseline_prob = 1/(1+exp(-1 *(s2s$coefficients["(Intercept)",]['Estimate'])))
estimated_prob = 1/(1+exp(-1 *(s2s$coefficients["(Intercept)",]['Estimate'] + s2s$coefficients["LGBTInterest",]['Estimate'])))
confint <- 1.96*s2s$coefficients['LGBTInterest',]['Std. Error']
lgbt_prob_upr = 1/(1+exp(-1 *(s2s$coefficients["(Intercept)",]['Estimate'] + 
                                s2s$coefficients["LGBTInterest",]['Estimate'] + 
                                confint)))
lgbt_prob_lwr = 1/(1+exp(-1 *(s2s$coefficients["(Intercept)",]['Estimate'] + 
                               s2s$coefficients["LGBTInterest",]['Estimate'] - 
                               confint)))

pct_point_diff = estimated_prob - baseline_prob

estimate_data <- data.frame(
  LGBTInterest = c("No LGBT Interest", "LGBT Interest"),
  estimate = c(baseline_prob, estimated_prob),
  estimate_upr = c(baseline_prob, lgbt_prob_upr),
  estimate_lwr = c(baseline_prob, lgbt_prob_lwr),
  fillcolor = c("#E69F00", "#56B4E9")
)

estimate_data$LGBTInterest = relevel(estimate_data$LGBTInterest, "No LGBT Interest")

ggplot(data=estimate_data, aes(x=LGBTInterest, y=estimate, fill=LGBTInterest)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = estimate_lwr, ymax = estimate_upr), width=0.1) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  scale_y_continuous(expand = c(0,0), labels = scales::percent, limits=c(0,1)) +
  geom_text(aes(label=sprintf("%1.0f%%", 100*estimate)), vjust=1.5, #hjust=0.7, 
            size=10, color="#ffffff", fontface="bold") +
  theme(axis.text.y = element_text(size=20),
        axis.text.x = element_text(size=20),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
  ylab("") + xlab("")

###### NOW TEST THE HYPOTHESIS OF A POLITICAL CORRELATION
## Caveat: we did not take a random sample of cities.
## Also, the majority of the largest US cities (or city-including regions)
## voted primarily for Clinton. Sampling bias may prevent us
## from detecting an effect in this case. 

### FIELDS IN THE DATA:
### Election: What candidate received more votes in the 2016 presidential election
### City: City Name
### CityWide: Whether we determined the city had access to the feature
### Population.Estimate.2016: US Census Population Estimate for that City

citywide <- read.csv("reactions_survey_cities.csv")

summary(glm(CityWide ~ Trump.Clinton, family=binomial, data=subset(citywide, Population.Estimate.2016>600000)))
