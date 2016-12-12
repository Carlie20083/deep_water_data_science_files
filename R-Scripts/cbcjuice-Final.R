## Simulate data
write.table(cbc.df, file = "~/datasets/orangejuicesim.csv", append = FALSE, sep = ",")


attrib <- list(pulp = c("none", "some", "lots"), 
               juice = c("frozen", "fresh"),
               added = c("vit_D", "vit_B", "multi"), 
               price = c("3.89", "4.59", "4.89"))

coef.names <- NULL
for (a in seq_along(attrib)) {
  coef.names <- c(coef.names, 
                  paste(names(attrib)[a], 
                        attrib[[a]][-1], sep=""))
}
coef.names
mu <- c(-1, -1, 0.5, -1, -2, -1, -2)
names(mu) <- coef.names
mu
Sigma <- diag(c(0.3, 1, 0.1, 0.3, 1, 0.2, 0.3))
dimnames(Sigma) <- list(coef.names, coef.names)
Sigma["pulpsome", "pulplots"] <- Sigma["pulplots", "pulpsome"] <- 0.3

set.seed(33040)
resp.id <- 1:200 # respondent ids
healthy <- sample(c("yes", "no"), size=length(resp.id), replace=TRUE, 
                  prob=c(0.3, 0.7))
library(MASS)
coefs <- mvrnorm(length(resp.id), mu=mu, Sigma=Sigma)
colnames(coefs) <- coef.names
coefs[healthy=="yes", "addedvit_B"] <- coefs[healthy=="yes", "addedvit_B"] + 2
coefs[healthy=="yes", "addedmulti"] <- coefs[healthy=="yes", "addedmulti"] + 1.5

nques <- 15
nalt <- 3

profiles <- expand.grid(attrib)
nrow(profiles)
head(profiles)
profiles.coded <- model.matrix(~ pulp + juice + added + price, data=profiles)[,-1]
head(profiles.coded)

cbc.df <- data.frame(NULL)
for (i in seq_along(resp.id)) {
  profiles.i <- sample(1:nrow(profiles), size=nques*nalt)
  utility <- profiles.coded[profiles.i,] %*% coefs[i,] 
  wide.util <- matrix(data=utility, ncol=nalt, byrow=TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs, 1, function(x) sample(1:nalt, size=1, prob=x))
  choice <- rep(choice, each=nalt)==rep(1:nalt, nques)
  conjoint.i <- data.frame(resp.id=rep(i, nques), 
                           ques = rep(1:nques, each=nalt), 
                           alt = rep(1:nalt, nques), 
                           healthy = rep(healthy[i], nques), 
                           profiles[profiles.i,], 
                           choice = as.numeric(choice))
  cbc.df <- rbind(cbc.df, conjoint.i)
}  

# cleanup!:
rm(a, i, resp.id, healthy, mu, Sigma, coefs, coef.names, attrib,
   conjoint.i, profiles, profiles.i, profiles.coded, utility, 
   wide.util, probs, choice, nalt, nques)

## END data simulation

# Choice data descriptives
str(cbc.df)
head(cbc.df)
summary(cbc.df)

# write.csv(cbc.df, "rintro-chapter13conjoint.csv", row.names=FALSE)

summary(cbc.df)
xtabs(choice ~ pulp, data=cbc.df)
xtabs(choice ~ juice, data=cbc.df)
xtabs(choice ~ added, data=cbc.df)
xtabs(choice ~ price, data=cbc.df)


#_______________________________________________________
# Fitting a choice model with mlogit using the long format 
install.packages()
library(mlogit)
cbc.mlogit <- mlogit.data(data=cbc.df, choice="choice", shape="long", 
                          varying=3:6, alt.levels=paste("pos",1:3), 
                          id.var="resp.id")


m1 <- mlogit(choice ~ 0 + pulp + juice + added + price, data = cbc.mlogit)
summary(m1)

m2 <- mlogit(choice ~ pulp + juice + added + price, data = cbc.mlogit)
summary(m2)

lrtest(m1, m2)
m3 <- mlogit(choice ~ 0 + pulp + juice + added 
             + as.numeric(as.character(price)), 
             data = cbc.mlogit)
summary(m3)

lrtest(m1, m3)

# Willingness to pay
coef(m3)["added"]/(-coef(m3)["as.numeric(as.character(price))"]/1)

predict.mnl <- function(model, data) {
  # Function for predicting shares from a multinomial logit model
    # model: mlogit object returned by mlogit()
     # data: a data frame containing the set of designs for which you want to
     #       predict shares.  Same format as the data used to estimate model.
    data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
    utility <- data.model%*%model$coef
    share <- exp(utility)/sum(exp(utility))
   cbind(share, data) }

(new.data <- expand.grid(attrib)[c(8, 1, 3, 41, 49, 26), ])
(new.data1 <- expand.grid(attrib)[c(18, 1, 3, 22, 49, 29), ])
