## From the handout Covariance and correlation; simulating correlated data
## covariance.pdf. Use cholesky to generate correlated data.
## As another option, we use a trick from Richard Vogg blog https://r-vogg-blog.netlify.app/
# Simulating correlated data
## See also Kay IPRP p. 475.

library(tidyverse)

## random matrix iid
uncor_mat <- matrix(rnorm(4*1000,0,1),nrow=1000)  ## note this is wide nvars-by-nobs

# define a matrix of desired correlations between variables
Q <- matrix(c(1,0.3,0.4,0.2,  0.3,1,-0.3,0.3,  0.4,-0.3,1,-0.3,  0.2,0.3,-0.3,1), ncol = nvars)
L <- t(chol(Q))
Z <- uncor_mat %*% t(L)  ## Z has desired correlations

cor(Z)

### from Richard Vogg blog https://r-vogg-blog.netlify.app/

k <- 2000
married <- sample(c("Y","N"),k,replace=T)

data <- data.frame(id=1:k,married) %>%
    mutate(
        age=ifelse(married=="Y", rnorm(k, 40, sd = 10), rnorm(k, 30, sd= 12)) %>%
            pmax(18) %>% #every client should be at least 18
            round()
    )

## or use case_when()

 min(data$age)

 data %>% pmax(20)

table(is.na(data))

####
#### Now correlated. Uses Cholesky decomposition.
#### 1. generate uncorrelated data
#### 2. generate helper data matrix, X, uncorrelated
#### 3. define matrix of correlations Q
#### 4. decompose Q = LL^T
#### 5. Z = LX  has Q as corr matrix
#### 6. Use Z to re-order the rows of original data set. For each column in Z
####     -  sort rows in Z by column.
####     - similarly, sort the data rows according to the same column
####     -replace the column in Z with that of the sorted data.

k <- 10

age <- rnorm(k,mean=35,sd=10) %>% pmax(18) %>% round()
balance <- rexp(k,rate=0.001) %>% round(2)
tenure <- rnorm(k,mean=15,sd=5) %>% pmax(1) %>% round()
kids_cnt <- sample(0:6,k,replace=T,prob=c(100,120,80,30,5,2,1))

data <- data.frame(age,balance,kids_cnt,tenure)

#same size
nvars <- ncol(data)
numobs <- nrow(data)

## random matrix iid
rnorm_helper <- matrix(rnorm(nvars*numobs, 0, 1), ncol = nvars) 

# define a matrix of desired correlations between variables
Q <- matrix(c(1,0.3,0.4,0.2,  0.3,1,-0.3,0.3,  0.4,-0.3,1,-0.3,  0.2,0.3,-0.3,1), ncol = nvars)
L <- t(chol(Q))
Z <- rnorm_helper %*% t(L)  ## Z has desired correlations

cor(Z)

raw <- as.data.frame(Z,row.names = NULL,optional = FALSE)  
names(raw) <- names(data)

cor(raw)

##
## re-order see also dplyr::desc(), arrange()

for(name in names(raw)) {
    raw <- raw[order(raw[,name]),]
    data <- data[order(data[,name]),]
    raw[,name] <- data[,name]
}

cor(raw)

## dplyr

for(name in names(raw)) {
  raw <- arrange(raw, name)
  data <- arrange(data, name)
  raw[,name] <- data[,name]
}

cor(raw)



