### The numbers at the top of each code chunk
### correspond to the numbers in the HTML file.

### 1
x <- 3
2*x
###

### 2
pp_orig <- read.csv("~/Downloads/pp.csv", check.names=FALSE)
es_orig <- read.csv("~/Downloads/es.csv", check.names=FALSE)
pk_orig <- read.csv("~/Downloads/pk.csv", check.names=FALSE)
all_orig <- read.csv("~/Downloads/all.csv", check.names=FALSE)
###

### 3
#install.packages('ggplot2')
#install.packages('dplyr')
###

### 4
library(ggplot2)
library(dplyr)
###

### 5
test_function <- function(a,b,c){
    d <- a^2+3*b+c^4
    d <- 0.5*d
    return(d)
}

test_function(3,2,1)
d
###

### 6
head(all_orig)
###

### 7
dim(all_orig)
colnames(all_orig)
###

### 8
all_orig[1:10,1:5]
###

### 9
remove <- c('C+/-','G+/-','xG+/-','iPENT','iPEND','iP+/-','ZSR','TOI% QoT','CF% QoT','TOI% QoC','CF% QoC')
###

### 10
x <- all_orig
x[,remove] <- list(NULL)
###

### 11
colnames(x) <- gsub(" ", "", colnames(x))
colnames(x) <- gsub("%", "pct", colnames(x))
colnames(x) <- gsub("/", ".", colnames(x))
###

### 12
colnames(x)[5:ncol(x)] <- paste('all', colnames(x)[5:ncol(x)], sep = "_")
###

### 13
colnames(x)
###

### 14
clean_cols <- function(x,y){
    x[,remove] <- list(NULL)
    colnames(x) <- gsub(" ","",colnames(x))
    colnames(x) <- gsub("%","pct",colnames(x))
    colnames(x) <- gsub("/",".",colnames(x))
    colnames(x)[5:ncol(x)] <- paste(y, colnames(x)[5:ncol(x)], sep = "_")
    return(x)
}

pp <- clean_cols(pp_orig,'pp')
es <- clean_cols(es_orig,'es')
pk <- clean_cols(pk_orig,'pk')
all <- clean_cols(all_orig,'all')
###

### 15
total <- merge(x = all, y = pp, by = c('Player','Season','Team','Position'), all.x = TRUE)
total <- merge(x = total, y = es, by = c('Player','Season','Team','Position'), all.x = TRUE)
total <- merge(x = total, y = pk, by = c('Player','Season','Team','Position'), all.x = TRUE)
###

### 16
colnames(total)
###

### 17
summary(total[,1:16])
###

### 18
lagged <- total %>%
    group_by(Player) %>%
    mutate_at(
        vars(all_TOI:pk_TOIpct),
        funs("lag1" = lag,"lag2" = lag(.,2),"lag3" = lag(.,3))) %>%
    filter(es_TOI >= 400 & pp_TOI >= 0 & pk_TOI >= 0) %>%
    ungroup
###

### 19
dim(lagged)
head(lagged)
###

### 20
ggplot(data=total,aes(all_P)) + geom_histogram(fill="skyblue",col="black")
###

### 21
ggplot(data=total,aes(x=es_P,y=pp_P)) + geom_point(col="darkred")
###

### 22
model1_data <- lagged[,c('all_P','all_P_lag1')]

model1 <- lm(all_P ~ .,data=model1_data)
#model1 <- lm(all_P ~ all_P_lag1,data=lagged)
summary(model1)
###

### 23
ggplot(data=model1,aes(x=all_P_lag1,y=all_P)) + geom_point() + geom_smooth(method='lm',formula=y~x)
###

### 24
lagged$Position <- factor(lagged$Position)
levels(lagged$Position)

### 25
levels(lagged$Position) <- c('C','C','D','L','L','L','R','R')
levels(lagged$Position)
###

### 26
model2_data <- lagged[,c('all_P.60','all_P.60_lag1','all_P1.60_lag1', 'all_P.60_lag2',
                          'Position', 'all_GS_lag1', 'all_GS_lag2')]

model2 <- lm(all_P.60 ~ ., data=model2_data)
#model2 <- lm(all_P.60 ~ all_P.60_lag1 + all_P1.60_lag1 + all_P.60_lag2 +
#             Position + all_GS_lag1 + all_GS_lag2',data=lagged)
summary(model2)
###

### 27
ggplot(data=model2,aes(model2$residuals)) + geom_histogram(fill="violetred3",col="black",alpha=0.65)
###

### 28
model2_fulldata <- subset(lagged, all_P.60_lag2 >= 0)
model2_fulldata$Prediction <- predict(model2,newdata=model2_fulldata)
model2_fulldata$Residual <- model2$residuals
model2_fulldata <- model2_fulldata[,c('Player','Season','Team','Position','all_P.60','all_P.60_lag1',
                                      'Prediction','Residual')]
head(model2_fulldata[order(-model2_fulldata$Residual),])
head(model2_fulldata[order(model2_fulldata$Residual),])
###

### 29
ggplot(data=lagged,aes(all_P.60)) + geom_histogram(fill="orange2",col="black") +
scale_x_continuous(limits = c(0,4),breaks=seq(0,4,.5))
###

### 30
ggplot(data=model2_fulldata,aes(Prediction)) + geom_histogram(fill="skyblue3",col="black") +
scale_x_continuous(limits = c(0,4),breaks=seq(0,4,.5))
###