iris<-read.csv(file="D:/uczelnia/AI/poker-hand-testing.data",header=FALSE)
summary(iris)
colnames(iris) <- c('S1',  'C1', 'S2', 'C2', 'S3', 'C3', 'S4', 'C4', 'S5', 'C5','class')
table(iris$class)

iris$class <- factor(iris$class)

library(neuralnet)
size.sample<-floor(5/1000*nrow(iris))

set.seed(101)
samples_id <- sample(1:nrow(iris), size.sample)
iristrain <- iris[c(samples_id),]
irisvalidation <- iris[-c(samples_id),] #oznacza ze tych nie biore

nnet_iristrain <- iristrain
nnet_iristrain$zero <- iristrain$class == '0'
nnet_iristrain$one <- iristrain$class == '1'
nnet_iristrain$two <- iristrain$class == '2'
nnet_iristrain$three <- iristrain$class == '3'
nnet_iristrain$four <- iristrain$class == '4'
nnet_iristrain$five <- iristrain$class == '5'
nnet_iristrain$six <- iristrain$class == '6'
nnet_iristrain$seven <- iristrain$class == '7'
nnet_iristrain$eight <- iristrain$class == '8'
nnet_iristrain$nine <- iristrain$class == '9'


nn <- neuralnet( zero +  one + two + three + four + five + six + seven + eight + nine ~
                S1 + C1 + S2 + C2 + S3 + C3 + S4 + C4 + S5 + C5 ,
                data=nnet_iristrain,
                hidden=c(1))

#plot(nn)
#nn <- neuralnet(cp+im+imU+om+pp ~
#                 mcg+gvh+lip+chg+aac+alm1+alm2,
#               data=nnet_iristrain,
               #hidden=c(10))  #10 sie nie zbiega
#               hidden=c(5),stepmax = 1e+06)

mypredict <- compute(nn, irisvalidation[-11])$net.result
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), maxidx)

prediction <- c('one', 'two','three', 'four', 'five', 'six', 'seven', 'eight', 'nine')[idx]
pred<-table(prediction, irisvalidation$class)

A<-as.matrix(pred)

pred
A
sum(diag(A))/sum(A)
