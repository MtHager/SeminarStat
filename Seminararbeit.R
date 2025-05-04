library(foreign)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(caret)
library(VIM)
library(ggplot2)
#-------------------------------demographic data-------------------------------#
data.demographic <- as.data.frame(read.xport("DEMO_L.XPT"))

#extract the relevant demographic data 
data.demographic <- data.demographic[c("SEQN","RIDAGEYR","RIAGENDR")]
data.demographic <- rename(data.demographic, age=RIDAGEYR)
data.demographic <- mutate(data.demographic, 
                           gender = recode(RIAGENDR, `1` = "M", `2` = "F"),
                           .keep ="unused")



#-----------------blood pressure data (Systolic and Diastolic)-----------------#
data.bloodpressure <- as.data.frame(read.xport("BPXO_L.XPT"))

#extract only the Systolic bloddpressure and average the Messurements
data.bloodpressure <- data.bloodpressure[c("SEQN","BPXOSY1","BPXOSY2","BPXOSY3")]

#calculate the average systolic blood pressure
data.bloodpressure$average.systolic.bp <- rowMeans(data.bloodpressure[,c("BPXOSY1","BPXOSY2","BPXOSY3")], na.rm=TRUE)

#hightened Systolic blood pressure (class)
data.bloodpressure <- mutate(data.bloodpressure, hypertension = if_else(average.systolic.bp >= 125, "Yes", "No"))

data.bloodpressure <- select(data.bloodpressure, SEQN, hypertension)
#remove NAs
data.bloodpressure <- na.omit(data.bloodpressure)
#----------------------------Questionnaire  Data---------------------------------#
#Tobacco Use
data.questionare.tobacco <- as.data.frame(read.xport("SMQRTU_L.XPT"))
#SMDANY - Used any tobacco product last 5 days?
data.questionare.tobacco <- data.questionare.tobacco[c("SEQN","SMDANY")]
data.questionare.tobacco <- mutate(data.questionare.tobacco, 
                                   tobacco.use = case_match(SMDANY,  7~ 2, 9 ~ 2, NA ~ 2, .default = SMDANY), 
                                   .keep="unused")

#Oral Health
data.questionare.oral <- as.data.frame(read.xport("OHQ_L.XPT"))
#OHQ845 - Rate the health of your teeth and gums
data.questionare.oral <- data.questionare.oral[c("SEQN","OHQ845")]
data.questionare.oral <- mutate(data.questionare.oral, 
                                oral.health = case_match(OHQ845, 7 ~ 5, 9 ~ 5, NA ~ 5, .default = OHQ845), 
                                .keep="unused")


#Diabetes
data.questionare.diabetes <- as.data.frame(read.xport("DIQ_L.XPT"))
#DIQ010 - Doctor ever told you have diabetes
data.questionare.diabetes <- data.questionare.diabetes[c("SEQN","DIQ010")]
data.questionare.diabetes <- mutate(data.questionare.diabetes, 
                                    diabetes = case_match(DIQ010,2 ~ 3, 3 ~ 2, 7 ~ 2, 9 ~ 2 , NA ~ 2, .default = DIQ010), 
                                    .keep="unused")


#Alcohol Use
data.questionare.alcohol <- as.data.frame(read.xport("ALQ_L.XPT"))
#ALQ111 - Ever had a drink of any kind of alcohol
data.questionare.alcohol <- data.questionare.alcohol[c("SEQN","ALQ111")]
data.questionare.alcohol <- mutate(data.questionare.alcohol, 
                                   alcohol.use = case_match(ALQ111,7 ~ 2, 9 ~ 2, NA ~ 2, .default = ALQ111), 
                                   .keep="unused")

#-------------------------------Laboratory Data--------------------------------#

#Cholesterol-Total
data.lab.cholesterol <- as.data.frame(read.xport("TCHOL_L.XPT"))
#LBXTC - Total Cholesterol (mg/dL)
data.lab.cholesterol <- data.lab.cholesterol[c("SEQN","LBXTC")]
data.lab.cholesterol <- rename(data.lab.cholesterol, cholesterol=LBXTC)


#Vitamin D
data.lab.vitamin.D <- as.data.frame(read.xport("VID_L.XPT"))
#LBXVD3MS - 25OHD3 (nmol/L)
#LBXVD2MS - 25OHD2 (nmol/L)
data.lab.vitamin.D <- data.lab.vitamin.D[c("SEQN","LBXVD2MS","LBXVD3MS")]
data.lab.vitamin.D <- rename(data.lab.vitamin.D, vitamin.D3=LBXVD3MS, vitamin.D2=LBXVD2MS)

#-------------------------------Examination Data-------------------------------#
#Body Measures
data.examination.body <- as.data.frame(read.xport("BMX_L.XPT"))
#BMXWT - Weight (kg)
#BMXHT - Standing Height (cm)
#BMXBMI - Body Mass Index (kg/m**2)
data.examination.body <- data.examination.body[c("SEQN","BMXWT","BMXHT","BMXBMI")]
data.examination.body <- rename(data.examination.body, weight=BMXWT, height=BMXHT, BMI=BMXBMI)

#------------------------------------Merge Data--------------------------------#
#Data with available blood pressure measurements
data.full <- merge(data.bloodpressure, data.demographic, by = 'SEQN', all.x=TRUE)
#data(examination)
data.full <- merge(data.full, data.examination.body,by = 'SEQN', all.x=TRUE)
#data(lab)
data.full <- merge(data.full, data.lab.vitamin.D,by = 'SEQN', all.x=TRUE)
data.full <- merge(data.full, data.lab.cholesterol,by = 'SEQN', all.x=TRUE)
#data(questionaire)
data.full <- merge(data.full, data.questionare.tobacco,by = 'SEQN', all.x=TRUE)
data.full <- merge(data.full, data.questionare.oral,by = 'SEQN', all.x=TRUE)
data.full <- merge(data.full, data.questionare.diabetes,by = 'SEQN', all.x=TRUE)
data.full <- merge(data.full, data.questionare.alcohol,by = 'SEQN', all.x=TRUE)

#filter for adults
data.full <- filter(data.full, age >= 18)

#na_counts_base <- lapply(data.full, function(x) sum(is.na(x)))
#print(na_counts_base)

#remove SEQN
data.full <- select(data.full, -SEQN)

#missing value imputation
#aggr(data.full, numbers = TRUE, prop = c(TRUE, FALSE))
data.full<- kNN(data.full, imp_var=FALSE)

data.full <- na.omit(data.full)
dim(data.full)
#--------------------------------Classification Tree---------------------------#
set.seed(3333)
#train and test set
# 5 equal samples
folds <- sample(1:10, size = nrow(data.full), replace = TRUE)
#first fifth assigned to test subset, the rest to training
test <- folds == 1


data.test <- data.full[test,]
data.train <- data.full[!test,]
#sum(data.test$hypertension=="Yes")
#sum(data.train$hypertension=="Yes")

#fit tree based model
tree.fit <- rpart(hypertension ~., data = data.train, method = "class")
rpart.plot(tree.fit)
printcp(tree.fit)

prediction.tree <- predict(tree.fit, newdata=data.test ,type = 'class')

confusionMatrix(as.factor(data.test$hypertension), prediction.tree)

#
min.ind <- which.min(tree.fit$cptable[, "xerror"])
min.cp <- tree.fit$cptable[min.ind, "CP"]

#tree pruning
prune.fit <-prune(tree.fit, cp = min.cp) 
rpart.plot(prune.fit) 


prediction.tree <- predict(prune.fit, newdata=data.test ,type = 'class')

confusionMatrix(as.factor(data.test$hypertension), prediction.tree)

#----------------------------bagging (classification)--------------------------#
bagging.fit <- randomForest(as.factor(hypertension)~. , data = data.train, mtry=12, type="classification")

print(bagging.fit)
predictions.bagging <- predict(bagging.fit, newdata=data.test , type = "class")
table(data.test$hypertension, predictions.bagging)

confusionMatrix(as.factor(data.test$hypertension), predictions.bagging)
varImpPlot(bagging.fit)

predictions.bagging <- as.factor(as.vector(predictions.bagging))

#plotting the error rates
bag.error.data <- data.frame(trees=rep(1:nrow(bagging.fit$err.rate), times=3),
                                 type=rep(c("OOB", "No", "Yes"), each=nrow(bagging.fit$err.rate)),
                                 error=c(bagging.fit$err.rate[,"OOB"],
                                         bagging.fit$err.rate[,"No"],
                                         bagging.fit$err.rate[,"Yes"]))


ggplot(data=bag.error.data, aes(x=trees, y=error))+
  geom_line(aes(color=type))+
  ggtitle("Bagging Error Rates")

#----------------------------Random Forest(classification)---------------------#

forest.fit <- randomForest(as.factor(hypertension)~. , data = data.train, type="classification")

print(forest.fit)
predictions.forest <- predict(forest.fit, newdata=data.test , type = "class")
table(data.test$hypertension, predictions.forest)

confusionMatrix(as.factor(data.test$hypertension), predictions.forest)
varImpPlot(forest.fit)

predictions.forest <- as.factor(as.vector(predictions.forest))


#plotting the error rates
rforest.error.data <- data.frame(trees=rep(1:nrow(forest.fit$err.rate), times=3),
                                 type=rep(c("OOB", "No", "Yes"), each=nrow(forest.fit$err.rate)),
                                 error=c(forest.fit$err.rate[,"OOB"],
                                         forest.fit$err.rate[,"No"],
                                         forest.fit$err.rate[,"Yes"]))


ggplot(data=rforest.error.data, aes(x=trees, y=error))+
  geom_line(aes(color=type))+
  ggtitle("Random Forest Error Rates")

#----------------------------------------plots---------------------------------#

draw_confusion_matrix <- function(cm) {
  
  total <- sum(cm$table)
  res <- as.numeric(cm$table)
  
  # Generate color gradients. Palettes come from RColorBrewer.
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  
  # set the basic layout
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Confusion Matrix', cex.main=2)
  
  # create the matrix 
  classes = colnames(cm$table)
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  
  # add in the cm results
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}
M1 <- confusionMatrix(as.factor(data.test$hypertension), prediction.tree)
draw_confusion_matrix(M1)

M2 <- confusionMatrix(as.factor(data.test$hypertension), predictions.bagging)
draw_confusion_matrix(M2)

M3 <- confusionMatrix(as.factor(data.test$hypertension), predictions.forest)
draw_confusion_matrix(M3)

