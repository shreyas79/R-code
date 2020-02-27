#this is a problem of converting unsupervised problem into supervised problem to 
# and finding the probability density
rm(list = ls())


library(ElemStatLearn)

data("marketing")
data = data("marketing")
data = marketing
demographic = c("Sex", "Martial_Status", "Age", "Education", "Occupation", "Income", "Years_In_BayArea", "Dual_Incomes", "Numbers_in_Household","Number_of_Children", "Householder_Status", "Type_of_Home", "Ethinic_Classification", "Language_in_Home")

N = dim(data)[1]
Sex = sample(c(1,2), N,replace = T)
Martial_Status = sample(seq(1,5), N, replace = T)
Age = sample(seq(1,7), N, replace = T)
Education = sample(seq(1,6), N, replace = T)
Occupation = sample(seq(1,9), N, replace = T)
Income = sample(seq(1,9), N, replace = T)
Years_In_BayArea = sample(seq(1,5), N, replace = T)
Dual_Incomes = sample(seq(1,3), N, replace = T)
Numbers_in_Household = sample(seq(1,9), N, replace = T)
Number_of_Children = sample(seq(1,9), N, replace = T)
Householder_Status = sample(seq(1,3), N, replace = T)
Type_of_Home = sample(seq(1,5), N, replace = T)
Ethinic_Classification = sample(seq(1,8), N, replace = T) 
Language_in_Home = sample(seq(1,3), N, replace = T)

train = data.frame(Sex, Martial_Status, Age, Education, Occupation, Income, Years_In_BayArea, Dual_Incomes, Numbers_in_Household,Number_of_Children, Householder_Status, Type_of_Home, Ethinic_Classification, Language_in_Home)
names(train) = demographic
train$target = 1

reference = train
for(i in 1:ncol(reference)){
  reference[,i] = sample(reference[,i], nrow(reference), replace = F)
}

reference$target = 0
# combining both the data
comb = rbind(reference, train); rm(reference, train)

# to change the caterogical data
comb$Sex = as.factor(as.character(comb$Sex))
comb$Martial_Status = as.factor(as.character(comb$Martial_Status))
comb$Occupation = as.factor(as.character(comb$Occupation))
comb$Dual_Incomes = as.factor(as.character(comb$Dual_Incomes))
comb$Householder_Status = as.factor(as.character(comb$Householder_Status))
comb$Type_of_Home = as.factor(as.character(comb$Type_of_Home))
comb$Ethinic_Classification = as.factor(as.character(comb$Ethinic_Classification))
comb$Language_in_Home = as.factor(as.character(comb$Language_in_Home))

library(rpart)
comb$target = as.factor(as.character(comb$target))
model = rpart(target~., comb)
plot(model)
summary(model)
predicted = predict(model, comb[,-c(15)])
predicted
