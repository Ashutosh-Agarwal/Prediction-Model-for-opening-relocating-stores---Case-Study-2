summary(Data_Problem_2)
Data_P2 = Data_Problem_2[complete.cases(Data_Problem_2),c(-48)]
attach(Data_P2)
model1 = lm(REVENUE_2013~., data = Data_P2)
summary(model1)


model2 = lm(REVENUE_2013~U_CITY
            +U_STATE
            +TOT_ATTRITION_2013
            +NUM_CUST_ACC_REPS
            +NUM_STORE_MANAGERS
            +NUM_EMP_PAY_TYPE_H
            +AVG_PAY_RATE_PAY_TYPE_H
            +AVG_PAY_RATE_PAY_TYPE_S
            +FRONTAGE_ROAD*MARKETING_EXP_2012
            +TOT_NUM_LEADS+NUM_CONVERTED_TO_AGREEMENT
            +URBANICITY
            +CYA21V001+PERC_CYEA07V001+PERC_CYEA07V004, data = Data_P2)
summary(model2)

step(lm(REVENUE_2013~U_CITY
        +U_STATE
        +TOT_ATTRITION_2013
        +NUM_CUST_ACC_REPS
        +NUM_STORE_MANAGERS
        +NUM_EMP_PAY_TYPE_H
        +AVG_PAY_RATE_PAY_TYPE_H
        +AVG_PAY_RATE_PAY_TYPE_S
        +FRONTAGE_ROAD+MARKETING_EXP_2012
        +TOT_NUM_LEADS+NUM_CONVERTED_TO_AGREEMENT
        +URBANICITY
        +CYA21V001+PERC_CYEA07V001+PERC_CYEA07V004, data = Data_P2),direction = "backward")

model3 = lm(REVENUE_2013~U_CITY
            +U_STATE
            +TOT_ATTRITION_2013
            +NUM_CUST_ACC_REPS
            +NUM_STORE_MANAGERS
            +NUM_EMP_PAY_TYPE_H
            +AVG_PAY_RATE_PAY_TYPE_H
            +AVG_PAY_RATE_PAY_TYPE_S
            +FRONTAGE_ROAD+MARKETING_EXP_2012
            +TOT_NUM_LEADS+NUM_CONVERTED_TO_AGREEMENT
            +URBANICITY
            +CYA21V001+PERC_CYEA07V001+PERC_CYEA07V004, data = Data_P2)
summary(model3)

Data_P2$pred_Revenue_2013 = predict(model3, Data_P2)
attach(Data_P2)
Data_P2$diff_Revenue_2013 = REVENUE_2013-pred_Revenue_2013
Data_P2$store_performance = ifelse(Data_P2$diff_Revenue_2013>0,"HIGH","LOW")

length(Data_P2$store_performance[Data_P2$store_performance=="HIGH"])
length(unique(Data_P2$U_CITY[Data_P2$store_performance=="HIGH"]))
length(unique(Data_P2$U_STATE[Data_P2$store_performance=="HIGH"]))
length(unique(Data_P2$SQUARE_FEET[Data_P2$store_performance=="HIGH"]))


data_decision_tree = Data_P2[,c("CYA21V001","PERC_CYEA07V001","PERC_CYEA07V004","store_performance","TOT_NUM_LEADS","NUM_CONVERTED_TO_AGREEMENT","URBANICITY","NUM_EMP_PAY_TYPE_H","MARKETING_EXP_2012","AVG_PAY_RATE_PAY_TYPE_H","AVG_PAY_RATE_PAY_TYPE_S","FRONTAGE_ROAD","SQUARE_FEET","NUM_CUST_ACC_REPS","TOT_ATTRITION_2013","NUM_STORE_MANAGERS")]

library(rpart)
library(rpart.plot)

#decision
dtm = rpart(store_performance~. , data_decision_tree, method = "class")
rpart.plot(dtm, type = 4, extra = 101)


write.csv(Data_P2,"./Cleaned_Data.csv")
write.csv(data_decision_tree,"./Desicion_Tree_Data.csv")
