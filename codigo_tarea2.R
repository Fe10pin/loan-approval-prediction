setwd("~/Downloads")
datos <- read.csv("Loan.csv")
datos$Education <- as.factor(datos$Education)
str(datos)

modeloderegresion <- lm(Loan~.,data = datos)
summary(modeloderegresion)

y_hat <- fitted(modeloderegresion)
y_hat
which(y_hat > 1)
which(y_hat < 0)
clientes_mayor_1 <- datos[y_hat > 1, ]
clientes_menor_0 <- datos[y_hat < 0, ]
clientes_mayor_1
clientes_menor_0
min(y_hat)
max(y_hat)

modelologit <- glm(Loan~., family = binomial(link = logit), data = datos)
summary(modelologit)

y_hat_logit <- predict(modelologit, type="response")
head(y_hat_logit)
umbral <- mean(datos$Loan)
y_pred <- ifelse(y_hat_logit >= umbral, 1, 0)
y_pred
matriz_confusion <- table(Real = datos$Loan, Predicho = y_pred)
matriz_confusion
pcp_global <- mean(y_pred == datos$Loan) * 100
pcp_global

mean(datos$Income)
coef_logit <- coef(modelologit)
beta_1 <- coef_logit["Income"]
beta_1
