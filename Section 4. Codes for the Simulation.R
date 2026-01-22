# Transportability for replications in IS (01/20/2026)
# Section 4: Simulation

rm(list=ls())
n <- 1000000
set.seed(5)

# 1) Generate the source data (original experiment)
u1_s <- rnorm(n, 0, 1)  
W_source <- rbinom(n, 1, 0.2)
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(5*X_source + W_source + u1_s))))                             
Y_source <- 0.5*Z_source + 0.5*W_source + u1_s 

SimulatedData_source <- data.frame(X_source,Z_source,Y_source,W_source, u1_s)  

# 2) Generate the target data (replication experiment)
set.seed(5)
u1_t <- rnorm(n, 0, 1)
W_target <- rbinom(n, 1, 0.8)
X_target <- rbinom(n, 1, 0.5)     
Z_target <- rbinom(n, 1, 1/(1+exp(-(5*X_target + W_target + u1_t + 10*W_target*X_target))))
Y_target <- 0.5*Z_target + 0.5*W_target + u1_t

SimulatedData_target <- data.frame(X_target,Z_target,Y_target,W_target, u1_t)

# 3) Task 1: Comparing ATEs of Source and Target
# 3-1) Source
model_ref_source <- lm(Y_source ~ X_source, data = SimulatedData_source)
summary(model_ref_source)

# 3-2) Target
model_ref_target <- lm(Y_target ~ X_target, data = SimulatedData_target)
summary(model_ref_target)

# 3-3) Compare the ATEs using t-test
SimulatedData_source$Population <- 0
SimulatedData_target$Population <- 1
Temp1 <- SimulatedData_source[,c('X_source','Y_source','Population')]
colnames(Temp1) <- c('X','Y','Pop')
Temp2 <- SimulatedData_target[,c('X_target','Y_target','Population')]
colnames(Temp2) <- c('X','Y','Pop')
SimulatedData_combined <- rbind(Temp1,Temp2)
SimulatedData_combined$X_Pop <- SimulatedData_combined$X*SimulatedData_combined$Pop
model_ref_combined <- lm(Y ~ Pop + X + X_Pop, data = SimulatedData_combined)
summary(model_ref_combined)

# 4) Task 2: Replicating ATE using the transportability framework 
# 4-1) P(y|do(x), w)
P_y_x0_w0 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[1,3]
P_y_x1_w0 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[2,3]
P_y_x0_w1 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[3,3]
P_y_x1_w1 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[4,3]

# 4-2) P*(W)
P_w0 <- table(W_target)[1]/n
P_w1 <- table(W_target)[2]/n

# 4-3) P*(y|do(X=0)) and P*(y|do(X=1))
P_y_x0 <- P_y_x0_w0*P_w0 + P_y_x0_w1*P_w1
P_y_x1 <- P_y_x1_w0*P_w0 + P_y_x1_w1*P_w1

# 4-4) ATE in target
P_y_x1-P_y_x0

# 4-5) Calculate the SE of the transportability estimates using the bootstrapping method
# 4-5-1) Resampling and calculate the estimates
SimulatedData_source_cntrl <- SimulatedData_source[which(X_source==0),]
SimulatedData_source_trtmt <- SimulatedData_source[which(X_source==1),]

B <- 10000
Boot_transportability_estimate <- c()
for (i in 1:B) {
  n <- 1000000
  Bootsample_cntrl_indicies <- sample(1:nrow(SimulatedData_source_cntrl), size = 500169, replace = TRUE)
  Bootsample_cntrl <- SimulatedData_source_cntrl[Bootsample_cntrl_indicies, ]
  
  Boostample_trtmt_indicies <- sample(1:nrow(SimulatedData_source_trtmt), size = 499831, replace = TRUE)
  Boostample_trtmt <- SimulatedData_source_trtmt[Boostample_trtmt_indicies, ]
  
  Bootsample_source <- rbind(Bootsample_cntrl,Boostample_trtmt)
  
  Bootsample_W_target <- sample(SimulatedData_target$W_target, size = nrow(SimulatedData_target), replace = TRUE)
  
  # P(y|do(x), w)
  P_y_x0_w0 <- aggregate(Y_source ~ X_source + W_source, data=Bootsample_source, mean)[1,3]
  P_y_x1_w0 <- aggregate(Y_source ~ X_source + W_source, data=Bootsample_source, mean)[2,3]
  P_y_x0_w1 <- aggregate(Y_source ~ X_source + W_source, data=Bootsample_source, mean)[3,3]
  P_y_x1_w1 <- aggregate(Y_source ~ X_source + W_source, data=Bootsample_source, mean)[4,3]
  
  # P*(W)
  P_w0 <- table(Bootsample_W_target)[1]/n
  P_w1 <- table(Bootsample_W_target)[2]/n
  
  # P*(y|do(X=0)) and P*(y|do(X=1))
  P_y_x0 <- P_y_x0_w0*P_w0 + P_y_x0_w1*P_w1
  P_y_x1 <- P_y_x1_w0*P_w0 + P_y_x1_w1*P_w1
  
  # ATE in target
  Boot_transportability_estimate[i] <- P_y_x1-P_y_x0
}

hist(Boot_transportability_estimate)
sd(Boot_transportability_estimate)     # se = 0.00382655. Note: it took 1 day to complete the estimation.

# 5) Task 3: Replicating subgroup-specific effects using the transportability framework
# 5-1) OLS
model_ref_target_Wspecific <- lm(Y_target ~ X_target + X_target + X_target*W_target, data = SimulatedData_target)
summary(model_ref_target_Wspecific)

# 5-2) Calculating the Std of the CATE when W=1 using either Wooldridge method or delta method.
# 5-2-1) method in Introductory Econometrics: A Modern Approach by Wooldridge.(refer to my note in the notebook)
SimulatedData_target$Combined_XandW <- SimulatedData_target$X_target*SimulatedData_target$W_target-SimulatedData_target$W_target
model_ref_target_Wspecific_combined <- lm(Y_target ~ X_target + W_target + Combined_XandW, data = SimulatedData_target)
summary(model_ref_target_Wspecific_combined)

# 5-2-2) Delta method
cov_matrix <- vcov(model_ref_target_Wspecific)
print(cov_matrix)
cov_matrix["X_target:W_target", "W_target"]
variance_of_combined_effect <- 1.403662e-05 + 2.808697e-05 + 2*-1.403662e-05
sqrt(variance_of_combined_effect)

# 5-3) transportability estimates
P_y_x1_w0-P_y_x0_w0
P_y_x1_w1-P_y_x0_w1



