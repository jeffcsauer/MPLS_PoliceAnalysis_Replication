library(sf)
library(geepack)
library(gee)
library(ncpen)

expit <- function(x){return(exp(x)/(1 + exp(x)))}

#D:\Dropbox\Minneapolis Police Data
#MplsCleaned <- read.csv("D:/Dropbox/Minneapolis Police Data/Data/processed/MplsCleanedThrough2021.csv")
MplsCleaned <- read.csv("MplsCleanedThrough2021.csv")

df = data.frame(MplsCleaned)

df = df[df$personSearch != "",]
df = df[df$vehicleSearch != "",]

############  Data Processing ###########

df$date = as.Date(df$date)

df$postFloyd = df$date > "2020-05-25"   # George Floyd murdered May 25th 2020

df$personSearch = as.numeric(as.factor(df$personSearch)) - 1
df$vehicleSearch = as.numeric(as.factor(df$vehicleSearch)) - 1

df$raceRecode = factor(df$raceRecode, levels = c('White', 'Black', 'Latino', 'Native American', 'Asian', 'Other', 'Unknown'))
df$neighborhood = as.factor(df$neighborhood)

df$day = as.numeric(df$date) - as.numeric(min(as.Date(df$date)))

df = df[order(df$neighborhood),]

df$reason[which(is.na(df$reason))] = "Unknown"

df$gender = factor(df$gender)
df$problem = factor(df$problem)
df$reason = factor(df$reason)
df$preRaceRecode = factor(df$preRaceRecode)

#### One-Step Gee Function  #######

one_step_gee <- function(formula, cluster, floyd_interaction, race_interaction, df){
  
  outcome = all.vars(formula)[1] # Outcome variable in formula
  
  covariates = all.vars(formula)[2:length(all.vars(formula))] # Covariates in formula
  
  glm_model = glm(formula, family = "binomial", data = df) # Fit GLM model
  beta_glm = coef(glm_model)    # GLM coefficients
  
  cluster_lvls = levels(factor(df[,cluster])) # Cluster Levels
  
  p = length(beta_glm)     # Dimension of beta
  N = length(cluster_lvls) # Number of cluster levels
  
  n = rep(NA, N)  # Number of observations per cluster
  ss_e = rep(NA, N)  # ((sum e_ij)^2 - sum(e_ij^2))/2
  
  # Create design matrix
  x = rep(1, dim(df)[1])
  for (j in covariates){
    if (class(df[,j]) == "factor"){
      x = cbind(x, to.indicators(df[,j]))
    } else {
      x = cbind(x, df[,j])
    }
  }
  if (!is.na(race_interaction)){
    for (j in race_interaction){
      if (class(df[,j]) != "factor"){
        x = cbind(x, to.indicators(df[,"raceRecode"])*df[,j])
      } 
    }
  }
  if (!is.na(floyd_interaction)){
    for (j in floyd_interaction){
      if (class(df[,j]) == "factor"){
        x = cbind(x, df[,"postFloyd"]*to.indicators(df[,j]))
      } else {
        x = cbind(x, df[,"postFloyd"]*df[,j])
      }
    }
  }
  
  i_cntr = 0  # Counter for loop
  for (i in cluster_lvls){
    i_cntr = i_cntr + 1
    
    df_i = df[which(df[,cluster] == i),] # Create subset of cluster i
    
    n_i = dim(df_i)[1]  # Get size of cluster i
    n[i_cntr] = n_i     # Store size of cluster i 
    
    mu_i = predict(glm_model, newdata = df_i, type = 'response') # Expected outcome for cluster i

    Y_i = df_i[,outcome] # Outcome for cluster i
    
    v_i = mu_i*(1 - mu_i) # Estimated variance
    
    e_i = (Y_i - mu_i)/sqrt(v_i)  # standardized residual

    ss_e[i_cntr] = (sum(e_i)^2 - sum(e_i^2))/2
  }
  
  rho_hat = sum(ss_e)/(sum(n*(n-1))/2) # Estimate of rho
  
  sum_e_i = rep(NA, N)  # Vector of sum(e_ij)
  
  A_i = matrix(NA, nrow = N, ncol = p)  #
  W = matrix(0, nrow = p, ncol = p)  # 
  
  u = rep(0, p)
  
  i_cntr = 0
  for (i in cluster_lvls){
    
    i_cntr = i_cntr + 1
    
    df_i = df[which(df[,cluster] == i),] # Create subset of cluster i
    
    n_i = dim(df_i)[1]  # Get size of cluster i

    mu_i = predict(glm_model, newdata = df_i, type = 'response') # Expected outcome for cluster i
    
    Y_i = df_i[,outcome] # Outcome for cluster i
    
    v_i = mu_i*(1 - mu_i) # Estimated variance
    
    e_i = (Y_i - mu_i)/sqrt(v_i)  # standardized residual
    
    sum_e_i[i_cntr] = sum(e_i)  # Store sum of e_ij
    
    x_i = x[which(df[,cluster] == i),] # Design matrix for just cluster obs
    
    d_i = mu_i*(1 - mu_i)*x_i
    
    A_i[i_cntr,] = colSums(d_i/sqrt(v_i))
    
    # Calulate W_i
    
    B_i = (rho_hat/(1-rho_hat + n_i*rho_hat))*
      colSums(d_i/sqrt(v_i))%*%t(colSums(d_i/sqrt(v_i)))
    
    C_i = matrix(0, nrow = p, ncol = p)
    
    for (j in 1:n_i){
      #d_i_j = mu_i[j]*(1 - mu_i[j])*x_i[j,]
      d_i_j = as.matrix(d_i[j,])
    
      if (dim(d_i_j)[1] ==  1){
        C_i = C_i + t(d_i_j)%*%d_i_j/v_i[j] 
      } else {
        C_i = C_i + d_i_j%*%t(d_i_j)/v_i[j] 
      }
    }
    
    #d_i*((Y_i - mu_i)/v_i)
    
    W_i = C_i - B_i
    
    W = W + W_i
  
  }
  
  beta_gee = beta_glm - solve(W)%*%colSums(A_i*sum_e_i*rho_hat/(1 - rho_hat + n*rho_hat))  
  
  # Calculate covariance matrix
  
  U_beta_gee =  matrix(0, nrow = p, ncol = p)
  W_beta_gee = matrix(0, nrow = p, ncol = p)
  
  i_cntr = 0
  for (i in cluster_lvls){
    
    i_cntr = i_cntr + 1
    
    df_i = df[which(df[,cluster] == i),] # Create subset of cluster i
    
    n_i = dim(df_i)[1]  # Get size of cluster i
    
    x_i = x[which(df[,cluster] == i),] # Design matrix for just cluster obs

    mu_i = expit(as.matrix(x_i)%*%beta_gee)[,1]
        
    Y_i = df_i[,outcome] # Outcome for cluster i    
    
    v_i = mu_i*(1 - mu_i)
    
    e_i = (Y_i - mu_i)/sqrt(v_i)
    
    d_i = mu_i*(1 - mu_i)*x_i
    
    B_i = (rho_hat/(1-rho_hat + n_i*rho_hat))*
      colSums(d_i/sqrt(v_i))%*%t(colSums(d_i/sqrt(v_i)))
    
    C_i = matrix(0, nrow = p, ncol = p)
    
    for (j in 1:n_i){
      d_i_j = as.matrix(d_i[j,])
      
      if (dim(d_i_j)[1] ==  1){
        C_i = C_i + t(d_i_j)%*%d_i_j/v_i[j] 
      } else {
        C_i = C_i + d_i_j%*%t(d_i_j)/v_i[j] 
      }
    }
    
    W_i = C_i - B_i
    
    W_beta_gee = W_beta_gee + W_i
    
    u_i = colSums(d_i*((Y_i - mu_i)/v_i)) - (rho_hat/(1-rho_hat + n_i*rho_hat))*colSums(d_i/sqrt(v_i))*sum(e_i)
    
    U_beta_gee = U_beta_gee + u_i%*%t(u_i)
  }
  
  W_inv = solve(W_beta_gee)
  
  cov_gee = W_inv%*%U_beta_gee%*%W_inv

  list_results = list(beta_gee, cov_gee)
  names(list_results) <- c("beta", "variance")
  
  return(list_results)
}

gee_uni_pers = one_step_gee(personSearch ~ raceRecode + postFloyd + 
                              postFloyd:raceRecode, "neighborhood", "raceRecode", NA, df)

or_uni_pers = exp(gee_uni_pers$beta[2:7])
se_uni_pers = sqrt(diag(gee_uni_pers$variance)[2:7]) 
ci_u_uni_pers = exp(gee_uni_pers$beta[2:7] + qnorm(0.975)*se_uni_pers)
ci_l_uni_pers = exp(gee_uni_pers$beta[2:7] - qnorm(0.975)*se_uni_pers)


or_uni_pers_post = exp(gee_uni_pers$beta[2:7] + gee_uni_pers$beta[9:14])
se_uni_pers_post = sqrt((diag(gee_uni_pers$variance)[2:7] + diag(gee_uni_pers$variance)[9:14] 
                         + 2*diag(gee_uni_pers$variance[2:7,9:14])))
ci_u_uni_pers_post = exp(gee_uni_pers$beta[2:7] + gee_uni_pers$beta[9:14] + qnorm(0.975)*se_uni_pers_post)
ci_l_uni_pers_post = exp(gee_uni_pers$beta[2:7] + gee_uni_pers$beta[9:14] - qnorm(0.975)*se_uni_pers_post)

gee_uni_veh = one_step_gee(vehicleSearch ~ raceRecode + postFloyd + 
                             postFloyd:raceRecode, "neighborhood", "raceRecode", NA, df)

or_uni_veh = exp(gee_uni_veh$beta[2:7])
se_uni_veh = sqrt(diag(gee_uni_veh$variance)[2:7])
ci_u_uni_veh = exp(gee_uni_veh$beta[2:7] + qnorm(0.975)*se_uni_veh)
ci_l_uni_veh = exp(gee_uni_veh$beta[2:7] - qnorm(0.975)*se_uni_veh)

or_uni_veh_post = exp(gee_uni_veh$beta[2:7] + gee_uni_veh$beta[9:14])
se_uni_veh_post = sqrt((diag(gee_uni_veh$variance)[2:7] + diag(gee_uni_veh$variance)[9:14] 
                         + 2*diag(gee_uni_veh$variance[2:7,9:14])))
ci_u_uni_veh_post = exp(gee_uni_veh$beta[2:7] + gee_uni_veh$beta[9:14] + qnorm(0.975)*se_uni_veh_post)
ci_l_uni_veh_post = exp(gee_uni_veh$beta[2:7] + gee_uni_veh$beta[9:14] - qnorm(0.975)*se_uni_veh_post)

gee_multi_pers = one_step_gee(personSearch ~ raceRecode + gender + preRaceRecode +
                                    problem + reason + veilAfterSunset + lat + long  +
                                    postFloyd + postFloyd:raceRecode,
                                   "neighborhood", "raceRecode", NA, df)

or_multi_pers = exp(gee_multi_pers$beta[2:7])
se_multi_pers = sqrt(diag(gee_multi_pers$variance)[2:7])
ci_u_multi_pers = exp(gee_multi_pers$beta[2:7] + qnorm(0.975)*se_multi_pers)
ci_l_multi_pers = exp(gee_multi_pers$beta[2:7] - qnorm(0.975)*se_multi_pers)


or_multi_pers_post = exp(gee_multi_pers$beta[2:7] + gee_multi_pers$beta[28:33])
se_multi_pers_post = sqrt((diag(gee_multi_pers$variance)[2:7] + diag(gee_multi_pers$variance)[28:33] 
                         + 2*diag(gee_multi_pers$variance[2:7,28:33])))
ci_u_multi_pers_post = exp(gee_multi_pers$beta[2:7] + gee_multi_pers$beta[28:33] + qnorm(0.975)*se_multi_pers_post)
ci_l_multi_pers_post = exp(gee_multi_pers$beta[2:7] + gee_multi_pers$beta[28:33] - qnorm(0.975)*se_multi_pers_post)


gee_multi_veh = one_step_gee(vehicleSearch ~ raceRecode + gender + preRaceRecode +
                               problem + reason + veilAfterSunset + lat + long  +
                               postFloyd + postFloyd:raceRecode,
                             "neighborhood", "raceRecode", NA, df)

or_multi_veh = exp(gee_multi_veh$beta[2:7])
se_multi_veh = sqrt(diag(gee_multi_veh$variance)[2:7])
ci_u_multi_veh = exp(gee_multi_veh$beta[2:7] + qnorm(0.975)*se_multi_veh)
ci_l_multi_veh = exp(gee_multi_veh$beta[2:7] - qnorm(0.975)*se_multi_veh)

or_multi_veh_post = exp(gee_multi_veh$beta[2:7] + gee_multi_veh$beta[28:33])
se_multi_veh_post = sqrt((diag(gee_multi_veh$variance)[2:7] + diag(gee_multi_veh$variance)[28:33] 
                           + 2*diag(gee_multi_veh$variance[2:7,28:33])))
ci_u_multi_veh_post = exp(gee_multi_veh$beta[2:7] + gee_multi_veh$beta[28:33] + qnorm(0.975)*se_multi_veh_post)
ci_l_multi_veh_post = exp(gee_multi_veh$beta[2:7] + gee_multi_veh$beta[28:33] - qnorm(0.975)*se_multi_veh_post)
