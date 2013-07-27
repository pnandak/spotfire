#

source("assign1dat.R");

eps <- 1e-12;

load(".Rdata.assign1d");

names(model2);
model2$value;
model2$par;
eigen(model2$hessian)$val;
Hinv2 <- solve(model2$hessian);
cbind(model2$par,sqrt(diag(Hinv2)));
# rho
print(rhovec <- 2*(1/(1+exp(-model2$par[length(model2$par)])))-1);

stepfunc2(model2$par);

stepfunc2i <- function(b, YY=Y, XX2=X2, XX3=X3) {
  b2 <- b[1:(dim(XX2)[2])];
  b3 <- b[((dim(XX2)[2])+(1:(dim(XX3)[2])))];
  rho <- 2*(1/(1+exp(-b[length(b)])))-1;
  Z2 <- XX2 %*% b2;
  Z3 <- XX3 %*% b3;
  g0 <- pnorm(-Z2);
  h0 <- stN2cdf(cbind(-Z2,-Inf),cbind(Inf,-Z3), rho=rho);
  h1 <- stN2cdf(cbind(-Z2,-Z3),matrix(Inf,length(Z2),2), rho=rho);
  -log(ifelse(YY[,1]==1,g0, ifelse(YY[,2]==1,h1,ifelse(YY[,3]==1,h0,NA))));
}

grad2 <- matrix(NA, dim(Y)[1], length(model2$par));
B0 <- model2$par;
L2i <- stepfunc2i(B0);
for (j in 1:length(model2$par)) {
  B0j <- B0;
  B0j[j] <- B0[j] + eps;
  L2ij <- stepfunc2i(B0j);
  grad2[,j] <- (L2ij-L2i)/eps
}
apply(grad2,2,sum);
opg2 <- t(grad2) %*% grad2;
eigen(opg2)$value;
mat2 <- cbind(model2$par,sqrt(diag(Hinv2)),sqrt(diag(Hinv2 %*% opg2 %*% Hinv2)));
dimnames(mat2)[[2]] <- c("est","invH","sand");
mat2;

names(model3);
model3$value;
model3$par;
eigen(model3$hessian)$val;
q(save="no");
Hinv3 <- solve(model2$hessian);
cbind(model2$par,sqrt(diag(Hinv3)));
# rho
print(rhovec <- 2*(1/(1+exp(-model3$par[length(model3$par)-(2:0)])))-1);

stepfunc3(model3$par, YY=W);

stepfunc3i <- function(b, YY=Y, XX1=X1, XX2=X2, XX3=X3) {
  b1 <- b[1:(dim(XX1)[2])];
  b2 <- b[(dim(XX1)[2])+(1:(dim(XX2)[2]))];
  b3 <- b[(dim(XX1)[2])+(dim(XX2)[2])+(1:(dim(XX3)[2]))];
  r12 <- 2*(1/(1+exp(-b[length(b)-2])))-1;
  r13 <- 2*(1/(1+exp(-b[length(b)-1])))-1;
  r23 <- 2*(1/(1+exp(-b[length(b)])))-1;
  sigma <- matrix(c(1,r12,r13,r12,1,r23,r13,r23,1),3,3);
  Z1 <- XX1 %*% b1;
  Z2 <- XX2 %*% b2;
  Z3 <- XX3 %*% b3;
  Infmat <- array(-Inf,dim(cbind(Z1,Z2)));
  g00 <- stN2cdf(Infmat,cbind(-Z1,-Z2), rho=r12);
  g10 <- stN2cdf(cbind(-Z1,-Inf),cbind(Inf,-Z2), rho=r12);
  h10 <- stN3cdf(cbind(-Z1,-Z2,-Inf),cbind(Inf,Inf,-Z3), sigma=sigma);
  Infmat <- array(Inf,dim(cbind(Z1,Z2,Z3)));
  h11 <- stN3cdf(cbind(-Z1,-Z2,-Z3),Infmat, sigma=sigma);
  -log(ifelse(YY[,2]==1,ifelse(YY[,1]==1,g00,g10), ifelse(YY[,3]==1,h11,h10)));
}

grad3 <- matrix(NA, dim(Y)[1], length(model3$par));
B0 <- model3$par;
L3i <- stepfunc3i(B0, YY=W);
for (j in 1:length(model3$par)) {
  B0j <- B0;
  B0j[j] <- B0[j] + eps;
  L3ij <- stepfunc3i(B0j, YY=W);
  grad3[,j] <- (L3ij-L3i)/eps
}
apply(grad3,2,sum);
opg3 <- t(grad3) %*% grad3;
eigen(opg3)$value;
mat3 <- cbind(model3$par,sqrt(diag(Hinv3)),sqrt(diag(Hinv3 %*% opg3 %*% Hinv3)));
dimnames(mat3)[[2]] <- c("est","invH","sand");
mat3;
