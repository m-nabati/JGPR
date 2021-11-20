rm(list = ls()) #clear global environment

#load current script directory
script.dir <- dirname(sys.frame(1)$ofile)
source(sprintf('%s/JGPR.R', script.dir))

devs = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2)
iter = 30

aRMSE.vs.dev.jgpr = matrix(0, nrow = iter, ncol = length(devs))
aRRMSE.vs.dev.jgpr = matrix(0, nrow = iter, ncol = length(devs))
aRMSE.vs.dev.cgpr = matrix(0, nrow = iter, ncol = length(devs))
aRRMSE.vs.dev.cgpr = matrix(0, nrow = iter, ncol = length(devs))

RRMSE = function(y, y.pred, y.tr){
  y.m = apply(y.tr, 2, mean)
  rrmse = sqrt(colSums((y-y.pred)^2) / colSums(sweep(y, 2, y.m, "-")^2) )
  return(rrmse)
}

for(m in 1:iter){
  
  for(j in 1:length(devs)){
    
    x.new = as.matrix(seq(0, 10, 0.01), ncol = 1)
    y.r = matrix(0, nrow = length(x.new), ncol = 8)
    y.r[, 1] = sin(x.new)
    y.r[, 2] = sin(x.new+0.2)
    y.r[, 3] = sin(x.new+0.4)
    y.r[, 4] = sin(x.new+0.6)
    y.r[, 5] = sin(x.new+0.8)
    y.r[, 6] = sin(x.new+1)
    y.r[, 7] = sin(x.new+1.2)
    y.r[, 8] = sin(x.new+1.4)
    
    s = 11111;
    noiseDev = devs[j]
    x = as.matrix(seq(0, 10, 0.5), ncol = 1)
    y = matrix(0, nrow = length(x), ncol = 8)
    set.seed(20*s*j+m)
    y[, 1] = sin(x) + rnorm(length(x), 0, noiseDev)
    set.seed(21*s*j+m)
    y[, 2] = sin(x+0.2) + rnorm(length(x), 0, noiseDev)
    set.seed(22*s*j+m)
    y[, 3] = sin(x+0.4) + rnorm(length(x), 0, noiseDev)
    set.seed(23*s*j+m)
    y[, 4] = sin(x+0.6) + rnorm(length(x), 0, noiseDev)
    set.seed(24*s*j+m)
    y[, 5] = sin(x+0.8) + rnorm(length(x), 0, noiseDev)
    set.seed(25*s*j+m)
    y[, 6] = sin(x+1) + rnorm(length(x), 0, noiseDev)
    set.seed(26*s*j+m)
    y[, 7] = sin(x+1.2) + rnorm(length(x), 0, noiseDev)
    set.seed(27*s*j+m)
    y[, 8] = sin(x+1.4) + rnorm(length(x), 0, noiseDev)
    
    
    y.real = matrix(0, nrow = length(x), ncol = 8)
    y.real[, 1] = sin(x)
    y.real[, 2] = sin(x+0.2)
    y.real[, 3] = sin(x+0.4)
    y.real[, 4] = sin(x+0.6)
    y.real[, 5] = sin(x+0.8)
    y.real[, 6] = sin(x+1)
    y.real[, 7] = sin(x+1.2)
    y.real[, 8] = sin(x+1.4)
    
    kern = quote(v1^2*exp(-d^2/v2^2))
    model = JGPR(x, y, kern = kern, init.params = c(0.1, 1, 0.1), MaxIter = 100)
    pred = model$predict(x.new)
    
    # matplot(y.r, type = "l")
    # matplot(pred$pred.mu, type = "l")
    #-------------------------------------------------------------------------------
    models = vector("list", 8)
    preds = vector("list", 8)
    
    for(i in 1:8){
      models[[i]] = JGPR(x, y[,i], kern = kern, init.params = c(0.1, 1, 0.1), MaxIter = 100)
      preds[[i]] = models[[i]]$predict(x.new)
    }
    
    #------------------------------------------------------------------------------
    RMSE = matrix(0, ncol = 8, nrow = 2)
    
    for(i in 1:8){
      RMSE[1,i] = sqrt(mean((pred$pred.mu[,i] - sin(x.new+0.2*(i-1)))^2)) #JGPR
      RMSE[2,i] = sqrt(mean((preds[[i]]$pred.mu - sin(x.new+0.2*(i-1)))^2)) #CGP
    }
    
    aRMSE.vs.dev.jgpr[m, j] = mean(RMSE[1,i])
    aRMSE.vs.dev.cgpr[m, j] = mean(RMSE[2,i])
    #------------------------------------------------------------------------------
    
    y.pred.cgpr = matrix(0, ncol = 8, nrow = 1001)
    
    for(i in 1:8){
      y.pred.cgpr[,i] = preds[[i]]$pred.mu
    }
    
    rrmses.jgpr = RRMSE(y.r, pred$pred.mu, y)
    rrmses.cgpr = RRMSE(y.r, y.pred.cgpr, y)
    
    aRRMSE.vs.dev.jgpr[m, j] = mean(rrmses.jgpr)
    aRRMSE.vs.dev.cgpr[m, j] = mean(rrmses.cgpr)
    
  }
}

#---------------------------------------------------------------------------------
cairo_pdf(sprintf("%s/NOiseVsDev.pdf", script.dir) , width = 25, height = 15)
par(mai=rep(2, 4), mgp=c(7,2,0), family = "serif")
plot(devs, apply(aRRMSE.vs.dev.cgpr, 2, mean), type = "b", col = "red", pch = 15, xlab = expression("Noise deviant "(sigma)), ylab = "aRRMSE", cex=4, cex.axis = 4, cex.lab = 4)
lines(devs, apply(aRRMSE.vs.dev.jgpr, 2, mean), type = "b", col = "blue", pch = 19, cex=4, cex.axis = 4, cex.lab = 4)
grid()
legend(x="topleft", legend=c("CGRP", "JGPR"), text.width=c(0.15, 0.2),
       col = c("red", "blue"), lwd = c(1,1), pch = c(15, 19), cex=4)
dev.off()
