rm(list = ls()) #clear global environment
script.dir <- dirname(sys.frame(1)$ofile) #load current script directory
source(sprintf('%s/JGPR.R', script.dir)) #load JGPR from the current directory

#-------------------------------------------------------------------------------
#preparing data

x.tr = as.matrix(seq(0, 10, 0.5), ncol = 1) #training inputs
x.ts = as.matrix(seq(0, 10, 0.01), ncol = 1) #test inputs

y.tr = matrix(0, nrow = length(x.tr), ncol = 8) #real values of training outputs
y.trn = matrix(0, nrow = length(x.tr), ncol = 8) #training outputs + noise
y.ts = matrix(0, nrow = length(x.ts), ncol = 8) #test outputs

noiseDev = 0.8
s = 11111

for (i in 1:8) {
  set.seed((20+i-1)*s)
  
  y.tr[, i] = sin(x.tr + 0.2*(i-1))
  y.trn[, i] = sin(x.tr + 0.2*(i-1)) + rnorm(length(x.tr), 0, noiseDev)
  y.ts[, i] = sin(x.ts + 0.2*(i-1))

}

#-------------------------------------------------------------------------------
#define kernel function
kern = quote(v1^2*exp(-d^2/v2^2))

#-------------------------------------------------------------------------------
#JGPR
model = JGPR(x.tr, y.trn, kern = kern, init.params = c(0.1, 1, 0.1), MaxIter = 100)
pred = model$predict(x.ts)

#-------------------------------------------------------------------------------
#CGPR or conventional GPR (if we put one-dimensional data to the JGPR it acts the same as CGPR)
models = vector("list", 8)
preds = vector("list", 8)

for(i in 1:8){
	models[[i]] = JGPR(x.tr, y.trn[,i], kern = kern, init.params = c(0.1, 1, 0.1), MaxIter = 100)
	preds[[i]] = models[[i]]$predict(x.ts)
}

#-------------------------------------------------------------------------------
#plot
ylabs = c(expression(sin(x.tr)), expression(sin(x.tr+0.2)), expression(sin(x.tr+0.4)), expression(sin(x.tr+0.6)),
          expression(sin(x.tr+0.8)), expression(sin(x.tr+1)), expression(sin(x.tr+1.2)), expression(sin(x.tr+1.4)))

cairo_pdf(sprintf("%s/Toy.pdf", script.dir) , width = 25, height = 15)
layout(matrix(c(1,2,3,4,5,6,7,8,9,9,9,9), ncol = 4, byrow = TRUE), heights=c(1, 1, 0.2))
par(mai=rep(1.2, 4), mgp=c(5,2,0), family = "serif")
for(i in 1:8){
  plot(x.ts, sin(x.ts+(i-1)*0.2), type = "l", col = "green", ylim = c(-2.5, 2.5), xlab = expression(x), ylab = ylabs[i], lty = 1, cex=4, cex.axis = 4, cex.lab = 4)
  lines(x.ts, preds[[i]]$pred.mu, type = "l", col = "deeppink", lty = 3, lwd = 4)
  lines(x.ts, pred$pred.mu[,i], type = "l", col = "blue", lty = 2, lwd = 5)
  points(x.tr, y.trn[,i], col = "cyan", pch = 19, cex = 2)
  title(i, cex.main=4)
  grid()
}
par(mar = c(0,0,0,0), family = "serif")
plot.new()
legend(x="center", ncol = 4, legend=c("Real function", "Real function + Noise", "CGPR", "JGPR"), text.width=c(0.15, 0.2, 0.2, 0.15),
       col = c("green", "cyan", "deeppink", "blue"), lty = c(1, NA, 3, 2), lwd = c(1,1,4,5), pch = c(NA, 19, NA, NA), seg.len = 5, cex=4)
dev.off()