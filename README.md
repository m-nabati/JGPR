# Joint Gaussian Process Regression (JGPR)
This is the implementation of this paper for multi-target regression problems, named Joint Gaussian Process Regression (JGPR).

# R Packages
We have tested the JGPR with **R 4.1.2** and the following versions of packages:
  
| package  | version |
| ------------- | ------------- |
| Deriv  | 4.1.3  |
| rdist  | 0.0.5  |
| progress  | 1.2.2  |
| R.utils  | 2.11.0  |

# Geting started with JGPR
The JGPR takes the following parameters and returns a trained model.
  - **inputs:** training inputs
  - **outputs:** training outputs (targets)
  - **kern:** user define kernel
  - **init.params:**  initializing of kernel parameters
  - **fix.noise:** If true the noise kernel is not optimized in the training phase
  - **inp:** preprocessing of inputs (it can be 'st' or 'norm', which show standardization and normalization process, respectively)
  - **out:** preprocessing of outputs (it can be 'st' or 'norm', which show standardization and normalization process, respectively)
  - **MaxIter:**  maximum Iteration for training
  - **break.tol:** if true Iteration stops with special condition
  - **e:** multiply parameter to Back tracking line search
  - **init.rate:** initialization rate of optimization

Then, the trained model can predict the targets of newly arrived test samples. The predict function takes the following parameters:
  - **newInputs:** new inputs for predicting the targets
  - **sigma.opt:** Noise kernel parameter (this parameter is optimized in the training process by default. However, it can be set manully here)


## example
```R
rm(list = ls()) #clear global environment
script.dir <- dirname(sys.frame(1)$ofile) #load current script directory
source(sprintf('%s/JGPR.R', script.dir)) #load JGPR from the current directory

#-------------------------------------------------------------------------------
#preparing data
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
noiseDev = 0.8
x = as.matrix(seq(0, 10, 0.5), ncol = 1)
y = matrix(0, nrow = length(x), ncol = 8)
set.seed(20*s)
y[, 1] = sin(x) + rnorm(length(x), 0, noiseDev)
set.seed(21*s)
y[, 2] = sin(x+0.2) + rnorm(length(x), 0, noiseDev)
set.seed(22*s)
y[, 3] = sin(x+0.4) + rnorm(length(x), 0, noiseDev)
set.seed(23*s)
y[, 4] = sin(x+0.6) + rnorm(length(x), 0, noiseDev)
set.seed(24*s)
y[, 5] = sin(x+0.8) + rnorm(length(x), 0, noiseDev)
set.seed(25*s)
y[, 6] = sin(x+1) + rnorm(length(x), 0, noiseDev)
set.seed(26*s)
y[, 7] = sin(x+1.2) + rnorm(length(x), 0, noiseDev)
set.seed(27*s)
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

#-------------------------------------------------------------------------------
#define kernel function
kern = quote(v1^2*exp(-d^2/v2^2))

#-------------------------------------------------------------------------------
#JGPR
model = JGPR(x, y, kern = kern, init.params = c(0.1, 1, 0.1), MaxIter = 100)
pred = model$predict(x.new)
```
![This is an image](https://github.com/m-nabati/JGPR/blob/main/Toy.svg)
Please note that JGPR act the same as conventional GPR (CGPR) for one-dimensional target.
 
# Kernel function
 The JGPR can be run with a user-defined kernel function. We can use ```v1, v2, ..., vm``` coefficients inside the ```quote``` function for defining a kernel. These coefficients are optimized in the training phase. Also, ```d``` and ```ip``` indicate the euclidean distance and inner product, respectively. In the following we have provided some examples of kernels with their code for defining the kernels.
 
| kernel  | code |
| ------------- | ------------- |
| <img src="https://render.githubusercontent.com/render/math?math=k(x_i, x_j) = v_1^2 exp({ - \dfrac{d^2(x_i , x_j)}{v_2^2}) }"> | ```quote(v1^2*exp(-d^2/v2^2))``` |
| <img src="https://render.githubusercontent.com/render/math?math=k(x_i, x_j) = v_1^2 x_i x_j">  | ```quote(v1^2*ip)```  |
| <img src="https://render.githubusercontent.com/render/math?math=k(x_i, x_j) = v_1^2 exp\left(- \dfrac{ sin^2(d(x_i, x_j) / v_2^2) }{v_3^2} \right)">  | ```quote(v1^2*exp(-(sin(d/v2^2))^2/v3^2))```  |

The ```v1, v2, ..., vm``` variables are optimized with initialization values, which is set in the ```init.params``` in the ```JGPR``` function. The ```init.params``` takes m+1 values, which the last value is initialization value of the noise kernel.
 
# Citation
Please cite JGPR in your publications if it helps your research. The following references are the BibTeX format of our paper and related letter for the JGPR.
```BibTeX
@ARTICLE{Nabati2021,
  author={Nabati, Mohammad and Ghorashi, Seyed Ali and Shahbazian, Reza},
  journal={IEEE Communications Letters}, 
  title={Joint Coordinate Optimization in Fingerprint-Based Indoor Positioning}, 
  year={2021},
  volume={25},
  number={4},
  pages={1192-1195},
  doi={10.1109/LCOMM.2020.3047352}}
}
```

 
