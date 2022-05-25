# Joint Gaussian Process Regression (JGPR)
This is the implementation of [this paper](https://link.springer.com/article/10.1007/s10994-022-06170-3) for multi-target regression problems, named Joint Gaussian Process Regression (JGPR).

# R Packages
We have tested the JGPR with **R 4.1.2** and the following packages:
  
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
  - **kern:** user-defined kernel
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


## Example
to load the ```JGPR``` function from your current directory you can use the following code:
```R
script.dir <- dirname(sys.frame(1)$ofile) #load current script directory
source(sprintf('%s/JGPR.R', script.dir)) #load JGPR from the current directory
```
The following code is a part of ```example.R``` file.
```R
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
```
The following figure shows the result of using JGPR and conventional GPR in the toy multi-target regression problem. To get this figure please run the ```example.R``` file.
![result](https://github.com/m-nabati/JGPR/blob/main/Toy.svg)
Please note that JGPR acts the same as conventional GPR (CGPR) for a one-dimensional target.
 
# Kernel function
The JGPR can be run with a user-defined kernel function. We can use ```v1, v2, ..., vm``` coefficients inside the ```quote``` function for defining a kernel. These coefficients are optimized in the training phase. Also, ```d``` and ```ip``` indicate the euclidean distance and inner product, respectively. We have provided some examples of kernels with their respected code.
 
| kernel  | code |
| ------------- | ------------- |
| <img src="https://render.githubusercontent.com/render/math?math=k(x_i, x_j) = v_1^2 exp({ - \dfrac{d^2(x_i , x_j)}{v_2^2}) }"> | ```quote(v1^2*exp(-d^2/v2^2))``` |
| <img src="https://render.githubusercontent.com/render/math?math=k(x_i, x_j) = v_1^2 x_i x_j">  | ```quote(v1^2*ip)```  |
| <img src="https://render.githubusercontent.com/render/math?math=k(x_i, x_j) = v_1^2 exp\left(- \dfrac{ sin^2(d(x_i, x_j) / v_2^2) }{v_3^2} \right)">  | ```quote(v1^2*exp(-(sin(d/v2^2))^2/v3^2))```  |

The ```v1, v2, ..., vm``` variables are optimized with initialization values, which is set in the ```init.params``` in the ```JGPR``` function. The ```init.params``` takes ```m+1``` values, which the last value is initialization value of the noise kernel. For instance, if we have ```v1``` and ```v2``` for defination of the kernel, the ```init.params``` takes three parameters (e.g., ```c(1, 1, 0.05)```).
 
# Citation
Please cite JGPR in your publications if it helps your research. The following references are the BibTeX format of our paper and related letter to the JGPR.
```BibTeX
@ARTICLE{Nabati2022,
  author={Nabati, Mohammad and Ghorashi, Seyed Ali and Shahbazian, Reza},
  journal={Machine Learning}, 
  title={JGPR: a computationally efficient multi-target Gaussian process regression algorithm}, 
  year={2022}}
}
```
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

 
