# Joint Gaussian Process Regression (JGPR)
This is the implementation of this paper for multi-target regression problems, named Joint Gaussian Process Regression (JGPR).

# R Packages
   - Deriv
   - rdist
   - progress
   - R.utils

# Geting started with JGPR
The JGPR takes the following parameters and returns a trained model.
  - **inputs:** training inputs
  - **outputs:** training outputs (targets)
  - **kern:** user define kernel
  - **init.params:**  initializing of kernel parameters
  - **inp:** preprocessing of inputs (it can be 'st' or 'norm', which show standardization and normalization process, respectively)
  - **out:** preprocessing of outputs (it can be 'st' or 'norm', which show standardization and normalization process, respectively)
  - **MaxIter:**  maximum Iteration for training
  - **break.tol:** if true Iteration stops with special condition
  - **e:** multiply parameter to Back tracking line search

Then, the trained model can predict the targets of newly arrived test samples. The predict function takes the following parameters:
  - **newInputs:** new inputs for predicting the targets
  - **sigma.opt:** Noise kernel parameter (this parameter is optimized in the training process by default. However, it can be set manully here)


## example
```
code
```
 
 
