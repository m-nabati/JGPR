JGPR <- function(inputs, outputs, kern = NULL, init.params = NaN, fix.noise = F, inp = NaN, out = NaN, MaxIter = 100, break.tol = F, e = 0.25, init.rate = 0.1){
  #inputs: train inputs
  #outputs: train outputs
  #kern: user define kernel
  #init.params:  initializing of kernel parameters and noise kernel
  #inp: preprocessing of inputs
  #out: preprocessing of outputs
  #MaxIter:  maximum Iteration for training
  #break.tol: if true Iteration stops with special condition
  #e: multiply parameter to Back tracking line search

  require(Deriv)
  require(rdist)
  require(progress)
  require(R.utils)

  grad <- function(params){
    grad.kerns = array(0, dim = c(L, N, N))

    lst_params = as.list(setNames(params[1:(L-1)], op_vars), all.names = T)
    for(i in 1:(L-1)){
      grad.kerns[i,,] = doCall(kern_gxs[[i]], args = c(lst_params, list(d = d, ip = ip)))
    }
    grad.kerns[L,,] = diag(2*params[L], nrow = N)



    # m = 0
    # for(i in 1:MO){
    #   alpha = inv.kernl %*% as.matrix(outputs[,i])
    #   m = m +  alpha %*% t(alpha) - inv.kernl
    # }

    alpha = inv.kernl %*% outputs
    m = alpha %*% t(alpha) - MO * inv.kernl

    grad.direction = matrix(0, nrow = L, ncol = 1)

    for(i in 1:L){
      grad.direction[i] = -1/2 * sum(diag( m %*% grad.kerns[i,,] ))
    }

    return(grad.direction)
  }

  predict <- function(newInputs, sigma.opt = NULL){
    #newInputs: new inputs for predicting output
    #sigma.opt: Noise kernel parameter

    newInputs = as.matrix(newInputs)
    N.ts = nrow(newInputs)
    if(inp == 'st'){
      newInputs = sweep(newInputs, 2, mean.in, "-")
      newInputs = sweep(newInputs, 2, sd.in, "/")
    }else if(inp == 'norm'){
      newInputs = sweep(newInputs, 2, min.in, "-")
      newInputs = sweep(newInputs, 2, (max.in-min.in), "/")
    }

    params = as.matrix(optimized.params)
    if(!is.null(sigma.opt)){
      params = params[,ncol(params)]
      params[length(params)] = sigma.opt
      lst_params = as.list(setNames(params[1:(L-1)], op_vars), all.names = T)
      train.kernl = doCall(kern_fx, args = c(lst_params, list(d = d, ip = ip)))
      if(!fix.noise){
        train.kernl = train.kernl + diag(params[L]^2, nrow = N)
      }else{
        train.kernl = train.kernl + diag(init.params[L]^2, nrow = N)
      }
      inv.kernl = solve(train.kernl)
    }

    d.ts = dist(newInputs, method = "euclidean", diag = T, upper = T)
    d.ts = as.matrix(d.ts)
    ip.ts = newInputs %*% t(newInputs)
    lst_params = as.list(setNames(params[1:(L-1)], op_vars), all.names = T)
    test.kernl = doCall(kern_fx, args = c(lst_params, list(d = d.ts, ip = ip.ts)))
    if(!fix.noise){
      test.kernl = test.kernl + diag(params[L]^2, nrow = N.ts)
    }else{
      test.kernl = test.kernl + diag(init.params[L]^2, nrow = N.ts)
    }

    d.trs = cdist(newInputs, inputs)
    ip.trs = newInputs %*% t(inputs)
    lst_params = as.list(setNames(params[1:(L-1)], op_vars), all.names = T)
    test.train.kernl = doCall(kern_fx, args = c(lst_params, list(d = d.trs, ip = ip.trs)))


    pred.mu = pred.sd = matrix(0, nrow = nrow(newInputs), ncol = MO)

    if(out == 'st'){
      for(i in 1:MO){
        pred.mu[,i] = test.train.kernl %*% inv.kernl %*% outputs[,i]
        pred.mu[,i] = pred.mu[,i] * sd.ou[i] + mean.ou[i]
        pred.sd[,i] = sqrt(diag(test.kernl - test.train.kernl %*% inv.kernl %*% t(test.train.kernl)))
        pred.sd[,i] = pred.sd[,i] * sd.ou[i]
      }
    }else if(out == 'norm'){
      for(i in 1:MO){
        pred.mu[,i] = test.train.kernl %*% inv.kernl %*% outputs[,i]
        pred.mu[,i] = pred.mu[,i] * (max.ou[i]-min.ou[i]) + min.ou[i]
        pred.sd[,i] = sqrt(diag(test.kernl - test.train.kernl %*% inv.kernl %*% t(test.train.kernl)))
        pred.sd[,i] = pred.sd[,i] * abs(max.ou[i]-min.ou[i])
      }
    }else{
      for(i in 1:MO){
        pred.mu[,i] = test.train.kernl %*% inv.kernl %*% outputs[,i]
        pred.sd[,i] = sqrt(diag(test.kernl - test.train.kernl %*% inv.kernl %*% t(test.train.kernl)))
      }
    }


    return(list(pred.mu = pred.mu, pred.sd = pred.sd))
  }



  all_vars = all.vars(kern)

  op_vars = all_vars
  if(sum(all_vars=="d")){
    d = dist(inputs, method = "euclidean", diag = T, upper = T)
    d = as.matrix(d)
    op_vars = op_vars[! op_vars %in% c("d")]
  }else{
    d = 0
  }

  if(sum(all_vars=="ip")){
    ip = inputs %*% t(inputs)
    op_vars = op_vars[! op_vars %in% c("ip")]
  }else{
    ip = 0
  }

  args = setNames(rep(NA,length(all_vars)), all_vars)
  kern_fx = as.function(c(args, kern)) #define function of filling kernel
  n_op_vars = length(op_vars) #number of optimization parameters
  kern_gxs = vector("list", n_op_vars)

  for(i in 1:n_op_vars){
    g = D(kern, op_vars[i]) #get gradient of kernel with respect to i'th variable
    sg = Simplify(g) #simplify gradient
    gx <- as.function(c(args, sg)) #convert it to function
    kern_gxs[[i]] = gx #save gradient functions into list
  }


  inputs = as.matrix(inputs)
  outputs = as.matrix(outputs)
  N = nrow(inputs)
  MI = ncol(inputs)
  MO = ncol(outputs)
  L = length(init.params)


  if(inp == 'st'){
    mean.in =  apply(inputs, 2, mean)
    sd.in = apply(inputs, 2, sd)
    inputs = sweep(inputs, 2, mean.in, "-")
    inputs = sweep(inputs, 2, sd.in, "/")
  }else if(inp == 'norm'){
    max.in =  apply(inputs, 2, max)
    min.in =  apply(inputs, 2, min)
    inputs = sweep(inputs, 2, min.in, "-")
    inputs = sweep(inputs, 2, (max.in-min.in), "/")
  }


  if(out == 'st'){
    mean.ou =  apply(outputs, 2, mean)
    sd.ou = apply(outputs, 2, sd)
    outputs = sweep(outputs, 2, mean.ou, "-")
    outputs = sweep(outputs, 2, sd.ou, "/")
  }else if(out == 'norm'){
    max.ou =  apply(outputs, 2, max)
    min.ou =  apply(outputs, 2, min)
    outputs = sweep(outputs, 2, min.ou, "-")
    outputs = sweep(outputs, 2, (max.ou-min.ou), "/")
  }


  rate1 = rep(0, MaxIter)
  rate2 = rep(0, MaxIter)
  rate2[1] = init.rate
  Ta = 0.1
  log.max = rep(0, MaxIter+1)
  log.max2 = 0


  params = matrix(0, nrow = L, ncol = MaxIter+1)
  params[, 1] = init.params
  if(fix.noise){
    params[L, 1] = 0
  }
  grads = matrix(0, nrow = L, ncol = MaxIter+1)
  p = matrix(0, nrow = L, ncol = MaxIter+1)

  lst_params = as.list(setNames(params[1:(L-1),1], op_vars), all.names = T)
  kernl = doCall(kern_fx, args = c(lst_params, list(d = d, ip = ip)))
  if(!fix.noise){
    kernl = kernl + diag(params[L,1]^2, nrow = N)
  }else{
    kernl = kernl + diag(init.params[L]^2, nrow = N)
  }

  R = chol(kernl)
  inv.kernl = chol2inv(R)
  grads[,1] = grad(params[,1])
  p[,1] = -grads[,1]
  log.det.kernl = sum(2*log(diag(R)))

  for(i in 1:MO){
    log.max[1] = log.max[1] + 1/2*(log.det.kernl + t(outputs[,i]) %*% inv.kernl %*% outputs[,i] + N*log(2*pi))
  }

  print(sprintf("Iter %i ==> log.lik = %f", 0, log.max[1]))
  pb <- progress_bar$new(
    format = "rate::new_rate loglik::loglik it::current [:bar] :percent eta: :eta",
    total = MaxIter, clear = FALSE, width = 100)

  #----------------------------------------------------------------------------------
  #find optimum parameters
  for(it in 1:MaxIter){
    #----------------------------------------
    #back tracking line search with wolf condition
    for(j in 1:11){
      new.params = params[, it] + rate2[j] * p[,it]
      lst_params = as.list(setNames(new.params[1:(L-1)], op_vars), all.names = T)
      kernl = doCall(kern_fx, args = c(lst_params, list(d = d, ip = ip)))
      if(!fix.noise){
        kernl = kernl + diag(new.params[L]^2, nrow = N)
      }else{
        kernl = kernl + diag(init.params[L]^2, nrow = N)
      }
      R = chol(kernl)
      inv.kernl = chol2inv(R)
      log.det.kernl = sum(2*log(diag(R)))
      log.max2 = 0
      for(i in 1:MO){
        log.max2 = log.max2 + 1/2*(log.det.kernl+ t(outputs[,i]) %*% inv.kernl %*% outputs[,i] + N*log(2*pi))
      }
      if(log.max2 < log.max[it] + rate2[j]*e*(t(grads[,it])%*%p[,it])) break()
      rate2[j+1] = Ta * rate2[j]
    }
    rate1[it] = rate2[j] #initialize new rate
    #-----------------------------------------


    params[,it+1] = new.params
    log.max[it+1] = log.max2
    pb$tick(tokens = list(new_rate = rate1[it], loglik = log.max[it+1]))

    grads[,it+1] = grad(params[,it+1])
    Beta = t(grads[,it+1])%*%(grads[,it+1]-grads[,it]) / t(grads[,it])%*%(grads[,it])
    Beta = max(0, Beta)
    p[,it+1] = -grads[,it+1] + c(Beta) * p[,it]

    if(break.tol & abs(log.max[it+1] - log.max[it]) < break.tol) break()
  }
  cat("\n")

  log.max = log.max[log.max!=0]
  optimized.params = params[,it+1]
  if(fix.noise) params[L,] = init.params[L]
  return(list(predict = predict, params = params[,1:(it+1)], logLikelihood = log.max))
}
