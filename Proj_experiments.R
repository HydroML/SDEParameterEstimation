rm(list = ls())
setwd("C:/Users/joeja/Desktop/MATH/MATH605F/Project")
#Ornstein-Uhlenbeck, fix dt, vary T and N
N_max=10000
Tmax=6000
dt=Tmax/N_max
X_vec=rep(0,N_max)
W_vec=X_vec
X_vec[1]<- 0
W_vec[1]<- 0
b=2 # must be >0
sig=1
min_obs=10

for(i in 2:length(W_vec)){
  W_vec[i]= W_vec[i-1]+rnorm(1,mean = 0,sd=sqrt(dt))
}


for(i in 2:length(X_vec)){
  X_vec[i]= X_vec[i-1] - b*X_vec[i-1]*dt+ sig*(W_vec[i]- W_vec[i-1])
}

jpeg(paste0("Pavliotis_static_dt_rawdata.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(X_vec)),X_vec,type="l",ylab="X_t",xlab = "T")
dev.off()
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(X_vec)),X_vec,type="l",ylab="X_t",xlab = "T")

drift_est_vec= rep(0,N_max)
noise_est_vec= rep(0,N_max)

for(i in min_obs:length(drift_est_vec)){
  drift_est_vec[i]= -sum(X_vec[1:(i-1)]*(X_vec[2:i]- X_vec[1:(i-1)]))/sum(X_vec[1:(i-1)]^2 * dt)
  noise_est_vec[i] = sqrt(sum((X_vec[2:i]- X_vec[1:(i-1)])^2)/(i*dt))
}

drift_error= abs(b- drift_est_vec[min_obs:length(drift_est_vec)])
diff_error= abs(sig- noise_est_vec[min_obs:length(noise_est_vec)])

par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(drift_error)),drift_error,type="l",lwd=2,xlab="T", ylab="absolute error",
     ylim=c(0,max(drift_error,diff_error)*1.2))
lines(seq(from=min_obs*dt,to=Tmax,length.out=length(diff_error)),diff_error,col="blue",lwd=2)
legend("topright", legend=c("drift", "diffusion"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)

jpeg(paste0("Pavliotis_static_dt.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(drift_error)),drift_error,type="l",lwd=2,xlab="T", ylab="absolute error",
     ylim=c(0,max(drift_error,diff_error)*1.2))
lines(seq(from=min_obs*dt,to=Tmax,length.out=length(diff_error)),diff_error,col="blue",lwd=2)
legend("topright", legend=c("drift", "diffusion"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)
dev.off()


#negative coef pavlliotis
N_max=10000
Tmax=6000
dt=Tmax/N_max
X_vec=rep(0,N_max)
W_vec=X_vec
X_vec[1]<- 0
W_vec[1]<- 0
b=-0.0006 # must be >0
sig=1
min_obs=1000

for(i in 2:length(W_vec)){
  W_vec[i]= W_vec[i-1]+rnorm(1,mean = 0,sd=sqrt(dt))
}


for(i in 2:length(X_vec)){
  X_vec[i]= X_vec[i-1] - b*X_vec[i-1]*dt+ sig*(W_vec[i]- W_vec[i-1])
}

jpeg(paste0("negbPavliotis_static_dt_rawdata.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(X_vec)),X_vec,type="l",ylab="X_t",xlab = "T")
dev.off()
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(X_vec)),X_vec,type="l",ylab="X_t",xlab = "T")

drift_est_vec= rep(0,N_max)
noise_est_vec= rep(0,N_max)

for(i in min_obs:length(drift_est_vec)){
  drift_est_vec[i]= -sum(X_vec[1:(i-1)]*(X_vec[2:i]- X_vec[1:(i-1)]))/sum(X_vec[1:(i-1)]^2 * dt)
  noise_est_vec[i] = sqrt(sum((X_vec[2:i]- X_vec[1:(i-1)])^2)/(i*dt))
}

drift_error= abs(b- drift_est_vec[min_obs:length(drift_est_vec)])
diff_error= abs(sig- noise_est_vec[min_obs:length(noise_est_vec)])

par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(drift_error)),drift_error,type="l",lwd=2,xlab="T", ylab="absolute error",
     ylim=c(0,max(drift_error,diff_error)*1.2))
lines(seq(from=min_obs*dt,to=Tmax,length.out=length(diff_error)),diff_error,col="blue",lwd=2)
legend("topright", legend=c("drift", "diffusion"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)

jpeg(paste0("negbPavliotis_static_dt.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(drift_error)),drift_error,type="l",lwd=2,xlab="T", ylab="absolute error",
     ylim=c(0,max(drift_error,diff_error)*1.2))
lines(seq(from=min_obs*dt,to=Tmax,length.out=length(diff_error)),diff_error,col="blue",lwd=2)
legend("topright", legend=c("drift", "diffusion"),
       col=c("black", "blue"),lty=1, lwd=3, cex=0.8)
dev.off()


# fix N, vary T and dt
T_max_vec=round(exp(seq(from=4,to=7,by=0.1)))
N_max=5000
replicates=500
drift_error= rep(0,length(T_max_vec))
diff_error= rep(0,length(T_max_vec))

for(j in 1:length(T_max_vec)){
  for(r in 1:replicates){
    Tmax=T_max_vec[j]
    dim=2
    dt=Tmax/N_max
    
    X_vec=rep(0,N_max)
    W_vec=X_vec
    X_vec[1]<- 0
    W_vec[1]<- 0
    b=2 # must be >0
    sig=1
    
    for(i in 2:length(W_vec)){
      W_vec[i]= W_vec[i-1]+rnorm(1,mean = 0,sd=sqrt(dt))
    }
    
    
    for(i in 2:length(X_vec)){
      X_vec[i]= X_vec[i-1] - b*X_vec[i-1]*dt+ sig*(W_vec[i]- W_vec[i-1])
    }
    
    drift_est= -sum(X_vec[-length(X_vec)]*(X_vec[-1]- X_vec[-length(X_vec)]))/sum(X_vec[-length(X_vec)]^2 * dt)
    noise_est = sqrt(sum((X_vec[-1]- X_vec[-length(X_vec)])^2)/(i*dt))

    
    drift_error[j]= drift_error[j]+abs(b- drift_est)
    diff_error[j]=  diff_error[j]+ abs(sig- noise_est)
    
  }
}


par(mar = c(4.1, 4.1, 0.75, 2.1))
plot(T_max_vec/N_max,drift_error/replicates,type="l",lwd=2,xlab="dt", ylab="absolute error",
     ylim=c(0,max(drift_error,diff_error)/replicates))
lines(T_max_vec/N_max,diff_error/replicates,col="blue",lwd=2)
legend("topright", legend=c("drift", "diffusion"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)

jpeg(paste0("Pavliotis_static_N.jpeg"),width = 5, height = 3.5, units = 'in',res = 400)
par(mar = c(4.1, 4.1, 0.75, 2.1))
plot(T_max_vec/N_max,drift_error/replicates,type="l",lwd=2,xlab="dt", ylab="absolute error",
     ylim=c(0,max(drift_error,diff_error)/replicates))
lines(T_max_vec/N_max,diff_error/replicates,col="blue",lwd=2)
legend("topright", legend=c("drift", "diffusion"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)
dev.off()


# fix T, vary N and dt
N_max_vec=round(exp(seq(from=9,to=13,by=0.5)))
T_max=1
replicates=500
drift_error= rep(0,length(N_max_vec))
diff_error= rep(0,length(N_max_vec))

for(j in 1:length(N_max_vec)){
  for(r in 1:replicates){
    N_max=N_max_vec[j]
    dim=2
    dt=Tmax/N_max
    
    X_vec=rep(0,N_max)
    W_vec=X_vec
    X_vec[1]<- 0
    W_vec[1]<- 0
    b=2 # must be >0
    sig=1
    
    for(i in 2:length(W_vec)){
      W_vec[i]= W_vec[i-1]+rnorm(1,mean = 0,sd=sqrt(dt))
    }
    
    
    for(i in 2:length(X_vec)){
      X_vec[i]= X_vec[i-1] - b*X_vec[i-1]*dt+ sig*(W_vec[i]- W_vec[i-1])
    }
    
    drift_est= -sum(X_vec[-length(X_vec)]*(X_vec[-1]- X_vec[-length(X_vec)]))/sum(X_vec[-length(X_vec)]^2 * dt)
    noise_est = sqrt(sum((X_vec[-1]- X_vec[-length(X_vec)])^2)/(i*dt))
    
    
    drift_error[j]= drift_error[j]+abs(b- drift_est)
    diff_error[j]=  diff_error[j]+ abs(sig- noise_est)
    
  }
}


par(mar = c(4.1, 4.1, 0.75, 4.4))
plot(log(N_max_vec),drift_error/replicates,type="l",lwd=2,xlab="log N", ylab="drift absolute error")
par(new = TRUE)
plot(log(N_max_vec),diff_error/replicates,col="blue",lwd=2,axes=F, bty = "n", xlab = "", ylab = "",type="l")
axis(side=4, at = pretty(range(diff_error/replicates)))
mtext("diffusion absolute error", side=4, line=3)
legend("topright", legend=c("drift", "diffusion"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)

jpeg(paste0("Pavliotis_static_T.jpeg"),width = 5, height = 3.5, units = 'in',res = 400)
par(mar = c(4.1, 4.1, 0.75, 4.4))
plot(log(N_max_vec),drift_error/replicates,type="l",lwd=2,xlab="log N", ylab="drift absolute error")
par(new = TRUE)
plot(log(N_max_vec),diff_error/replicates,col="blue",lwd=2,axes=F, bty = "n", xlab = "", ylab = "",type="l")
axis(side=4, at = pretty(range(diff_error/replicates)))
mtext("diffusion absolute error", side=4, line=3)
legend("topright", legend=c("drift", "diffusion"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)
dev.off()


# pavliotis vs pederson estimates static dt
dt=10
N_max_vec=round(exp(seq(from=3,to=7,by=0.5)))
T_max=1
replicates=100
drift_error= rep(0,length(N_max_vec))
diff_error= rep(0,length(N_max_vec))

drift_error_pet= rep(0,length(N_max_vec))
diff_error_pet= rep(0,length(N_max_vec))

for(j in 1:length(N_max_vec)){
  for(r in 1:replicates){
    N_max=N_max_vec[j]
    dim=2
    Tmax=dt*N_max
    
    X_vec=rep(0,N_max)
    W_vec=X_vec
    X_vec[1]<- 0
    W_vec[1]<- 0
    b=0.1 # must be >0
    sig=0.2
    
    for(i in 2:length(W_vec)){
      W_vec[i]= W_vec[i-1]+rnorm(1,mean = 0,sd=sqrt(dt))
    }
    
    
    for(i in 2:length(X_vec)){
      X_vec[i]= X_vec[i-1] - b*X_vec[i-1]*dt+ sig*(W_vec[i]- W_vec[i-1])
    }
    
    drift_est= -sum(X_vec[-length(X_vec)]*(X_vec[-1]- X_vec[-length(X_vec)]))/sum(X_vec[-length(X_vec)]^2 * dt)
    noise_est = sqrt(sum((X_vec[-1]- X_vec[-length(X_vec)])^2)/(i*dt))
    
    
    drift_est_pet= -(1/dt)*log(abs(sum(X_vec[-length(X_vec)]*X_vec[-1]))/sum(X_vec[-length(X_vec)]^2))
    noise_est_pet = sqrt(abs(((-2*drift_est_pet)/((length(X_vec)-1)*(1- exp(2*dt*drift_est_pet)))) * sum((X_vec[-1]- X_vec[-length(X_vec)]*exp(dt*drift_est_pet))^2)))
    
    
    drift_error[j]= drift_error[j]+abs(b- drift_est)
    diff_error[j]=  diff_error[j]+ abs(sig- noise_est)
    drift_error_pet[j]= drift_error_pet[j]+abs(b- drift_est_pet)
    diff_error_pet[j]=  diff_error_pet[j]+ abs(sig- noise_est_pet)
    
  }
}


par(mar = c(4.1, 4.1, 0.75, 2.1))
plot(N_max_vec*dt,drift_error/replicates,type="l",lwd=2,xlab="T", ylab="absolute error",
     ylim=c(0,max(drift_error,drift_error_pet)/replicates))
lines(N_max_vec*dt,drift_error_pet/replicates,col="blue",lwd=2)
legend("topright", legend=c("drift", "drift pedersen"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)

jpeg(paste0("Pavliotis_vs_pedersen_static_dt.jpeg"),width = 5, height = 3.5, units = 'in',res = 400)
par(mar = c(4.1, 4.1, 0.75, 2.1))
plot(N_max_vec*dt,drift_error/replicates,type="l",lwd=2,xlab="T", ylab="absolute error",
     ylim=c(0,max(drift_error,drift_error_pet)/replicates))
lines(N_max_vec*dt,drift_error_pet/replicates,col="blue",lwd=2)
legend("topright", legend=c("drift", "drift pedersen"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)
dev.off()

par(mar = c(4.1, 4.1, 0.75, 2.1))
plot(N_max_vec*dt,diff_error/replicates,type="l",lwd=2,xlab="T", ylab="absolute error",
     ylim=c(0,max(diff_error,diff_error_pet)/replicates))
lines(N_max_vec*dt,diff_error_pet/replicates,col="blue",lwd=2)
legend("topright", legend=c("diffusion", "diffusion pedersen"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)



# pavliotis vs pederson estimates static T
N_max_vec=round(exp(seq(from=9,to=13,by=0.5)))
T_max=1
replicates=100
drift_error= rep(0,length(N_max_vec))
diff_error= rep(0,length(N_max_vec))

drift_error_pet= rep(0,length(N_max_vec))
diff_error_pet= rep(0,length(N_max_vec))

for(j in 1:length(N_max_vec)){
  for(r in 1:replicates){
    N_max=N_max_vec[j]
    dim=2
    dt=Tmax/N_max
    
    X_vec=rep(0,N_max)
    W_vec=X_vec
    X_vec[1]<- 0
    W_vec[1]<- 0
    b=2 # must be >0
    sig=1
    
    for(i in 2:length(W_vec)){
      W_vec[i]= W_vec[i-1]+rnorm(1,mean = 0,sd=sqrt(dt))
    }
    
    
    for(i in 2:length(X_vec)){
      X_vec[i]= X_vec[i-1] - b*X_vec[i-1]*dt+ sig*(W_vec[i]- W_vec[i-1])
    }
    
    drift_est= -sum(X_vec[-length(X_vec)]*(X_vec[-1]- X_vec[-length(X_vec)]))/sum(X_vec[-length(X_vec)]^2 * dt)
    noise_est = sqrt(sum((X_vec[-1]- X_vec[-length(X_vec)])^2)/(i*dt))
    
    
    drift_est_pet= -(1/dt)*log(sum(X_vec[-length(X_vec)]*X_vec[-1])/sum(X_vec[-length(X_vec)]^2))
    noise_est_pet = ((-2*drift_est_pet)/((length(X_vec)-1)*(1- exp(2*dt*drift_est_pet)))) * sum((X_vec[-1]- X_vec[-length(X_vec)]*exp(dt*drift_est_pet))^2)
    
    
    drift_error[j]= drift_error[j]+abs(b- drift_est)
    diff_error[j]=  diff_error[j]+ abs(sig- noise_est)
    drift_error_pet[j]= drift_error_pet[j]+abs(b- drift_est_pet)
    diff_error_pet[j]=  diff_error_pet[j]+ abs(sig- noise_est_pet)
    
  }
}


par(mar = c(4.1, 4.1, 0.75, 2.1))
plot(log(N_max_vec),drift_error/replicates,type="l",lwd=2,xlab="log N", ylab="absolute error",
     ylim=c(0,max(drift_error,drift_error_pet)/replicates))
lines(log(N_max_vec),drift_error_pet/replicates,col="blue",lwd=2)
legend("topright", legend=c("drift", "drift pedersen"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)

jpeg(paste0("Pavliotis_vs_pedersen_static_T.jpeg"),width = 5, height = 3.5, units = 'in',res = 400)
par(mar = c(4.1, 4.1, 0.75, 2.1))
plot(log(N_max_vec),drift_error/replicates,type="l",lwd=2,xlab="log N", ylab="absolute error",
     ylim=c(0,max(drift_error,drift_error_pet)/replicates))
lines(log(N_max_vec),drift_error_pet/replicates,col="blue",lwd=2)
legend("topright", legend=c("drift", "drift pedersen"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)
dev.off()




# pavliotis vs pederson estimates static N
T_max_vec=round(exp(seq(from=4,to=7,by=0.1)))
N_max=5000
replicates=100
drift_error= rep(0,length(T_max_vec))
diff_error= rep(0,length(T_max_vec))

drift_error_pet= rep(0,length(T_max_vec))
diff_error_pet= rep(0,length(T_max_vec))

for(j in 1:length(T_max_vec)){
  for(r in 1:replicates){
    Tmax=T_max_vec[j]
    dim=2
    dt=Tmax/N_max
    
    X_vec=rep(0,N_max)
    W_vec=X_vec
    X_vec[1]<- 0
    W_vec[1]<- 0
    b=2 # must be >0
    sig=1
    
    for(i in 2:length(W_vec)){
      W_vec[i]= W_vec[i-1]+rnorm(1,mean = 0,sd=sqrt(dt))
    }
    
    
    for(i in 2:length(X_vec)){
      X_vec[i]= X_vec[i-1] - b*X_vec[i-1]*dt+ sig*(W_vec[i]- W_vec[i-1])
    }
    
    drift_est= -sum(X_vec[-length(X_vec)]*(X_vec[-1]- X_vec[-length(X_vec)]))/sum(X_vec[-length(X_vec)]^2 * dt)
    noise_est = sqrt(sum((X_vec[-1]- X_vec[-length(X_vec)])^2)/(i*dt))
    
    
    drift_est_pet= -(1/dt)*log(sum(X_vec[-length(X_vec)]*X_vec[-1])/sum(X_vec[-length(X_vec)]^2))
    noise_est_pet = ((-2*drift_est_pet)/((length(X_vec)-1)*(1- exp(2*dt*drift_est_pet)))) * sum((X_vec[-1]- X_vec[-length(X_vec)]*exp(dt*drift_est_pet))^2)
    
    
    drift_error[j]= drift_error[j]+abs(b- drift_est)
    diff_error[j]=  diff_error[j]+ abs(sig- noise_est)
    drift_error_pet[j]= drift_error_pet[j]+abs(b- drift_est_pet)
    diff_error_pet[j]=  diff_error_pet[j]+ abs(sig- noise_est_pet)
    
  }
}


par(mar = c(4.1, 4.1, 0.75, 2.1))
plot(T_max_vec/N_max,drift_error/replicates,type="l",lwd=2,xlab="dt", ylab="absolute error",
     ylim=c(0,max(drift_error,drift_error_pet)/replicates))
lines(T_max_vec/N_max,drift_error_pet/replicates,col="blue",lwd=2)
legend("topright", legend=c("drift", "drift pedersen"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)

jpeg(paste0("Pavliotis_vs_pedersen_static_N.jpeg"),width = 5, height = 3.5, units = 'in',res = 400)
plot(T_max_vec/N_max,drift_error/replicates,type="l",lwd=2,xlab="dt", ylab="absolute error",
     ylim=c(0,max(drift_error,drift_error_pet)/replicates))
lines(T_max_vec/N_max,drift_error_pet/replicates,col="blue",lwd=2)
legend("topright", legend=c("drift", "drift pedersen"),
       col=c("black", "blue"),lty=1, cex=0.8,lwd=3)
dev.off()




#########################################################################################################33####################
#######################################33# stationary bistable process####################################################
####################################################################################################################
N_max=50000
Tmax=5000
dt=Tmax/N_max
X_vec=rep(0,N_max)
W_vec=X_vec
X_vec[1]<- 0
W_vec[1]<- 0
alpha=1 # must be >0
beta=1
min_obs=1000

for(i in 2:length(W_vec)){
  W_vec[i]= W_vec[i-1]+rnorm(1,mean = 0,sd=sqrt(dt))
}


for(i in 2:length(X_vec)){
  X_vec[i]= X_vec[i-1] + (alpha*X_vec[i-1] - beta*X_vec[i-1]^3)*dt+ (W_vec[i]- W_vec[i-1])
}

jpeg(paste0("Pavliotis_bistable_static_dt_rawdata.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(X_vec)),X_vec,type="l",ylab="X_t",xlab = "T")
dev.off()
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,length.out=length(X_vec)),X_vec,type="l",ylab="X_t",xlab = "T")

alpha_est_vec= rep(0,N_max)
beta_est_vec= rep(0,N_max)

for(i in min_obs:length(alpha_est_vec)){
  B1=sum(X_vec[1:(i-1)]* (X_vec[2:i]-X_vec[1:(i-1)] ))
  B3=sum(X_vec[1:(i-1)]^3* (X_vec[2:i]-X_vec[1:(i-1)] ))
  M2=sum(X_vec[1:(i-1)]^2* dt)
  M4=sum(X_vec[1:(i-1)]^4* dt)
  M6=sum(X_vec[1:(i-1)]^6* dt)
  alpha_est_vec[i]= (B1*M6-B3*M4)/(M2*M6-M4^2)
  beta_est_vec[i]= (B1*M4- B3*M2)/(M2*M6- M4^2)
}




jpeg(paste0("Pavliotis_poly_static_dt.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(seq(from=min_obs*dt,to=Tmax,by=dt),abs(alpha- alpha_est_vec[min_obs:length(alpha_est_vec)]),type="l",lwd=2,
     xlab="T", ylab="drift MLE absolute error")
lines(seq(from=min_obs*dt,to=Tmax,by=dt),abs(beta- beta_est_vec[min_obs:length(beta_est_vec)]),col="red",lwd=2)
legend("topright", legend=c("alpha", "beta"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()



##################################################################################################################
##########################    multi dimentional process (additive noise)   ##################################################
################################################################################################################
library(mvnfast)
N_max=10000
Tmax=1
dim=2
dt=Tmax/N_max
X_vec=matrix(0,nrow = N_max,ncol = dim)
W_vec=X_vec
X_vec[1,]<- c(1.87,-0.98)
W_vec[1,]<- 0
A=matrix(c(1.76,0.98,-0.1,0),nrow=dim)
G=matrix(c(-0.11,-0.29,-0.14,-0.22),nrow=dim)

for(i in 2:N_max){
  W_vec[i,]= W_vec[i-1,]+rnorm(dim,mean = 0,sd=sqrt(dt))
}


for(i in 2:N_max){
  X_vec[i,]= X_vec[i-1,] + A%*%X_vec[i-1,]*dt + G%*%(W_vec[i,]- W_vec[i-1,])
}

matplot((X_vec),type="l")

dX_vec=matrix(X_vec[2:nrow(X_vec),]-X_vec[1:(nrow(X_vec)-1),],ncol = ncol(X_vec))
GTG_est= cov(dX_vec)*(nrow(dX_vec)-1)/Tmax


MultiGaussianLike_foropt<-function(params,X_vec,GTG_est){
  A_est=matrix(params,nrow = ncol(X_vec))
  Lik=1
  cur_sig= dt* GTG_est
  cur_mu= t(matrix(X_vec,ncol = dim)) + A_est%*%t(matrix(X_vec,ncol = dim))*dt
  
  for(i in 2:nrow(X_vec)){
    Lik=Lik+ dmvn(X=X_vec[i,], mu=cur_mu[,i-1], sigma=cur_sig, ncores = 1, log = T)
  }
  return(-Lik)
}


res <- optim(as.numeric(A)+2, MultiGaussianLike_foropt,method = "BFGS",X_vec=X_vec,GTG_est=GTG_est,
             control = list(trace=1,parscale=rep(1,length(as.numeric(A))) ) ) 
res

matrix(res$par,nrow = dim)
A


preterb=seq(from=-0.4,to=0,by=0.01)
preterb_g=preterb
preterb_a=preterb
for(i in 1:length(preterb)){
  preterb_g[i]= MultiGaussianLike_foropt(as.numeric(A),GTG_est=(G+preterb[i])%*%t((G+preterb[i])), X_vec = X_vec)
  preterb_a[i]= MultiGaussianLike_foropt(as.numeric(A)+preterb[i],GTG_est=G%*%t(G), X_vec = X_vec)
}


jpeg(paste0("perterbG_vs_perterbA_lik_additive.jpeg"),width = 5, height = 4.5, units = 'in',res = 400)
plot(preterb,preterb_g,type="l",ylim=c(min(preterb_g,preterb_a),max(preterb_a,preterb_g)),lwd=2,ylab="neg log lik", xlab="additive perturbation")
lines(preterb,preterb_a,col="red",lwd=2)
legend("topright", legend=c("perturbing G", "perturbing A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()


preterb=seq(from=1,to=1.03,by=0.001)
preterb_g=preterb
preterb_a=preterb
for(i in 1:length(preterb)){
  preterb_g[i]= MultiGaussianLike_foropt(as.numeric(A),GTG_est=(G*preterb[i])%*%t((G*preterb[i])), X_vec = X_vec)
  preterb_a[i]= MultiGaussianLike_foropt(as.numeric(A)*preterb[i],GTG_est=G%*%t(G), X_vec = X_vec)
}
jpeg(paste0("perterbG_vs_perterbA_lik_multiplicative.jpeg"),width = 5, height = 4.5, units = 'in',res = 400)
plot(preterb,preterb_g,type="l",ylim=c(min(preterb_g,preterb_a),max(preterb_a,preterb_g)),lwd=2,ylab="neg log lik", xlab="multiplicative perturbation")
lines(preterb,preterb_a,col="red",lwd=2)
legend("topright", legend=c("perturbing G", "perturbing A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()


N_max_vec=round(exp(seq(from=4,to=9,by=1)))
preterb_g=rep(0,length(N_max_vec))
preterb_a=rep(0,length(N_max_vec))
replicates=50

for(j in 1:length(N_max_vec)){
  Tmax=1
  dim=2
  N_max=N_max_vec[j]
  dt=Tmax/N_max
  for(r in 1:replicates){
    X_vec=matrix(0,nrow = N_max,ncol = dim)
    W_vec=X_vec
    X_vec[1,]<- c(1.87,-0.98)
    W_vec[1,]<- 0
    
    for(i in 2:N_max){
      W_vec[i,]= W_vec[i-1,]+rnorm(dim,mean = 0,sd=sqrt(dt))
    }
    for(i in 2:N_max){
      X_vec[i,]= X_vec[i-1,] + A%*%X_vec[i-1,]*dt + G%*%(W_vec[i,]- W_vec[i-1,])
    }
    dX_vec=matrix(X_vec[2:nrow(X_vec),]-X_vec[1:(nrow(X_vec)-1),],ncol = ncol(X_vec))
    GTG_est= cov(dX_vec)*(nrow(dX_vec)-1)/Tmax
    
    res <- optim(as.numeric(A)+2, MultiGaussianLike_foropt,method = "BFGS",X_vec=X_vec,GTG_est=GTG_est,
                 control = list(trace=1,parscale=rep(1,length(as.numeric(A))) ) ) 
    res
    
    
    
    preterb_g[j]= preterb_g[j]+mean(abs(G%*%t(G)- GTG_est))
    preterb_a[j]= preterb_a[j]+mean(abs(matrix(res$par,nrow = dim)-A))
  }
  
}

plot(log(Tmax/N_max_vec),preterb_g/replicates,type="l",ylim=c(0,(max(preterb_a,preterb_g)/replicates))*1,lwd=2,xlab="log dt",ylab = "MAE")
lines(log(Tmax/N_max_vec),preterb_a/replicates,col="red",lwd=2)
legend("bottomright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)


jpeg(paste0("MAEvsDT_singletraj_additive.jpeg"),width = 5, height = 4.5, units = 'in',res = 400)
plot(log(Tmax/N_max_vec),preterb_g/replicates,type="l",ylim=c(0,(max(preterb_a,preterb_g)/replicates))*1,lwd=2,xlab="log dt",ylab = "MAE")
lines(log(Tmax/N_max_vec),preterb_a/replicates,col="red",lwd=2)
legend("bottomright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()





T_max_vec=seq(from=0.5,to=1.5,by=0.1)
preterb_g=rep(0,length(T_max_vec))
preterb_a=rep(0,length(T_max_vec))
replicates=75

for(j in 1:length(T_max_vec)){
  Tmax=T_max_vec[j]
  dim=2
  dt=5e-3
  N_max=round(Tmax/dt)
  
  for(r in 1:replicates){
    X_vec=matrix(0,nrow = N_max,ncol = dim)
    W_vec=X_vec
    X_vec[1,]<- c(1.87,-0.98)
    W_vec[1,]<- 0
    
    for(i in 2:N_max){
      W_vec[i,]= W_vec[i-1,]+rnorm(dim,mean = 0,sd=sqrt(dt))
    }
    for(i in 2:N_max){
      X_vec[i,]= X_vec[i-1,] + A%*%X_vec[i-1,]*dt + G%*%(W_vec[i,]- W_vec[i-1,])
    }
    dX_vec=matrix(X_vec[2:nrow(X_vec),]-X_vec[1:(nrow(X_vec)-1),],ncol = ncol(X_vec))
    GTG_est= cov(dX_vec)*(nrow(dX_vec)-1)/Tmax
    
    res <- optim(as.numeric(A)+2, MultiGaussianLike_foropt,method = "BFGS",X_vec=X_vec,GTG_est=GTG_est,
                control = list(trace=1,parscale=rep(1,length(as.numeric(A))) ) ) 
    res
    
    
    
    preterb_g[j]= preterb_g[j]+mean(abs(G%*%t(G)- GTG_est))
    preterb_a[j]= preterb_a[j]+mean(abs(matrix(res$par,nrow = dim)-A))
  }
  
}

plot(T_max_vec,preterb_g/replicates,type="l",ylim=c(0,(max(preterb_a,preterb_g)/replicates))*1,lwd=2,xlab="T",ylab = "MAE")
lines(T_max_vec,preterb_a/replicates,col="red",lwd=2)
legend("bottomright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)


jpeg(paste0("MAEvsT_singletraj_additive.jpeg"),width = 5, height = 4.5, units = 'in',res = 400)
plot(T_max_vec,preterb_g/replicates,type="l",ylim=c(0,(max(preterb_a,preterb_g)/replicates))*1,lwd=2,xlab="T",ylab = "MAE")
lines(T_max_vec,preterb_a/replicates,col="red",lwd=2)
legend("bottomright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()


#increase trajectories while everything else constant
A=matrix(c(1.76,0.98,-0.1,0),nrow=dim)
G=matrix(c(-0.11,-0.29,-0.14,-0.22),nrow=dim)
Traj_max_vec=seq(from=5,to=55,by=10)
preterb_g=rep(0,length(Traj_max_vec))
preterb_a=rep(0,length(Traj_max_vec))
replicates=15

MultiGaussianLike_foropt_mt<-function(params,X_vec,GTG_est){
  A_est=matrix(params[1:ncol(X_vec)^2],nrow = ncol(X_vec))
  L_est=matrix(0,nrow = ncol(X_vec),ncol = ncol(X_vec))
  L_est[lower.tri(L_est,diag = T)]<-params[(1+ncol(X_vec)^2):length(params)]
  Lik=1
  cur_sig= dt* L_est %*% t(L_est)#* GTG_est
  
  for(t in 1:dim(X_vec)[3]){
    curXvec=X_vec[,,t]
    cur_mu= t(matrix(curXvec,ncol = dim)) + A_est%*%t(matrix(curXvec,ncol = dim))*dt
    
    for(i in 2:nrow(curXvec)){
      Lik=Lik+ dmvn(X=curXvec[i,], mu=cur_mu[,i-1], sigma=cur_sig, ncores = 1, log = T)
    }
  }
  
  return(-Lik)
}

for(j in 1:length(Traj_max_vec)){
  Tmax=1
  dim=2
  N_max=50
  dt=Tmax/N_max
  traj=Traj_max_vec[j]
  
  for(r in 1:replicates){
    X_vec=array(0, dim=c(N_max, dim, traj))
    W_vec=X_vec
    X_vec[1,,]<- c(1.87,-0.98)
    W_vec[1,,]<- 0
    
    for(i in 2:N_max){
      W_vec[i,,]= W_vec[i-1,,]+rnorm(dim*traj,mean = 0,sd=sqrt(dt))
    }
    for(i in 2:N_max){
      X_vec[i,,]= X_vec[i-1,,] + A%*%X_vec[i-1,,]*dt + G%*%(W_vec[i,,]- W_vec[i-1,,])
    }
    dX_vec=array(X_vec[2:nrow(X_vec),,]-X_vec[1:(nrow(X_vec)-1),,],dim=c(N_max-1, dim, traj))
    
    GTG_est=0
    for(t in 1:traj){
      GTG_est= GTG_est+ t(dX_vec[,,t])%*% (dX_vec[,,t])/Tmax
    }
    GTG_est= GTG_est/traj
    
    trueL=t(chol(G%*%t(G)))[lower.tri(t(chol(G%*%t(G))),diag = TRUE)]
    
    
    res <- optim(par=c(as.numeric(A),trueL)+2, MultiGaussianLike_foropt_mt,method = "BFGS",X_vec=X_vec,GTG_est=GTG_est,
                 control = list(trace=1)) 
    res
    
    L_est=matrix(0,ncol = dim,nrow=dim)
    L_est[lower.tri(L_est,diag = T)]= res$par[(1+ncol(X_vec)^2):length(res$par)]
    GTG_est = L_est %*% t(L_est)
    
    
    
    preterb_g[j]= preterb_g[j]+mean(abs(G%*%t(G)- GTG_est)^2)
    preterb_a[j]= preterb_a[j]+mean(abs(matrix(res$par[1:ncol(X_vec)^2],nrow = dim)-A)^2)
  }
  
}

plot(Traj_max_vec,log10(preterb_g/replicates),type="l",ylim=c(log10(min(preterb_a,preterb_g)/replicates),log10(max(preterb_a,preterb_g)/replicates))*1
     ,lwd=2,xlab="# of trajectories",ylab = "log10 MSE")
lines(Traj_max_vec,log10(preterb_a/replicates),col="red",lwd=2)
legend("topright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)


jpeg(paste0("MAEvstraj_additive.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(Traj_max_vec,log10(preterb_g/replicates),type="l",ylim=c(log10(min(preterb_a,preterb_g)/replicates),log10(max(preterb_a,preterb_g)/replicates))*1
     ,lwd=2,xlab="# of trajectories",ylab = "log10 MSE")
lines(Traj_max_vec,log10(preterb_a/replicates),col="red",lwd=2)
legend("topright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()


jpeg(paste0("Wang_additive_rawdata.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
matplot(seq(from=0,to=T,length.out=nrow(X_vec)),X_vec[,,1],type="l",ylab = "X_t",xlab = "T",lwd=2)
dev.off()
par(mar = c(5.1, 4.1, 0.5, 2.1))
matplot(seq(from=0,to=T,length.out=nrow(X_vec)),X_vec[,,1],type="l",ylab = "X_t",xlab = "T",lwd=2)


##multi dimentional process (additive noise, radon nikodem)
library(mvnfast)
dim=2
A=matrix(c(1.76,0.98,-0.1,0),nrow=dim)
G=matrix(c(-0.11,-0.29,-0.14,-0.22),nrow=dim)
Traj_max_vec=seq(from=5,to=35,by=10)
preterb_g=rep(0,length(Traj_max_vec))
preterb_a=rep(0,length(Traj_max_vec))
preterb_g_rn=rep(0,length(Traj_max_vec))
preterb_a_rn=rep(0,length(Traj_max_vec))
replicates=15

MultiGaussianLike_foropt_mt<-function(params,X_vec,GTG_est){
  A_est=matrix(params[1:ncol(X_vec)^2],nrow = ncol(X_vec))
  L_est=matrix(0,nrow = ncol(X_vec),ncol = ncol(X_vec))
  L_est[lower.tri(L_est,diag = T)]<-params[(1+ncol(X_vec)^2):length(params)]
  Lik=1
  cur_sig= dt* L_est %*% t(L_est)#* GTG_est
  
  for(t in 1:dim(X_vec)[3]){
    curXvec=X_vec[,,t]
    cur_mu= t(matrix(curXvec,ncol = dim)) + A_est%*%t(matrix(curXvec,ncol = dim))*dt
    
    for(i in 2:nrow(curXvec)){
      Lik=Lik+ dmvn(X=curXvec[i,], mu=cur_mu[,i-1], sigma=cur_sig, ncores = 1, log = T)
    }
  }
  
  return(-Lik)
}

MultiGaussianLike_foropt_mt_rn<-function(params,X_vec,GTG_est){
  A_est=matrix(params[1:ncol(X_vec)^2],nrow = ncol(X_vec))
  L_est=matrix(0,nrow = ncol(X_vec),ncol = ncol(X_vec))
  L_est[lower.tri(L_est,diag = T)]<-params[(1+ncol(X_vec)^2):length(params)]
  diag(L_est)<-exp(diag(L_est))
  Lik= 1
  cur_sig= L_est %*% t(L_est)#* GTG_est
  Ls=solve(L_est)
  
  for(t in 1:dim(X_vec)[3]){
    curXvec=X_vec[,,t]
    cur_mu= A_est%*%t(matrix(curXvec,ncol = dim))
    for(i in 2:nrow(curXvec)){
      Lik=Lik+  cur_mu[,i-1] %*% Ls%*%t(Ls) %*% (curXvec[i,]-curXvec[i-1,]) - 0.5* cur_mu[,i-1] %*% Ls%*%t(Ls) %*% cur_mu[,i-1] *dt
    }
  }
  
  return(-Lik)
}

for(j in 1:length(Traj_max_vec)){
  Tmax=1
  dim=2
  N_max=50
  dt=Tmax/N_max
  traj=Traj_max_vec[j]
  
  for(r in 1:replicates){
    X_vec=array(0, dim=c(N_max, dim, traj))
    W_vec=X_vec
    X_vec[1,,]<- c(1.87,-0.98)
    W_vec[1,,]<- 0
    
    for(i in 2:N_max){
      W_vec[i,,]= W_vec[i-1,,]+rnorm(dim*traj,mean = 0,sd=sqrt(dt))
    }
    for(i in 2:N_max){
      X_vec[i,,]= X_vec[i-1,,] + A%*%X_vec[i-1,,]*dt + G%*%(W_vec[i,,]- W_vec[i-1,,])
    }
    dX_vec=array(X_vec[2:nrow(X_vec),,]-X_vec[1:(nrow(X_vec)-1),,],dim=c(N_max-1, dim, traj))
    
    GTG_est=0
    for(t in 1:traj){
      GTG_est= GTG_est+ t(dX_vec[,,t])%*% (dX_vec[,,t])/Tmax
    }
    GTG_est= GTG_est/traj
    
    trueL=t(chol(G%*%t(G)))[lower.tri(t(chol(G%*%t(G))),diag = TRUE)]
    
    
    res <- optim(par=c(as.numeric(A),trueL)+2, MultiGaussianLike_foropt_mt,method = "BFGS",X_vec=X_vec,GTG_est=GTG_est,
                 control = list(trace=1)) 
    res
    
    L_est=matrix(0,ncol = dim,nrow=dim)
    L_est[lower.tri(L_est,diag = T)]= res$par[(1+ncol(X_vec)^2):length(res$par)]
    GTG_est = L_est %*% t(L_est)
    
    preterb_g[j]= preterb_g[j]+mean(abs(G%*%t(G)- GTG_est)^2)
    preterb_a[j]= preterb_a[j]+mean(abs(matrix(res$par[1:ncol(X_vec)^2],nrow = dim)-A)^2)
    
    
    res <- optim(par=c(as.numeric(A),trueL)+2, MultiGaussianLike_foropt_mt_rn,method = "BFGS",X_vec=X_vec,GTG_est=GTG_est,
                 control = list(trace=1)) 
    res
    
    L_est=matrix(0,ncol = dim,nrow=dim)
    L_est[lower.tri(L_est,diag = T)]= res$par[(1+ncol(X_vec)^2):length(res$par)]
    GTG_est = L_est %*% t(L_est)
    
    preterb_g_rn[j]= preterb_g[j]+mean(abs(G%*%t(G)- GTG_est)^2)
    preterb_a_rn[j]= preterb_a[j]+mean(abs(matrix(res$par[1:ncol(X_vec)^2],nrow = dim)-A)^2)
  }
  
}

plot(Traj_max_vec,log10(preterb_g/replicates),type="l",ylim=c(log10(min(preterb_a,preterb_g)/replicates),log10(max(preterb_a,preterb_g)/replicates))*1
     ,lwd=2,xlab="# of trajectories",ylab = "log10 MSE")
lines(Traj_max_vec,log10(preterb_a/replicates),col="red",lwd=2)
legend("topright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)


jpeg(paste0("MAEvstraj_additive.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(Traj_max_vec,log10(preterb_g/replicates),type="l",ylim=c(log10(min(preterb_a,preterb_g)/replicates),log10(max(preterb_a,preterb_g)/replicates))*1
     ,lwd=2,xlab="# of trajectories",ylab = "log10 MSE")
lines(Traj_max_vec,log10(preterb_a/replicates),col="red",lwd=2)
legend("topright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()



##############################################################################################################
###################    multi dimentional process (multiplicative noise)   3####################################################
##################################################################################################################################
A=matrix(c(1.76,0.98,-0.1,0),nrow=dim)
G1=matrix(c(-0.11,-0.29,-0.14,-0.22),nrow=dim)
G2=matrix(c(-0.17,0.81,0.59,0.18),nrow=dim)
m=2
Traj_max_vec=seq(from=10,to=60,by=10)
preterb_g=rep(0,length(Traj_max_vec))
preterb_a=rep(0,length(Traj_max_vec))
replicates=25

MultiGaussianLike_foropt_mt<-function(params,X_vec){
  A_est=matrix(params[1:ncol(X_vec)^2],nrow = ncol(X_vec))
  G_params=params[(ncol(X_vec)^2+1):length(params)]
  G1_est=matrix(G_params[1:(length(G_params)/m)],nrow=ncol(X_vec))
  G2_est=matrix(G_params[(1+ length(G_params)/m):length(G_params)],nrow=ncol(X_vec))
  Lik=1
  
  for(t in 1:dim(X_vec)[3]){
    curXvec=X_vec[,,t]
    cur_mu= t(matrix(curXvec,ncol = dim)) + A_est%*%t(matrix(curXvec,ncol = dim))*dt
    for(i in 2:nrow(curXvec)){
      G_est= cbind(G1_est%*% curXvec[i-1,], G2_est%*% curXvec[i-1,])
      cur_sig= (sqrt(dt)* G_est)%*% t(sqrt(dt)*G_est)
      donttry=F
      tryCatch( { chol(cur_sig)}
                , error = function(e) { donttry <<-T })
      
      if(donttry==F){
        Lik=Lik+ dmvn(X=curXvec[i,], mu=cur_mu[,i-1], sigma=cur_sig, ncores = 1, log = T)
      } else{
        Lik = Lik - 0
      }
      #print(dmvn(X=curXvec[i,], mu=cur_mu[,i-1], sigma=cur_sig, ncores = 1, log = T))
    }
  }
  
  return(-Lik)
}

for(j in 1:length(Traj_max_vec)){
  Tmax=1
  dim=2
  N_max=50
  dt=Tmax/N_max
  traj=Traj_max_vec[j]
  
  for(r in 1:replicates){
    X_vec=array(0, dim=c(N_max, dim, traj))
    W_vec=X_vec
    X_vec[1,,]<- c(1.87,-0.98)
    W_vec[1,,]<- 0
    
    for(i in 2:N_max){
      W_vec[i,,]= W_vec[i-1,,]+rnorm(dim*traj,mean = 0,sd=sqrt(dt))
    }
    
    for(t in 1:traj){
      for(i in 2:N_max){
        G= cbind(G1%*% X_vec[i-1,,t], G2%*% X_vec[i-1,,t])
        X_vec[i,,t]= X_vec[i-1,,t] + A%*%X_vec[i-1,,t]*dt + G%*%(W_vec[i,,t]- W_vec[i-1,,t])
      }
    }
    
    
    res <- optim(par=c(as.numeric(A),as.numeric(G1),as.numeric(G2)), MultiGaussianLike_foropt_mt,method = "BFGS",X_vec=X_vec,
                 control = list(trace=1)) 
    res
    
    
    G_params=res$par[(ncol(X_vec)^2+1):length(res$par)]
    G1_est=matrix(G_params[1:(length(G_params)/m)],nrow=ncol(X_vec))
    G2_est=matrix(G_params[(1+ length(G_params)/m):length(G_params)],nrow=ncol(X_vec))
    
    randx= matrix(c(1.33,0.72),ncol = 1)
    GTG_est = G1_est %*%randx %*% t(randx) %*%t(G1_est) + G2_est %*%randx %*% t(randx) %*%t(G2_est)
    TrueGGT=G1%*%randx %*% t(randx) %*%t(G1) + G2 %*%randx %*% t(randx) %*%t(G2)
    
    
    preterb_g[j]= preterb_g[j]+mean(abs(TrueGGT- GTG_est)^2)
    preterb_a[j]= preterb_a[j]+mean(abs(matrix(res$par[1:ncol(X_vec)^2],nrow = dim)-A)^2)
  }
  
}



plot(Traj_max_vec,log10(preterb_g/replicates),type="l",ylim=c(log10(min(preterb_a,preterb_g)/replicates),log10(max(preterb_a,preterb_g)/replicates))*1
     ,lwd=2,xlab="# of trajectories",ylab = "log10 MSE")
lines(Traj_max_vec,log10(preterb_a/replicates),col="red",lwd=2)
legend("topright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)


jpeg(paste0("MAEvstraj_multiplicative.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(Traj_max_vec,log10(preterb_g/replicates),type="l",ylim=c(log10(min(preterb_a,preterb_g)/replicates),log10(max(preterb_a,preterb_g)/replicates))*1
     ,lwd=2,xlab="# of trajectories",ylab = "log10 MSE")
lines(Traj_max_vec,log10(preterb_a/replicates),col="red",lwd=2)
legend("topright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()


jpeg(paste0("Wang_multiplicative_rawdata.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
matplot(seq(from=0,to=T,length.out=nrow(X_vec)),X_vec[,,1],type="l",ylab = "X_t",xlab = "T",lwd=2)
dev.off()
par(mar = c(5.1, 4.1, 0.5, 2.1))
matplot(seq(from=0,to=T,length.out=nrow(X_vec)),X_vec[,,1],type="l",ylab = "X_t",xlab = "T",lwd=2)


##############################################################################################################
###################   chaning starting points   ##############################################################
###############################################################################################################

A=matrix(c(1.76,0.98,-0.1,0),nrow=dim)
G=matrix(c(-0.11,-0.29,-0.14,-0.22),nrow=dim)
Traj_max_vec=seq(from=5,to=55,by=10)
preterb_g=rep(0,length(Traj_max_vec))
preterb_a=rep(0,length(Traj_max_vec))
replicates=15

MultiGaussianLike_foropt_mt<-function(params,X_vec,GTG_est){
  A_est=matrix(params[1:ncol(X_vec)^2],nrow = ncol(X_vec))
  L_est=matrix(0,nrow = ncol(X_vec),ncol = ncol(X_vec))
  L_est[lower.tri(L_est,diag = T)]<-params[(1+ncol(X_vec)^2):length(params)]
  Lik=1
  cur_sig= dt* L_est %*% t(L_est)#* GTG_est
  
  for(t in 1:dim(X_vec)[3]){
    curXvec=X_vec[,,t]
    cur_mu= t(matrix(curXvec,ncol = dim)) + A_est%*%t(matrix(curXvec,ncol = dim))*dt
    
    for(i in 2:nrow(curXvec)){
      Lik=Lik+ dmvn(X=curXvec[i,], mu=cur_mu[,i-1], sigma=cur_sig, ncores = 1, log = T)
    }
  }
  
  return(-Lik)
}

for(j in 1:length(Traj_max_vec)){
  Tmax=1
  dim=2
  N_max=50
  dt=Tmax/N_max
  traj=Traj_max_vec[j]
  
  for(r in 1:replicates){
    X_vec=array(0, dim=c(N_max, dim, traj))
    W_vec=X_vec
    X_vec[1,,]<- rnorm(dim)
    W_vec[1,,]<- 0
    
    for(i in 2:N_max){
      W_vec[i,,]= W_vec[i-1,,]+rnorm(dim*traj,mean = 0,sd=sqrt(dt))
    }
    for(i in 2:N_max){
      X_vec[i,,]= X_vec[i-1,,] + A%*%X_vec[i-1,,]*dt + G%*%(W_vec[i,,]- W_vec[i-1,,])
    }
    dX_vec=array(X_vec[2:nrow(X_vec),,]-X_vec[1:(nrow(X_vec)-1),,],dim=c(N_max-1, dim, traj))
    
    GTG_est=0
    for(t in 1:traj){
      GTG_est= GTG_est+ t(dX_vec[,,t])%*% (dX_vec[,,t])/Tmax
    }
    GTG_est= GTG_est/traj
    
    trueL=t(chol(G%*%t(G)))[lower.tri(t(chol(G%*%t(G))),diag = TRUE)]
    
    
    res <- optim(par=c(as.numeric(A),trueL)+2, MultiGaussianLike_foropt_mt,method = "BFGS",X_vec=X_vec,GTG_est=GTG_est,
                 control = list(trace=1)) 
    res
    
    L_est=matrix(0,ncol = dim,nrow=dim)
    L_est[lower.tri(L_est,diag = T)]= res$par[(1+ncol(X_vec)^2):length(res$par)]
    GTG_est = L_est %*% t(L_est)
    
    
    
    preterb_g[j]= preterb_g[j]+mean(abs(G%*%t(G)- GTG_est)^2)
    preterb_a[j]= preterb_a[j]+mean(abs(matrix(res$par[1:ncol(X_vec)^2],nrow = dim)-A)^2)
  }
  
}

jpeg(paste0("MAEvstraj_additive_randx0.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(Traj_max_vec,log10(preterb_g/replicates),type="l",ylim=c(log10(min(preterb_a,preterb_g)/replicates),log10(max(preterb_a,preterb_g)/replicates))*1
     ,lwd=2,xlab="# of trajectories",ylab = "log10 MSE")
lines(Traj_max_vec,log10(preterb_a/replicates),col="red",lwd=2)
legend("topright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()



##############################################################################################################
###################   changing dimension   ##############################################################
###############################################################################################################
MultiGaussianLike_foropt_mt<-function(params,X_vec,GTG_est){
  A_est=matrix(params[1:ncol(X_vec)^2],nrow = ncol(X_vec))
  L_est=matrix(0,nrow = ncol(X_vec),ncol = ncol(X_vec))
  L_est[lower.tri(L_est,diag = T)]<-params[(1+ncol(X_vec)^2):length(params)]
  Lik=1
  cur_sig= dt* L_est %*% t(L_est)#* GTG_est
  
  for(t in 1:dim(X_vec)[3]){
    curXvec=X_vec[,,t]
    cur_mu= t(matrix(curXvec,ncol = dim)) + A_est%*%t(matrix(curXvec,ncol = dim))*dt
    
    for(i in 2:nrow(curXvec)){
      
      donttry=F
      tryCatch( { chol(cur_sig)}
                , error = function(e) { donttry <<-T })
      
      if(donttry==F){
        Lik=Lik+ dmvn(X=curXvec[i,], mu=cur_mu[,i-1], sigma=cur_sig, ncores = 1, log = T)
      } else{
        Lik = Lik - 0
      }
      
    }
  }
  
  return(-Lik)
}


dim_vec=seq(from=2,by=1,to=4)
preterb_g=rep(0,length(dim_vec))
preterb_a=rep(0,length(dim_vec))
replicates=25
for(d in 1:length(dim_vec)){
  dim=dim_vec[d]
  A=matrix(rnorm(dim^2,sd=2)+1,nrow=dim)
  G=matrix(rnorm(dim^2,sd=2)+1,nrow=dim)
  Tmax=1
  N_max=50
  dt=Tmax/N_max
  traj=25
  
  for(r in 1:replicates){
    X_vec=array(0, dim=c(N_max, dim, traj))
    W_vec=X_vec
    X_vec[1,,]<- rnorm(dim)
    W_vec[1,,]<- 0
    
    for(i in 2:N_max){
      W_vec[i,,]= W_vec[i-1,,]+rnorm(dim*traj,mean = 0,sd=sqrt(dt))
    }
    for(i in 2:N_max){
      X_vec[i,,]= X_vec[i-1,,] + A%*%X_vec[i-1,,]*dt + G%*%(W_vec[i,,]- W_vec[i-1,,])
    }
    dX_vec=array(X_vec[2:nrow(X_vec),,]-X_vec[1:(nrow(X_vec)-1),,],dim=c(N_max-1, dim, traj))
    
    GTG_est=0
    for(t in 1:traj){
      GTG_est= GTG_est+ t(dX_vec[,,t])%*% (dX_vec[,,t])/Tmax
    }
    GTG_est= GTG_est/traj
    
    trueL=t(chol(G%*%t(G)))[lower.tri(t(chol(G%*%t(G))),diag = TRUE)]
    
    
    res <- optim(par=c(as.numeric(A),trueL), MultiGaussianLike_foropt_mt,method = "BFGS",X_vec=X_vec,GTG_est=GTG_est,
                 control = list(trace=1)) 
    res
    
    L_est=matrix(0,ncol = dim,nrow=dim)
    L_est[lower.tri(L_est,diag = T)]= res$par[(1+ncol(X_vec)^2):length(res$par)]
    GTG_est = L_est %*% t(L_est)
    
    
    
    preterb_g[d]= preterb_g[d]+mean(abs(G%*%t(G)- GTG_est)^2)
    preterb_a[d]= preterb_a[d]+mean(abs(matrix(res$par[1:ncol(X_vec)^2],nrow = dim)-A)^2)
  }
  
}





jpeg(paste0("MAEvsDIM_additive_randx0.jpeg"),width = 5, height = 3, units = 'in',res = 400)
par(mar = c(5.1, 4.1, 0.5, 2.1))
plot(dim_vec,log10(preterb_g/replicates),type="l",ylim=c(log10(min(preterb_a,preterb_g)/replicates),log10(max(preterb_a,preterb_g)/replicates))*1
     ,lwd=2,xlab="# of dimentions",ylab = "log10 MSE")
lines(dim_vec,log10(preterb_a/replicates),col="red",lwd=2)
legend("topright", legend=c("G", "A"),
       col=c("black", "red"),lty=1, cex=0.8,lwd=3)
dev.off()


#################################################
#################### other #######################################
###########################################################


for(i in 2:N_max){
  G= cbind(G1%*% X_vec[i-1,], G2%*% X_vec[i-1,])
  X_vec[i,]= X_vec[i-1,] + A%*%X_vec[i-1,]*dt + G%*%(W_vec[i,]- W_vec[i-1,])
}

matplot((X_vec),type="l")

MultiGaussianPDF<-function(X_i,mu,Sig){
  #(2*pi)^(-length(mu)/2)*det(Sig)^(-0.5)*exp(-0.5*t(X_i- mu)%*%solve(Sig)%*%(X_i - mu))
  dmvn(X=X_i, mu=mu, sigma=Sig, ncores = 1, log = T)
  #dmvnorm(X_i,mean=mu,sigma = Sig)
}


MultiGaussianLike_foropt<-function(params,X_vec){
  A_est=matrix(params[1:ncol(X_vec)^2],nrow = ncol(X_vec))
  G_params=params[(ncol(X_vec)^2+1):length(params)]
  G1_est=matrix(G_params[1:(length(G_params)/m)],nrow=ncol(X_vec))
  G2_est=matrix(G_params[(1+ length(G_params)/m):length(G_params)],nrow=ncol(X_vec))
  Lik=1
  dX_vec=matrix(X_vec[2:nrow(X_vec),]-X_vec[1:(nrow(X_vec)-1),],ncol = ncol(X_vec))
  for(i in 1:nrow(dX_vec)){
    cur_mu= A_est%*%X_vec[i,]*dt
    G_est= cbind(G1_est%*% X_vec[i,], G2_est%*% X_vec[i,])
    cur_sig= dt* G_est%*% t(G_est)
    Lik=Lik+log(max(MultiGaussianPDF(X_i= dX_vec[i,],mu=cur_mu,Sig=cur_sig),.Machine$double.xmin))
  }
  return(-Lik)
}

MultiGaussianLike_foropt(params=c(as.numeric(A),as.numeric(G1),as.numeric(G2)), X_vec = X_vec)
MultiGaussianLike_foropt(params=c(as.numeric(A)-1,as.numeric(G1),as.numeric(G2)), X_vec = X_vec)
MultiGaussianLike_foropt(params=c(as.numeric(A),as.numeric(G1)-1,as.numeric(G2)-1), X_vec = X_vec)

res <- optim(c(as.numeric(A),as.numeric(G1),as.numeric(G2))+2, MultiGaussianLike_foropt, method = "BFGS",X_vec=X_vec,control = list(trace=1))
res

matrix(res$par[1:ncol(X_vec)^2],nrow = dim)
A

G1%*% t(G1) + G2%*% t(G2)
G_params=res$par[(ncol(X_vec)^2+1):length(res$par)]
G1_est=matrix(G_params[1:(length(G_params)/m)],nrow=ncol(X_vec))
G2_est=matrix(G_params[(1+ length(G_params)/m):length(G_params)],nrow=ncol(X_vec))
G1_est%*% t(G1_est) + G2_est%*% t(G2_est)

preterb=seq(from=-2.5,to=2.5,by=0.05)
preterb_g=preterb
preterb_a=preterb
for(i in 1:length(preterb)){
  preterb_g[i]= MultiGaussianLike_foropt(params=c(as.numeric(A),as.numeric(G1)-preterb[i],as.numeric(G2)-preterb[i]), X_vec = X_vec)
  preterb_a[i]= MultiGaussianLike_foropt(params=c(as.numeric(A)-preterb[i],as.numeric(G1),as.numeric(G2)), X_vec = X_vec)
}
plot(preterb,preterb_g)
plot(preterb,preterb_a)


preterb=seq(from=-10,to=10,by=0.5)
preterb_a=preterb
preterb_a_gdiff=preterb
for(i in 1:length(preterb)){
  preterb_a[i]= MultiGaussianLike_foropt(params=c(as.numeric(A)-preterb[i],as.numeric(G1),as.numeric(G2)), X_vec = X_vec)
  preterb_a_gdiff[i]= MultiGaussianLike_foropt(params=c(as.numeric(A)-preterb[i],as.numeric(G1)-5,as.numeric(G2)-5), X_vec = X_vec)
}
plot(preterb,preterb_a)
plot(preterb,preterb_a_gdiff)


preterb=seq(from=-0.05,to=0.3,by=0.01)
preterb_g=preterb
preterb_g_adiff=preterb
for(i in 1:length(preterb)){
  preterb_g[i]= MultiGaussianLike_foropt(params=c(as.numeric(A),as.numeric(G1)-preterb[i],as.numeric(G2)-preterb[i]), X_vec = X_vec)
  preterb_g_adiff[i]= MultiGaussianLike_foropt(params=c(as.numeric(A)-5,as.numeric(G1)-preterb[i],as.numeric(G2)-preterb[i]), X_vec = X_vec)
}
plot(preterb,preterb_g)
plot(preterb,preterb_g_adiff)


preterb=seq(from=0.5,to=2.5,by=0.05)
preterb_g=preterb
preterb_a=preterb
for(i in 1:length(preterb)){
  preterb_g[i]= MultiGaussianLike_foropt(c(as.numeric(A),as.numeric(G1)*preterb[i],as.numeric(G2)*preterb[i]), X_vec = X_vec)
  preterb_a[i]= MultiGaussianLike_foropt(c(as.numeric(A)*preterb[i],as.numeric(G1),as.numeric(G2)), X_vec = X_vec)
}
plot(preterb,preterb_g)
plot(preterb,preterb_a)


N_max_vec=seq(from=1000,to=6000,by=100)
preterb_g=N_max_vec
preterb_a=N_max_vec
preterb=1

for(j in 1:length(N_max_vec)){
  Tmax=1
  dim=2
  dt=Tmax/N_max_vec[j]
  X_vec=matrix(0,nrow = N_max,ncol = dim)
  W_vec=X_vec
  X_vec[1,]<- c(1.87,-1)
  W_vec[1,]<- 0
  
  for(i in 2:N_max){
    W_vec[i,]= W_vec[i-1,]+rnorm(dim,mean = 0,sd=sqrt(dt))
  }
  for(i in 2:N_max){
    X_vec[i,]= X_vec[i-1,] + A%*%X_vec[i-1,]*dt + G%*%(W_vec[i,]- W_vec[i-1,])
  }
  good= MultiGaussianLike_foropt(c(as.numeric(A),as.numeric(G)), X_vec = X_vec)
  preterb_g[j]= good -MultiGaussianLike_foropt(c(as.numeric(A),as.numeric(G)+preterb), X_vec = X_vec)
  preterb_a[j]= good -MultiGaussianLike_foropt(c(as.numeric(A)+preterb,as.numeric(G)), X_vec = X_vec)
}
plot(Tmax/N_max_vec,preterb_a,type="l")
lines(Tmax/N_max_vec,preterb_g,col="red")



##multi dimentional process (multiplicative noise, optimizing separately)
library(mvtnorm)
N_max=15000
Tmax=1
dim=1
m=2
dt=Tmax/N_max
X_vec=matrix(0,nrow = N_max,ncol = dim)
W_vec=matrix(0,nrow = N_max,ncol = m)
X_vec[1,]<- 1.87 #c(1.87,-0.98)
W_vec[1,]<- 0
A=1.76 # matrix(c(1.76,0.98,-0.1,0),nrow=dim)
G1=-0.11 # matrix(c(-0.11,-0.29,-0.14,-0.22),nrow=dim)
G2=-0.17 #  matrix(c(-0.17,0.81,0.59,0.18),nrow=dim)
min_obs=100

for(i in 2:N_max){
  W_vec[i,]= W_vec[i-1,]+rnorm(dim,mean = 0,sd=sqrt(dt))
}


for(i in 2:N_max){
  G= cbind(G1%*% X_vec[i-1,], G2%*% X_vec[i-1,])
  X_vec[i,]= X_vec[i-1,] + A%*%X_vec[i-1,]*dt + G%*%(W_vec[i,]- W_vec[i-1,])
}

matplot((X_vec),type="l")

MultiGaussianPDF<-function(X_i,mu,Sig){
  #(2*pi)^(-length(mu)/2)*det(Sig)^(-0.5)*exp(-0.5*t(X_i- mu)%*%solve(Sig)%*%(X_i - mu))
  
  dmvnorm(X_i,mean=mu,sigma = Sig)
}


MultiGaussianLike_foropt<-function(params,X_vec,A_params=NULL,G_params=NULL){
  if(is.null(G_params) & !is.null(A_params)) G_params=params[1:length(params)]
  if(is.null(G_params) & is.null(A_params)) G_params=params[(ncol(X_vec)^2+1):length(params)]
  if(is.null(A_params)) A_params=params[1:ncol(X_vec)^2]
  
  
  A_est=matrix(A_params,nrow = ncol(X_vec))
  G1_est=matrix(G_params[1:(length(G_params)/m)],nrow=ncol(X_vec))
  G2_est=matrix(G_params[(1+ length(G_params)/m):length(G_params)],nrow=ncol(X_vec))
  Lik=1
  dX_vec=matrix(X_vec[2:nrow(X_vec),]-X_vec[1:(nrow(X_vec)-1),],ncol = ncol(X_vec))
  for(i in 1:nrow(dX_vec)){
    cur_mu= A_est%*%X_vec[i,]*dt
    G_est= cbind(G1_est%*% X_vec[i,], G2_est%*% X_vec[i,])
    cur_sig= dt* G_est%*% t(G_est)
    Lik=Lik+log(max(MultiGaussianPDF(X_i= dX_vec[i,],mu=cur_mu,Sig=cur_sig),.Machine$double.xmin))
  }
  return(-Lik)
}

MultiGaussianLike_foropt(params=c(as.numeric(A),as.numeric(G1),as.numeric(G2)), X_vec = X_vec)

res <- optim(c(as.numeric(G1),as.numeric(G2))+2, MultiGaussianLike_foropt, method = "BFGS",X_vec=X_vec,A_params= as.numeric(A)+2,control = list(trace=1,parscale=rep(1,length(c(as.numeric(G1),as.numeric(G2))+2))))
res


G_sofar<-res$par

G1%*% t(G1) + G2%*% t(G2)
G_params=G_sofar
G1_est=matrix(G_params[1:(length(G_params)/m)],nrow=ncol(X_vec))
G2_est=matrix(G_params[(1+ length(G_params)/m):length(G_params)],nrow=ncol(X_vec))
G1_est%*% t(G1_est) + G2_est%*% t(G2_est)

#check if G optimization was any good
preterb=seq(from=-3,to=3,by=0.1)
preterb_g=preterb
preterb_g_adiff=preterb
for(i in 1:length(preterb)){
  preterb_g[i]= MultiGaussianLike_foropt(params=c(as.numeric(A),as.numeric(G1)-preterb[i],as.numeric(G2)-preterb[i]), X_vec = X_vec)
  preterb_g_adiff[i]= MultiGaussianLike_foropt(params=c(as.numeric(A)+2,as.numeric(G1)-preterb[i],as.numeric(G2)-preterb[i]), X_vec = X_vec)
}
plot(preterb,preterb_g)
plot(preterb,preterb_g_adiff)



res <- optim(as.numeric(A)+2, MultiGaussianLike_foropt, method = "BFGS",X_vec=X_vec,G_params= G_sofar,control = list(trace=1,parscale=rep(1,length(as.numeric(A)+2))))
res

A_sofar<-res$par

#check if A optimization was any good
preterb=seq(from=-2,to=2,by=0.1)
preterb_a=preterb
preterb_a2=preterb
for(i in 1:length(preterb)){
  preterb_a[i]= MultiGaussianLike_foropt(params=c(as.numeric(A)-preterb[i],G_sofar), X_vec = X_vec)
  preterb_a2[i]= MultiGaussianLike_foropt(params=c(as.numeric(A)-preterb[i],as.numeric(G1),as.numeric(G2)), X_vec = X_vec)
}
plot(preterb,preterb_a)
plot(preterb,preterb_a2)

res <- optim(c(A_sofar,G_sofar), MultiGaussianLike_foropt, method = "BFGS",X_vec=X_vec,control = list(trace=1,parscale=rep(1,length(c(A_sofar,G_sofar)))))
res



