##### Library routines

# bin data into yearly values
makeYearlyAverages <- function (js,ys) {
   yrmin <- floor(min(js));
   yrmax <- floor(max(js));
   nbyr  <- yrmax - yrmin + 1;
   byrn  <- floor(js) - yrmin + 1;
   yry   <- 1:nbyr;
   yrd   <- 1:nbyr;
   yrx   <- 1:nbyr;
   for (byr in 1:nbyr) {
       bss <- (byrn==byr);
       mns <- mean(ys[bss]);
       sds <- sd(ys[bss]);
       mnx <- mean(js[bss]);
       yry[byr] <- mns;
       yrd[byr] <- sds;
       yrx[byr] <- mnx;
       #print (paste("Byr:",byr," x:",mnx," mean:",mns," sd:",sds));
   }
   return (data.frame(j=yrx, y=yry, d=yrd));
}

# bin annual data
makeRunningAverages <- function(djjs,diff,nbin) {
   binn <- floor(nbin*0.9999*djj/12)+1
   ayy  <- 1:nbin;
   ady  <- 1:nbin;
   axx  <- 1:nbin;
   for (bin in 1:nbin) {
       bss <- (binn==bin);
       mns <- mean(diff[bss]);
       sds <- sd(diff[bss]);
       mnx <- mean(djj[bss]/12);
       ayy[bin] <- mns;
       ady[bin] <- sds;
       axx[bin] <- mnx;
       #print (paste("Bin:",bin," x:",mnx," mean:",mns," sd:",sds));
   }
   axx[1]    <- 0;
   axx[nbin] <- 1;
   return (data.frame(dj=axx, dy=ayy, dd=ady));
}

# analysis
analysis <- function(offset) {
   co2maxdy <<- max(co2$y-co2$p);
   co2mindy <<- min(co2$y-co2$p);
   co2ddy   <<- co2maxdy-co2mindy;
   # temperature
   temp1    <<- temp0[temp0[,"YEAR"]>=Y0 & temp0[,"YEAR"]<Y1,];
   tmp      <<- data.frame(j = temp1[,"YEAR"] + 0.5 - Y0, y=temp1[,"MEAN"]);
   tmp$p    <<- predict(lmMod,data.frame(x = tmp$j,x2 = tmp$j*tmp$j,x3 = tmp$j*tmp$j*tmp$j));
   tempMod  <<- lm(y ~ x, data=data.frame(x=tmp$p,y=tmp$y));
   tmp$py   <<- predict(tempMod,data.frame(x=tmp$p));
   tmp$dy   <<- tmp$y-tmp$py;
   co2$pt   <<- predict(tempMod,data.frame(x=co2$y));
   tmpmaxdy <<- max(tmp$dy)[1];
   tmpmindy <<- min(tmp$dy)[1];
   tmpddy   <<- tmpmaxdy-tmpmindy;
   # linear rescale temperature to co2 scale (does not affect correlation)
   tmp$sdy  <<- ((tmp$dy-tmpmindy)/tmpddy)*co2ddy+co2mindy;
   # scale coefficients co2 -> temperature
   cftemp   <<- coef(tempMod);
   temperature <<-bquote(~ omega["HMGT"] == .(round(cftemp["(Intercept)"],4))
   	       + .(round(cftemp["x"],9)) * ~omega["ML-CO2"]);
   # interpolate co2 to temperature times...
   co2tmp   <<- approx(co2$j,co2$y-co2$p,tmp$j+offset);
   sstemp   <<- !is.na(co2tmp$y)
   oxtemp   <<- co2tmp$y[sstemp]; # interpolated co2 anomaly to temperature...
   oytemp   <<- tmp$sdy[sstemp] # temperature anomaly
   ctemp    <<- round(cor(oxtemp,oytemp),3);
   print(paste("Offset:",offset," Temp-corr:",ctemp));
   ########
   test <<-cor.test(oxtemp,oytemp)
   correlation <<- paste0(offset,"Year offset => r=",round(test$estimate,3),", p=",round(test$p.value,8)," ");
   print(test); 
   corrx    <<- c(corrx,offset);
   corrt    <<- c(corrt,ctemp);
}


################################################

colt="black";      lwdt=3;  ltyt=1;
cole="black";      lwde=3;  ltye=2;
colc="darkgray";       lwdc=6;  ltyc=1;
colc2="lightgray";  lwdc2=1; ltyc2=1;
coltr="black"; lwdtr=1; ltytr=5;

tm=Sys.time()
#t=format(tm,format='%Y%m%d_%H%M%S')
t=format(tm,format='%Y%m%d')

#Y0 <- 1970.0;
#Y1 <- 1990.0;

Y0 <- 1991.0;
Y1 <- 2020.0;

copyright <- "FTT/IMB (c)";
# volcanoes

vadj5<- c(0,0.5);
tyr5 <- c("Puyehue-Cordón Caulle");
vyr5 <- c(2011.5);
#
vadj6<- c(0,0.5);
tyr6 <- c("Pinatubo");
vyr6 <- c(1991.5);

# correlation

mcorr=0;
moffs=0.0;

# get CO2 measurements from file (Manua Loa CO2)
data <- read.csv(file="esrl.txt",sep=" ",head=TRUE)

sel  <- data[,"PPM"] > 0 & data[,"JJ"] > Y0 & data[,"JJ"] < Y1;
bulk <- sel & data[,"JJ"]<2020.0;
late <- sel & data[,"JJ"]>=2020.0;

jj <-data[sel,"JJ"] - Y0;
ppm <-data[sel,"PPM"];

ppmn <-data[bulk,"PPM"];
jjs=data[bulk,"JJ"]-Y0;
jjn=data[late,"JJ"]-Y0;

djj <-  (jj - floor(jj))*12;
djjs <- (jjs - floor(jjs))*12;
djjn <- (jjn - floor(jjn))*12;

ppms <- data[bulk,"PPM"];
ppmn <- data[late,"PPM"];

# Estimate the rest parameters using a linear model

lmMod <- lm(y ~ x + x2, data=data.frame(x=jjs, x2=jjs*jjs, x3=jjs*jjs*jjs, y=ppms, d=djjs));
cf <- coef(lmMod);
trend=bquote(~ omega["ML-CO2"](Year) == .(round(cf["(Intercept)"],3))  +  
              .(round(cf["x"],6))  * (Year - .(Y0)) +
	      .(round(cf["x2"],9)) * (Year - .(Y0))^2);# +",
	      #cf["x3"],"*(t -",Y0,")^3 
pred  <- predict(lmMod,data.frame(x = jj,     x2 = jj*jj, x3 = jj*jj*jj));
predn <- predict(lmMod,data.frame(x = jjn,    x2 = jjn*jjn, x3 = jjn*jjn*jjn));

co2   <- makeYearlyAverages(jjs,ppms);
co2$j[1]=0.5;

co2$p  <- predict(lmMod,data.frame( x = co2$j,     x2 = co2$j * co2$j, x3 = co2$j * co2$j * co2$j ));

nbin  <- 50;
diff  <- ppm-pred
avg   <- makeRunningAverages(djjs,diff,nbin);

nms   <- approx(avg$dj*12, avg$dy, djj);
nmn   <- approx(avg$dj*12, avg$dy, djjn);
nsd   <- approx(avg$dj*12, avg$dd, djjn);

# get HMGT temperature data from file (HadCrut4)
temp0 <- read.csv(file="uea.txt",sep=" ",head=TRUE) 

offsets <- c(-1,0,1);

corrx=c();
corrt=c();

for (offset in -1:1) {
 analysis(offset);
};
corr <- data.frame(x=corrx,t=corrt);

## max correlation:
#offset <- 0.75*1.0;
#analysis(offset);

# Plot fitted curve

#single <- TRUE;
single <- FALSE;
############## co2 time series with trend...
if (single) {
   print("Combining plots...");
   pdf(paste0("fig0_",t,".pdf"),height=5,width=10);
} else {
   pdf(paste0("fig01_",t,".pdf"),height=5,width=5);
}

par(mfrow=c(1,1));

plot(jj+Y0, ppm,col=colc2,lwd=lwdc2,lty=ltyc2,xlab="Year",ylab="Concentration (ppm)",
	    main=paste("CO2 measurements at Mauna Loa (ML-CO2)"));

# yearly co2
lines(co2$j+Y0, co2$y, col = colc, lwd = lwdc, lty=ltyc);

# trend
lines(jj+Y0, pred, col = coltr, lwd = lwdtr, lty=ltytr);


#mtext(trend,side=1,outer=FALSE,cex=0.5);

legend("topleft", legend=c("ML-CO2 annual average", "ML-CO2 weekly average", expression(omega["ML-CO2"])),
       col=c(colc, colc2, coltr), lty=c(ltyc,NA,ltytr), lwd=c(lwdc,lwdc2,lwdtr),pch=c(NA,1,NA),cex=0.8);

if (single) {
   mtext(copyright,side=4,cex=0.5)
   title(sub=trend, adj=1, line=-1.1, cex.sub=0.65, font=1 )
} else {
   dev.off()
   ############## temperature and co2 trend (not scaled)
   pdf(paste0("fig02_",t,".pdf"),height=5,width=5);
   par(mfrow=c(1,1));
};

# temperature
plot(tmp$j+Y0, tmp$y,col=colt,type="l",lwd=lwdt,lty=ltyt,xlab="Year",ylab="Temperature (K)",
	     main=paste("HadCRUT4 median global temperature (HMGT)"));

# co2
lines(co2$j+Y0, co2$pt, col = colc, lwd = lwdc, lty=ltyc);

# trend
lines(tmp$j+Y0, tmp$py, col = coltr, lwd = lwdtr, lty=ltytr);

print (temperature) ;

legend("topleft", legend=c("transformed ML-CO2","HMGT", expression(omega["HMGT"])),
    col=c(colc, colt, coltr), lty=c(ltyc,ltyt,ltytr), lwd=c(lwdc,lwdt,lwdtr),cex=0.8);

if (single) {
   mtext(copyright,side=4,cex=0.5)
   title(sub=temperature, adj=1, line=-1.1, cex.sub=0.65, font=1 )
} else {
  dev.off()
   ############## temperature anomalies (scaled) and co2 anomalies
   pdf(paste0("fig03_",t,".pdf"),height=5,width=5);
   par(mfrow=c(1,1));
};

plot(co2$j+Y0,co2$y-co2$p ,col=colc,xlab="Year",ylab="Concentration (ppm)",main=paste("ML-CO2 and mapped HMGT anomalies"),type="l",lwd=lwdc,lty=ltyc);

# 
lines(co2$j+Y0, co2$p-co2$p, col = coltr, lwd = lwdtr, lty=ltytr);

# temp
lines(tmp$j+Y0, tmp$sdy, col = colt, lwd = lwdt, lty=ltyt);

points(vyr5,vyr5-vyr5,type="p", col="black",cex=2);
text(vyr5,vyr5-vyr5+0.06,labels=tyr5, col="black",cex=1,adj=vadj5);
points(vyr6,vyr6-vyr6,type="p", col="black",cex=4);
text(vyr6,vyr6-vyr6+0.11,labels=tyr6, col="black",cex=1,adj=vadj6);

legend("topright", legend=c("ML-CO2 anomalies", "Mapped HMGT anomalies"),
    col=c(colc, colt), lty=c(ltyc,ltyt), lwd=c(lwdc,lwdt),cex=0.8);

if (single) {
   mtext(copyright,side=4,cex=0.5)
   title(sub=correlation, adj=1, line=-1.1, cex.sub=0.65, font=1 )
}

dev.off()
