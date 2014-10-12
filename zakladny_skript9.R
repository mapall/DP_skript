########################################################
# parametre
#Dynamizované premenné
#tau1=0.1;tau2=0.9;  # danova sadzba zmenene na dynamicku premennu
#def1=1.1;def2=1.1    # koeficient deficitu >1 - zadlzovanie, <1 - setrenie
#r2=1.1
#####################
a1 = 5;a2=5    # potencialny produkt
d1=0.1;d2=0.1      # odpisova sadzba  (0 < d  1)
r2=1;     # urocitel dlhu krajiny

#  Cas
tmin=2; tmax= 300   # cele cislo

#  nemenit, len ak pridavame nove premenne
Y1=rep(0,tmax);Y2=rep(0,tmax);
R1=rep(0,tmax);R2=rep(0,tmax);
PI1=rep(0,tmax);PI2=rep(0,tmax);PI=rep(0,tmax);
I1=rep(0,tmax);I2=rep(0,tmax);
K1=rep(0,tmax);K2=rep(0,tmax);
Yinf1=rep(0,tmax);Yinf2=rep(0,tmax);
Dlh1=rep(0,tmax);Dlh2=rep(0,tmax);
W1=rep(0,tmax);W2=rep(0,tmax);
r1=rep(0,tmax);
def1=rep(0,tmax);tau1=rep(0,tmax);
def2=rep(0,tmax);tau2=rep(0,tmax);

# pociatocne hodnoty
K1[2]=5;K2[2]=5;
I1[2]=2;I2[2]=2;
Yinf1[2]=3;Yinf2[2]=3;
Dlh1[2]=0;Dlh2[2]=0;
r1[2]=1;
def1[2]=1.1;tau1[2]=0.2;
def2[2]=1.1;tau2[2]=0.2;

for (t in tmin:(tmax-1)){
  # produkcna funkcia 
  Y1[t] = a1 * (1-exp(-K1[t]))                # Y1 = a1*(1-exp(-K1)) nelinearna s max. a1
  Y2[t] = a2 * (1-exp(-K2[t]))                # Y2 = a2*(1-exp(-K2))
  # prijem vlady
  R1[t] = tau1[t]*Y1[t] 
  R2[t] = tau2[t]*Y2[t] 
  # zisk firmy vznika z prevadzok v oboch krajinach tu vyspelostinfrastruktury vylepsuje produkciu
  PI1[t] = (Yinf1[t]/Yinf2[t])*Y1[t]-R1[t]
  PI2[t] = (Yinf2[t]/Yinf1[t])*Y2[t]-R2[t]
  PI[t] = PI1[t]+PI2[t]
  # prechodova rovnica pre investicie
  I1[t+1]=+PI[t]*tau2[t]/(tau1[t]+tau2[t])  # dI1.... I1^(tau+1)-I1^tau
  I2[t+1]=+PI[t]*tau1[t]/(tau1[t]+tau2[t])
  #######################
  #   I1[t+1]=+PI[t]*PI1[t]/(PI1[t]+PI2[t])  # dI1.... I1^(tau+1)-I1^tau
  #   I2[t+1]=+PI[t]*PI2[t]/(PI1[t]+PI2[t])
  #######################
  # prechodova rovnica pre kapital
  if ((1-d1)*K1[t]+I1[t]>0){
    K1[t+1]=(1-d1)*K1[t]+I1[t]               # K1^(tau+1)=(1-d)K^tau + I^tau
  }else{
    K1[t+1]= 0
  }
  if ((1-d2)*K2[t]+I2[t]){
    K2[t+1]=(1-d2)*K2[t]+I2[t]
  }else{
    K2[t+1]= 0
  }
  # naklady na infrastrukturu def_i je koeficient deficitu >1 - zadlzovanie, <1 - setrenie
  #prechodova rovnica na infrastrukturu
  Yinf1[t+1]=(1-d1)*Yinf1[t]+def1[t] * R1[t]       # je to odnota infrastruk z minulosti bez odpisov + nove investicie 
  Yinf2[t+1]=(1-d2)*Yinf2[t]+def2[t] * R2[t]
  # prechodov? rovnica ?rokovej sadzby
  r1[t+1]=-(Dlh2[t]+(def2[t]-1)*R2[t])/(Dlh1[t]+(def1[t]-1)*R1[t])
  # prechodova rovnica dlhu
  Dlh1[t+1]=(Dlh1[t]+(def1[t]-1)*R1[t])*r1[t+1]
  Dlh2[t+1]=(Dlh2[t]+(def2[t]-1)*R2[t])*r2
  # blahobyt prvej a druhej krajiny
  W1[t+1]=(K1[t+1]+Yinf1[t+1]-Dlh1[t+1])
  W2[t+1]=(K2[t+1]+Yinf2[t+1]-Dlh2[t+1])
  #PID controller - urcenie dnaovej sadzby pre obdobie t  
  # krajina 1
  if (W1[t-1] > W1[t]){
    def1[t+1]=0.99*def1[t];
  } else {
    def1[t+1]=def1[t]
  }
  if(R1[t]<=R1[t-1]){
    if(tau1[t]>tau1[t-1]) {
      tau1[t+1]=0.9*tau1[t];
    }else{
      tau1[t+1]= 1/0.9*tau1[t]
    }
  }else{
    tau1[t+1]=tau1[t]
  }
  # krajina 2
  if (W2[t-1] > W2[t]){
    def2[t+1]=0.9*def2[t];
  } else {
    def2[t+1]=def2[t]
  }
  if(R2[t]<=R2[t-1]){
    if(tau2[t]>tau2[t-1]) {
      tau2[t+1]=0.9*tau2[t];
    }else{
      tau2[t+1]= 1/0.9*tau2[t]
    }
  }else{
    tau2[t+1]=tau2[t]
  }
}
udaje=data.frame(Y1,Y2,R1,R2,Yinf1,Yinf2,PI1,PI2,PI,I1,I2,K1,K2,Dlh1,Dlh2,W1,W2,r1,def1,tau1,def2,tau2)
plot.ts(udaje[tmin:(tmax-1),1:6])
plot.ts(udaje[tmin:(tmax-1),7:12])
plot.ts(udaje[tmin:(tmax-1),13:18])
plot.ts(udaje[tmin:(tmax-1),19:22])
dim(udaje)

udaje