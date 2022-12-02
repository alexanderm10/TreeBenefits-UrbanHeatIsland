qair2rh <- function(qair, temp, press = 1013.25){
  es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}

rh2qair <- function(rh, temp, press = 1013.25){
  es = 6.112 * exp((17.67*temp)/(temp+243))
  e=rh*es
  return(e)
}

TEMP=35
RH=90

# TEMP=(79-32)*5/9
# RH=60
# TEMP=20
# RH=50
# Calculate Wet Bulb Temperature according to Stull
Tw=TEMP*atan(0.151977*(RH + 8.313659)^(1/2)) + atan(TEMP + RH) - atan(RH - 1.676331) + 0.00391838*(RH)^(3/2) * atan(0.023101*RH) - 4.686035
Tw
Tw*9/5+32

TwStull = TEMP*atan(0.151977*(RH + 8.313659)^(1/2)) + atan(TEMP + RH) - atan(RH - 1.676331) + 0.00391838*(RH)^(3/2) *atan(0.023101*RH) - 4.686035
# Calculate Wet Bulb Temperature according to Sadeghi et al 2013




# -----------------------
TEMP=30
RH=90
QAIR = rh2qair(RH, TEMP)*.1
PRESS=1009.5*.1
# PRESS = 101.3*exp(-204/8200)


# method=Sadeghi 2013
if(TEMP>=0){
  a=0.611
  b=17.368
  c=238.88
} else {
  a=0.611
  b=17.368
  c=247.15
}

gamma = 0.4 # psychometric constant
# GAMMA = 0.00066*P
lambda = (2*a*(exp((b*TEMP)/(TEMP+c)) - 2*exp((b*TEMP)/(TEMP+2*c)) + 1))/(TEMP^2)
psi = a - gamma*PRESS*TEMP-QAIR
zeta = (4*a*exp((b*TEMP)/(TEMP+2*c)) - a*exp((b*TEMP)/(TEMP+c)) - 3*a)/TEMP
phi=zeta*TEMP + gamma*PRESS
delta = phi^2 - 4*lambda*psi

# Optional: PRESS = 101.3*exp(-ELEV/8200)

Tw = (-phi + sqrt(delta))/(2*lambda)




# Option 3 from Jensen et al 1990; https://www.campbellsci.com/forum?forum=1&l=thread&tid=1651
# 1) compute e as [es(T)*rH/100]
PRESS=1009.5 *.1 # convert millibar to kPa
TEMP=35
RH=90

ES = 0.611*exp(17.27*TEMP/(TEMP+237.3)) #in kPa
# T is drybulb temp in C

E = (RH/100)* 0.611*exp(17.27*TEMP/(TEMP+237.3))  #where e is ambient vapor pressure in kPa

# 2) compute dewpoint temperature (Td)
Td = (116.9+237.3*log(E))/(16.78-log(E)) #in C

# 3) compute wet bulb temperature (Tw)
GAMMA = 0.00066*PRESS #where P is ambient barometric pressure in kPa
DELTA = 4098*E/(Td+237.3)^2
Tw = ((GAMMA*TEMP)+(DELTA*Td))/(GAMMA+DELTA)


T13 <- TEMP-0.33*(TEMP-Td) 

