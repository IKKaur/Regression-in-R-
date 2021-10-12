# Regression-in-R-
# I will be running a few regression models in R and comparing them

# Load Anscombe's data 
data(anscombe)
# View dataset
View(anscombe) 
summary(anscombe)

## Simple version
## Plotting relation in x1 and y1 for general understanding
plot(anscombe$x1,anscombe$y1)

![Rplot14](https://user-images.githubusercontent.com/90278106/136883476-55cc62b2-34bd-45ae-a48e-7659aa846288.png)
The coordinates of x1 and y1 are a little scatter but still have a positive corelation.

## Create 4 models for regression
# Model 1 
lm1 <- lm(y1 ~ x1, data=anscombe)
summary(lm1)
# Model 2
lm2 <- lm(y2 ~ x2, data=anscombe)
summary(lm2)
# Model 3
lm3 <- lm(y3 ~ x3, data=anscombe)
summary(lm3)
# Model 4 
lm4 <- lm(y4 ~ x4, data=anscombe)
summary(lm4)

## Comparison of Models
	   p-value   	Adjusted R2
Model 1   0.00217	   0.6275
Model 2	  0.002179	   0.6292
Model 3	  0.002176	   0.6292
Model 4   0.002165	   0.6297
![image](https://user-images.githubusercontent.com/90278106/136884392-8c104142-0d08-4ff5-b7bc-d94ba419eed3.png)

## Now create plots with the corelation line 
# Plot 1 
plot(anscombe$x1,anscombe$y1)
abline(coefficients(lm1))
# Plot 2
plot(anscombe$x2,anscombe$y2)
abline(coefficients(lm2))
# Plot 3
plot(anscombe$x3,anscombe$y3)
abline(coefficients(lm3))
# Plot 4
plot(anscombe$x4,anscombe$y4)
abline(coefficients(lm4))

![2021-10-11 (2)](https://user-images.githubusercontent.com/90278106/136885751-cec946a7-b301-4d02-8057-965be1c98ee1.png)

# ANOVA Table 
ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))

for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}

# We can also have a combined output of the plots to make it easier to compare
# It can be done in R itself
# This time we are going to change the color of the co-ordinates to make it look attractive 
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))

# Plot charts using for loop
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)

![Rplot20](https://user-images.githubusercontent.com/90278106/136886650-2d414765-31d2-40ba-9c5c-4506b849e744.png)
