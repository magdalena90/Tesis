
library(MASS)
library(ggplot2)

############################################################################################################################################
############################################################################################################################################
# LDA
############################################################################################################################################
# Crear las dos clases de puntos:

n <- 1000
Sigma <- matrix(c(2,-1,-1,2),2,2)
mu1 <- c(0,0)
mu2 <- c(10,10)

set.seed(1234)
c1 <- cbind(mvrnorm(n, mu = mu1, Sigma), rep("Clase 1", n))
c2 <- cbind(mvrnorm(n, mu = mu2, Sigma), rep("Clase 2", n))

data_lda <- data.frame(rbind(c1,c2))
names(data_lda) <- c("X", "Y", "Clases")
data_lda$X <- as.numeric(as.character(data_lda$X))
data_lda$Y <- as.numeric(as.character(data_lda$Y))

remove(Sigma, c1,c2)
############################################################################################################################################
# LDA Y puntos por los que pasa el hiperplano separador

lda_model <- lda(Clases ~., data_lda, prior = c(0.5,0.5))

W <- lda_model$scaling                                    # Winv <- W%*%t(W) no sé por qué funciona
# el hiperplano es ortogonal a:
l <- ginv(W)*(lda_model$means[1,] - lda_model$means[2,])
# y como pasa por lda_model$means, también pasa por:
p1 <- colMeans(lda_model$means) + c(l[1], -l[2])
p2 <- colMeans(lda_model$means) - c(l[1], -l[2])
ps <- data.frame(rbind(p1,p2))

remove(lda_model,W,l,p1,p2)
############################################################################################################################################
# Gráficas
ggplot(data_lda, aes(x=X, y=Y, color=Clases)) + geom_point(alpha = 0.6, size = 2.5) + theme_bw()  + 
  theme(axis.line = element_line(colour = "gray"), panel.grid.major = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank(), legend.key=element_rect(colour='transparent') ) +
  ggtitle("Distribucion de puntos LDA") +
  scale_colour_manual(values=c("#99cc00", "#ff3377")) +
  geom_line(data=ps, aes(x=X, y=Y), color="gray", size=1)


############################################################################################################################################
############################################################################################################################################
# QDA

sigma1 <- matrix(c(2,-1,-1,2),2,2)
sigma2 <- matrix(c(1,0,0,1),2,2)

set.seed(1234)
c3 <- cbind(mvrnorm(n, mu = mu1, Sigma=sigma1), rep(1, n))
c4 <- cbind(mvrnorm(n, mu = mu2, Sigma=sigma2), rep(2, n))

data_qda <- data.frame(rbind(c3,c4))
names(data_qda) <- c("X", "Y", "Clases")
data_qda$X <- as.numeric(as.character(data_qda$X))
data_qda$Y <- as.numeric(as.character(data_qda$Y))
data_qda$Clases <- as.factor(data_qda$Clases)

remove(sigma1, sigma2, c3, c4)
############################################################################################################################################
# QDA
qda_model <- qda(Clases ~., data_qda, prior = c(0.5,0.5))

x = seq(min(data_qda[,1]), max(data_qda[,1]), len = n)
y = seq(min(data_qda[,2]), max(data_qda[,2]), len = n)
z = data.frame(as.matrix(expand.grid(x,y), 0))
names(z) = c("X","Y")
grid_pred <- predict(qda_model, z)$class

data_dec_bound <- data.frame(cbind(z,grid_pred))
names(data_dec_bound) <- c("X","Y","Clases")
data_dec_bound$X <- as.numeric(as.character(data_dec_bound$X))
data_dec_bound$Y <- as.numeric(as.character(data_dec_bound$Y))

remove(x,y,z,grid_pred)
############################################################################################################################################
# Gráficas
ggplot(data_qda, aes(x=X, y=Y, color=Clases)) + geom_point(alpha = 0.6, size = 2.5) + theme_bw()  + 
  theme(axis.line = element_line(colour = "gray"), panel.grid.major = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank(), legend.key=element_rect(colour='transparent') ) +
  ggtitle("Distribucion de puntos QDA") +
  scale_colour_manual(values=c("#99cc00", "#ff3377")) +
  stat_contour(data=data_dec_bound, aes(x=X, y=Y, z=as.numeric(as.character(Clases))), color="gray")



