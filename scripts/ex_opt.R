require(rBayesianOptimization)
set.seed(2108)

############################
# FUNÇÂO DE TESTE COMPLEXA #
############################
f = function(x,y){
  fim = (.7*(exp(-(x - 2)^2) + exp(-(x - 6)^2/10) + 1/ (x^2 + 1)) + 
        .2*(exp(-(y - 2)^2) + exp(-(y - 6)^2/10) + 1/ (y^2 + 1)) +
        .8*(exp(-(y - 2)^2) + exp(-(y - 6)^2/10) + 1/ (y^2 + 1)) +
        (exp(-(y - 2)^2) + exp(-(y - 6)^2/10) + 1/ (y^2 + 1)))*(sin(x)*cos(y))^2
  
  return(list(Score = fim))
}

x = seq(0,5,length.out = 300)
y = seq(0,5,length.out = 300)

# -------------------- #
# SEARCH GRID COMPLETE #
# -------------------- #
require(ggplot2)

grid = expand.grid(x,y)
colnames(grid) = c("x","y")

print(paste0("A grid que será usada como base tem ",nrow(grid)," linhas"))

grid$Value = f(grid$x,grid$y)$Score

ggplot(grid,aes(x=x,y=y,col=Value,z=Value)) + 
  geom_point() + 
  geom_contour(color = "black") + 
  scale_colour_gradientn(colours=rev(rainbow(2))) +
  ggtitle("Região de valores")

print(paste0("O valor máximo encontrado na grid foi ",round(max(grid$Value),4)))


########################
# OPTIMIZAÇÃO BAYSIANA #
########################
OPT_Res <- BayesianOptimization(f,
                                bounds = list(x = c(0, 5),
                                              y = c(0, 5)),
                                init_points = 10, n_iter = 20,
                                acq = "poi", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

print(paste0("O valor máximo encontrado na optimização bayesiana com ",nrow(OPT_Res$History)," iterações foi ",round(OPT_Res$Best_Value,4)))

ggplot() + 
  geom_point(data = grid,aes(x=x,y=y,col=Value)) + 
  geom_contour(data = grid,aes(x=x,y=y,col=Value,z=Value),color = "black") +  
  scale_colour_gradientn(colours=rev(rainbow(2))) +
  geom_text(data = OPT_Res$History,aes(x=x,y=y,label=1:nrow(OPT_Res$History))) +
  ggtitle("Região de valores")
