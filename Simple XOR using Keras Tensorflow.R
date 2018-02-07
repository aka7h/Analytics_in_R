library(keras)

#simple XOR using Keras in R

X = rbind(c(0,0),c(0,1),c(1,0),c(1,1))
Y = rbind(c(0),c(0),c(1),c(1))

model <- keras_model_sequential()

model %>% layer_dense(units = 5,activation='relu', input_shape=c(2)) %>% layer_dense(units=1,activation="sigmoid")

model %>% compile(optimizer = "sgd", loss="mse")

#before fitting
model %>% evaluate(X,Y)

model %>% fit(X,Y,epochs=10000, verbose=FALSE)

#after fitting
model %>% evaluate(X,Y)
