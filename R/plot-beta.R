#Code author: Mine Dogucu
library(ggplot2)


plot_beta <- function(alpha, beta){
  ggplot(data = data.frame(x = c(0, 1)),
         aes(x)) +
    stat_function(fun = dbeta,
                  n = 101,
                  args = list(shape1 = alpha,
                              shape2=beta)) +
    labs(x = expression(pi), y =
           expression(paste("f(",pi,")")),
         title = paste0("Beta(", alpha, ",",beta, ")")) +
    theme(plot.title = element_text(hjust = 0.5))
  
}
