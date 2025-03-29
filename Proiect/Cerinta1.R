# -------------------------------------------1----------------------------------------------
set.seed(123)  # Asigură reproducibilitatea

# Parametrii problemei
n <- 10  # Numărul de etape
lambda <- runif(n, 0.5, 2)  # Ratele exponențiale pentru fiecare etapă
alpha <- runif(n, 0.5, 1)  # Probabilitățile de a continua

# Funcția de simulare a lui T
simulate_T <- function() {
  T_total <- 0
  i <- 1
  while (i <= n) {
    T_total <- T_total + rexp(1, lambda[i])  # Simulează timpul pentru etapa i
    if (runif(1) > alpha[i]) return(list(T_total, FALSE, i))  # Probabilitatea de oprire
    i <- i + 1
  }
  return(list(T_total, TRUE, n))  # Finalizare activitate
}

# Simulăm 10^6 valori
n_sim <- 10^6
sim_results <- replicate(n_sim, simulate_T(), simplify = FALSE)
t_values <- sapply(sim_results, function(x) x[[1]])
completion_flags <- sapply(sim_results, function(x) x[[2]])
stopping_stages <- sapply(sim_results, function(x) x[[3]])

# Aproximarea lui E(T)
E_T <- mean(t_values)
cat("Estimarea lui E(T):", E_T, "\n")

# Reprezentare grafică
hist(t_values, breaks = 100, probability = TRUE, 
     main = "Distribuția simulată a lui T", 
     xlab = "Timp total T", col = "skyblue", border = "white")

# Suprapunere densitate estimată
lines(density(t_values), col = "red", lwd = 2)

# ------------------------------------------2------------------------------------------

# Calcul exact al lui E(T)
E_T_exact <- sum(sapply(1:n, function(i) {
  (1 / lambda[i]) * prod(alpha[1:(i-1)])
}))
cat("Valoarea exactă a lui E(T):", E_T_exact, "\n")

#-------------------------------------------3-------------------------------------------

# Aproximare probabilitate finalizare activitate
P_completion <- mean(completion_flags)
cat("Probabilitatea ca persoana A finalizează activitatea:", P_completion, "\n")

#-------------------------------------------4---------------------------------------------

# Aproximare P(T <= sigma)
sigma <- 5  # Definim o valoare pentru sigma
P_T_leq_sigma <- mean(t_values <= sigma)
cat("Probabilitatea ca T <=", sigma, "este", P_T_leq_sigma, "\n")

#-------------------------------------------5--------------------------------------------

# Timp minim și maxim pentru finalizările reușite
t_values_completed <- t_values[completion_flags]
T_min <- min(t_values_completed)
T_max <- max(t_values_completed)
cat("Timpul minim de finalizare:", T_min, "\n")
cat("Timpul maxim de finalizare:", T_max, "\n")

# Reprezentare grafică a timpilor de finalizare
hist(t_values_completed, breaks = 100, probability = TRUE, 
     main = "Distribuția timpilor de finalizare", 
     xlab = "Timp total T (doar pentru finalizări)", col = "lightgreen", border = "white")

#------------------------------------------6---------------------------------------------

# Probabilitatea de oprire înainte de etapa k
stop_probabilities <- sapply(2:n, function(k) mean(stopping_stages < k))

# Afișare probabilități
cat("Probabilități de oprire înainte de fiecare k:\n")
print(data.frame(k = 2:n, P_stop = stop_probabilities))

# Reprezentare grafică
plot(2:n, stop_probabilities, type = "b", col = "blue", pch = 19, lwd = 2,
     main = "Probabilitatea de oprire înainte de etapa k",
     xlab = "Etapa k", ylab = "Probabilitate de oprire", 
     ylim = c(0, 1))
grid()
