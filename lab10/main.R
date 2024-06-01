ex_I_1 = function(n) {
    nc = 0;
    for (i in 1:n) {
        x = runif(1, -1, 1); 
        y = runif(1, -1, 1); 
        z = runif(1, -1, 1); 
        
        if (x*x + y*y + z*z <= 1) {
            nc = nc + 1;
        }
    }
    
    aria = 8 * nc / n;
    print(abs(4 * pi / 3 - aria));                   #eroare absoluta
    print(abs(4 * pi / 3 - aria)) / abs(4 * pi / 3); #eroarea relativa
    print(4 * pi / 3);
    return(aria);
}

ex_I_2 = function(n) {
    nc = 0;
    for (i in 1:n) {
        x = runif(1, 1 / 2, 2); 
        y = runif(1, 0, 9 / 8); 
        ymax = -2 * (x ^ 2) + 5 * x - 2;
        if (y <= ymax) {
            nc = nc + 1;
        }
    }
    # constanta de corectie e aria dreptunghiului in care alegi punctele
    aria = (27 / 16) * nc / n;
    print(abs(9 / 8 - aria));
    print(abs(9 / 8 - aria)) / abs(9 / 8);
    print(9 / 8);
    return(aria);
}

mc_integration = function(n) {
    sum = 0;
    for (i in 1:n) {
        u = runif(1, 0, 10);
        sum = sum + exp(-u * u / 2);
    }
    return(10 * sum / n);
}

mc_integration_mean_sd = function(k, n) {
    estimates = vector();
    for(i in 1:k)
        estimates[i] = mc_integration(n);
    print(mean(estimates));
    print(sd(estimates));
}

mc_integration_improved = function(n) {
    sum = 0;
    for (i in 1:n) {
        u = rexp(1, 1);
        sum = sum + exp(-u * u) / exp(-u);
    }
    return(sum / n);
}

ex_II_1_b = function(n) {
    sum = 0;
    for (i in 1:n) {
        u = runif(1, 1, 4);
        sum = sum + exp(u);
    }
    val = 3 * sum / n;
    res = 51.87987;
    print(abs(val - res));
    print(abs(val - res)) / abs(val);
    print(val);
    return(val);
}

ex_II_1_d = function(n, a) {
    sum = 0;
    for (i in 1:n) {
        u = runif(1, 1, a);
        sum = sum + (1 / (4 * (u ^ 2) - 1));
    }
    val = (a - 1) * sum / n;
    res = log(3) / 4; # -0.2876
    print(abs(val - res));
    print(abs(val - res)) / abs(val);
    return(val);
}

ex_II_2_first = function(n, lambda) {
    sum = 0;
    for (i in 1:n) {
        u = rexp(1, lambda);
        sum = sum + exp(-2 * u * u) / (lambda * exp(-lambda * u));
    }
    val = sum / n;
    res = sqrt(pi / 8);
    #print(abs(val - res));
    #print(abs(val - res)) / abs(val);
    return(val);
}

ex_II_2_second = function(k, n, lambda) {
    estimates = vector();
    for(i in 1:k)
        estimates[i] = ex_II_2_first(n, lambda);
    print(mean(estimates));
    print(sd(estimates));
}

Nr_days = function() {
    nr_days = 1;
    last_errors = c(27, 31);
    nr_errors = 27;
    while(nr_errors > 0) {
        lambda = min(last_errors);
        nr_errors = rpois(1, lambda);
        last_errors = c(nr_errors, last_errors[1]);
        nr_days = nr_days + 1;
    }
    return(nr_days);
}
MC_nr_days = function(N) {
    s = 0;
    for(i in 1:N)
        s = s + Nr_days();
    return(s/N);
}

ex_III_1 = function() {
    nr_days = 1;
    last_errors = c(13, 15, 9);
    nr_errors = 13;
    while(nr_errors > 0) {
        lambda = mean(last_errors);
        nr_errors = rpois(1, lambda);
        last_errors = c(nr_errors, last_errors[1], last_errors[2]);
        nr_days = nr_days + 1;
    }
    return(nr_days);
}
ex_III_1_mean = function(N) {
    s = 0;
    for(i in 1:N)
        s = s + ex_III_1();
    return(s/N);
}

ex_III_2 = function() {
    x = runif(1, 0, 1);
    if (x < 0.75) {
        return(rexp(1, 4));
    }
    else {
        return(rexp(1, 12));
    }
}
ex_III_2_mean = function(N) {
    s = 0;
    for(i in 1:N)
        s = s + ex_III_2();
    return(s/N);
}

Nr_days_prob = function() {
    nr_days = 2;
    last_errors = c(18, 22, 28);
    nr_errors = 18;
    while(nr_errors > 0) {
        lambda = min(last_errors);
        nr_errors = rpois(1, lambda);
        last_errors = c(nr_errors, last_errors[1:2]) ;
        nr_days = nr_days + 1;
    }
    return(nr_days);
}
MC_nr_days_21_prob = function(N) {
    s = 0;
    for(i in 1:N) {
        if(Nr_days_prob() > 21)
        s = s + 1;
    }
    return(s/N);
}
MC_nr_days_21_improv = function(epsilon, prob) {
    alfa = 1 - prob;
    p_0 = MC_nr_days_21_prob(2000);
    N = p_0 * (1 - p_0) * (qnorm(alfa / 2) / epsilon) ^ 2; 
    print(N);
    MC_nr_days_21_prob(N);
}
