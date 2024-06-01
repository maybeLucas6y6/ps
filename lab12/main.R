confidence_interval_z = function(n, sample_mean, alfa, sigma) {
     critical_z = -qnorm(alfa / 2);
     left = sample_mean - critical_z * sigma/sqrt(n);
     right = sample_mean + critical_z * sigma/sqrt(n);
     print(left)
     print(right)
}

confidence_interval_t = function(n, sample_mean, alfa, s) {
     critical_t = -qt(alfa / 2, n - 1);
     left = sample_mean - critical_t * s/sqrt(n);
     right = sample_mean + critical_t * s/sqrt(n);
     print(left)
     print(right)
}

# media de sel cu mean si dev std cu sd

ex_III_5 = function(filename, alfa) {
    v = scan(filename);
    critical_z = -qnorm(alfa / 2);
    n = length(v);
    sample_mean = mean(v);
    sigma = sd(v);
     critical_t = -qt(alfa / 2, n - 1);
     left = sample_mean - critical_t * s/sqrt(n);
     right = sample_mean + critical_t * s/sqrt(n);
     print(left)
     print(right)
}

# type = 'l', 'r', 's'
proportion_test = function(n, p_prim, p_0, alfa, type) {
    z_score = (p_prim - p_0) / sqrt(p_0 * (1 - p_0) / n);
    print(z_score);
    if (type == 'l') {
        critical_z = qnorm(alfa); # critical_z < 0
        print(critical_z);
        if (z_score < critical_z) {
            print("H0 se respinnge si se accepta Ha.");
        }
        else {
            print("H0 nu se poate respinge");
        }
    }
    else if (type == 'r') {
        critical_z = -qnorm(alfa); # critical_z > 0
        print(critical_z);
        if (z_score > critical_z) {
            print("H0 se respinnge si se accepta Ha.");
        }
        else {
            print("H0 nu se poate respinge");
        }
    }
    else if (type == 's') {
        critical_z = -qnorm(alfa / 2);
        print(critical_z);
        if (abs(z_score) > abs(critical_z)) {
            print("H0 se respinnge si se accepta Ha.");
        }
        else {
            print("H0 nu se poate respinge");
        }
    }
}

# H0: p = 0.6
# Ha: p > 0.6
# proportion_test(100, 0.63, 0.6, 0.01, 'r'
ex_IV_2 = function() {
    # H0: p = 0.1
    # Ha: p > 0.1
    proportion_test(150, 0.13, 0.1, 0.01, 'r');
}