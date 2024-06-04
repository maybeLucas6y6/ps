confidence_interval_z = function(n, sample_mean, alfa, sigma) {
    critical_z = -qnorm(alfa / 2);
    left = sample_mean - critical_z * sigma/sqrt(n);
    right = sample_mean + critical_z * sigma/sqrt(n);
    print(left)
    print(right)
}

ex_D_1 = function() {
    filename = "probabilitati.csv";
    data = read.csv(filename, header = TRUE);
    data = data[["probabilitati"]]
    data_mean = mean(data);
    n = length(data);
    sigma = sqrt(92.16);
    
    alfa = 0.05;
    print("Pentru alfa 0.05")
    confidence_interval_z(n, data_mean, alfa, sigma);
    
    alfa = 0.01;
    print("Pentru alfa 0.01")
    confidence_interval_z(n, data_mean, alfa, sigma);
}

print("Ex 1")
ex_D_1();

confidence_interval_t = function(n, sample_mean, alfa, s) {
    critical_t = -qt(alfa / 2, n - 1);
    left = sample_mean - critical_t * s/sqrt(n);
    right = sample_mean + critical_t * s/sqrt(n);
    print(left)
    print(right)
}

ex_D_2 = function() {
    filename = "statistica.csv";
    data = read.csv(filename, header = TRUE);
    data = data[["statistica"]]
    data_mean = mean(data);
    dist_std = sd(data);
    n = length(data);
    
    alfa = 0.05;
    print("Pentru alfa 0.05")
    confidence_interval_t(n, data_mean, alfa, dist_std);
    
    alfa = 0.01;
    print("Pentru alfa 0.01")
    confidence_interval_t(n, data_mean, alfa, dist_std);
}

print("Ex 2")
ex_D_2();

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

ex_D_3 = function() {
    # H0: p = 0.85
    # Ha: p > 0.85
    
    print("Pentru 5% nivel de semnificatie");
    proportion_test(100, 0.86, 0.85, 0.05, 'r');
    
    print("Pentru 1% nivel de semnificatie");
    proportion_test(100, 0.86, 0.85, 0.01, 'r');
}

print("Ex 3");
ex_D_3();