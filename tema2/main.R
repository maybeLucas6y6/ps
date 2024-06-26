r = 3;
R = 10;
ex_B1 = function(N) {
    nc = 0;
    for (i in 1:N) {
        x = runif(1, -R-r, R+r);
        y = runif(1, -R-r, R+r);
        z = runif(1, -r, r);
        
        if (z^2 + (sqrt(x^2 + y^2) - R)^2 < r^2) {
            nc = nc + 1;
        }
    }
    v = ((2 * (R+r))^2 * (2*r)) * nc / N;
    return(v);
}

print("Ex B1")
print("Valoare exacta volum tor");
valoare_exacta = 2 * pi^2 * R * r^2;
print(valoare_exacta);

print("Valoare calculata pentru volumul torului cu esantion de dimensiune 10000")
v = ex_B1(10000);
print(v);
print("Eroare relativa");
print(abs(s - v) / valoare_exacta);

print("Valoare calculata pentru volumul torului cu esantion de dimensiune 20000")
v = ex_B1(20000);
print(v);
print("Eroare relativa");
print(abs(s - v) / valoare_exacta);

print("Valoare calculata pentru volumul torului cu esantion de dimensiune 50000")
v = ex_B1(50000);
print(v);
print("Eroare relativa");
print(abs(s - v) / valoare_exacta);

ex_B2 = function(N) {
    nc = 0;
    for (i in 1:N) {
        x = runif(1, 0, 2);
        y = runif(1, 0, 3);
        if (y>=0 && y<=2 * x && y<=6 - 3 * x) {
            nc = nc + 1;
        }
    }
    return((nc/N) * 6);
}
print("Ex B2")
print("Arie triunghi: ")
print(ex_B2(20000))
print("Valoarea exacta este 2.4")

ex_B3_a = function(N) {
    sum = 0;
    for (i in 1:N) {
        x = runif(1, -1, 1);
        sum = sum + (2 * x - 1) / (x * x - x - 6);
    }
    print("Valoare integrala:")
    print(log(3) - log(2));
    print("Valoare integrala (MC):")
    print(2 * sum / N);
}
ex_B3_a(10000);

ex_B3_b = function(N) {
    sum = 0;
    for(i in 1:N){
        x = runif(1, 3 + 10 ^ (-100), 11);
        sum = sum + ((x + 4) / ((x - 3) ^ (1 / 3)));
    }
    print("Valoare integrala:")
    print(61.2);
    print("Valoare calculata (MC):")
    print((8 - 10 ^ (-100)) * sum / N);
}
ex_B3_b(10000);

ex_B3_c = function(N, lambda){
    sum = 0;
    for(i in 1:N) {
        u = rexp(1, lambda);
        sum = sum + (u * exp(-u * u)) / (lambda * exp(-lambda * u));
    }
    print("Valoare integrala:")
    print(1/2)
    print("Valoare integrala (MC): ")
    print(sum / N);
}
ex_B3_c(10000, 1);

# ex_B4 

years_until_target = function(initial_users, target_users, n, p, q) {
    years = 0;
    current_users = initial_users;
    while (current_users < target_users) {
        new_users = rbinom(1, n, p);
        leaving_users = current_users * q; 
        current_users = current_users + new_users - leaving_users;
        years = years + 1;
    }
    return(years);
}

ex_B4_a = function(k) {
    initial_users = 10000;
    target_users = 15000;
    n = 1000;
    p = 0.25;
    q = 0.01;
    
    vals = vector();
    for (i in 1:k) {
        vals[i] = years_until_target(initial_users, target_users, n, p, q);
    }
    return(mean(vals));
}

print("Ani necesari pt atingerea target-ului de 15000")
print(ex_B4_a(10000))

ex_B4_b = function(k) {
    s = 0;
    target_years = 40 + 10 / 12;
    for (i in 1:k) {
        if (years_until_target(10000, 15000, 1000, 0.25, 0.01) <= target_years) {
            s = s + 1;
        }
    }
    return(s / k);
}

print("Probabilitatea ceruta")
print(ex_B4_b(10000));

ex_B4_c = function() {
    alfa = 1 - 0.99;
    z = qnorm(alfa / 2);
    epsilon = 0.01;
    p = 0.246;
    n_min = p * (1 - p) * (z / epsilon) ^ 2;
    return(ex_B4_b(n_min + 1));
}

print("Estimare");
print(ex_B4_c())