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

s = 2 * pi^2 * R * r^2;
print("Eroare volum tor");
print(abs(s - ex_B1(10000)) / s);
print(abs(s - ex_B1(20000)) / s);
print(abs(s - ex_B1(50000)) / s);

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
print("arie triunghi: ")
print(ex_B2(20000))

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

# ex_B4 inca nu stiu
#ex_B4 = function()