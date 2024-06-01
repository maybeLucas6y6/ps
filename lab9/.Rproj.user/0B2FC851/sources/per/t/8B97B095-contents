density_exponential = function(lambda, a, n) {
    x = seq(0, a, length = n); 
    y = dexp(x, lambda);
    plot(x, y, type = 'l');
    # (1, 20, 300)
}

density_gauss = function(m, sigma, a, n) {
    x = seq(m - a, m + a, length = n); 
    y = dnorm(x, m, sigma);
    plot(x, y, type = 'l');
    # (2, 1.3, 5, 300)
    # (0, 1, 5, 300)
}

density_gamma = function(shape, rate, a, n) {
    x = seq(0, a, length = n); 
    y = dgamma(x, shape, rate);
    plot(x, y, type = 'l');
    # (2, 1.3, 15, 400)
}

density_student = function(r, a, n) {
    x = seq(-a, a, length = n); 
    y = dt(x, r);
    plot(x, y, type = 'l');
    # (3, 8, 300)
}

lnm_poisson = function(lambda, n) {
    sum = 0;
    for (i in 1:n) {
        sum = sum + rpois(1, lambda);
    }
    return(sum/n);
    # (2, 5000)
    # sau
    # return(mean(rpois(n, lambda)));
}

lnm_gamma = function(shape, rate, n) {
    print(shape / rate);
    return(mean(rgamma(n, shape, rate)));
    # (2, 1.5, 10000)
}

lnm_student = function(r, n) {
    return(mean(rt(n, r)));
    # (3, 10000)
}

lnm_exp = function(lambda, n) {
    print(1 / lambda);
    return(mean(rexp(n, lambda)));
    # (3, 10000)
}

tlc_poisson = function(lambda, n, N, z) {
    expectation = lambda;
    st_dev = sqrt(lambda);
    upper_bound = z * st_dev/sqrt(n) + expectation;
    sum = 0;
    for(i in 1:N) {
        x_n = mean(rpois(n, lambda));
        if(x_n <= upper_bound) {
            sum = sum + 1;
        }
    }
    print(pnorm(z));
    return(sum/N);
    # (2, 50, 10000, 1)
}

tlc_exponential = function(lambda, n, N, z) {
    expectation = 1 / lambda;
    st_dev = 1 / lambda;
    upper_bound = z * st_dev/sqrt(n) + expectation;
    sum = 0;
    for(i in 1:N) {
        x_n = mean(rexp(n, lambda));
        if(x_n <= upper_bound) {
            sum = sum + 1;
        }
    }
    print(pexp(z));
    return(sum/N);
    # (3, 40, 10000, 2)
}

tlc_gamma = function(shape, rate, n, N, z) {
    expectation = shape / rate;
    st_dev = sqrt(shape) / rate;
    upper_bound = z * st_dev/sqrt(n) + expectation;
    sum = 0;
    for(i in 1:N) {
        x_n = mean(rgamma(n, shape, rate));
        if(x_n <= upper_bound) {
            sum = sum + 1;
        }
    }
    print(pgamma(z, shape, rate));
    return(sum/N);
    # (2, 1.5, 50, 10000, 4)
}

demoivre_laplace = function(n, p, k) {
    expectation = n * p;
    st_dev = sqrt(n * p * (1 - p));
    xd = (k + 0.5 - expectation) / st_dev;
    xs = (k - 0.5 - expectation) / st_dev;
    print(pnorm(xd) - pnorm(xs));
    print(dbinom(k, n, p));
    # (30, 0.3, 15)
}

demoivre_laplace_1 = function(n, p, k1, k2) {
    expectation = n * p;
    st_dev = sqrt(n * p * (1 - p));
    xd = (k2 + 0.5 - expectation) / st_dev;
    xs = (k1 - 0.5 - expectation) / st_dev;
    print(pnorm(xd) - pnorm(xs));
    print(sum(dbinom(k1:k2, n, p)));
    # (25, 0.7, 10, 20) prob ca o dnorm cu param 25, 0.7 sa fie intre 10 si 20
}