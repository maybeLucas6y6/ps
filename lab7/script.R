# tutorial
dispersie = function(x, p) {
    media = sum(x * p);
    dispersia = sum(p * (x - media) ^ 2);
    return(dispersia);
}

# ex1
ex1 = function(v) {
    print(min(v));
    print(max(v));
    print(mean(v));
    print(sum(v));
    print(min(v) / max(v));
    h = 0;
    for (i in 1:length(v)) {
        if (v[i] > 40) {
            h = h + 1;
        }
    }
    print(h);
    print((length(v) - h) / length(v));
}

# ex2
ex2d = function(v) {
    n = length(v);
    y = vector();
    for (i in 1:(n - 1)) {
        y[i] = (min(v[1:i]) / max(v[(i+1):n]));
    }
    return(y);
}

# ex3
ex3 = function(file_name) {
    v = scan(file_name); 
    n = length(v);
    y = vector();
    for (i in 1:(n - 1)) {
        y[i] = min(v[1:i]) / max(v[(i+1):n]);
    }
    return(y);
}

# ex4: grafic binomial
ex4 = function(n, p) {
    x = 0:n;
    y = dbinom(x, n, p);
    barplot(y);
}

# ex5
ex5a = function(n, p) {
    return(max(dbinom(0:n, n, p)));
}
ex5b = function(n, p, k) {
    return(sum(dbinom(0:(k - 1), n, p)));
}
ex5c = function(n, p, k, m) {
    return(sum(dbinom(k:m, n, p)));
}

# ex6
ex6a = function(n, p) {
    return(sum(dgeom(n - 1, p)));
}

# ex9
ex9 = function(n, p) {
    print(barplot(
        dgeom(1:n, p),
        names.arg = 1:n,
        main = "Densitatea repartitiei geometrice",
        xlab = "Valoare",
        ylab = "Probabilitate"));
}

# ex10
ex10 = function(n, lambda) {
    print(barplot(
        dpois(1:n, lambda),
        names.arg = 1:n,
        main = "Densitatea repartitiei Poisson",
        xlab = "Valoare",
        ylab = "Probabilitate"));
}