exA_1_a = function(lambda, p, k, m, n) {
    interval = k:m;
    poisson = dpois(interval, lambda);
    geometrica = dgeom(interval - 1, p);
    binomiala = dbinom(interval, n, p);
    print(poisson);
    print(geometrica);
    print(binomiala);
}
exA_1_a(10, 0.5, 7, 18, 25);

exA_1_b = function(lambda, p, k, m, n) {
    interval = k:m;
    poisson = dpois(interval, lambda);
    geometrica = dgeom(interval - 1, p);
    binomiala = dbinom(interval, n, p);
    barplot(poisson, main = "Poisson", names.arg = interval);
    barplot(geometrica, main = "Geometrica", names.arg = (interval - 1));
    barplot(binomiala, main = "Binomiala", names.arg = interval);
}
par(mfrow = c(3, 2));
exA_1_b(10, 0.5, 7, 18, 25);

exA_1_c = function(lambda) {
    i = 0;
    k = dpois(i, lambda);
    while (k <= (1 - 10^-6)) {
        i = i + 1;
        k = k + dpois(i, lambda);
    }
    return(k);
}
exA_1_c(10);

exA_2_a = function() {
    file = read.csv("note_PS.csv", header = T, sep = ',');
    prob = file[['P']];
    stat = file[['S']];
    fr_abs_prob = as.vector(table(prob));
    fr_abs_stat = as.vector(table(stat));
    print(fr_abs_prob);
    print(fr_abs_stat);
    fr_rel_prob = fr_abs_prob / length(prob);
    fr_rel_stat = fr_abs_stat / length(stat);
    print(fr_rel_prob);
    print(fr_rel_stat);
    
    mean_prob = mean(prob);
    mean_stat = mean(stat);
    print(mean_prob);
    print(mean_stat);
}
exA_2_a();

exA_2_b = function(filename, esantion) {
    file = read.csv(filename, header = T, sep = ',');
    if (esantion == 'P') {
        x = file[['P']] 
    }
    else if (esantion == 'S') {
        x = file[['S']] 
    }
    m = mean(x);
    s = sd(x);
    v = vector();
    h = 1;
    for (i in 1:length(x)) {
        if (x[i] < m + 2 * s && x[i] > m - 2 * s) {
            v[h] = x[i];
            h = h + 1;
        }
    }
    print(as.vector(table(v)));
    hist(v, right = TRUE, breaks = 0:10);
    return(v);
}
exA_2_b("note_PS.csv", 'P');
exA_2_b("note_PS.csv", 'S');