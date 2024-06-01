tutorial_stem = function() {
    x = c(11, 14, 21, 32, 17, 24, 21, 35, 52, 44, 21, 28, 36, 49, 41, 19, 20, 34, 37, 29)
    stem(x)
}

ex_II_1 = function(filename) {
    x = scan(filename);
    print(mean(x));
    print(median(x));
}

ex_II_2 = function(filename) {
    life = read.csv(filename, header = T, sep = ',');
    males = life[['male']];
    females = life[['female']];
    print(mean(males));
    print(median(males));
    print(mean(females));
    print(median(females));
}

outliers_mean = function(x) {
    m = mean(x);
    s = sd(x);
    v = vector();
    h = 1;
    for (i in 1:length(x)) {
        if (x[i] < m + 2 * s && x[i] > m - 2 * s) {
            v[h] = x[i];
            h = h + 1;
        }
        else {
            print(x[i]);
        }
    }
    return(v);
}

outliers_median = function(x) {
    m = median(x);
    q = as.vector(quantile(x));
    q1 = q[2];
    q3 = q[4];
    iqr = q3 - q1;
    v = vector();
    h = 1;
    for (i in 1:length(x)) {
        if (x[i] < q3 + 1.5 * iqr && x[i] > q1 - 1.5 * iqr) {
            v[h] = x[i];
            h = h + 1;
        }
        else {
            print(x[i]);
        }
    }
    print(v);
}

outliers_mean_txt = function(filename) {
    x = scan(filename);
    m = mean(x);
    s = sd(x);
    v = vector();
    h = 1;
    for (i in 1:length(x)) {
        if (x[i] < m + 2 * s && x[i] > m - 2 * s) {
            v[h] = x[i];
            h = h + 1;
        }
        else {
            print(x[i]);
        }
    }
    print(v);
}

outliers_median_txt = function(filename) {
    x = scan(filename);
    m = median(x);
    q = as.vector(quantile(x));
    q1 = q[2];
    q3 = q[4];
    iqr = q3 - q1;
    v = vector();
    h = 1;
    for (i in 1:length(x)) {
        if (x[i] < q3 + 1.5 * iqr && x[i] > q1 - 1.5 * iqr) {
            v[h] = x[i];
            h = h + 1;
        }
        else {
            print(x[i]);
        }
    }
    print(v);
}