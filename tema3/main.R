ex_C1_a = function(n) {
    v = vector()
    for (i in 1:n) {
        v[i] = runif(1); 
    }
    print(v);
    vs = sort(v);
    print(vs);
    ind = vector();
    for (i in 1:n) {
        for (j in 1:n) {
            if (v[i] == vs[j]) {
                ind[i] = j;
            }
        }
    }
    print(ind);
}
ex_C1_a(10);

generare_sir = function(n, k) {
    m = matrix(nrow = n, ncol = k);
    for (i in 1:n) {
        for (j in 1:n) {
            x = runif(1);
            if (x >= 0.5) {
                m[i,j] = 1;
            }
            else {
                m[i,j] = 0;
            }
        }
    }
    return(m);
}
print(generare_sir(10, 4));