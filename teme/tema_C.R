ex_C1_a = function(n) {
    v = vector()
    for (i in 1:n) {
        v[i] = runif(1); 
    }
    vs = sort(v);
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

generare_sir = function(n, k) {
    m = matrix(nrow = n, ncol = k);
    for (i in 1:n) {
        for (j in 1:k) {
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

concat_elem_to_row = function(mat, row, e) {
    dimensiuni = dim(mat);
    n = dimensiuni[1];
    k = dimensiuni[2];
    m = matrix(NA, nrow = n, ncol = k + 1);
    m[ ,1:k] = mat;
    m[row, k+1] = e;
    return(m);
}

push_row_to_matrix = function(mat, e) {
    dimensiuni = dim(mat);
    n = dimensiuni[1];
    k = dimensiuni[2];
    m = matrix(NA, nrow = n + 1, ncol = k);
    m[1:n, ] = mat;
    m[n + 1, ] = e;
    return(m);
}

# ex_C1_b
comparare = function(a, b) {
    L = min(length(a), length(b));
    for (i in 1:L) {
        if (a[i] > b[i]) {
            return(1);
        }
        else if (a[i] < b[i]) {
            return(-1);
        }
    }
    
    while (a[length(a)] == b[length(b)]) {
        a = append(a, sample(c(0, 1)));
        b = append(b, sample(c(0, 1)));
    }
    
    if (a[length(a)] > b[length(b)]) {
        return(1);
    }
    else {
        return(-1);
    }
}

ex_C1_c = function(mat) {
    dimensiuni = dim(mat);
    n = dimensiuni[1];
    k = dimensiuni[2];
    if (is.na(mat[1, 1])) {
        return(matrix(NA, nrow = 0, ncol = 0));
    }
    if (n == 1) {
        return(mat);
    }
    
    pivot_idx = sample(1:n, 1);
    pivot_word = mat[pivot_idx, ];
    mai_mici = matrix(NA, nrow = 1, ncol = k);
    mai_mici_empty = TRUE;
    mai_mari = matrix(NA, nrow = 1, ncol = k);
    mai_mari_empty = TRUE;
    
    for (i in 1:n) {
        if (i == pivot_idx) {
            next;
        }
        
        word = mat[i, ];
        if (comparare(word, pivot_word) == 1) {
            if (mai_mari_empty == TRUE) {
                mai_mari = matrix(word, nrow = 1, ncol = k, byrow = TRUE);
                mai_mari_empty = FALSE;
            }
            else {
                mai_mari = push_row_to_matrix(mai_mari, word); 
            }
        }
        else {
            if (mai_mici_empty == TRUE) {
                mai_mici = matrix(word, nrow = 1, ncol = k, byrow = TRUE);
                mai_mici_empty = FALSE;
            }
            else {
                mai_mici = push_row_to_matrix(mai_mici, word); 
            }
        }
    } 
    
    mai_mici_sortate = ex_C1_c(mai_mici);
    mai_mari_sortate = ex_C1_c(mai_mari);
    # transpunerea e pentru as.vector sa ia elementele pe linie in loc de coloana
    cele_trei_parti = c(as.vector(t(mai_mici_sortate)), pivot_word, as.vector(t(mai_mari_sortate)));
    return(matrix(cele_trei_parti, nrow = n, ncol = k, byrow = TRUE));
}

ex_C1_d = function(n, k) {
    rand_mat = generare_sir(n, k); 
    rand_mat_sorted = ex_C1_c(rand_mat);
    permutare = vector();
    for (i in 1:n) {
        for (j in 1:n) {
            if (identical(rand_mat[i, ], rand_mat_sorted[j, ])) {
                rand_mat_sorted[j, 1] = 2;
                permutare[i] = j; 
                break;
            }
        }
    }
    return(permutare);
}

genereaza_graf = function(n) {
    m = matrix(nrow = n, ncol = n);
    for (i in 1:(n - 1)) {
        m[i,i] = 0;
        for (j in (i + 1):n) {
            x = runif(1);
            if (x >= 0.5) {
                m[i,j] = 1;
                m[j,i] = 1;
            }
            else {
                m[i,j] = 0;
                m[j,i] = 0;
            }
        }
    }
    m[n, n] = 0;
    return(m);
}

ex_C2_a = function(graf) {
    dimensiuni = dim(graf);
    n = dimensiuni[1];
    A = sample(1:n, n / 2); # B va fi G \ A
    B = vector();
    for (i in 1:n) {
        gasit = FALSE;
        for (j in 1:(n / 2)) {
            if (i == A[j]) {
                gasit = TRUE;
                break;
            } 
        } 
        if (gasit == FALSE) {
            B = c(B, i); 
        }
    }
    
    muchii_taiate = 0;
    for (i in 1:(n / 2)) {
        for (j in 1:(n / 2)) {
            if (graf[A[i], B[j]] == 1) {
                muchii_taiate = muchii_taiate + 1;
            }
        }
    }
    return(muchii_taiate); 
}

ex_C2_a(genereaza_graf(10));

ex_C2_b = function(n, k) {
    graf = genereaza_graf(n);
    maxx = 0;
    for (i in 1:k) {
        x = ex_C2_a(graf); 
        if (x > maxx) {
            maxx = x;
        }
    }
    return(maxx);
}

# C.1.a
print("Se genereaza o permutare folosind sortare de vectori generati aleator")
ex_C1_a(10);

print("Se genereaza o lista de 10 cuvinte a cate 4 caractere fiecare")
mtx = generare_sir(10, 4); 
print(mtx);

# C.1.b
print("Exemple folosind functia de comparare stricta")
print(comparare(c(1, 0, 0), c(1, 0, 0))) # sunt egale, deci ar trebui sa adauge caractere la final
print(comparare(c(1, 1, 0), c(1, 0, 0))) # primul e mai mare decat al doilea, deci ar trebui sa returneze 1
print(comparare(c(1, 1, 0), c(1, 1, 1))) # primul e mai mare decat al doilea, deci ar trebui sa returneze -1

# C.1.c
print("Lista de cuvinte sortata (quick sort)")
print(ex_C1_c(mtx));

# C.1.d
print("Permutare folosind metoda cu lista de cuvinte")
print(ex_C1_d(10, 4)); 

# C.2.a
print("Se 'ghiceste' taietura maxima intr-o singura rulare")
print(ex_C2_a(genereaza_graf(10)));

# C.2.b
print("Pentru a maximiza sansele de a gasi rezultatul corect, se ruleaza de mai multe ori algoritmul (exemplu: 10000 ori)")
print(ex_C2_b(10, 10000));