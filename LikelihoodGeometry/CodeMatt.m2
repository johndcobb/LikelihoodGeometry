

computeLCTM = method();
computeLCTM(ToricModel) := (X) -> (
    if not member("LC", keys X.cache) then (
        p := local p;
        u := local u;
        numcol := local numcol;
        M := local M;
        -- Retrieve matrix A from the cache
        A := X.cache#"mat";
        if A === null then error "Matrix A is not defined in the cache of x";

        numcol := numColumns(A);
        R := QQ[p_1..p_numcol, u_1..u_numcol, Degrees => {numcol:{1,0}, numcol:{0,1}}];
        toric := toricIdeal(A, R);
        M := reshape(R^numcol, R^2, matrix({gens R}));
        I := toric + minors(2, A * M);
        L := saturate(I, sum entries M_0); 

        -- Cache the computed "LC" value
        X.cache#"LC" = L;
    );

    -- Return the cached "LC" value
    X.cache#"LC"
);