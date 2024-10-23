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
        psum := sum entries M_0;
        L := saturate(I, psum); 

        -- Cache the computed "LC" value
        X.cache#"LC" = L;
    );

    -- Return the cached "LC" value
    X.cache#"LC"
);

computeMLDTM = method();
computeMLDTM(ToricModel) := (X) -> (
    if member("MLD", keys X.cache) then return X.cache#"MLD"; -- check if its already been computed
    if not member("LC", keys X.cache) then (
        computeLC(X);
    );
    curLC := X.cache#"LC";
    
    -- Return the cached "LC" value
    curVars := vars ring curLC;
    numVars := numColumns(curVars);
    numVars2 := numVars//2;
    use ring curLC;
    randu := for i from 0 to (numVars2-1) list random(numVars2*20);
    useVars := for i from 0 to (numVars2-1) list curVars_i_0;
    curR := QQ[useVars];
    useVars3 := for i from 0 to (numVars2-1) list (vars curR)_i_0;
    specmap := map(curR, ring curLC, join(useVars3, randu));
    X.cache#"MLD" = degree (specmap curLC);

    X.cache#"MLD"
);

--symbolic ml degree solver is done.

--write Mle estimate solver

--solve ml degree

--make some conjectures regarding indepence model degree


