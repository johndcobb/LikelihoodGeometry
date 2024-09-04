LCRing = method(Options => {
        Start => 1,
        Symbols => (getSymbol "x", getSymbol "y"),
        CoefficientRing => QQ
        })

LCRing(ZZ, List) := Ring => opts -> (n, params) -> (
    -*
    -- n: This is the dimension of the projective space P^n x P^n, indexed 1..n by default
    -- params: a list of parameter symbols (or ring variables that can be used as variables in a new ring)
    -- output: A ring R that is the coordinate ring of P^n x P^n
    -- R_0..R_(n-1): are the coordinate functions for the first factor
    -- R_n..R_(2n-1): are the coordinate functions for the second factor
    -- R_(2n).. are the parameters
    *-
    kk := opts.CoefficientRing;
    s := opts#Symbols#1;
    c := opts#Symbols#0;
    start := opts#Start;
    R := if #params > 0
      then kk[c_start..c_(start+n-1), s_start..s_(start+n-1),params,MonomialOrder=>{2*n,#params}]
      else kk[c_start..c_(start+n-1), s_start..s_(start+n-1)];
  --  R.numOscillators = n;
    R
    )
LCRing ZZ := Ring => opts -> n -> LCRing(n, {}, opts)

computeIndependenceLC = method()
computeIndependenceLC(ZZ, ZZ) := Ideal => (m, n) -> (
    -*
    Here we can describe inputs and outputs
    *-
    p := local p;
    u := local u;
    x := local x;
    R := ZZ/32003[p_(1,1)..p_(m,n), u_(1,1)..u_(m,n), x];
    P := transpose(genericMatrix(R,p_(1,1),n,m));
    Lcol := matrix {for i from 1 to m list sum(for j from 1 to n list u_(i,j))};
    Lrow := for i from 1 to n list sum(for j from 1 to m list u_(j,i));
    Lrow = matrix {append(Lrow,x)};
    P = P | transpose(Lcol);
    P = P || Lrow;
    I := eliminate({x}, minors(2, P));
    S := ZZ/32003[p_(1,1)..p_(m,n), u_(1,1)..u_(m,n)];
    f := map(S,R,gens S | {0});
    f(I))

computeToricLC = method()
computeToricLC(Matrix) := Ideal => A -> (
    p := local p;
    u := local u;
    numcol := local numcol;
    M := local M;
    numcol = numColumns(A);
    R := ZZ/32003[p_1..p_numcol, u_1..u_numcol, Degrees => {numcol:{1,0}, numcol:{0,1}}];
    toric := toricIdeal(A,R);
    M = reshape(R^numcol,R^2,matrix({gens R}));
    I := toric+minors(2,A*M);
    L := saturate(I,sum entries M_0);
    L)

computeToricLCwithToric = method()
computeToricLCwithToric(Matrix) := Ideal => A -> (
    p := local p;
    u := local u;
    numcol := local numcol;
    M := local M;
    numcol = numColumns(A);
    R := ZZ/32003[p_1..p_numcol, u_1..u_numcol, Degrees => {numcol:{1,0}, numcol:{0,1}}];
    toric := toricIdeal(A,R);
    M = reshape(R^numcol,R^2,matrix({gens R}));
    I := toric+minors(2,A*M);
    L := saturate(I,sum entries M_0);
    L,toric)

--take a sequence of matrices, first the full A matrix, then the A_i submatrices
--This is to test daves suggested method that the u*p actually need to be split
computeToricLCwithToricSeq = method()
computeToricLCwithToricSeq(List) := Ideal => A -> (
    p := local p;
    u := local u;
    numcol := local numcol;
    M := local M;
    numcol = numColumns(A_0);
    R := ZZ/32003[p_1..p_numcol, u_1..u_numcol, Degrees => {numcol:{1,0}, numcol:{0,1}}];
    toric := toricIdeal(A_0,R);
    M = reshape(R^numcol,R^2,matrix({gens R}));
    I := toric+ (sum for i from 1 to #A-1 list minors(2,A_i*M));
    L := saturate(I,sum entries M_0);
    L2 := saturate((toric + minors(2,A_0*M)),sum entries M_0);
    L,toric,L2)

computeLC = method()
computeLC(Ideal) := Ideal => I -> (
    U := local U;
    d := local d;
    u := local u;
    pmat := local pmat;
    umat := local umat;
    R := ring(I);
    n := numgens R;
    S := coefficientRing(R)[gens R, u_1..u_n];
    varList := for i from 0 to #(gens R) -1 list S_i;
    varList2 := for i from 0 to #(gens R) -1 list 1_S;
    f := map(S,R,varList);
    J := (matrix {varList2}) || transpose(f jacobian(I));
    Q := minors(codim(I)+1,jacobian(I));

    U = reshape(S^n,S^2,matrix{gens S});
    pmat = transpose(matrix{U_0});
    umat = transpose(matrix{U_1});
    Jaug := umat || J*diagonalMatrix(varList);

    H := ideal((sum flatten entries pmat)*(product flatten entries pmat));

    L := saturate(f(I) + minors(codim(I)+2,Jaug),H+(f(Q)));
    L)

computenwayIndependenceLC = method()
computenwayIndependenceLC(List) := Ideal => inplist -> (
    -*
    Input: A matrix A describing the polytope of a toric ideal
    *-
    p := local p;
    u := local u;
    curM := local curM;
    curu := local curu;
    currow := local currow;
    inplist = new Sequence from inplist;
    ilist := new List from inplist;
    nums := for i from 0 to #inplist-1 list fold((n,m)->n*m, ilist)/ilist_i;
    fixListsEnding := for i from 1 to #inplist list (
        if i == 1 then (
        for j from 1 to inplist_(i-1) list (1:j)| new Sequence from inplist_{1 ..#inplist-1}
        );
        if i == #inplist then (
        for j from 1 to inplist_(i-1) list (new Sequence from inplist_{0 ..#inplist-2}) | (1:j)
        );
        for j from 1 to inplist_(i-1) list (new Sequence from inplist_{0 ..i-2}) |(1:j)| (new Sequence from inplist_{i..#inplist-1})
    );

    fixListsStart := for i from 1 to #inplist list (
        if i == 1 then (
        for j from 1 to inplist_(i-1) list (1:j)| (#inplist-1 : 1)
        );
        if i == #inplist then (
        for j from 1 to inplist_(i-1) list (#inplist-1:1) | (1:j)
        );
        for j from 1 to inplist_(i-1) list (i-1:1) |(1:j)| (#inplist-i:1)
    );
    deglist1 := fold((n,m)->n*m,ilist) : {1,0};
    deglist2 := fold((n,m)->n*m,ilist) : {0,1};
    deglist := new List from join(deglist1,deglist2);
    start := #inplist:1;
    R := ZZ/32003[p_start..p_inplist,u_start..u_inplist, Degrees => deglist];
    Ms := {};
    for i from 0 to #inplist-1 do (
        curM = {};
        for j from 0 to #fixListsStart_i-1 do(
                currow = new List from p_(fixListsStart_i_j)..p_(fixListsEnding_i_j);
            curu = sum new List from u_(fixListsStart_i_j)..u_(fixListsEnding_i_j);
            currow = append(currow,curu);
            curM = append(curM,currow);
        );
        Ms = append(Ms,matrix curM)
        );  
    Ilist := for i from 0 to #Ms-1 list minors(2,Ms_i);
    I := sum Ilist;
I)

--------- The below is for testing conjectures. I pull out the matrices that create the ideal in the method above.

nwayindependenceMatrices = method()
nwayindependenceMatrices(List) := List => inplist -> (
    p := local p;
    u := local u;
    curM := local curM;
    curu := local curu;
    currow := local currow;
    inplist = new Sequence from inplist;
    ilist := new List from inplist;
    nums := for i from 0 to #inplist-1 list fold((n,m)->n*m, ilist)/ilist_i;
    fixListsEnding := for i from 1 to #inplist list (
        if i == 1 then (
        for j from 1 to inplist_(i-1) list (1:j)| new Sequence from inplist_{1 ..#inplist-1}
        );
        if i == #inplist then (
        for j from 1 to inplist_(i-1) list (new Sequence from inplist_{0 ..#inplist-2}) | (1:j)
        );
        for j from 1 to inplist_(i-1) list (new Sequence from inplist_{0 ..i-2}) |(1:j)| (new Sequence from inplist_{i..#inplist-1})
    );

    fixListsStart := for i from 1 to #inplist list (
        if i == 1 then (
        for j from 1 to inplist_(i-1) list (1:j)| (#inplist-1 : 1)
        );
        if i == #inplist then (
        for j from 1 to inplist_(i-1) list (#inplist-1:1) | (1:j)
        );
        for j from 1 to inplist_(i-1) list (i-1:1) |(1:j)| (#inplist-i:1)
    );
    deglist1 := fold((n,m)->n*m,ilist) : {1,0};
    deglist2 := fold((n,m)->n*m,ilist) : {0,1};
    deglist := new List from join(deglist1,deglist2);
    start := #inplist:1;
    R := ZZ/32003[p_start..p_inplist,u_start..u_inplist, Degrees => deglist];
    Ms := {};
    for i from 0 to #inplist-1 do (
        curM = {};
        for j from 0 to #fixListsStart_i-1 do(
                currow = new List from p_(fixListsStart_i_j)..p_(fixListsEnding_i_j);
            curu = sum new List from u_(fixListsStart_i_j)..u_(fixListsEnding_i_j);
            currow = append(currow,curu);
            curM = append(curM,currow);
        );
        Ms = append(Ms,matrix curM)
        );  
Ms)

--------- The below is for testing conjectures. I pull out the matrices that create the ideal of the model 

computenwayindependenceModel = method()
computenwayindependenceModel(List) := Ideal => inplist -> (
    p := local p;
    u := local u;
    curM := local curM;
    curu := local curu;
    currow := local currow;
    inplist = new Sequence from inplist;
    ilist := new List from inplist;
    nums := for i from 0 to #inplist-1 list fold((n,m)->n*m, ilist)/ilist_i;
    fixListsEnding := for i from 1 to #inplist list (
        if i == 1 then (
        for j from 1 to inplist_(i-1) list (1:j)| new Sequence from inplist_{1 ..#inplist-1}
        );
        if i == #inplist then (
        for j from 1 to inplist_(i-1) list (new Sequence from inplist_{0 ..#inplist-2}) | (1:j)
        );
        for j from 1 to inplist_(i-1) list (new Sequence from inplist_{0 ..i-2}) |(1:j)| (new Sequence from inplist_{i..#inplist-1})
    );

    fixListsStart := for i from 1 to #inplist list (
        if i == 1 then (
        for j from 1 to inplist_(i-1) list (1:j)| (#inplist-1 : 1)
        );
        if i == #inplist then (
        for j from 1 to inplist_(i-1) list (#inplist-1:1) | (1:j)
        );
        for j from 1 to inplist_(i-1) list (i-1:1) |(1:j)| (#inplist-i:1)
    );
    deglist1 := fold((n,m)->n*m,ilist) : {1,0};
    deglist2 := fold((n,m)->n*m,ilist) : {0,1};
    deglist := new List from join(deglist1,deglist2);
    start := #inplist:1;
    R := ZZ/32003[p_start..p_inplist,u_start..u_inplist, Degrees => deglist];
    Ms := {};
    for i from 0 to #inplist-1 do (
        curM = {};
        for j from 0 to #fixListsStart_i-1 do(
                currow = new List from p_(fixListsStart_i_j)..p_(fixListsEnding_i_j);
            curM = append(curM,currow);
        );
        Ms = append(Ms,matrix curM)
        );
    Ilist := for i from 0 to #Ms-1 list minors(2,Ms_i);
    I := sum Ilist;
I)

computenwayToricMatrix = method()
computenwayToricMatrix(List) := Ideal => inplist -> (
    -*
    Input: A matrix A describing the polytope of a toric ideal
    *-
    p := local p;
    u := local u;
    currow := local currow;
    curM := local curM;
    curu := local curu;
    --currow := local currow;
    inplist = new Sequence from inplist;
    start := #inplist:1;
    theSeq := start .. inplist;
    ilist := new List from inplist;
    nums := fold((n,m)->n*m, ilist);
    firstRow := new List from (nums:1);
    AMatrix := {firstRow};
    for i from 0 to #inplist-1 do (
        for j from 1 to inplist_i-1 do (
            currow = {};
            for k in theSeq list (
                if k_i == inplist_i then (
                    currow = append(currow, 1);
                    continue;
                );
                currow = append(currow, 0);
            );
        );
        AMatrix = append(AMatrix,currow);
    );
matrix AMatrix)

-- We could separate out the helper functions here.