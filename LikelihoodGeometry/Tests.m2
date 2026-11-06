-- test 0
TEST ///  -* making a custom pmf *-
f = new HashTable from {
    1 => 0.2,
    2 => 0.3,
    3 => 0.5
};
g = x -> if f#?x then f#x else 0;
X = discreteRandomVariable 3;
assert isWellDefined X;
assert(mean X == 2 and variance X == 2/3);
X = discreteRandomVariable(3,g);
assert isWellDefined X;
assert(states X == {1,2,3});
assert(abs(mean(X)-23/10) < 10^(-9));
assert(abs(variance(X)-61/100) < 10^(-9));
assert(member(sample X, {1,2,3}));
Z = discreteRandomVariable(3, i -> if i == 2 then 1 else 0);
assert(sample(Z,5) == {2,2,2,2,2});
///

-- test 1
TEST /// -- This is Example 1.2 from Package introduction paper
g = new HashTable from {
    1 => 0.5,
    2 => 0.3,
    3 => 0.2
};
f = x -> if g#?x then g#x else 0;
Y = discreteRandomVariable(3, f)
assert isWellDefined Y;
assert(states Y == {1,2,3});
assert(abs(mean(Y)-17/10) < 10^(-9));
assert(abs(variance(Y)-61/100) < 10^(-9));
assert(all(sample(Y,2), x -> member(x,{1,2,3})));
Z = discreteRandomVariable 2;
assert(states {Y,Z} == {{1,1},{1,2},{2,1},{2,2},{3,1},{3,2}});
assert(all(mean {Y,Z} - {17/10,3/2}, x -> abs x < 10^(-9)));
assert(all(variance {Y,Z} - {61/100,1/4}, x -> abs x < 10^(-9)));
assert(member(sample {Y,Z}, states {Y,Z}));
assert(all(sample({Y,Z},3), x -> member(x, states {Y,Z})));
///

-- test 2
TEST /// -- This is Example 1.1 from Package introduction paper
R = QQ[p_0,p_1,p_2];
M = ideal(4*p_0*p_2-p_1^2);
S = LCRing(M);
assert(numgens S == 6);
assert(leadTerm(S_0*S_2+S_1^2) == S_0*S_2);
L = computeLC(M,S);
assert(ring L === S);
(p0,p1,p2,u0,u1,u2) = toSequence gens S;
expected = ideal(p1^2-4*p0*p2, 4*p2*u0-p1*u1+2*p2*u1-2*p1*u2, 2*p1*u0-2*p0*u1+p1*u1-4*p0*u2);
assert(L == expected);
setRandomSeed 20261105;
assert(MLdegree(M) == 1);
///

-- test 3
TEST ///
R = QQ[p_0,p_1,p_2];
M = ideal(p_0*p_2-p_1^2);
A = toricPolytope(M);
assert(numColumns A == 3 and rank A == 2);
assert(toricIdeal(A,R) == M);
P = toricModel(A);
assert(toricIdeal(P,R) == M);
assert(toricIdeal(toricModel M,R) == M);
setRandomSeed 20261105;
assert(MLdegree(P) == 2);
///

-- test 4
TEST /// -- This is Example 1.3 from Package introduction paper
A = matrix{{1,1,1,1},{1,2,3,4}};
X = toricModel(A)
R = QQ[p_0,p_1,p_2,p_3];
M = minors(2, matrix{{p_0,p_1,p_2},{p_1,p_2,p_3}});
assert(toricIdeal(toricPolytope(M),R) == M);
assert(M == toricIdeal(X,R));
-- The twisted cubic has codimension two and three minimal generators.
assert(codim M == 2 and numgens M == 3);
S = LCRing(M);
L = computeLC(M,S);
assert(L == computeLC(X,S));
setRandomSeed 20261105;
assert(MLdegree(M) == 3);
///

-- test 5
TEST /// -- This is Example 1.4 from Package introduction paper
a = discreteRandomVariable 2;
b = discreteRandomVariable 2;
c = discreteRandomVariable 2;
S = {{a,b}, {b,c}};
L = {a,b,c};
expected = matrix{{1,1,0,0,0,0,0,0},{0,0,1,1,0,0,0,0},{0,0,0,0,1,1,0,0},{0,0,0,0,0,0,1,1},{1,0,0,0,1,0,0,0},{0,1,0,0,0,1,0,0},{0,0,1,0,0,0,1,0},{0,0,0,1,0,0,0,1}};
assert(makeLogLinearMatrix(S,L) == expected);
///

-- test 6
TEST ///
a = discreteRandomVariable 2;
b = discreteRandomVariable 2;
c = discreteRandomVariable 2;
S = {{a,b}, {b,c}};
L = {a,b,c};
G = graph{{a,b},{b,c}};
assert(makeLogLinearMatrix(G) == makeLogLinearMatrix(S,L));
R = QQ[p_0..p_7];
expected = ideal(p_0*p_5-p_1*p_4,p_2*p_7-p_3*p_6);
assert(toricIdeal(toricModel G,R) == expected);
I = toricIdeal G;
phi = map(R,ring I,gens R);
assert(phi(I) == expected);
setRandomSeed 20261105;
assert(MLdegree(toricModel G) == 1);
///

-- test 7
TEST /// 
a = discreteRandomVariable(2); b = discreteRandomVariable(2); c = discreteRandomVariable(3); d = discreteRandomVariable(3); e = discreteRandomVariable(2); f = discreteRandomVariable(2);
G = graph({a,b,c,d,e,f}, matrix{{0,0,0,0,0,0},{0,0,1,0,0,0},{0,1,0,0,0,0},{0,0,0,0,1,1},{0,0,0,1,0,1},{0,0,0,1,1,0}});
X = toricModel(G);
R = LCRing(X);
L = computeLC(X,R);
assert(numgens R == 288);
assert(ring L === R);
setRandomSeed 20261105;
assert(MLdegree(X) == 1);
///

-- test 8
TEST ///
S = rationalNormalScroll({1,2,3})
R = LCRing(S)
assert(numgens R == 12);
assert(leadTerm(R_0*R_2+R_1^2) == R_0*R_2);
L = computeLC(S,R)
assert(ring L === R);
setRandomSeed 20261105;
assert(MLdegree(S) == 3)
///


--test 9
TEST ///
A= matrix{{1,1,1},{1,2,3}};
X = toricModel(A)
L = computeLC(X)
I = computeLC(toricIdeal(X))
phi = map(ring L, ring I, gens ring L)
assert(L == phi(I));
setRandomSeed 20261105;
assert(MLdegree(X) == 2);
///


-- test 10
TEST ///
X = rationalNormalScroll({5})
T = QQ[q_0..q_4];
assert(toricIdeal(X,T) == minors(2,matrix{{q_0,q_1,q_2,q_3},{q_1,q_2,q_3,q_4}}));
A = matrix{{1,1,1,1},{1,2,3,4}};
X = toricModel(A)
R = QQ[p_0,p_1,p_2,p_3];
M = minors(2, matrix{{p_0,p_1,p_2},{p_1,p_2,p_3}});
assert(toricIdeal(toricPolytope(M),R) == M);
assert(M == toricIdeal(X,R));
Y = rationalNormalScroll({2,3});
assert(toricIdeal(Y,T) == minors(2,matrix{{q_0,q_2,q_3},{q_1,q_3,q_4}}));
///

-- test 11
TEST /// -- Joint independence of a binary and a ternary variable
a = discreteRandomVariable 2;
b = discreteRandomVariable 3;
X = toricModel graph({a,b},{});
R = LCRing X;
(p0,p1,p2,p3,p4,p5,u0,u1,u2,u3,u4,u5) = toSequence gens R;
P = matrix{{p0,p1,p2},{p3,p4,p5}};
M1 = P | matrix{{u0+u1+u2},{u3+u4+u5}};
M2 = transpose(P) | matrix{{u0+u3},{u1+u4},{u2+u5}};
expected = minors(2,M1) + minors(2,M2);
L = computeLC(X,R);
assert(ring L === R);
assert(L == expected);
-- Check the stated Groebner basis, not just the ideal it generates.
assert(ideal leadTerm gens L == ideal leadTerm gens gb L);
setRandomSeed 20261105;
assert(MLdegree(X) == 1);
-- A new ambient ring must be honored even after caching a correspondence.
S = QQ[q_0..q_5,v_0..v_5];
J = computeLC(X,S);
assert(ring J === S);
phi = map(S,R,gens S);
assert(J == phi(expected));
///

-- test 12
TEST /// -- The general toric path must also honor a new ambient ring
X = toricModel matrix{{1,1,1},{0,1,2}};
R = LCRing X;
L = computeLC(X,R);
S = QQ[q_0..q_2,v_0..v_2];
J = computeLC(X,S);
assert(ring J === S);
phi = map(S,R,gens S);
assert(J == phi(L));
setRandomSeed 20261105;
assert(MLdegree(X) == 2);
///

-- test 13
TEST /// -- Maximal cliques can have different sizes and share vertices
(a,b,c,d) = (discreteRandomVariable 2, discreteRandomVariable 2, discreteRandomVariable 2, discreteRandomVariable 2);
G = graph({a,b,c,d},{{a,b},{a,c},{b,c},{c,d}});
expected = matrix{
    {1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1},
    {1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0},
    {0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0},
    {0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0},
    {0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1}};
assert(makeLogLinearMatrix G == expected);
///
