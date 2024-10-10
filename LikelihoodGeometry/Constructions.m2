
rationalNormalScroll = method()
rationalNormalScroll List := ToricModel => L -> (
    if all(L, x -> class x === ZZ) != true then error "--expected a list of integers";
    matrixList := {};
    i := 0;
    c := #L;
    for n in L do (
        Alist := {toList(n:1)};
        for k from 1 to i do (
            Alist = Alist | {toList(n:1)};
        );
        for k from 1 to c-i-1 do (
            Alist = Alist | {toList(n:0)};
        );
        Alist = Alist | {toList(1..n)};

        matrixList = matrixList | {Alist};
        i = i + 1;
    );
    A := matrix matrixList_0;
    for i from 1 to #matrixList-1 do (
        A = A | matrix matrixList_i;
    );
    toricModel A
)