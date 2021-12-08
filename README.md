```
fun factorialAux (n,acc) = if n = 0 then acc else factorialAux(n-1, acc*n)
fun factorial(n) = factorialAux (n,1)

fun lengthAux(acc, L) = if (L = []) then acc
    else lengthAux(acc+1, tl(L));
fun length(L) = lengthAux(0, L);

fun reverseDigitsAux(acc, n) = if n < 10 then acc*10 + n
    else reverseDigitsAux(acc*10 + n mod 10, n div 10);
fun reverseDigits n = if n < 0 then ~(reverseDigitsAux(0,~n))
    else reverseDigitsAux(0, n);

fun min(X:int, Y:int) = if X < Y then X else Y;

fun getN(L,M) = if M < 1 then []
    else if L = [] then []
    else hd(L)::getN(tl(L),M-1);

fun removeN(L,M) = if M < 1 then L (* Removing 0 elements *)
    else if L = [] then [] (* Can't remove elements from an empty list *)
    else removeN(tl(L), M-1);

fun interleaveLists(L1,L2,Stride,MaxElements) = 
    if MaxElements < 1 then []
    else if L1 = [] andalso L2 = [] then []
    else getN(L1,min(Stride,MaxElements)) 
    @ interleaveLists(L2, removeN(L1,Stride), Stride, MaxElements - min(length(L1), Stride));
      (* min(length(L1),Stride) is how many elements were taken via getN *)

fun sumPairs(L) = case L of
    [] => []
    |[h] => [h]
    |h::b::t => (h+b)::sumPairs(t);

fun sumPairs2(L) =
    if L = [] then []
    else if length(L) = 1 then hd(L)::sumPairs2(tl(L))
    else hd(L)+hd(tl(L))::sumPairs2(tl(tl(L)));


fun groupDupesAux(L,Acc,ele) =
    if L = [] then [Acc] (* when reach end of list, return entire set *)
    else if hd(L) = ele then groupDupesAux(tl(L), hd(L)::Acc, ele) (* add to accumulator *)
    else [Acc] @ groupDupesAux(tl(L), [hd(L)], hd(L)); (* append accumulator and then reset it*)

fun groupDupes(L) = 
    if L = [] then []
    else groupDupesAux(tl(L),[hd(L)], hd(L)); 


fun last(L) = if L = [] then 0
    else if tl(L) = [] then hd(L)
    else last(tl(L));

fun kth(L,K) = if L = [] then 0
    else if K = 0 then hd(L)
    else kth(tl(L), K-1);

fun reverseAux(L, Acc) = if L = [] then Acc
    else reverseAux(tl(L),hd(L)::Acc);

fun reverse(L) = reverseAux(L, []);

fun palindrome(L) = L = reverse(L);

fun flat(L) = if L = [] then []
    else hd(L) @ flat(tl(L));

fun gcd(N,M) = if N=M then N
  else if N>M then gcd(M,N-M)
  else gcd(N,M-N);
fun fraction (n,d) =
  let val k = gcd (n,d)
    in
    ( n div k , d div k )
    end;
(*Instead of calling gcd twice, we can call it just once*)

(*powerset is powerset of tail concatenated with inserting the head into each element of the powerset of tail*)
fun insertAll(ele,L) = if L = [] then []
    else (ele::hd(L))::(insertAll(ele,tl(L)));
(*L is a list of lists, so adding ele to each list*)
fun powerset(L) = if L = [] then [[]]
    else powerset(tl(L)) @ insertAll(hd(L),powerset(tl(L)));


fun compress(L) = if L = [] then []
    else if tl(L) = [] then L (*singleton list is kept*)
    else if hd(L) = hd(tl(L)) then compress(tl(L)) (*consecutive duplicate, so remove*)
    else hd(L)::compress(tl(L)); (* keep if not duplicate *)

fun remove(x,lst) = case lst of
  [] => []
  |h::t => if (x=h) then remove(x,t) else h::(remove(x,t));

fun removedupl(lst) = case lst of
  [] => []
  |h::t => h::removedupl(remove(h,lst));
(*removedupl [3,2,4,6,4,3,2,3,4,3,2,1] = [3,2,4,6,1] *)

fun packHelper(Current, Prefix, L) = 
    if L = [] then [Prefix] (* when reach end of list, return final accumulator *)
    else if Current = hd(L) then packHelper(Current, hd(L)::Prefix, tl(L)) (* Continue accumulating Prefix*)
    else Prefix::packHelper(hd(L), [hd(L)], tl(L)); (*append prefix and reset accumulator*)

fun pack(L) = if L = [] then []
    else packHelper(hd(L),[hd(L)],tl(L));

[1,1,1,1]::[[6]];
(*same as pack([1,1,1,1,6]) = packHelper(1,[1,1,1,1],[6]) = [1,1,1,1]::packHelper(6,[6],nil) = [1,1,1,1]::[[6]]*)
(*[[6]] would be the base case and cause the creation of the lists of lists*)

fun encodeHelper(L) = if L = [] then []
    else (length(hd(L)),hd(hd(L)))::encodeHelper(tl(L));
fun encode(L) = encodeHelper(pack(L));

fun unpackList(N,E) =  if N=0 then [] 
    else E::unpackList(N-1,E);
(*unpacking a tuple requires going through N recursions to create a list of N E elements*)

fun unpack(L:(int * string)list) = 
    if L=[] then [] 
    else unpackList(#1(hd(L)),#2(hd(L))) @ unpack(tl(L));
(*unpack head tuple (returns list) and append to unpacking tail of tuples *)

fun range(N,M) = if N = M then [N] else N::range(N+1,M);

fun map(f,[]) = []
    | map(f,L) = (f(hd(L)))::(map(f,tl(L)));

fun incr(L) = map(fn x => x+1, L);

fun filter(f, []) = []
  | filter(f, h::t) = if f(h) then h::(filter(f,t)) else filter(f,t);

fun findFirst(f, []) = raise Fail "No such element"
  | findFirst(f, h::t) = if f(h) then h else findFirst(f,t);

fun myFind pred nil = raise Fail "No such element"
| myFind pred (H::T) = if pred H then H else myFind pred T;

fun reduce f B nil = B
| reduce f B (H::T) = f(H, reduce f B T);
(* reduce is foldr, lazy evaluation
must reach the end of the list and then go right to left*)

fun sumList aList = reduce (op +) 0 aList;
sumList [1,2,3]; (* 6 *)
(* +(1, reduce + 0 [2,3]) = +(1 + (2, reduce + 0 [3])) = +(1 + 2 + (3, reduce + 0 []) = +(1 + 2 + 3 + 0) *)

fun foldl(f: ''a*'b->'b, Acc: 'b, L: ''a list):'b =
    if L=[] then Acc
    else foldl(f, f(hd(L),Acc), tl(L));

fun sumList2 L = foldl((op +), 0, L);
sumList2 [1,2,3]; (* 6 *)
(* foldl is not lazy (early evaluation, start summing as you are iterating) 
folding left repeatedly applies the function as you traverse left to right*)


fun collect(B, combine, accept, nil) = accept(B)
| collect(B, combine, accept, H::T) = collect(combine(B,H), combine, accept, T);
(* collect is like foldl except instead of returning the accumulator at the end, 
it applies the accept function to it before returning*)

fun average(aList) = if aList = [] then 0.0 
    else collect((0,0), (fn ((total,count),X) => (total+X,count+1)), 
    (fn (total,count) => real(total)/real(count)), aList);

fun myInterleave(X,[]) = [[X]]
    |myInterleave(X,H::T) = (X::H::T)::(map((fn L => H::L), myInterleave(X,T)));

    (* (X::H::T) is a list, so the return type of myInterleave is a list of lists *)
    (* myInterleave(1,[2,3,4]) =
        X = 1 in all calls
        H = 2 T = [3,4] in XHT
        (1::2::3::4::nil)::map( L => H::L, myInterleave(1,[3,4]) H=2
              XHT

        H = 3 T = [4] in XHT
        (1::2::3::4::nil)::(1::3::4::nil)::map(L=>H::L, myInterleave(1,[4]) H = 3
                                XHT

        H = 4 T = nil in XHT
        (1::2::3::4::nil)::(1::3::4::nil)::(1::4::nil)::map(L=>H::L, myInterleave(1,[])) H = 4
                                               XHT

        H = nil T = nil in XHT
        (1::2::3::4::nil)::(1::3::4::nil)::(1::4::nil)::([[1]])
                                                           XHT
        Lists rewritten without cons for clarity
        Now apply maps to ([1,2,3,4])::([1,3,4])::([1,4])::([[1]])
         First mapping, insert 4 into head of [1], then cons the next list
         H=4 ([1,2,3,4])::([1,3,4])::([ [1,4],[4,1] ])

         Second mapping, insert 3 into heads of [1,4] and [4,1], then cons the next list
         H=3 ([1,2,3,4])::([ [1,3,4], [3,1,4], [3,4,1] ])

         Third mapping, insert 2 into heads of [1,3,4] and [3,1,4] and [3,4,1], then cons the final list
         H=2 ([ [1,2,3,4], [2,1,3,4], [2,3,1,4], [2,3,4,1] ])
        *)

fun appendAll([]) = []
    |appendAll(H::T) = H @ (appendAll(T));

fun permute([]) = [[]]
    |permute(H::T) = appendAll(map((fn L => myInterleave(H,L)), permute(T)));
(*interleave the head in all possible ways in all possible permutations of the tail *)
(* permute([2,3]) = [[2,3],[3,2]]
    permute([1,2,3]) = [[1,2,3], [2,1,3], [2,3,1]] @ [[1,3,2], [3,1,2], [3,2,1]]
    interleave 1 in all permutations of [2,3] *)

fun partition(p,[]) = ([],[])
    |partition(p,x::xs) =
        let val (S,B) = partition (p,xs)
            in if x < p then (x::S,B) else (S,x::B)
        end;
fun sort2([]) = []  
    |sort2(x::xs) =
        let val (S,B) = partition (x,xs)
            in (sort2 S) @ (x :: (sort2 B))  (*Sort the two sublists (smaller & bigger generated by the partition function*) 
        end; 

(*
fun split_helper(L: ''a list, Acc:''a list * ''a list):''a list * ''a list =
    if L=[] then Acc
    else split_helper(tl(L), (#2(Acc), (hd(L)) :: #1(Acc)));

fun split(L) = split_helper(L, ([], []));

fun sort order [] = []
    |sort order [x] = [x]
    |sort order xs =
        let fun merge [] M = M
        |merge L [] = L        
        |merge (L as x::xs) (M as y::ys) =
            if order(x,y) then x::merge xs M
            else y::merge L ys
        val (ys,zs) = split xs
            in merge (sort order ys) (sort order zs) end; *)

fun take(L) =
  if L = nil then nil
  else hd(L)::skip(tl(L))
and
  skip(L) =
    if L=nil then nil
    else take(tl(L));

fun merge([],R) = R
  | merge(L,[]) = L
  | merge(x::xl,y::yl) = if (x:int)<y then x::merge(xl,y::yl)
    else y::merge(x::xl,yl);
fun sort(L) = if L=[] orelse tl(L)=[] then L
  else merge(sort(take(L)),sort(skip(L)));


fun member(X,L) = if (L=[]) then false
    else if (X=hd(L)) then true
    else member(X,tl(L));

fun union(L1, L2) = if (L1=[]) then L2
    else if (L2=[]) then L1
    else if (member(hd(L1),L2)) then union(tl(L1), L2)
    else hd(L1)::union(tl(L1),L2);

fun intersection(L1, L2) = if (L1=[]) then []
    else if (L2=[]) then []
    else if (member(hd(L1),L2)) then hd(L1)::intersection(tl(L1),L2)
    else intersection(tl(L1),L2);

fun subset(L1, L2) = if (L1=[]) then true
    else if (L2=[] andalso L1=[]) then true
    else if (L2=[]) then false
    else if (member(hd(L1),L2)) then subset(tl(L1),L2)
    else false;

fun minus(L1, L2) = if (L1=[]) then []
    else if (L2=[]) then L1
    else if (member(hd(L1),L2)) then minus(tl(L1),L2)
    else hd(L1)::minus(tl(L1),L2);

fun product_help(X,L2) = if (L2=[]) then [] (*No pairs available for creation *)
    else (X,hd(L2))::product_help(X,tl(L2));

fun product(L1,L2) =
    if (L1=[]) then []
    else union(product_help(hd(L1),L2), product(tl(L1),L2));

fun minMaxHelper(min,max,L) = if (L=[]) then (min,max) 
    else if hd(L) < min then minMaxHelper(hd(L),max, tl(L))
    else if hd(L) > max then minMaxHelper(min, hd(L),tl(L))
    else minMaxHelper(min, max, tl(L));
fun minMax(L) = if (L = []) then (0,0)
    else minMaxHelper(hd(L),hd(L),L);

fun max(X:int, Y:int) = if X>Y then X else Y;

fun longestPrefix(L,P) = if L=[] then P
    else if member(hd(L),P) then P (*when reach duplicate, return P*)
    else longestPrefix(tl(L), hd(L)::P); (*add to prefix only when unique member*)

fun longestSublist(L) =
    if L=[] then 0
    else max( length(longestPrefix(L,[])), longestSublist(tl(L)) );

longestSublist([1,1,2,3,2,1,2,3,4,5,6,6]); (* 6 *)

fun odd(n) = if n=0 then false else even(n-1)
    and
    even(n) = if n=0 then true else odd(n-1);

fun opKnuth 1 a b = Math.pow (a,b)
| opKnuth n a b = opKnuth (n−1) a
(opKnuth (n−1) b b);


```