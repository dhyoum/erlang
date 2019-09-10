### recursive call

  일반적으로 아래와 같이 재귀함수로 만들 수 있다.
  하지만 이렇게 하면 stack depth 가 깊어진다.  
  * 함수를 호출하고, 결과값을 얻은 이후에 또 다른 연산이 기다리고 있다. *

```erlang
fac(N) when N == 0 -> 1;
fac(N) when N > 0  -> N*fac(N-1).

len([]) -> 0;
len([_|T]) -> 1 + len(T).
```

  꼬리 재귀 호출  
  함수 호출 하면서, **결과를 인자로 넘겨주는 방식** 으로 만든다.

```erlang
tail_fac(N) -> tail_fac(N,1).
tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1, N*Acc).

tail_len(L) -> tail_len(L,0).
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T,Acc+1).
```

  더 많은 예재를 원한다면 [클릭](https://learnyousomeerlang.com/static/erlang/recursive.erl)

### High Order function

   함수를 다른 함수의 인자로 넘기는 것이 가능한 함수 : 함수형 프로그래밍의 기본
   이것이 바로 Lamdba 대수에서 기본 원리 아닌가.!

아래와 같은 hhfuns 라는 모듈이 있다.
```erlang
-module(hhfuns).
-compile(export_all).
one() -> 1.
two() -> 2.
add(X,Y) -> X() + Y().
```

이를 Erlang Shell 에서 어떻게 사용할 것인가?
```erlang
1> c(hhfuns).
{ok, hhfuns}
2> hhfuns:add(one, two).       %% one, two 는 그냥 atom 으로 해석하므로 Error
** exception error: bad function one
in function  hhfuns:add/2
3> hhfuns:add(1,2).
** exception error: bad function 1
in function  hhfuns:add/2

%% 각각 function 임을 알려줘야, VM 이 변수에 Binding  할 수 있다.
4> hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).  
3
```

### Anonymous functions
  작성하는 방식
```erlang
Fn = fun() -> a end.
```
```erlang
base(A) ->
  B = A + 1,
  F = fun() -> A * B end,  %% fun 내부에서 A  가 access 된다.
  F().
```

### map
list 를 순회하며 동일한 함수를 수행하는 함수.
```erlang
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].
```
예)
```erlang
lists:map(fun(A) -> 2*A end, lists:seq(1,10)).
[2,4,6,8,10,12,14,16,18,20]
```

### filter
list 를 순회하며 조건에 만족하는 원소를 걸러내는 함수.
```erlang
filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
 
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
  end.
```
예)
```erlang
lists:filter(fun(X) -> X rem 2 == 0 end, lists:seq(1,10)).
[2,4,6,8,10]
```

### fold
list 를 순회하며, 각 함수를 적용하여, 결과를 구하는 함수
예를 들어서, sum 같은 함수가 되겠다. 
```erlang
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).
```
예)
```erlang
lists:foldl(fun(A,B) -> A + B end, 0, lists:seq(1, 10)).
55
```
