### List

- List 는 erlang 의 시작과 끝이다. 이유는 간단하다, 일단 for 문이 없다. 이런식으로 해야한다.

```erlang
for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I) | for(I+1, Max, F)].

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].
```

### con operator
```erlang
1> [1|[]] =:= [1].
2> [1|[2|[]]] =:= [1,2].
```

### 유용한 함수들
```erlang
1> lists:all(fun(X) -> X rem 2 == 0 end, [1,2,3]). %% 리스트의 모든 요소가 조건에 부합하는지 검사합니다.
false
2> lists:all(fun(X) -> X < 5 end, [1,2,3]).
true
4> lists:any(fun(X) -> X rem 2 == 0 end, [1,2,3]). %% 리스트의 어느 한 요소라도 조건에 부합하는지 검사합니다.
true
5> lists:seq(1, 10). %% 첫 번째 param부터 두 번째 param까지의 연속적인 수의 리스트를 생성합니다.
[1,2,3,4,5,6,7,8,9,10]
6> lists:seq(1, 20, 3). %% 세 번째 param으로 주기를 만들 수 있습니다.
[1,4,7,10,13,16,19]
7> lists:seq(-1, -10, -1). %% 다음과 같이 감소하는 리스트를 만들 수 있습니다.
[-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]
8> lists:sort([1,3,2,7,6,4,5]). %% 리스트를 정렬합니다.
[1,2,3,4,5,6,7]
9> lists:sort(fun(X,Y) -> X > Y end, [1,3,2,7,6,4,5]). %% functional object를 줘서 정렬 조건을 부여할 수도 있습니다.
[7,6,5,4,3,2,1]
10> lists:sum([1,2,3,4,5]).
15
15> A = [{X, some_value} || X <- lists:seq(1,5)].
[{1,some_value},
 {2,some_value},
 {3,some_value},
 {4,some_value},
 {5,some_value}]
16> lists:keyfind(3, 1, A). %% tuple 리스트에서 특정 key를 가진 요소를 찾습니다. 두 번째 param은 tuple 내에서 비교할 key의 위치입니다. (*1부터 시작합니다.*)
{3,some_value}
```


### List Comprehensions

```erlang
1> [2*N || N <- [1,2,3,4]].
[2,4,6,8]
```
```erlang
2> [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
[2,4,6,8,10]
```

  with patern matching 
```erlang
6> Weather = [{toronto, rain}, {montreal, storms}, {london, fog},  
6>            {paris, sun}, {boston, fog}, {vancouver, snow}].
[{toronto,rain},
{montreal,storms},
{london,fog},
{paris,sun},
{boston,fog},
{vancouver,snow}]
7> FoggyPlaces = [X || {X, fog} <- Weather].
[london,boston]
```
