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
