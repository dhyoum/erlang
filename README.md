## Installation
```
apt-get install erlang
```

## 자료형
### Number
```erlnag
1> A = 1.
2> "CAT".     %% 문자열은 List 의 특수한 형태.
"CAT"
3> $C.
67
4> $A.
65
5> $T.
84
6> [67,65,84].
"CAT"
7> 16#0F.    %% Base#Value , Base 는 2..32 범위를 갖는다.
15
```
### Atom
  enum 과 같은 상수.  전역으로 관리되고, 별도의 import 는 필요 없다.
  - 소문자로 시작하는 문자 
  - ' ' 작은 따옴표로 묶여 있는 모든 것
### Term
  erlang 의 데이터 들의 통칭하는 단어로 사용하는 것 같다.
  Term 들 간의 비교
```erlang
(test@KR-00000515)8> 5 =:= 5.	%% 동일한가?
true
(test@KR-00000515)9> 1 =:= 0.   
false
(test@KR-00000515)10> 1 =/= 0.  %% 동일하지 않은가?
true
(test@KR-00000515)11> 5 == 5.0.  %% 동등한가?
true
(test@KR-00000515)12> 5 /= 5.0.  %% 동등하지 않은가?
false
```
### Tuple
   중괄호 ( { } ) 로 표현되는 데이터.   {Element1, Element2, ..., ElementN}.   
   하나의 자료를 이루는 **구조체** 같은 것.  
   **{point, {X, Y}}** 이렇게 데이터의 이름을 부여한 방식이 가독성이 더 좋다.
   
### List
   \[ \] 로 표현되는 데이터. \[Element1, Element2, ..., ElementN\].  
   ** 배열 ** 이다.

### Record
   이름이 있는 Tuple 이다. ( 실제로 내부적으로는 Tuple 이다. )
   따라서, 구조체를 대표하는 이름과 각 Field 의 이름이 있다.
   ```erlang
    rd(person, {name=undefined, age=0}).   %% undefined, 0 은 각각 default 값
                                           %% rd 는 record 를 정의하는 함수
   ```
   
### Binary
```erlang
A = <<5, 8>>.   %% binary 로 0x0508 을 의미한다.
B = <<"cat" >>. %% <<99,97,116>> 과 동일
```


### Bit Syntax

### Maps

## Function

### ...


## Module
  functions 또는 attributes (구조체) 들을 별도의 파일로 관리

```erlang
-module(Name). %% 첫줄은 항상 module 로 시작. Name 과 파일명은 일치
-export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).  %% 외부에 제공하는 기능
```
예)
```erlang
-module(myfunc).
-export([add/2]).
add(A,B) ->
  A + B.
```
  다른 모듈에서 사용할 때.
```erlang
-import(Module, [Function1/Arity, ..., FunctionN/Arity]).
```
  shell 에서 compile
```erlang
2> c(myfunc).      %% 또는 c(myfunc, [debug_info, export_all]).
{ok,myfunc}
3> myfunc:add(7,2).
9
(test@KR-00000515)29> myfunc:module_info().
[{module,myfunc},
 {exports,[{add,2},{module_info,0},{module_info,1}]},
 {attributes,[{vsn,[182319581640350949953262776855260433542]}]},
 {compile,[{version,"7.2.1"},
           {options,[debug_info,
                     {i,"C:/Users/edooyou/eclipse-workspace/test/include"},
                     nowarn_export_all,nowarn_export_vars,nowarn_shadow_vars,
                     warn_unused_function,warn_deprecated_function,
                     nowarn_obsolete_guard,nowarn_unused_import,
                     warn_unused_vars,warn_unused_record]},
           {source,"c:/Users/edooyou/eclipse-workspace/test/src/myfunc.erl"}]},
 {md5,<<137,41,121,99,101,173,60,171,183,223,36,20,33,98,
        12,134>>}]
```

## 문서화
  문서화를 위해서 만들어 내는 것임.
  compiler 를 완전히 무시함.
  
#### Type expression
형식
```erlang
@type newType() = TypeExpression
```
예)
```erlang
@type onOff() = on | off.
@type person() = {person, name(), age()}.
```

#### Function expression
형식
```erlang
 @spec funcName(Arg1, ... Argn) -> Val
```
예)
```erlang
-module(math).
-export([fac/1]).

%% @spec fac((int)) -> int().

fac(0) -> 1;
fac(N) -> N * fac(N-1).
```
좀더 복잡한 예)
```erlang
@spec file:open(string(), [mode()]) -> {ok, file_handle()} | error().
@type error() = {error, string()}.
@type mode() = read | write | compressed | raw | binary | ...
```
