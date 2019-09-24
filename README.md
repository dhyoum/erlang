## Installation
```sh
apt-get install erlang
```

* compile & execute
```sh
  erlc helloworld.erl
  erl -noshell -s helloworld start -s init stop
```
-s helloworld start  
    hellworld:start() 함수를 실행한다.  
-s init stop  
    apply(hello, start, []) 가 완료되면, init:stop() 함수를 실행한다.

* Escript 로 실행하기
```sh
  #!/usr/bin/env escript
  main(_) ->
      io:format("hello").
```

### online test
[link](https://www.tutorialspoint.com/compile_erlang_online.php)

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
1> 5 =:= 5.	%% 동일한가?
true
2> 1 =:= 0.   
false
3> 1 =/= 0.  %% 동일하지 않은가?
true
4> 5 == 5.0.  %% 동등한가?
true
5> 5 /= 5.0.  %% 동등하지 않은가?
false
```
### Tuple
   중괄호 ( { } ) 로 표현되는 데이터.
   하나의 자료를 이루는 **구조체** 같은 것.  
   ```erlang
   {Element1, Element2, ..., ElementN}.
   ```
   **{point, {X, Y}}** 이렇게 데이터의 이름을 부여한 방식이 가독성이 더 좋다.
   
### List
   \[ \] 로 표현되는 데이터.  
   ```erlang
   [Element1, Element2, ..., ElementN].
   ```
   **배열** 이다.

### Record
   이름이 있는 Tuple 이다. ( 실제로 내부적으로는 Tuple 이다. )  
   따라서, 구조체를 대표하는 이름과 각 Field 의 이름이 있다.

* Syntax
```erlang
#recordname {fieldName1 = value1, fieldName2 = value2 .. fieldNameN = valueN}
```
  * Defining a Record
   ```erlang
    rd(person, {name = "", phone = [], address}).
   ```   
  * Creating a Record
  ```erlang
     #person{phone=[0,8,2,3,4,3,1,2], name="Robert"}.
  ```

  * Accessing a Record Field
  ```erlang  
     P = #person{name = "Joe", phone = [0,8,2,3,4,3,1,2]}.
     P#person.name.
  ```    

  * Updating a Record
  ```erlang
     P1 = #person{name="Joe", phone=[1,2,3], address="A street"}.
     P2 = P1#person{name="Robert"}.
  ```

  * Pattern Matching
  ```erlang
     P3 = #person{name="Joe", phone=[0,0,7], address="A street"}.
     #person{name = Name} = P3, Name.   
  ```

* 별도의 파일로 기록하고, 이를 Erlang Shell 에서 읽어 드리는 방법.  

    filename : test.hdr
```erlang
-record(person, {name=undefined, age=0}).
-record(company, {name=undefined, year=0}).
```
```sh
1> rr("test.hdr"). %% rr 명령어를 이용해서, reocord 를 읽어드린다.
[company,person]
2> rl().
```  

* Nested Records
```erlang
-record(nrec0, {name = "nested0"}).
-record(nrec1, {name = "nested1", nrec0=#nrec0{}}).
-record(nrec2, {name = "nested2", nrec1=#nrec1{}}).

N2 = #nrec2{},
```

* Access 하는 방법

```erlang
"nested0" = N2#nrec2.nrec1#nrec1.nrec0#nrec0.name,
N0n = N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = "nested0a"},
```

* 아래와 같이, 각 filed 에 대해서 type 을 명세할 수는 있지만, 실제로 값을 제한하거나 하는 것은 아니다.

```erlang
-record(person, {name :: 'undefined' | 'a' | 'b', age :: 0..127}).
```
   
### Binary
```erlang
A = <<5, 8>>.   %% binary 로 0x0508 을 의미한다.
B = <<"cat" >>. %% <<99,97,116>> 과 동일
```

### Bit Syntax

* to be updated


### Maps

  key - value 로 구성된 데이터 구조.  
  python 에서 dictionary 와 동등하다.  
  update / remove 를 이용하여, 새로운 Map 을 반환한다.
  
```erlang
-module(map).
-compile([export_all]).

write(String, Value) ->
    io:format("~p = ~p~n", [String, Value]).

run() ->
    M1 = #{name => "Joe Doe", age => 25},
    write("Map", M1),
    write("Name", maps:get(name, M1)),
    write("Degree", maps:get(degree, M1, defaultdegree)),

    Keyname = randomkey,
    case maps:find(Keyname, M1) of
        {ok, Value} ->
            write("Found value", Value);
        error ->
            write("No value found for key", Keyname)
    end,
    ok.
```

  * Update
```erlang  
   Map = #{"a" => 1}.
   #{"a" => 1}
   > maps:update("a", 42, Map).
   #{"a" => 42}
```

  * Remove
```erlang
  > Map = #{"a" => 1}.
  #{"a" => 1}
  > maps:remove("a",Map).
  #{}
  > maps:remove("b",Map).
  #{"a" => 1}
```

  * [property list](http://erlang.org/doc/man/proplists.html) 와의 변환
  ```erlang
    List = [{"a",ignored},{1337,"value two"},{42,value_three},{"a",1}],
    maps:from_list(List).
    #{42 => value_three,1337 => "value two","a" => 1}
  ```
  ```erlang
    Map = #{42 => value_three,1337 => "value two","a" => 1},
    maps:to_list(Map).
    [{42,value_three},{1337,"value two"},{"a",1}]
  ```

## Module
  functions 또는 attributes (구조체) 들을 별도의 파일로 관리

```erlang
-module(Name). %% 첫줄은 항상 module 로 시작. Name 과 파일명은 일치
-export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).  %% 외부에 제공하는 기능
```
예)
filename : myfunc.erl
```erlang
-module(myfunc).
-export([add/2]).
add(A,B) ->
  A + B.
```
filename : mydata.hrl
```erlang
-include("other.hrl").          %% 절대패스 or 상대패스
-include_lib("lib/other2.hrl"). %% 현재 lib 의 현재버전을 확인하여, 그 아래의 other2.hrl 을 가지고 온다. 
-define(DEFAULT, noname).
-record(person, {name=?DEFAULT, age}).
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
