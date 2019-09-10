## common test

말이 필요없다. 일단 실행부터 해보자.

### Quict test
아래와 같이, 빈 디렉토리 ct 를 만들고, ct_run 명령어를 입력해 보자.
```
mkdir ct; cd ct
ct_run
```
ct_run 의 자세한 옵션을 위해서는 ct_run -help 를 입력하면 된다.

ct_run 명령어는 해당 directory 에서 erl 파일을 검색하여, common test 를 수행하는 명령어이다.  
여러 파일이 있다면 여러 TEST 가 모두 수행된다.  
다음에는 간단한 test 가 동작하는 예제를 만들자.  

filename : example_SUITE.erl
```erlang
-module(example_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test1/1, test2/1]).

all() -> [test1, test2].

test1(_Config) ->
	ct:pal("test1"),
	1 = 1.
    
test2(_Config) ->
	A = 10,
	1/A.
```
```
mkdir logdir
ct_run -logdir logdir
Updating /home/dhyoum/ct/logdir/index.html ... done
Updating /home/dhyoum/ct/logdir/all_runs.html ... done
```
다음은 test 를 수행하기 이전과 이후에 각각 전처리 / 후처리가 있는 방식의 예이다.

```erlang
-module(state_SUITE).
-include_lib("common_test/include/ct.hrl").
 
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([ets_tests/1]).
 
all() -> [ets_tests].
 
init_per_testcase(ets_tests, Config) ->
  TabId = ets:new(account, [ordered_set, public]),
  ets:insert(TabId, {andy, 2131}),
  ets:insert(TabId, {david, 12}),
  ets:insert(TabId, {steve, 12943752}),
  [{table,TabId} | Config].
 
end_per_testcase(ets_tests, Config) ->
  ets:delete(?config(table, Config)).
 
ets_tests(Config) ->
  TabId = ?config(table, Config),
  [{david, 12}] = ets:lookup(TabId, david),
  steve = ets:last(TabId),
  true = ets:insert(TabId, {zachary, 99}),
  zachary = ets:last(TabId).
```
여기서 **Config** 에 대해서 알아야 한다.  

Config
     - TEST 과정에서 필요한 데이터를 저장, 관리하는 proplist 이다.  
     - 가급적이면, 직접 proplist 를 handling 하지 말고, ?config(Key, List) 를 이용하자.

* ct_run 을 수행할 때, spec 파일을 별도로 만들어서 각각에 TEST 에 대한 로그 및 상세 설정도 가능하다.
