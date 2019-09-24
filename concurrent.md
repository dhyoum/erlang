#### 병행성(Concurrent) 프로그래밍

* spwan, send, receive 세가지의 함수로 구현이 된다.

```erlang
Pid = spawn(Fun)
```
Fun 을 평가하는 새로운 프로세스를 생성한다.  
Pid 는 프로세스로 메시지를 보낼 때 사용할 수 있다.


```erlang
Pid ! Message
```
해당 Pid로 Message 를 비동기(asynchronous) 하게 전송한다.  
전송결과가 Message 이므로, 아래와 같이 모든 프로세스로 메시지를 M 을 전달할 수 도 있다.   
```erlang
Pid1 ! Pid2 ! Pid3 ! ... ! M
```


```erlang
receive
  Pattern1 [ when Guard1] ->
    Expression1;
  Pattern2 [ when Guard2] ->    
    Expression2;
  ...
end
```
어떤 메시지가 프로세스에 도착하면, 시스템은 그 메시지를 Pattern1 부터 차례대로 matching 시킨다.  
어떤 서버가 기능을 제공하려면, 일반적으로 무한 loop 를 running 해야하고, 따라서 아래와 같은 방식으로 구현이 될 것이다.

```erlang
-module(sum_server).
-export([loop/0]).

loop() ->
    receive
        {A, B} ->
            io:format("Recieved ~p~n", [{A,B}]),
            loop();
        Other ->
            io:format("Wrong format ~p~n", [Other]),
            loop()
    end.
```
이걸 그냥 수행하면, 그냥 receive 상태에서 아무것도 할 수 가 없다.
spawn 으로 수행해서, 메시지를 송수신 할 수 있는 Actor 로 만들고, 메시지를 전달해보자.
```erlang
4> Pid = spawn(fun sum_server:loop/0).
<0.558.0>
6> Pid ! {1,2}.
Recieved {1,2}
{1,2}
```
