## 병행성(Concurrent) 프로그래밍

* spwan, send, receive 세가지의 함수로 구현이 된다.

### spawn

```erlang
Pid = spawn(Fun)
```
Fun 을 평가하는 새로운 프로세스를 생성한다.  
Pid 는 프로세스로 메시지를 보낼 때 사용할 수 있다.  

* MFA 로 spawn 하기 - 동적 코드 로딩의 시작.
```erlang
spawn(Mod, FuncName, Args)
```

### Send

```erlang
Pid ! Message
```
해당 Pid로 Message 를 비동기(asynchronous) 하게 전송한다.  
전송결과가 Message 이므로, 아래와 같이 모든 프로세스로 메시지를 M 을 전달할 수 도 있다.   
```erlang
Pid1 ! Pid2 ! Pid3 ! ... ! M
```

### Receve

Erlang 에서는 각 프로세스는 연결된 mailbox 를 하나씩 가지고, 프로세스로 메시지를 전달하면, mailbox 로 유입되고, 그 mailbox 는 recieve 구문을 통해서만 검사가 이뤄진다.  
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
어떤 서버가 기능을 제공하려면, 일반적으로 무한 loop 로 running 하면서, 메시지를 기다리고 각 메시지에 따라서 다양한 동작을 수행한다.  

아래와 같은 방식으로 구현이 될 것이다.

```erlang
-module(sum_server).
-export([loop/0]).

loop() ->
    receive
        {A, B} ->
            io:format("Recieved ~p~n", [{A,B}]),
            loop();    %% tail-recursive call
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

### Client-Server Model

Erlang 의 Pattern Matching 을 이용하여,    
recieve 구문을 각 Sender 와 Message 별로 구분해서 처리하도록 하면, 더욱 근사한 Client-Server 를 구현할 수 있다.
```erlang
-module(fun_server).
-export([loop/0, rpc/2]).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {From, {func1, A, B}} ->
            From ! {self(), A+B},
            loop();
        {From, {func2, A, B}} ->
            From ! {self(), A*B},
            loop()
    end.
```
```erlang
21> c(fun_server).
{ok,fun_server}
22> Pid = spawn(fun fun_server:loop/0).
<0.20776.0>
23> fun_server:rpc(Pid, {func1, 10,20}).
30
24> fun_server:rpc(Pid, {func2, 10,20}).
200
```

### 등록된 프로세스

시스템에 Pid 와 식별자를 등록하여, 송수신을 편하게 이용하는 방법.  

```erlang
register(Atom, Pid)
unregister(Atom)
whereis(Atom)
registered()
```


### 프로세스 연결하기

프로세스간에 오류의 확산 경로를 정의하는 것. 즉, 한쪽 프로세스에서 오류(죽거나 Exception 이 발생한 경우)가 발생하면, **link** 되어 있는 모든 프로세스로 오류를 전달한다.  

* **link** 는 기본적으로 bi-directional 이다. 따라서, 다루기 까다로운 경우도 많다.

* 내가 생성한 프로세스가 멎어도 몰라.
```erlang
Pid = spawn(func() -> .. end)
```
* 내가 생성한 프로세스가 멎어도 나도 죽어. ( link 만 하고 아무것도 하지 않은 상태는 그냥 죽는 것 )
```erlang
Pid = spawn_link(func() -> .. end)
```
* 내가 생성한 프로세스가 멎으면 내가 어떻게든 해볼께.
```erlang
process_flag(trap_exit, true),  %% 시스템 프로세스(다른 프로세스의 종료시그널을 trap 할 수 있는 프로세스) 로 전환
Pid = spawn(func() -> .. end),
...
loop(...).
loop(State) ->
    received
        {'EXIT', SomePid, Reason} ->
            %% 오류로 뭔가 처리가 필요.!
            loop(State1);
        ....
    end
```

* 시스템 프로세스  
    종료 신호를 받아도, 죽지 않는 프로세스.  
    trap_exit 의 설정으로 여러가지 동작이 이뤄진다.  
    
  | trap_exit | 종료신호 | action |
  |-----------|---------|--------|
  | true      | kill    | 죽는다. 종료신호 killed 를 연결 집합으로 통보  | 
  | true      | X       | 메일박스에 {'EXIT', Pid, X} 추가 |
  | false     | normal  | 계속한다. 아무것도 하지 않고 신호가 사라진다. |
  | false     | kill    | 죽는다. 종료신호 killed 를 연결 집합으로 통보  | 
  | false     | X       | 죽는다. 종료신호 X 를 연결 집합으로 통보 |
  

### OTP 프로그램

* link
http://erlang.org/doc/design_principles/des_princ.html#otp%20design%20principles
