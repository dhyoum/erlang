# Installation
```
apt-get install erlang
```

# 자료형
### Number
```erlnag
1> A = 1.
2> "CAT".     %% 문자열은 List 의 특수한 형태일이다.
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
  erlang 의 데이터 구조
  Term 들 간의 비교
```erlang
	(test@KR-00000515)8> 5 =:= 5.
	true
	(test@KR-00000515)9> 1 =:= 0.
	false
	(test@KR-00000515)10> 1 =/= 0.
	true
	(test@KR-00000515)11> 5 == 5.0.
	true
	(test@KR-00000515)12> 5 /= 5.0.
  false
```
