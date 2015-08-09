Stempel (polish stemmer) for Erlang
=================

Start node in dev mode

```erlang
erl -pa $(pwd)/ebin deps/*/ebin -sname alice
```

Start application

```erlang
application:ensure_all_started(estem).
```

Test that java works

```erlang
estem_java:pid().
```

Run stemmer

```erlang
estem_java:stem([<<"do">>, <<"wynajęcia">>, <<"przytulne">>, <<"bezczynszowe">>, <<"mieszkaniefgxxdd">>, <<"444">>]).
```

Whole example
```erlang
~/erlang/estem|master $ erl -pa $(pwd)/ebin deps/*/ebin -sname alice
Erlang/OTP 17 [erts-6.4] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.4  (abort with ^G)
(alice@theta)1> application:ensure_all_started(estem).
16:58:07.890 [info] Application lager started on node alice@theta
16:58:08.040 [info] Java Info:  Estem Node Started at: xxxd_java@theta
16:58:08.040 [notice] Java node started
16:58:08.045 [info] Application estem started on node alice@theta
{ok,[syntax_tools,compiler,goldrush,lager,estem]}
(alice@theta)2>
(alice@theta)2> estem_java:pid().
<10787.1.0>
(alice@theta)3>
(alice@theta)3> estem_java:stem([<<"do">>, <<"wynajęcia">>, <<"przytulne">>, <<"bezczynszowe">>, <<"mieszkaniefgxxdd">>, <<"444">>]).
[<<"do">>,
 <<119,121,110,97,106,25,99,105,97>>,
 <<"przytulny">>,<<"bezczynszowy">>,<<"mieszkaniefgxxdda">>,
 <<"444">>]
```

Credits
=======

Credits to Fernando Benavides from Inaka and his sample project for jinterface
https://github.com/elbrujohalcon/jinterface-sample/
