# whoisd

whois protocol implementation in Erlang

## Build

```
rebar3 compile
```

## listener and acceptor structure

```
  _____________
 |             |
 | Application |
 |_____________|
      |
  ____|_______
 |            |
 | Supervisor |
 |____________|
   |  |
  ____|_____     ____________      ____      __________
 |          |   |            |    (    )    |          |
 | Listener |---| Supervisor |---( pool )---| Acceptor |
 |__________|   |____________|    (____)    |__________|
   |
   |
  _|________     ____________      ____      __________
 |          |   |            |    (    )    |          |
 | Listener |---| Supervisor |---( pool )---| Acceptor |
 |__________|   |____________|    (____)    |__________|



```
