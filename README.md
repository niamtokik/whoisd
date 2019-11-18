# whoisd

whois (RFC3912) protocol implementation in Erlang

## Build

```
rebar3 compile
rebar3 escriptize
```

## Test

```
rebar3 test
rebar3 ct
```

## Server Usage

```
whoisd [-address Address] [-port Port]
```

 * default `Address`: 127.0.0.1
 * default `Port`: 4343

## Client Usage

```
whois [-host Host] [-port Port]
```

 * default `Host`: 127.0.0.1
 * default `Port`: 43
