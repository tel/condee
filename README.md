# condee

A little expression evaluator that could.

```
> condee "2+2"
ok num 4.0
> condee "2+2 > 3"
ok bool true
> condee "2+2 > 3 | false"
ok bool true
> condee "2+2 > 3 & false"
ok bool false
> condee "2~2"
ok bool true
> condee "2~2.01"
ok bool true
> condee "2~2.1"
ok bool false
```

Tested with GHC 7.6. Build is `make all`.
