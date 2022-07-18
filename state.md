# State

```type State s a = s -> (s, a)```

Keep in mind that the standard libraries define State in a slightly different way!

``` haskell
yield :: a -> State s a
yield :: a -> s -> (s, a)
yield v = \origState -> (origState, v)
yield v origState = (origState, v)
```

``` haskell
get :: State s s
get :: s -> (s, a)
get = \origState -> (origState, origState)
get origState = (origState, origState)
```

``` haskell
put :: s -> State s ()
put :: s -> s -> (s, ())
put newState = \origState -> (newState, ())
put newState origState = (newState, ())
```

``` haskell
bindS :: State s a -> (a -> State s b) -> State s b
bindS f rest = \origState ->
  let (newState, retVal) = f origState in
  rest retVal newState
```
