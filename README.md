# Type class derivation in Scala 3

This repo contains the code to go with these slides: https://slides.com/cb372/type-class-derivation-in-scala-3

## How to run the examples

### Haskell

```
cd haskell/derivation-examples
stack run
```

### Rust

```
cd rust/derivation-examples
cargo run
```

### Scala 3 Show and Functor examples

```
sbt run
```

### Scala 3 Mu gRPC server example

```
sbt muService/run
```


## Notes on the Functor implementation

If you look carefully you'll see that `Functor.derived` is a `given`, which makes the use of `derives` completely redundant. `Functor` will be available even if the user doesn't bother to add `derives Functor` to their ADT, which is not great.

I wanted to rewrite `Functor` to follow the same pattern as `Show`, which is a lot simpler. It avoids the need for `given`, and it handles recursive datatypes nicely without the need for `LazyWrapper`. Unfortunately I couldn't quite get it to compile. You can see my attempt on the `rewrite-functor` branch.

PRs welcome :)
