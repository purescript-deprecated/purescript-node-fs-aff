# purescript-node-fs-aff

[![Latest release](http://img.shields.io/github/release/purescript-node/purescript-node-fs-aff.svg)](https://github.com/purescript-node/purescript-node-fs-aff/releases)
[![Build status](https://github.com/purescript-node/purescript-node-fs-aff/workflows/CI/badge.svg?branch=master)](https://github.com/purescript-node/purescript-node-fs-aff/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-node-fs-aff/badge)](https://pursuit.purescript.org/packages/purescript-node-fs-aff)

[Node.FS][node.fs] wrappers for [purescript-aff][aff].

The `Aff` monad lets you write async code with ease, and `node-fs-aff` lets you easily access the filesystem within `Aff`.

## Installation

```
spago install node-fs-aff
```

## Example

Consider asynchronously listing only non-hidden directories:

```purescript
isDotFile :: FilePath -> Boolean
isDotFile filepath = (charAt 0 filepath) == Just "."

isHiddenDirectory :: FilePath -> Aff Boolean
isHiddenDirectory filepath = do
  stat <- FSA.stat filepath
  pure $ FS.isDirectory stat && not (isDotFile filepath)
       
main :: Effect Unit
main = launchAff_ do
  files <- FSA.readdir "."
  files' <- filterM isHiddenDirectory (L.fromFoldable files) 
  liftEffect $ traverse log files'
```

You can see this example in action by running `spago run` in the example directory.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-node-fs-aff).

[node.fs]: http://github.com/purescript-node/purescript-node-fs
[aff]: https://github.com/slamdata/purescript-aff
