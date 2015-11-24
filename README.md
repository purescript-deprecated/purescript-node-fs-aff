## Node.FS.Aff

[![Build Status](https://travis-ci.org/purescript-node/purescript-node-fs-aff.svg)](https://travis-ci.org/purescript-node/purescript-node-fs-aff)

> [Node.FS][Node.FS] Wrappers for [purescript-aff][aff]

The `Aff` monad let's you write async code with ease.

#### Example

Consider asynchronously listing only non-hidden directories:

``` purescript
main = launchAff do
  files <- FS.readdir "."
  files' <- flip filterM files \file -> do
    stat <- FS.stat file
    return $
         FS.isDirectory stat
      && (maybe false (fromChar >>> (/= ".")) $ charAt 0 file)
  liftEff $ print files'
```

That was easy. Run `npm run example` to see it work.

#### Documentation

[Read the module docs][docs]


[Node.FS]: http://github.com/purescript-node/purescript-node-fs
[aff]: https://github.com/slamdata/purescript-aff
[docs]: http://github.com/purescript-node/purescript-node-fs-aff/blob/master/docs/Node/FS/Aff.md
