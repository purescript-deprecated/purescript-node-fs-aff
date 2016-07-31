# purescript-node-fs-aff

[![Latest release](http://img.shields.io/bower/v/purescript-node-fs-aff.svg)](https://github.com/purescript-node/purescript-node-fs-aff/releases)
[![Build Status](https://travis-ci.org/purescript-node/purescript-node-fs-aff.svg?branch=master)](https://travis-ci.org/purescript-node/purescript-node-fs-aff)
[![Dependency Status](https://www.versioneye.com/user/projects/579dffa9aa78d500469f9d71/badge.svg?style=flat)](https://www.versioneye.com/user/projects/579dffa9aa78d500469f9d71)

[Node.FS][Node.FS] Wrappers for [purescript-aff][aff]

The `Aff` monad lets you write async code with ease, and `node-fs-aff`
lets you easily access the filesystem within `Aff`.

## Example

Consider asynchronously listing only non-hidden directories:

``` purescript
main = launchAff do
  files <- FS.readdir "."
  files' <- flip filterM files \file -> do
    stat <- FS.stat file
    pure $
         FS.isDirectory stat
      && (maybe false (fromChar >>> (/= ".")) $ charAt 0 file)
  liftEff $ print files'
```

That was easy. Run `npm run example` to see it work.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-node-fs-aff).


[Node.FS]: http://github.com/purescript-node/purescript-node-fs
[aff]: https://github.com/slamdata/purescript-aff
