# Module Documentation

## Module Node.FS.Aff

[Node.FS][Node.FS] Wrappers for [purescript-aff][aff]

- [Module Documentation](docs/Node/FS/Aff.md)

The `Aff` monad let's you write async code with ease.

Consider asynchronously listing only non-hidden directories:

``` purescript
main = launchAff do
  files <- FS.readdir "."
  files' <- flip filterM files \file -> do
    stat <- FS.stat file
    return $
         FS.isDirectory stat
      && (maybe false (fromChar >>> (/= ".")) $ charAt 0 file)
  liftEff $ Debug.Trace.trace $ show files'
```

That was easy. For a working example, see [example.purs][example].
To build the example, run `gulp example`.

[Node.FS]: http://github.com/purescript-node/purescript-node-fs
[aff]: https://github.com/slamdata/purescript-aff
[example]: http://github.com/purescript-node/purescript-node-fs-aff/blob/master/example/example.purs
