## Module Node.FS.Aff

#### `rename`

``` purescript
rename :: forall eff. FilePath -> FilePath -> Aff (fs :: FS | eff) Unit
```


Rename a file.


#### `truncate`

``` purescript
truncate :: forall eff. FilePath -> Int -> Aff (fs :: FS | eff) Unit
```


Truncates a file to the specified length.


#### `chown`

``` purescript
chown :: forall eff. FilePath -> Int -> Int -> Aff (fs :: FS | eff) Unit
```


Changes the ownership of a file.


#### `chmod`

``` purescript
chmod :: forall eff. FilePath -> Perms -> Aff (fs :: FS | eff) Unit
```


Changes the permissions of a file.


#### `stat`

``` purescript
stat :: forall eff. FilePath -> Aff (fs :: FS | eff) Stats
```


Gets file statistics.


#### `link`

``` purescript
link :: forall eff. FilePath -> FilePath -> Aff (fs :: FS | eff) Unit
```


Creates a link to an existing file.


#### `symlink`

``` purescript
symlink :: forall eff. FilePath -> FilePath -> SymlinkType -> Aff (fs :: FS | eff) Unit
```


Creates a symlink.


#### `readlink`

``` purescript
readlink :: forall eff. FilePath -> Aff (fs :: FS | eff) FilePath
```


Reads the value of a symlink.


#### `realpath`

``` purescript
realpath :: forall eff. FilePath -> Aff (fs :: FS | eff) FilePath
```


Find the canonicalized absolute location for a path.


#### `realpath'`

``` purescript
realpath' :: forall eff cache. FilePath -> {  | cache } -> Aff (fs :: FS | eff) FilePath
```


Find the canonicalized absolute location for a path using a cache object
for already resolved paths.


#### `unlink`

``` purescript
unlink :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
```


Deletes a file.


#### `rmdir`

``` purescript
rmdir :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
```


Deletes a directory.


#### `mkdir`

``` purescript
mkdir :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
```


Makes a new directory.


#### `mkdir'`

``` purescript
mkdir' :: forall eff. FilePath -> Perms -> Aff (fs :: FS | eff) Unit
```


Makes a new directory with the specified permissions.


#### `readdir`

``` purescript
readdir :: forall eff. FilePath -> Aff (fs :: FS | eff) (Array FilePath)
```


Reads the contents of a directory.


#### `utimes`

``` purescript
utimes :: forall eff. FilePath -> Date -> Date -> Aff (fs :: FS | eff) Unit
```


Sets the accessed and modified times for the specified file.


#### `readFile`

``` purescript
readFile :: forall eff. FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Buffer
```


Reads the entire contents of a file returning the result as a raw buffer.


#### `readTextFile`

``` purescript
readTextFile :: forall eff. Encoding -> FilePath -> Aff (fs :: FS | eff) String
```


Reads the entire contents of a text file with the specified encoding.


#### `writeFile`

``` purescript
writeFile :: forall eff. FilePath -> Buffer -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
```


Writes a buffer to a file.


#### `writeTextFile`

``` purescript
writeTextFile :: forall eff. Encoding -> FilePath -> String -> Aff (fs :: FS | eff) Unit
```


Writes text to a file using the specified encoding.


#### `appendFile`

``` purescript
appendFile :: forall eff. FilePath -> Buffer -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
```


Appends the contents of a buffer to a file.


#### `appendTextFile`

``` purescript
appendTextFile :: forall eff. Encoding -> FilePath -> String -> Aff (fs :: FS | eff) Unit
```


Appends text to a file using the specified encoding.


#### `exists`

``` purescript
exists :: forall eff. String -> Aff (fs :: FS | eff) Boolean
```


Check to see if a file exists.


