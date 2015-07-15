'use strict';

var gulp = require('gulp')
  , purs = require('gulp-purescript')
  , run = require('gulp-run')
  , _ = require('lodash')
;

var src = [ 'src/**/*.purs'
          , 'bower_components/purescript-*/src/**/*.purs'
          ]
  , ffi = [ 'src/**/*.js'
          , 'bower_components/purescript-*/src/**/*.js'
          ]
  , example = [ 'example/example.purs' ]
;

gulp.task('psci', function() {
    return purs.psci({
        src: src
      , ffi: ffi
    }).pipe(gulp.dest('.'));
});

gulp.task('make', [ 'psci' ], function() {
    return purs.psc({
        src: src
      , ffi: ffi
    });
});

gulp.task('make:example', function() {
    return purs.psc({
        src: _.flatten([ src, example ])
      , ffi: ffi
    });
});

gulp.task('example', ['make:example'], function () {
    return purs.pscBundle({
        src: 'output/**/*.js'
      , main: 'Example.Main'
    }).pipe(run('node'));
});
