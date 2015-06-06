'use strict';

var gulp = require('gulp')
  , purs = require('gulp-purescript')
  , Promise = require('bluebird')
  , del = Promise.promisifyAll(require('del'))
  , _ = require('lodash')
;

var src = [ 'src/**/*.purs' ]
  , deps = [ 'bower_components/purescript-*/src/**/*.purs' ]
  , output = [ 'output' ]
;

gulp.task('psci', function() {
    return gulp
        .src(_.flatten([ src, deps ], false))
        .pipe(purs.dotPsci())
    ;
});

gulp.task('make', [ 'psci' ], function() {
    return gulp
        .src(_.flatten([ src, deps ], false))
        .pipe(purs.pscMake({}))
    ;
});
