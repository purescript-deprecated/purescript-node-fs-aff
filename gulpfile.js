'use strict';

/**
 * Add `./output` to path i.o. to load modules.
 * Warning: This is a hack
 * https://gist.github.com/branneman/8048520#6-the-hack
 */
process.env.NODE_PATH = __dirname + '/output';
require('module').Module._initPaths();

var gulp = require('gulp')
  , purs = require('gulp-purescript')
  , Promise = require('bluebird')
  , del = Promise.promisifyAll(require('del'))
  , _ = require('lodash')
;

var src = [ 'src/**/*.purs' ]
  , deps = [ 'bower_components/purescript-*/src/**/*.purs' ]
  , example = [ 'example/example.purs' ]
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

gulp.task('docs', function() {
    return gulp
        .src(src)
        .pipe(purs.pscDocs({}))
        .pipe(gulp.dest('README.md'))
    ;
});

gulp.task('make:example', function() {
    return gulp
        .src(_.flatten([ src, example, deps ], false))
        .pipe(purs.pscMake({}))
    ;
});

gulp.task('example', ['make:example'], function() {
    require('Example.Main').main();
});
