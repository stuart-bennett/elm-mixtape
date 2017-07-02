let gulp = require("gulp"),
    less = require("gulp-less");

gulp.task("less", () =>
    gulp.src("./src/less/main.less")
        .pipe(less({
            paths: ["./node_modules/bootstrap/less"]
        }))
        .pipe(gulp.dest("./app")));

gulp.task("default", ["less"]);
