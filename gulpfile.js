let gulp = require("gulp"),
    sass = require("gulp-sass");

gulp.task("sass", () =>
    gulp.src("./src/scss/main.scss")
        .pipe(sass({
            includePaths: ["./node_modules/bootstrap/scss"]
        })).on("error", sass.logError)
        .pipe(gulp.dest("./app")));

gulp.task("default", ["sass"]);
