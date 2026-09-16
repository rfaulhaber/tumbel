# Development tasks for tumbel.el. Run inside `nix develop`, or set EMACS to an
# Emacs that has elisp-lint (and its dependencies) on its load-path.

emacs := env('EMACS', 'emacs')
batch := emacs + ' -Q --batch -L . -L test'

# elisp-lint writes a scratch <dir>-autoloads.el next to the sources; never
# treat it as one.  The C locale keeps the order the same everywhere, so
# a compile that passes here passes in the Nix sandbox.
srcs := `LC_ALL=C ls tumbel*.el | grep -v -- '-autoloads\.el$' | tr '\n' ' '`
tests := `LC_ALL=C ls test/*-test.el | tr '\n' ' '`

default: compile

# Byte-compile every source file, treating warnings as errors
compile:
    {{batch}} --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile {{srcs}}

# Run the ERT suites in test/
test: compile
    {{batch}} -l ert {{prepend('-l ', tests)}} -f ert-run-tests-batch-and-exit

# checkdoc, package-lint, check-declare, indentation, fill-column, whitespace
lint:
    {{batch}} -l elisp-lint --eval '(setq package-lint-main-file "tumbel.el")' -f elisp-lint-files-batch {{srcs}}

# Remove byte-compiled and scratch files
clean:
    rm -f *.elc test/*.elc *-autoloads.el
