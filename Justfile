default:
    @just --choose

test:
    mkdir -p _checkouts
    test -e _checkouts/exerl || ln -s .. _checkouts/exerl
    rebar3 do eunit, ct
