-module(exerl_compile).

-export([
    compile/2
]).

compile(Paths, Dest) ->
    filelib:ensure_path(filename:join(Dest, "out")),
    'Elixir.Kernel.ParallelCompiler':compile_to_path(
        Paths,
        Dest
    ).
