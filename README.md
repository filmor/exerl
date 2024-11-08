exerl - Elixir interoperability for `rebar3`
============================================

This plugin provides the following functionality:

- Using Elixir itself as a dependency
- Compiling Elixir source files
- Building `mix` dependencies
- Additional commands for `rebar3`

## Usage

Add the plugin to your `rebar.config` and configure it:

```erlang
% The plugin itself
{plugins, [exerl]}.

% Inject step to consolidate protocols after compilation
{provider_hooks, [
    {post, [{compile, consolidate_protocols}]}
]}.

% Reference Elixir as a dependency
{deps, [
    {elixir_full, {ex, "1.17"}},
    % Use arbitrary hex dependencies...
]}.
```

Elixir files can be placed in the `src` directory.

The only additional command provided by this plugin right now is `rebar3 iex`
which starts an interactive Elixir shell with the project's dependencies loaded:

```shell
$ rebar3 iex
===> Verifying dependencies...
===> Compiling exerl
===> Consolidating protocols...
Erlang/OTP 26 [erts-14.2.5.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Interactive Elixir (1.16.2) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)>
```
