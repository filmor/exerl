exerl - Elixir interoperability for `rebar3`
============================================

This plugin provides the following functionality:

- Using Elixir itself as a dependency
- Compiling Elixir source files
- Building `mix` dependencies
- Additional commands for `rebar3`

## Usage

Requires Erlang/OTP 27 or newer.

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

Mix projects used as Git dependencies or local `_checkouts` are prepared
automatically before rebar discovers their dependencies. Exerl uses Hex's package
builder to generate `hex_metadata.config` (the package's `metadata.config`) and a
`rebar.config` containing its required production Hex dependencies. Optional and
development-only dependencies are not installed. An existing hand-written
`rebar.config` or `rebar.config.script` is left untouched.

Preparation runs directly in the Erlang VM using the same Elixir/Mix runtime as
compilation; Hex is installed automatically if missing. Mix projects with non-Hex
production dependencies are rejected rather than silently omitting those dependencies.

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

## Tests

Run `just test` (or `rebar3 do eunit, ct`). The Common Test suite builds the
consumer project in `test/exerl_SUITE_data/exerl_test` with this checkout as its
plugin. It checks mixed Erlang/Elixir compilation, Rebar and Mix dependencies,
protocol consolidation, and release assembly and execution in isolated Erlang VMs.
Stale consolidated copies are removed when a dependency replaces a protocol with
a regular module, so incremental builds do not rediscover the obsolete protocol.
The fixture retains the dependencies and lockfile from the standalone test project,
including Explorer's precompiled native library, so the first run needs network
access to download dependencies and Elixir for the current OTP version. Use a
current Rebar3 from `PATH`; the bundled Rebar3 3.24 cannot resolve all of the
fixture's dependencies.

Mix dependency builds run with an isolated project stack so Elixir 1.20 does not
reuse another project's dependency cache. Compiler diagnostics are returned to
Rebar as printable messages. The fixture's release configuration also excludes
Elixir's original `JSON.Encoder` protocol, keeping the consolidated copy instead.
Erlang-only applications and plugins do not start Elixir, preventing the host
installation from being initialized before the project's Elixir dependency loads.

GitHub Actions runs EUnit and Common Test on all supported combinations of
OTP 27-29 and Elixir 1.17-1.20, with patch versions pinned in the workflow:

| OTP | Elixir |
| --- | --- |
| 27 | 1.17, 1.18, 1.19, 1.20 |
| 28 | 1.18, 1.19, 1.20 |
| 29 | 1.20 |

Set `EXERL_TEST_ELIXIR` to an exact version (for example `1.18.4`), use matching
Erlang and Elixir executables on `PATH`, and use a fresh `REBAR_BASE_DIR` for
each runtime pair to reproduce a matrix entry locally. Both the test profile
and the consumer project use that version; only Elixir entries are removed
from the fixture's copied lockfile.
The suite verifies the Elixir version in the consumer VM. On OTP 29, CI
suppresses the new catch and match-alias warnings because older dependencies
treat warnings as errors.
