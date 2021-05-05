% erl-netrc

# Introduction
The erl-netrc project provides support for netrc files. Netrc files are used by
various programs to store credentials for multiple services. For example,
[Curl](https://curl.se) can use credentials from the netrc file of the current
user if the `-n` flag is provided.

# Conformance
While there are no formal specification for netrc files, implementations tend
to support the same set of common fields.

We currently accept the following fields:
- `machine <string>`
- `default`
- `port <integer>`
- `port <string>`
- `login <string>`
- `password <string>`
- `account <string>`

# Interface
## Error handling
Functions which can fail return either `ok` or `{ok, Result}` on success, or
`{error, Reason}` on failure. Error reasons are described by the
`netrc:error_reason/0` type.

The `netrc:format_error_reason/1` returns a human readable description of an
error reason.

## Loading netrc files
The `netrc:load/1` function loads a netrc file and parses its content,
returning the list of entries it contains.

The `netrc:load/0` operates similarly, but finds the location of the netrc
file of the current user on its own. The default path is `$HOME/.netrc`. This
path can be overriden using the `NETRC` environment variable.

## Searching for matching entries
The `netrc:search/2` function executes a search query against a list of netrc
entries and returns entries matching the query.

A query is a map containing the following entries:
- `machine`: a machine name.
- `port`: a port number or standard IANA port name (optional).
- `login`: a login string (optional).
- `account`: an account string (optional).

A netrc entry matches a query if all fields of the query are equal to all
corresponding fields of the entry.

Example:
```erlang
{ok, Entries} = netrc:load().
netrc:search(#{machine => <<"example.com">>, login => <<"bob">>}, Entries).
```

The `netrc:search/1` function operates similarly but uses the entries stored
in the cache.

Example:
```erlang
netrc:search(#{machine => <<"example.com">>, login => <<"bob">>}).
```

# Cache
The `netrc` OTP application loads the default netrc file at startup if it
exists and store its entries.

Functions such as `netrc:search/1` use the cache, removing the need to load
entries manually.
