# Distributed Graph Algorithm Playground (DGAP)

An OTP application with a simple API to run graph algorithms. [A vscode extension is available for this tool](https://github.com/krygergo/dgap-vsc-ext)

Graph algorithms are compiled as erlang modules. These modules must be implemented with a start function with argument defined like:

```-type vertex() :: { Id :: term(), Pid :: pid() }```

```-type argument() :: {Vertex :: vertex(), Edges :: [Neighbour :: vertex()]}```

#### Example

    leader_election({{Id, _Pid}, [{_LeftId, _LeftPid}, {_RightId, RightPid}]})

Above argument definition for the leader_election algorithm is taken from the algorithm_util module. The algorithm_util module contains some predefined graph algorithms.

## VSCode Extension

[A vscode extension](https://github.com/krygergo/dgap-vsc-ext) is available for this tool. Download the released vsix file from the linked repository and run the following command in a vscode termnial.

    code --install-extension path/to/dgap-win32-x64-<version>.vsix

## Build

    $ rebar3 as windows release

## Run

    $ _build/windows/rel/dgap/bin/dgap.cmd ext start

## API

A table outlining the API, excluding arguments and return values as these details can be readily referenced in the source code.

| **Module**    | **Function**  | **Description**                                                         |
| ------------- | ------------  | ----------------------------------------------------------------------- |
| algorithm     | compile       | Compiles a file and loads it into an algorithm                          |
| simulation    | add           | Creates a new graph with specfied id                                    |
| simulation    | topology      | Adds a new topology to an existing graph                                |
| simulation    | start         | Tell the graph to run the specified algorithm on every vertex           |
| simulation    | stop          | Tell the graph to stop every vertex                                     |
| simulation    | kill          | Kills and removes the graph                                             |
| simulation    | remove_link   | Removes the link between two vertices                                   |
| simulation    | reinsert_link | Reinserts the link between two vertices                                 |
| topology      | ring          | Creates a ring topology                                                 |
| topology      | random        | Creates a random topology                                               |
| event_handler | read_message  | Blocking call for reading messages sent between vertices within a graph |
| event_handler | read_result   | Blocking call for reading results received from vertices within a graph |
| event_handler | read_log      | Blocking call for reading log messages from vertices within a graph     |
