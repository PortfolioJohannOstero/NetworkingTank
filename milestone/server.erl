-module(server).

-export([start/1,
		 accept_loop/1]).

-define(Options, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]).

%% Clearing the screen
-define(CLEAR, "\e[H\e[2J").

%%%%%%%%%%%%%%%%
% ===Server=== %
%%%%%%%%%%%%%%%%

start(Port) ->
  {ok, Listener_Socket} = gen_tcp:listen(Port, ?Options),
  io:format(?CLEAR),
  io:format("Established server on: -~p-~n", [Port]),
  io:format("------------------------------~n"),
  io:format("Starting chat router...~n"),
  io:format("--~n"),
  register(router, spawn(client, router, [dict:new()])),
  accept_loop(Listener_Socket).

accept_loop(Listener_Socket) ->
  io:format("Server: Waiting for connections...~n", []),
  {ok, Socket} = gen_tcp:accept(Listener_Socket),
  io:format("Server: Connection accepted!~n", []),
  spawn(client, handler, [Socket]),
  accept_loop(Listener_Socket).

