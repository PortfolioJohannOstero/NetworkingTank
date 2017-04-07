%=---------------------=%
% Server TCP	: s_tcp
% --
% Johann Ostero ( P4220933 )
% Teesside University - Year 2
% ICA 1 - Networking and Concurrent Programming
% --
% This is a TCP module that allows for multi-connection, using multi-threading.
% It has very basic API's, such as starting the server and sending messages.
% It will spawn a listener, that then spawns a new acceptor for everytime it gets a connection,
%	creating a local message system between that TCP receive thread and the client
%=---------------------=% 

-module(s_tcp).

-export([start/1, sendTo/2]).
-include("records.hrl").

-define(m_tcp_options, [binary, {active, false}, {reuseaddr, true}]).

%% +==== API ====+ 
start(Port) ->
	spawn_link(fun() -> listen(Port) end).

sendTo(Client, Msg) ->
	gen_tcp:send(Client#r_client.r_tcp#r_tcp.socket, Msg).

%% +=== TCP Connection handling ===+ 
listen(Port) ->
	case gen_tcp:listen(Port, ?m_tcp_options) of
		{ok, LSocket} ->
			s_msg:message("TCP - Listening Established"),
			accept(LSocket);
		{error, Reason} ->
			s_msg:message("TCP - Listening Failed", Reason, error)
	end,
	s_msg:message("TCP - Listener Closed").
	
accept(LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			s_msg:message("TCP - Accept Established"),		
			spawn(fun() -> tcp_loop(Socket) end),			
			accept(LSocket);
		{error, Reason} ->
			s_msg:message("TCP - Accept Failed", Reason, error)
	end.
	
tcp_loop(Socket) -> 
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
			messageManager:parseTCP_BinaryMessage(Bin, Socket),
			tcp_loop(Socket);			

		{error, Reason} ->
			s_msg:message("TCP - Handler encountered an error", Reason, error),
			mr:forceRemoveSocket(Socket);
		
		CatchAll ->
			s_msg:message("TCP - Unknown message", CatchAll, warning),
			tcp_loop(Socket)
	end.
