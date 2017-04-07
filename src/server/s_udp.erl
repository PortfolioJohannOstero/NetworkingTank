%=---------------------=%
% Server UDP	: s_udp
% --
% Johann Ostero ( P4220933 )
% Teesside University - Year 2
% ICA 1 - Networking and Concurrent Programming
% --
% This is a UDP module that allows for a basic receiver that constantly runs on its own thread.
% When it receives a message, it will send it on to the messageManager.
% It has very basic API's, such as starting the server and sending messages.
%=---------------------=% 

-module(s_udp).

-export([start/1, close/1, sendTo/2]).

-include("records.hrl").

-define(m_tcp_options, [binary, {active, false}, {reuseaddr, true}]).

%% +======= API =======+ %%
start(Port) ->
	spawn_link(fun() -> open(Port) end).

close(Socket) ->
	gen_udp:close(Socket),
	s_msg:message("UDP Connection Closed").

sendTo(Client, Data) ->
	{Socket, Msg} = Data,
	gen_udp:send(Socket, Client#r_client.r_udp#r_udp.address, 
						 Client#r_client.r_udp#r_udp.port,
						 Msg).



%% +======= UDP methods =======+ %%
open(Port) ->
	case gen_udp:open(Port, ?m_tcp_options) of
		{ok, Socket} ->
			s_msg:message("UDP - Opened Established"),
			udp_loop(Socket);
		{error, Reason} ->
			s_msg:message("UDP - Failed to Open", Reason, error)
	end.
	
udp_loop(Socket) ->
	case gen_udp:recv(Socket, 0) of	
		{ok, {_, _, Bin}} ->
			messageManager:parseUDP_BinaryMessage(Bin, Socket),
			udp_loop(Socket);
		
		{error, Reason} ->
			s_msg:message("UDP - Handler encountered an error", Reason, error);

		CatchAll ->
			s_msg:message("UDP - Unknown message", CatchAll, warning),
			udp_loop(Socket)
	end.


