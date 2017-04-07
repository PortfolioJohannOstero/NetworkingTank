%=---------------------=%
% Server UDP Broadcast	: s_udpBroadcast
% --
% Johann Ostero ( P4220933 )
% Teesside University - Year 2
% ICA 1 - Networking and Concurrent Programming
% --
% This module is a very basic udp broadcast setup, that only sends messages.
% The message being sent is the servers local ip and server port, using the broadcast ip/port
% It will send out a message every 1 second
%=---------------------=% 


-module(s_udpBroadcast).

-define(m_broadcast, s_udpBroadcast). 

-export([start/3, stop/0]).

start(BroadcastIp, Port, Msg) ->
	case gen_udp:open(0, [binary, {broadcast, true}]) of
		{ok, Socket} ->
			global:trans({?m_broadcast, ?m_broadcast}, 
				fun() ->
					case global:whereis_name(?m_broadcast) of
						undefined ->
							s_msg:message("Broadcast Established"),
							Pid = spawn(fun() -> broadcaster(Socket, BroadcastIp, Port, Msg) end),
							gen_udp:controlling_process(Socket, Pid),
							global:register_name(?m_broadcast, Pid);
						Reason ->
							s_msg:message("Broadcast failed to establish", Reason, error)
					end
				end);
		{error, Reason} ->
			s_msg:message("Broadcast failed to establish", Reason, error)
	end.	

stop() ->
	global:trans({?m_broadcast, ?m_broadcast}, 
	fun() ->
		case global:whereis_name(?m_broadcast) of
			undefined ->
				s_msg:message("Broadcaster not found", error);
			_ ->
				global:send(?m_broadcast, close),
				s_msg:message("Broadcasting closed!"),
				global:unregister_name(?m_broadcast)
		end
	end).

broadcaster(Socket, BroadcastIp, Port, Msg) ->
	receive
		close ->
			ok;
		_ ->
			broadcaster(Socket, BroadcastIp, Port, Msg)
	after 1000 ->
		gen_udp:send(Socket, BroadcastIp, Port, Msg),
		broadcaster(Socket, BroadcastIp, Port, Msg)
	end.


