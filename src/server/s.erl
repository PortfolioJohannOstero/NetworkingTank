%=---------------------=%
% Server	: s
% --
% Johann Ostero ( P4220933 )
% Teesside University - Year 2
% ICA 1 - Networking and Concurrent Programming
% --
% This is the server; It is responsible for handling every other component:
%	- TCP
%	- UDP
%	- UDP Broadcast
%	- MessageRouter
%=---------------------=% 

-module(s).

-define(m_server, s).

-define(SERVER_PORT, 2000).
-define(SERVER_BPORT, 6000).

-export([start/0, stop/0]).

% Creates a global name, to stop duplicated servers
start() ->
	global:trans({?m_server, ?m_server}, 
		fun() ->
			case global:whereis_name(?m_server) of
				undefined ->
					s_msg:clearConsole(),
					s_msg:message("Created on Port", integer_to_list(?SERVER_PORT), normal),
					Pid = spawn(fun() -> initServer() end),
					global:register_name(?m_server, Pid);
				Reason ->
					s_msg:message("Already created", Reason, error)
			end
		end).

% Closes the server
stop() ->
	global:trans({?m_server, ?m_server},
	fun() ->
		case global:whereis_name(?m_server) of
			undefined ->
				s_msg:message("Server not available!");
			_ ->
				s_msg:message("Turning off server!"),
				global:unregister_name(?m_server)
		end
	end),

	mr:stop(),
	s_udpBroadcast:stop().


initServer() ->
	%% Gets the computers local IP and the broadcast IP
	NetworkLabel = "eth0",
	{ok, [{addr, IP}]} = inet:ifget(NetworkLabel, [addr]),
	{ok, [{broadaddr, BroadIP}]} = inet:ifget(NetworkLabel, [broadaddr]),

	%% Prints out inormation
	s_msg:message("IP Address", inet_parse:ntoa(IP), normal),
	s_msg:message("Broadcast IP Address", inet_parse:ntoa(BroadIP), normal),
	s_msg:message("Broadcast Port", integer_to_list(?SERVER_BPORT), normal),
	s_msg:cleanMessage("------------------"),

	%% Starts all the protocols and the MessageRouter
	mr:start(),
	s_tcp:start(?SERVER_PORT),
	s_udp:start(?SERVER_PORT),
	s_udpBroadcast:start(BroadIP, ?SERVER_BPORT, list_to_binary(integer_to_list(?SERVER_PORT))),
	s_msg:cleanMessage("------------------").

		
