%=---------------------=%
% Message Router	: mr
% --
% Johann Ostero ( P4220933 )
% Teesside University - Year 2
% ICA 1 - Networking and Concurrent Programming
% --
% This module handles routing the protocol messages and stores every client in the game.
% The possible routing ranges from a singular message to one specific person, to sending it to everyone,
% or have one client get all the clients sent to himself.
% the message router is the heart of the server.
% The Clients conists of records.
%=---------------------=% 


-module(mr).

-include("records.hrl").

-define(m_messageRouter, mr).

-export([start/0, stop/0,
		 registerClient/1, unregisterClient/1, forceRemoveSocket/1, 
		 udp_sendTo/4, tcp_sendTo/3,
		 tcp_sendToAll/2, udp_sendToAll/3,
		 joinClient/4, tcp_collectAllClients/1, 
		 setPosition/3, setRotation/2]).



%% +===== API =====+ %% 
start() ->
	global:trans({?m_messageRouter, ?m_messageRouter}, 
	fun() ->
		case global:whereis_name(?m_messageRouter) of
			undefined ->
				Pid = spawn(fun() -> messageRouter(dict:new()) end),
				global:register_name(?m_messageRouter, Pid),
				s_msg:message("Message Router Established (MR)");
			Reason ->
				s_msg:message("MR Already created", Reason, error)
		end
	end).

stop() ->
	global:trans({?m_messageRouter, ?m_messageRouter}, 
	fun() ->
		case global:whereis_name(?m_messageRouter) of
			undefined ->
				s_msg:message("MR not found", error);
			_ ->
				global:send(?m_messageRouter, close_router),
				global:unregister_name(?m_messageRouter),
				s_msg:message("MR Closed")
		end
	end).
	 

% +== Register
% Checks to see if the provided is of type record r_client
registerClient(Client) when is_record(Client, r_client) ->
	global:send(?m_messageRouter, {register_client, Client});

registerClient(_Client) ->
	s_msg:message("MR - ClientNameClient is not a client record!", warning).

% +== Unregister
unregisterClient(Name) ->
	global:send(?m_messageRouter, {unregister_client, Name}).

% Remove Client using Socket
forceRemoveSocket(Socket) -> 
	global:send(?m_messageRouter, {client_error, Socket}).

% +== send
tcp_sendToAll(SenderName, Msg) ->
	global:send(?m_messageRouter, {sendToAll_tcp, SenderName, Msg}).

udp_sendToAll(Socket, SenderName, Msg) ->
	global:send(?m_messageRouter, {sentToAll_udp, Socket, SenderName, Msg}).

tcp_sendTo(SenderName, ReceiverName, Msg) ->
	global:send(?m_messageRouter, {sendto_tcp, SenderName, ReceiverName, Msg}).

udp_sendTo(Socket, SenderName, ReceiverName, Msg) ->
	global:send(?m_messageRouter, {sendto_udp, Socket, SenderName, ReceiverName, Msg}).

% +== update
joinClient(ClientName, PosX, PosY, Angle) ->
	global:send(?m_messageRouter, {join_client, ClientName, PosX, PosY, Angle}).

tcp_collectAllClients(ClientName) ->
	global:send(?m_messageRouter, {collect_all_clients, ClientName}).

setPosition(Name, PosX, PosY) ->
	global:send(?m_messageRouter, {set_position, Name, PosX, PosY}).

setRotation(Name, Angle) ->
	global:send(?m_messageRouter, {set_rotation, Name, Angle}).


%% +==== Router ====+ %% 
messageRouter(Clients) ->
	receive
		close_router ->
			ok;
		%% +=== Register / Unregister ===+ % 
		{register_client, Client} ->
			messageRouter(regClient(Clients, Client));

		{unregister_client, Name} ->
			messageRouter(unregClient(Clients, Name));

		%% +=== Sending ===+ %%
		{sendto_tcp, SenderName, ReceiverName, Msg} ->
			sendTo(SenderName, ReceiverName, Msg, Clients, fun s_tcp:sendTo/2),
			messageRouter(Clients);

		{sendto_udp, Socket, SenderName, ReceiverName, Msg} ->
			sendTo(SenderName, ReceiverName, {Socket, Msg}, Clients, fun s_udp:sendTo/2),
			messageRouter(Clients);

		{sendToAll_tcp, SenderName, Msg} ->
			sendToAll(SenderName, Msg, Clients, fun s_tcp:sendTo/2),
			messageRouter(Clients);			

		{sentToAll_udp, Socket, SenderName, Msg} ->
			sendToAll(SenderName, {Socket, Msg}, Clients, fun s_udp:sendTo/2),
			messageRouter(Clients);

		%% +=== Updating ===+ %%
		{join_client, ClientName, PosX, PosY, Angle} ->
			messageRouter(clientJoinGame(ClientName, PosX, PosY, Angle, Clients));

		{collect_all_clients, ClientName} ->
			sendAllClientsToCurrentClient(ClientName, Clients),
			messageRouter(Clients);

		{set_position, Name, PosX, PosY} ->
			messageRouter(updatePosition(Name, Clients, PosX, PosY));
		
		{set_rotation, Name, Angle} ->
			messageRouter(updateRotation(Name, Clients, Angle));

		%% +=== Client Error ===+ %%
			%% if a client crashes or quits prematurely, the server will forcefully locate the Socket and remove them
		{client_error, Socket} ->
			dict:map(
				fun(K, V) ->
					if(V#r_client.r_tcp#r_tcp.socket == Socket) ->
						s_msg:message("Client removed", K, warning),
						sendToAll_notMonitored("removed_client:" ++ K, Clients, fun s_tcp:sendTo/2),
						messageRouter(dict:erase(K, Clients));
					true -> 
						ok
					end
				end,
				Clients);
			
		CatchAll ->
			s_msg:message("MR - Got unknown message", CatchAll, warning),
			messageRouter(Clients)
	end.


% +===== Register / Unregister Client =====+ %
regClient(Clients, NewClient) ->
	Name = NewClient#r_client.name, 	
	case dict:is_key(Name, Clients) of
		false ->
			s_msg:message("MR - Registered Client", Name, normal),
			s_msg:cleanMessage("¬"),
			s_tcp:sendTo(NewClient, "reg_success"),
			dict:store(Name, NewClient, Clients);
		true ->
			s_msg:message("MR - Already registered", Name, warning),
			s_msg:cleanMessage("¬"),
			s_tcp:sendTo(NewClient, "reg_failed"),
			Clients
	end.

unregClient(Clients, ClientName) ->
	case dict:is_key(ClientName, Clients) of
		true ->
			s_msg:message("MR - Unregistered Client", ClientName, normal),
			s_msg:cleanMessage("¬"),
			dict:erase(ClientName, Clients);
		false ->
			s_msg:message("MR - Client not found", warning),
			s_msg:cleanMessage("¬"),
			Clients
	end.

%% +===== Sending =====+ %%
%% +== Send to a specific client
sendTo(SenderName, ReceiverName, Msg, Clients, SendProtocol) ->
	case dict:is_key(SenderName, Clients) of
		false ->
			s_msg:message("MR - Sender is not registered", SenderName, normal),
			s_msg:cleanMessage("¬");
		true ->

			case dict:find(ReceiverName, Clients) of
				{ok, Receiver} ->
					SendProtocol(Receiver, Msg);
				error ->
					s_msg:message("MR - Receiver is not registered", ReceiverName, normal),
					s_msg:cleanMessage("¬")
			end
	end.

%% +== Send to all
sendToAll(SenderName, Msg, Clients, SendProtocol) ->
	case dict:is_key(SenderName, Clients) of
		true ->	
			dict:map(fun(K, V) ->
				if	K == SenderName ->
					ok;
				true ->
					s_msg:message("Sending to", K, normal),
					SendProtocol(V, Msg)
				end
		 	end, Clients);
		false ->
			s_msg:message("MR - Sender is not registered", SenderName, normal)
	end.

	%% not monitored, simply means that it will not check if the sender exists or not
sendToAll_notMonitored(Msg, Clients, SendProtocol) ->
	dict:map(fun(_, V) ->
				SendProtocol(V, Msg)
			 end, Clients).


%% +== Update data
clientJoinGame(Name, PosX, PosY, Angle, Clients) ->
	case dict:find(Name, Clients) of
		{ok, Client} ->
			s_msg:message("Client Joined the game", Name, normal),
			UpdatedClient = Client#r_client {
												r_pos=#r_pos{posX = PosX, posY = PosY},
												rotation = Angle
											},
			dict:store(Name, UpdatedClient, Clients);
		error ->
			s_msg:message("MR - Sender is not registered", Name, normal),
			Clients
	end.

sendAllClientsToCurrentClient(ClientName, Clients) ->
	case dict:find(ClientName, Clients) of
		{ok, Client} ->
			%% iterates over every client
			dict:map(fun(K, V) ->
				if	K == ClientName -> %% <-- doesn't send a message to the client sending the message
					ok;
				true ->
					PosX = V#r_client.r_pos#r_pos.posX,
					PosY = V#r_client.r_pos#r_pos.posY,
					Angle = V#r_client.rotation,
					s_tcp:sendTo(Client, <<"join_client:", K/binary, ":", PosX/binary, ":", PosY/binary, ":", Angle/binary>>)
				end
		 	end, Clients);
		error ->
			s_msg:message("MR - Sender is not registered", ClientName, normal)
	end.


updatePosition(Name, Clients, PosX, PosY) ->
	case dict:find(Name, Clients) of
		{ok, Client} ->
			UpdatedClient = Client#r_client{r_pos=#r_pos{posX = PosX, posY = PosY}},
			dict:store(Name, UpdatedClient, Clients);
		error ->
			s_msg:message("MR - Sender is not registered", Name, normal),
			Clients
	end.

updateRotation(Name, Clients, Angle) ->
	case dict:find(Name, Clients) of
		{ok, Client} ->
			UpdatedClient = Client#r_client{rotation = Angle},
			dict:store(Name, UpdatedClient, Clients);
		error ->
			s_msg:message("MR - Sender is not registered", Name, normal),
			Clients
	end.



