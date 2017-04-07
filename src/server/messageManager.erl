%=---------------------=%
% Message Manager	: messageManager
% --
% Johann Ostero ( P4220933 )
% Teesside University - Year 2
% ICA 1 - Networking and Concurrent Programming
% --
% This module is the middleman between the protocol messages and the message router
% It has two exported method, one for UDP and one for TCP.
% These methods parse the message and breaks into a list of values,
%	this is then used to determine what API to call in the messageRouter.
%=---------------------=% 


-module(messageManager).

-include("records.hrl").

-export([parseTCP_BinaryMessage/2, parseUDP_BinaryMessage/2]).

parseUDP_BinaryMessage(Bin, Socket) ->
	s_msg:message("Received", Bin, normal),
	case parser:binaryToList(Bin) of
		% +==== Sending ====+ %
		[<<"sendtoall">>, <<Name/binary>>, <<Msg/binary>>] ->
			mr:udp_sendToAll(Socket, Name, Msg);

		[<<"sendto">>, <<Name/binary>>, <<ReceiverName/binary>>, <<Msg/binary>>] ->
			mr:udp_sendTo(Socket, Name, ReceiverName, Msg);
		
		% +==== Updating ====+ %
			% updating the clients' position 
		[<<"set_pos">>, <<Name/binary>>, <<PosX/binary>>, <<PosY/binary>>] ->
			mr:setPosition(Name, PosX, PosY),
			mr:udp_sendToAll(Socket, Name, <<"update_pos:", Name/binary, ":", PosX/binary, ":", 
																			  PosY/binary>>);
			% updating the clients' rotation
		[<<"set_rot">>, <<Name/binary>>, <<Angle/binary>>] ->
			mr:setRotation(Name, Angle),
			mr:udp_sendToAll(Socket, Name, <<"update_rot:", Name/binary, ":", Angle/binary>>);

			% fires a projectile
		[<<"shoot">>, <<Name/binary>>, <<PosX/binary>>, <<PosY/binary>>, <<Angle/binary>>] ->
			mr:udp_sendToAll(Socket, Name, <<"shot:", Name/binary, ":", PosX/binary, ":", PosY/binary, ":", Angle/binary>>);

		CatchAll ->
			s_msg:message("Message Manager - Unknown UDP Message", CatchAll, warning)
	end.	

parseTCP_BinaryMessage(Bin, Socket) ->
	s_msg:message("Received", Bin, normal),
	case parser:binaryToList(Bin)  of
		% +==== Register / Unregister ====+ %
			% respawns the client
		[<<"respawn">>, <<Name/binary>>] ->
			mr:tcp_sendToAll(Name, <<"respawned:", Name/binary>>);

			% registering requires the local IP and the udp port to reduce the waiting time on both sides
		[<<"reg">>,  <<Name/binary>>, <<IP/binary>>, <<Port/binary>>, <<PosX/binary>>, <<PosY/binary>>] ->			
			NewClient = #r_client{name = Name, 
								  r_pos=#r_pos{posX = PosX, posY = PosY},
								  r_tcp=#r_tcp{socket = Socket},
								  r_udp=#r_udp{
											   address = getIPFromString(binary_to_list(IP)), % <-- converts the string to tuple
											   port = element(1, string:to_integer(binary_to_list(Port)))} % <- string to int
								 },
			mr:registerClient(NewClient);

		[<<"unreg">>, <<ClientName/binary>>] ->
			mr:unregisterClient(ClientName);

			% tells everyone that the current registered client is joining the game
		[<<"join">>, <<ClientName/binary>>, <<PosX/binary>>, <<PosY/binary>>, <<Angle/binary>>] ->
			mr:joinClient(ClientName, PosX, PosY, Angle),
			% tells everyone on the server about the client joining
			mr:tcp_sendToAll(ClientName, <<"join_client:", ClientName/binary,":",PosX/binary,":",PosY/binary,
																			 ":",Angle/binary>>),
			% sends everyone elses details to the newely joined client
			mr:tcp_collectAllClients(ClientName);

		% +==== Sending ====+ %
		[<<"sendto">>, <<Name/binary>>, <<ReceiverName/binary>>, <<Msg/binary>>] ->
			mr:tcp_sendTo(Name, ReceiverName, Msg); 

		[<<"sendtoall">>, <<Name/binary>>, <<Msg/binary>>] ->
			mr:tcp_sendToAll(Name, Msg);

		CatchAll ->
			s_msg:message("Message Manager - Unknown TCP Message", CatchAll, warning)
	end.

getIPFromString(IP) ->
	List = convertToInt(string:tokens(IP, ".")),
	list_to_tuple(List).

convertToInt([H|T]) ->
	[element(1, string:to_integer(H)) | convertToInt(T)];
convertToInt([]) ->
	[].
	

