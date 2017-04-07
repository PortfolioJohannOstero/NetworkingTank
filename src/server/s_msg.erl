%=---------------------=%
% Server Message	: s_msg
% --
% Johann Ostero ( P4220933 )
% Teesside University - Year 2
% ICA 1 - Networking and Concurrent Programming
% --
% This module handles all the server messages, allowing for
% server warnings, errors, clearing the console.
% It also allows for clean messages, meaning that it doesn't start with "Server:"
%=---------------------=% 

-module(s_msg).
-export([message/1, message/2, message/3, cleanMessage/1, 
		 clearConsole/0]).

-define(CLEAR, "\e[H\e[2J"). % "clears" the terminal

%+==== Server Messages ====+%
message(Msg) ->
	io:fwrite("Server: ~s~n", [Msg]).

message(Msg, MessageType) ->
	io:fwrite("~s ~s~n", [getMessageType(MessageType), Msg]).

message(Msg, AdditionalMessage, MessageType) ->
	io:fwrite("~s ~s: ~s~n", [getMessageType(MessageType), Msg, AdditionalMessage]).

cleanMessage(Msg) ->
	io:fwrite("~s~n", [Msg]).

clearConsole() ->
	io:fwrite(?CLEAR).


getMessageType(MessageType) ->
	case MessageType of
		warning ->
			"Server Warning:";
		error ->
			"Server Error:";
		_ ->
			"Server:"
	end.


