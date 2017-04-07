%=---------------------=%
% Parser	: parser
% --
% Johann Ostero ( P4220933 )
% Teesside University - Year 2
% ICA 1 - Networking and Concurrent Programming
% --
% This is a very basic module, that has two helper functions, for parsing messages
% when parsing the binaryToList, the value to parse against will always be ":"
%=---------------------=% 


-module(parser).

-define(m_seperator, ":").

-export([binaryToList/1, stringToIP/1]).

binaryToList(Bin) ->
	binary:split(Bin, <<":">>, [global]).

stringToIP(IP) ->
	string:tokens(IP, ".").
