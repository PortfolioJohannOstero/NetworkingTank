-module(client).

-export([handler/1,
		removeLastCharacter/1,
		start/1,
		printer/1,
		client_update/1,
		router/0, router/1,
	 	greet/1, send/2]).

%% Clearing the screen
-define(CLEAR, "\e[H\e[2J").

-define(Options, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]).


% Chat Router %
%%%%%%%%%%%%%%%
router() ->
  router([]).

router(Clients) ->
  receive
    {register, Socket, Alias} ->
		case dict:find(Alias, Clients) of
			{ok, _} ->
				%io:format("~p already registered!~n", [Alias]),
				%gen_tcp:send(Socket, term_to_binary(":User already registered!")),
				router(Clients);
			error ->
				io:format("~p registered!~n", [Alias]),
				gen_tcp:send(Socket, term_to_binary(":You have been registered!")),
				F = fun(_Alias, Socket) -> send(Socket, "Player '" ++ Alias ++ "' has joined") end,
				dict:map(F, Clients),
				router(dict:store(Alias, Socket, Clients))
		end;

	{unregister, Socket, Alias} ->
		case dict:find(Alias, Clients) of
			{ok, Socket} ->
				io:format("~p unregistered~n", [Alias]),
				gen_tcp:send(Socket, term_to_binary("You have been unregistered!")),
				router(dict:erase(Alias, Clients));
			error ->
				io:format("~p is not registered~n!", [Alias]),
				gen_tcp:send(Socket, term_to_binary("Invalid user! not registered!")),
				router(Clients)
		end;

	{send, Alias, Message} ->
		case dict:find(Alias, Clients) of
			{ok, Socket} ->
				gen_tcp:send(Socket, term_to_binary(Message));
			error ->
				ok
		end,
		router(Clients)
  end.

greet(Socket) ->
  gen_tcp:send(Socket, term_to_binary("greetings")).

send(Socket, Message) ->
 gen_tcp:send(Socket, term_to_binary(Message)).

% Client Handler %
%%%%%%%%%%%%%%%%%%
handler(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Bin} ->
      Cmd = binary_to_term(Bin),
      io:format("Command '~p' received.~n", [Cmd]),
	  case Cmd of

        {register, Alias} ->
			router ! {register, Socket, Alias};

		{unregister, Alias} ->
			router ! {unregister, Socket, Alias};

		{send, Data} ->
			NumberOfColons = string:chr(Data, $:),
			if NumberOfColons =/= 0 ->
				io:format("Direct Message found!~n"),
				[Alias | Message] = string:tokens(Data, ":"),
				FormattedMessage = string:join(Message, ":"),
				io:format("Message sent to: ~p and message is ~p~n", [Alias, FormattedMessage]),					
				router ! {send, Alias, FormattedMessage};
			true ->
				router ! {send, Data}
			end;

          _ ->
            io:format("Unsupported command.~n", [])
      end,
      handler(Socket);

    {error, _} ->
      io:format("Lost connection to client!~n", [])
  end.


% Chat Client %
%%%%%%%%%%%%%%%
removeLastCharacter(L) -> %% The list is reversed in order to remove the new line character, and then reverses it back.
  lists:reverse(tl(lists:reverse(L))). 

start(Port) ->
  {ok, Socket} = gen_tcp:connect("localhost", Port, ?Options),
  io:format(?CLEAR),
  io:format("Client connected to chat server! -~p-~n", [Port]),
  io:format("---------------------------------------~n"),
  io:format("Please register, by typing 'register' followed by your name!~n--~n"),
  spawn_link(client, printer, [Socket]),
  client_update(Socket).

printer(Socket) ->
  {ok, Bin} = gen_tcp:recv(Socket, 0),
  io:format("Received ~p.~n", [binary_to_term(Bin)]),
  printer(Socket).

client_update(Socket) ->
  Cmd = io:get_line(": "),
  [ExtractedCmd | Data] = string:tokens(removeLastCharacter(Cmd), " "),
  Atom = list_to_atom(ExtractedCmd),
  MessageData = string:join(Data, " "),
  gen_tcp:send(Socket, term_to_binary({Atom, MessageData})),
  client_update(Socket).
