-module(chat_client).
-export([start/0]).

%%% API
start() ->
    io:format("Enter server IP (default: localhost): "),
    IP = string:trim(io:get_line("")),
    RealIP = case IP of "" -> "localhost"; _ -> IP end,
    
    case gen_tcp:connect(RealIP, 4000, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            spawn(fun() -> listen_server(Socket) end),
            io:format("Connected to server at ~s:4000\n", [RealIP]),
            client_loop(Socket);
        {error, Reason} ->
            io:format("Failed to connect: ~p\n", [Reason])
    end.

%%% LISTEN FOR SERVER MESSAGES
listen_server(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("~s", [Data]),
            listen_server(Socket);
        {error, closed} ->
            io:format("Disconnected from server\n"),
            halt()
    end.

%%% USER INPUT LOOP
client_loop(Socket) ->
    io:format("> "),
    Input = string:trim(io:get_line("")),
    case Input of
        "exit" ->
            io:format("Exiting chat client...\n"),
            gen_tcp:close(Socket),
            halt();
        _ ->
            gen_tcp:send(Socket, Input ++ "\n"),
            client_loop(Socket)
    end.
