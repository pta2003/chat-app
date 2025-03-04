-module(chat_server).
-behaviour(gen_server).

%% API
-export([start_link/0, broadcast/2, list_users/0]).

%% Callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).

%% TCP API
-export([start_tcp_listener/0, accept_loop/1, handle_client/1]).

%%% API FUNCTIONS
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

broadcast(Username, Message) ->
    gen_server:cast(?MODULE, {broadcast, Username, Message}).

list_users() ->
    gen_server:call(?MODULE, list_users).

%%% INIT FUNCTION
init([]) ->
    chat_fsm:start_link(),
    chat_db:init_db(),
    spawn(fun start_tcp_listener/0),
    {ok, #{clients => #{}}}. %% clients = #{Username => Socket}

%%% TCP LISTENER
start_tcp_listener() ->
    {ok, ListenSocket} = gen_tcp:listen(4000, [binary, {packet, line}, {reuseaddr, true}, {active, false}]),
    io:format("[Server] Listening on port 4000...~n"),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(ClientSocket) end),
    accept_loop(ListenSocket).

%%% HANDLE CLIENT REQUESTS
handle_client(Socket) ->
    gen_tcp:send(Socket, "Welcome to chat server! Commands: register <username> <password> | login <username> <password> | send <message> | logout | list_users\n"),
    client_loop(Socket, undefined).

client_loop(Socket, Username) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {NewUsername, Response} = process_command(string:trim(Data), Socket, Username),
            gen_tcp:send(Socket, Response ++ "\n"),
            client_loop(Socket, NewUsername);
        {error, closed} ->
            case Username of
                undefined -> ok;
                _ -> gen_server:cast(?MODULE, {logout, Username})
            end,
            io:format("[Server] Client disconnected: ~p~n", [Username])
    end.

%%% PROCESS COMMANDS
process_command("exit", _Socket, Username) ->
    case Username of
        undefined -> {undefined, "Goodbye!"};
        _ -> gen_server:cast(?MODULE, {logout, Username}), {undefined, "Logged out!"}
    end;
process_command(Command, Socket, Username) ->
    Split = re:split(string:trim(Command), "\\s+", [{return, list}]), % Loại bỏ khoảng trắng thừa
    io:format("DEBUG: Command split = ~p~n", [Split]), % In ra để debug
    case Split of
        ["register", U, P] ->
            case chat_db:register_user(U, P) of
                {atomic, ok} -> {undefined, "Registered successfully!"};
                _ -> {undefined, "Registration failed!"}
            end;
        ["login", U, P] ->
            case chat_fsm:login(U, P) of
                {ok, _} ->
                    gen_server:cast(?MODULE, {login, U, Socket}),
                    {U, "Login successful!"};
                {error, _} -> {undefined, "Invalid login!"}
            end;
        ["send" | MsgList] when Username =/= undefined ->
            Msg = string:join(MsgList, " "),
            chat_fsm:send_message(Username, Msg),
            gen_server:cast(?MODULE, {broadcast, Username, Msg}),
            {Username, "Message sent!"};
        ["list_users"] when Username =/= undefined ->
            {ok, Users} = chat_fsm:get_online_users(),
            {Username, io_lib:format("Online users: ~p", [Users])};
        ["logout"] when Username =/= undefined ->
            gen_server:cast(?MODULE, {logout, Username}),
            {undefined, "Logged out!"};
        _ ->
            {Username, "Invalid command!"}
    end.

%%% SERVER EVENTS
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(list_users, _From, State) ->
    Clients = maps:keys(maps:get(clients, State, #{})),
    {reply, {ok, Clients}, State};
handle_call(_Request, _From, State) -> 
    {reply, ok, State}.

handle_cast({login, Username, Socket}, State) ->
    io:format("[Server] ~p joined the chat~n", [Username]),
    chat_fsm:add_user(Username),
    {noreply, State#{clients => maps:put(Username, Socket, maps:get(clients, State, #{}))}};
handle_cast({logout, Username}, State) ->
    io:format("[Server] ~p left the chat~n", [Username]),
    chat_fsm:remove_user(Username),
    {noreply, State#{clients => maps:remove(Username, maps:get(clients, State, #{}))}};
handle_cast({broadcast, Username, Msg}, State) ->
    Clients = maps:get(clients, State, #{}),
    lists:foreach(
        fun({_, Socket}) -> gen_tcp:send(Socket, io_lib:format("[~p]: ~p\n", [Username, Msg])) end,
        maps:to_list(Clients)
    ),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
