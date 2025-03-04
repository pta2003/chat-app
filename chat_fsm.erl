-module(chat_fsm).
-behaviour(gen_statem).

%% API
-export([start_link/0, login/2, logout/1, send_message/2, get_online_users/0, add_user/1, remove_user/1]).

%% Callbacks
-export([init/1, callback_mode/0, handle_event/4, code_change/4]).

%%% API FUNCTIONS
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

login(Username, Password) ->
    gen_statem:call(?MODULE, {login, Username, Password}).

logout(Username) ->
    gen_statem:call(?MODULE, {logout, Username}).

send_message(Username, Message) ->
    gen_statem:cast(?MODULE, {send_message, Username, Message}).

get_online_users() ->
    gen_statem:call(?MODULE, get_users).

add_user(Username) ->
    gen_statem:cast(?MODULE, {add_user, Username}).

remove_user(Username) ->
    gen_statem:cast(?MODULE, {remove_user, Username}).

%%% CALLBACKS

%% init/1: Khởi tạo trạng thái ban đầu
init([]) ->
    {ok, idle, #{users => #{}, online => []}}.

%% callback_mode/0: Sử dụng handle_event_function mode
callback_mode() ->
    handle_event_function.

%% handle_event/4: Xử lý các sự kiện
%% Trạng thái IDLE: chỉ xử lý yêu cầu login
handle_event({call, From}, {login, Username, Password}, idle, State) ->
    case chat_db:validate_user(Username, Password) of
        {atomic, true} ->  %% Sửa lỗi: xử lý cả trường hợp `{atomic, true}`
            OnlineUsers = lists:usort([Username | maps:get(online, State, [])]),
            NewState = State#{online => OnlineUsers},
            {next_state, active, NewState, [{reply, From, {ok, "Login successful"}}]};
        false ->
            {keep_state, State, [{reply, From, {error, "Invalid username or password"}}]};
        _ ->
            {keep_state, State, [{reply, From, {error, "Database error"}}]}
    end;
%% Nếu nhận các event khác ở trạng thái idle, giữ nguyên trạng thái
handle_event({call, From}, _Request, idle, State) ->
    {keep_state, State, [{reply, From, ok}]};
handle_event({cast, _}, _From, idle, State) ->
    {keep_state, State};
    
%% Trạng thái ACTIVE: xử lý logout và get_users qua call
handle_event({call, From}, {logout, Username}, active, State) ->
    OnlineUsers = lists:delete(Username, maps:get(online, State, [])),
    NewState = State#{online => OnlineUsers},
    NextState = case OnlineUsers of
                    [] -> idle;
                    _ -> active
                end,
    {next_state, NextState, NewState, [{reply, From, {ok, "Logged out"}}]};
handle_event({call, From}, get_users, active, State) ->
    {keep_state, State, [{reply, From, {ok, maps:get(online, State, [])}}]};
handle_event({call, From}, _Other, active, State) ->
    {keep_state, State, [{reply, From, ok}]};
    
%% Trạng thái ACTIVE: xử lý các event cast
handle_event({cast, {send_message, Username, Message}}, _From, active, State) ->
    chat_db:store_message(Username, Message, calendar:local_time()),
    {keep_state, State};
handle_event({cast, {add_user, Username}}, _From, _StateName, State) ->
    NewState = State#{online => lists:usort([Username | maps:get(online, State, [])])},
    {keep_state, NewState};
handle_event({cast, {remove_user, Username}}, _From, _StateName, State) ->
    OnlineUsers = lists:delete(Username, maps:get(online, State, [])),
    NextState = case OnlineUsers of
                    [] -> idle;
                    _ -> active
                end,
    NewState = State#{online => OnlineUsers},
    {next_state, NextState, NewState};
    
%% Xử lý mặc định: giữ nguyên trạng thái
handle_event(_EventType, _EventContent, _StateName, State) ->
    {keep_state, State}.

%% code_change/4: Hỗ trợ chuyển đổi phiên bản mã
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
