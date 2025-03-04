-module(chat_db).

%% API
-export([init_db/0, register_user/2, validate_user/2, store_message/3, get_messages/0]).

init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(users, [{attributes, [username, password]}, {disc_copies, [node()]}]),
    mnesia:create_table(messages, [{attributes, [sender, text, timestamp]}, {disc_copies, [node()]}]).

register_user(Username, Password) ->
    mnesia:transaction(fun() -> mnesia:write({users, Username, Password}) end).

validate_user(Username, Password) ->
    F = fun() -> case mnesia:read({users, Username}) of
                     [{users, Username, Password}] -> true;
                     _ -> false
                 end end,
    mnesia:transaction(F).

store_message(Sender, Text, Timestamp) ->
    mnesia:transaction(fun() -> mnesia:write({messages, Sender, Text, Timestamp}) end).

get_messages() ->
    F = fun() -> mnesia:select(messages, [{'_', [], ['$_']}]) end,
    {atomic, Messages} = mnesia:transaction(F),
    {ok, Messages}.