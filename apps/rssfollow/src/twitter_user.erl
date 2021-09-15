-module(twitter_user).
-behavior(gen_server).
-export([
  init/1,
  handle_cast/2,
  handle_call/3,
  get_by_username/1,
  start_link/0
]).

get_cache_pid() ->
  [{cache_pid, CachePid}] = ets:lookup(twitter_user_meta, cache_pid),
  CachePid.

start_link() ->
  gen_server:start_link(?MODULE, {}, []).

init(_Args) ->
  ets:new(twitter_user_meta, [set, named_table]),
  ets:insert(twitter_user_meta, {cache_pid, self()}),

  ets:new(twitter_user_by_username, [
    set,
    named_table
  ]),
  {ok, {}}.

handle_cast({set_cache, {TwitterUserName, User}}, State) ->
  ets:insert(twitter_user_by_username, {TwitterUserName,  User}),
  {noreply, State};
handle_cast(Msg, State) ->
  io:format("unknown msg: ~p~n", [Msg]),
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

set_cached_user(Entry) ->
  CachePid = get_cache_pid(),
  gen_server:cast(CachePid, {set_cache, Entry}).

get_cached_user(TwitterUserName) ->
  case ets:lookup(twitter_user_by_username, TwitterUserName) of
    [] -> undefined;
    [{_Name, User}] -> User
  end.


get_by_username(TwitterUserName) ->
  case get_cached_user(TwitterUserName) of
    undefined ->
      {ok, User} = twitter:get_user_by_username(TwitterUserName),
      set_cached_user({TwitterUserName, User}),
      {ok, User};

    User ->
      {ok, User}
  end.

