-module(twitter_tweets).
-behavior(gen_server).
-export([
  init/1,
  handle_cast/2,
  handle_call/3,
  get_by_user_id/1,
  start_link/0
]).

% 1 minute (in microseconds)
-define(REFRESH, 60_000_000).

get_cache_pid() ->
  [{cache_pid, CachePid}] = ets:lookup(twitter_tweets_meta, cache_pid),
  CachePid.

start_link() ->
  gen_server:start_link(?MODULE, {}, []).

init(_Args) ->
  ets:new(twitter_tweets_meta, [set, named_table]),
  ets:insert(twitter_tweets_meta, {cache_pid, self()}),

  ets:new(twitter_tweets_by_id, [
    set,
    named_table
  ]),
  {ok, {}}.

handle_cast({set_cache, {TwitterUserId, User}}, State) ->
  ets:insert(twitter_tweets_by_id, {TwitterUserId, erlang:timestamp(),  User}),
  {noreply, State};
handle_cast(Msg, State) ->
  io:format("unknown msg: ~p~n", [Msg]),
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

set_cached_tweets(Entry) ->
  CachePid = get_cache_pid(),
  gen_server:cast(CachePid, {set_cache, Entry}).

get_cached_tweets(TwitterUserId) ->
  case ets:lookup(twitter_tweets_by_id, TwitterUserId) of
    [] -> {stale, []};
    [{_Name, LastRefresh, Tweets}] ->
      case timer:now_diff(erlang:timestamp(), LastRefresh) >= ?REFRESH of
        true -> {stale, Tweets};
        false -> {ok, Tweets}
      end
  end.

get_by_user_id(TwitterUserId) ->
  case get_cached_tweets(TwitterUserId) of
    {stale, []} ->
      {ok, Tweets} = twitter:get_tweets_by_user_id(TwitterUserId),
      set_cached_tweets({TwitterUserId, Tweets}),
      {ok, Tweets};

    {stale, StaleTweets} ->
      [#{ <<"id">> := MostRecentTweetId }|_] = StaleTweets,
      {ok, FreshTweets} = twitter:get_tweets_by_user_id_since(TwitterUserId, MostRecentTweetId),
      Tweets = lists:sublist(FreshTweets ++ StaleTweets, 10),
      set_cached_tweets({TwitterUserId, Tweets}),
      {ok, Tweets};

    {ok, CachedTweets} ->
      {ok, CachedTweets}
  end.

