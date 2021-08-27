-module(twitter).
-export([
  get_profile_link/1,
  get_tweet_link/2,
  get_tweets_by_user_id/1,
  get_user_by_username/1
]).

-define(TWITTER_API, "https://api.twitter.com/2").

fetch_json(Path) ->
  Url = [?TWITTER_API, Path],
  Headers = [
    {<<"Authorization">>, [<<"Bearer ">>, os:getenv("TWITTER_BEARER_TOKEN")]}
  ],
  {ok, _, _, ClientRef} = hackney:get(Url, Headers),
  {ok, RespBodyString} = hackney:body(ClientRef),
  {ok, jiffy:decode(RespBodyString, [return_maps])}.

get_user_by_username(TwitterUserName) ->
  {ok, #{<<"data">> := [User]}} = fetch_json([
    "/users/by?",
    "usernames=", TwitterUserName,
    "&user.fields=description,protected"
  ]),
  {ok, User}.

get_tweets_by_user_id(UserId) when is_integer(UserId) ->
  get_tweets_by_user_id(io_lib:format("~p", [UserId]));

get_tweets_by_user_id(UserIdStr) ->
  {ok, #{<<"data">> := Tweets}} = fetch_json([
    "/users/", UserIdStr, "/tweets",
    "?tweet.fields=created_at",
    "&exclude=replies",
    "&max_results=10"
  ]),
  {ok, Tweets}.

get_profile_link(UserName) ->
  ["https://twitter.com/", UserName].

get_tweet_link(UserName, TweetId) ->
  [get_profile_link(UserName), "/status/", TweetId].

