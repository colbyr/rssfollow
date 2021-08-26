-module(twitter).
-export([get_user_by_username/1]).

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

