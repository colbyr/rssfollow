-module(twitter).
-export([
    get_tweets_by_user_id/1,
    get_tweets_by_user_id_since/2,
    get_user_by_username/1
]).

-define(TWITTER_API, "https://api.twitter.com/2").

fetch_json(Path) ->
    Url = [?TWITTER_API, Path],
    {ok, Token} = rssfollow_app:get_app_env(twitter_bearer_token),
    Headers = [
        {<<"Authorization">>, [<<"Bearer ">>, Token]}
    ],
    {ok, _, _, ClientRef} = hackney:get(Url, Headers),
    {ok, RespBodyString} = hackney:body(ClientRef),
    {ok, jiffy:decode(RespBodyString, [return_maps])}.

get_user_by_username(TwitterUserName) ->
    {ok, #{<<"data">> := [User]}} = fetch_json([
        "/users/by?",
        "usernames=",
        TwitterUserName,
        "&user.fields=description,protected,profile_image_url"
    ]),
    {ok, User}.

get_tweets_by_user_id(UserId) when is_integer(UserId) ->
    get_tweets_by_user_id(io_lib:format("~p", [UserId]));
get_tweets_by_user_id(UserIdStr) ->
    {ok, #{<<"data">> := Tweets}} = fetch_json([
        "/users/",
        UserIdStr,
        "/tweets",
        "?tweet.fields=created_at,lang",
        "&exclude=replies",
        "&max_results=10"
    ]),
    {ok, Tweets}.

get_tweets_by_user_id_since(UserId, SinceTweetId) when is_integer(UserId) ->
    get_tweets_by_user_id_since(io_lib:format("~p", [UserId]), SinceTweetId);
get_tweets_by_user_id_since(UserIdStr, SinceTweetId) ->
    {ok, Result} = fetch_json([
        "/users/",
        UserIdStr,
        "/tweets",
        "?tweet.fields=created_at,lang",
        "&exclude=replies",
        "&max_results=10",
        "&since_id=",
        SinceTweetId
    ]),
    case Result of
        #{<<"data">> := Tweets} ->
            {ok, Tweets};
        #{<<"meta">> := #{<<"result_count">> := 0}} ->
            {ok, []}
    end.
