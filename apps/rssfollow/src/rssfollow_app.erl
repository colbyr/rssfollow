%%%-------------------------------------------------------------------
%% @doc rssfollow public API
%% @end
%%%-------------------------------------------------------------------

-module(rssfollow_app).

-behaviour(application).

-export([
    get_app_env/1,
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    {ok, Port} = get_app_env(port),
    io:format("rssfollow started on port ~p~n", [Port]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, rssfollow, "static/index.html"}},
            {"/favicon.ico", cowboy_static,
                {priv_file, rssfollow, "static/assets/icon.png"}},
            {"/:twitter_user/tweets.rss", feed_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        rssfollow_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    rssfollow_sup:start_link().

stop(_State) ->
    ok.

%% @doc return a config value
get_app_env(Key) ->
    case application:get_env(rssfollow, Key) of
        {ok, Val} -> {ok, Val};
        undefined -> erlang:error(missing_env_value, [Key])
    end.
