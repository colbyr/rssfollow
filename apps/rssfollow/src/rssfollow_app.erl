%%%-------------------------------------------------------------------
%% @doc rssfollow public API
%% @end
%%%-------------------------------------------------------------------

-module(rssfollow_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
        {'_', [
          {"/", index_handler, []},
          {"/favicon.ico",
           cowboy_static,
           {priv_file, rssfollow, "static/assets/icon.png"}},
          {"/:twitter_user/feed.rss", feed_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(rssfollow_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    rssfollow_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
