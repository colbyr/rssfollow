%%%-------------------------------------------------------------------
%% @doc rssfollow top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rssfollow_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
      {twitter_user_cache,
          {twitter_user, start_link, []},
          permanent, 5000, worker, [twitter_user]},
      {twitter_tweet_cache,
          {twitter_tweets, start_link, []},
          permanent, 5000, worker, [twitter_tweets]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
