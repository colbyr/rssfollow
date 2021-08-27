-module(feed_handler).
-export([init/2]).

rss_channel(Content) ->
  xmerl:export_simple_content(
    [{rss,
      [{version, "2.0"}],
      [{channel, Content}]}],
    xmerl_xml
  ).

init(Req0, State) ->
  TwitterUserName = cowboy_req:binding(twitter_user, Req0),
  {ok,
   #{<<"id">> := UserId,
     <<"description">> := UserDescription,
     <<"name">> := Name}
  } = twitter:get_user_by_username(TwitterUserName),
  {ok, Tweets} = twitter:get_tweets_by_user_id(UserId),
  Body = rss_channel([
    {title, [[Name, " (@", TwitterUserName, ")"]]},
    {description, [[UserDescription]]},
    {items, [
      {title, [[Text]]} || #{<<"text">> := Text} <- Tweets
    ]}
  ]),
  Req = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/xml; charset=utf-8">>},
    Body,
    Req0
  ),
  {ok, Req, State}.

