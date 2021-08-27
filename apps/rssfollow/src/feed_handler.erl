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
  UserName = cowboy_req:binding(twitter_user, Req0),
  {ok,
   #{<<"id">> := UserId,
     <<"description">> := UserDescription,
     <<"name">> := Name,
     <<"profile_image_url">> := ProfileImage}
  } = twitter:get_user_by_username(UserName),
  {ok, Tweets} = twitter:get_tweets_by_user_id(UserId),

  Body = rss_channel([
    {title, [[Name, " (@", UserName, ")"]]},
    {description, [[UserDescription]]},
    {generator, [["rssfollow"]]},
    {link, [[io_twitter:get_profile_link(UserName)]]},
    {image, [
      {url, [[ProfileImage]]},
      {title, [[Name, " (@", UserName, ")"]]}
    ]},
    {items, [
      {item, [
        {title, [io_twitter:format_tweet_title(Tweet)]},
        {link, [io_twitter:get_tweet_link(UserName, Tweet)]},
        {description, [io_twitter:get_tweet_embed(UserName, Name, Tweet)]},
        {guid, [{isPermaLink, "true"}], [io_twitter:get_tweet_link(UserName, Tweet)]},
        {pubDate, [[CreatedAt]]}
      ]} || #{
        <<"created_at">> := CreatedAt
      } = Tweet <- Tweets
    ]}
  ]),

  Req = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/xml; charset=utf-8">>},
    Body,
    Req0
  ),
  {ok, Req, State}.

