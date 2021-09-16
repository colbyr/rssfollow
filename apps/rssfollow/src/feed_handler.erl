-module(feed_handler).
-export([init/2]).

rss_channel(Content) ->
    [
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
        xmerl:export_simple_content(
            [
                {rss,
                    [
                        {version, "2.0"},
                        {'xmlns:dc', "http://purl.org/dc/elements/1.1/"},
                        {'xmlns:content',
                            "http://purl.org/rss/1.0/modules/content/"},
                        {'xmlns:atom', "http://www.w3.org/2005/Atom"},
                        {'xmlns:media', "http://search.yahoo.com/mrss/"}
                    ],
                    [{channel, Content}]}
            ],
            xmerl_xml
        )
    ].

init(Req0, State) ->
    UserName = cowboy_req:binding(twitter_user, Req0),
    {ok, #{
        <<"id">> := UserId,
        <<"description">> := UserDescription,
        <<"name">> := Name,
        <<"profile_image_url">> := ProfileImage
    }} = twitter_user:get_by_username(UserName),
    {ok, Tweets} = twitter_tweets:get_by_user_id(UserId),

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
                {description, [
                    io_twitter:get_tweet_embed(UserName, Name, Tweet)
                ]},
                {'dc:creator', [["@", UserName]]},
                {guid, [{isPermaLink, "true"}], [
                    io_twitter:get_tweet_link(UserName, Tweet)
                ]},
                {pubDate, [io_twitter:format_tweet_date(Tweet)]}
            ]}
         || Tweet <- Tweets
        ]}
    ]),

    Req = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/xml; charset=utf-8">>},
        Body,
        Req0
    ),
    {ok, Req, State}.
