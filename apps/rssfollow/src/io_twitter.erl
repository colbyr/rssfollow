-module(io_twitter).
-export([
    get_profile_link/1,
    get_tweet_embed/3,
    get_tweet_link/2,
    format_tweet_date/1,
    format_tweet_title/1
]).

get_profile_link(UserName) ->
    ["https://twitter.com/", UserName].

format_tweet_date(#{<<"created_at">> := CreatedAt}) ->
    Date = ec_date:parse(binary_to_list(CreatedAt)),
    [ec_date:format("D, d M Y H:i:s \\G\\M\\T", Date)].

get_tweet_embed(
    UserName,
    Name,
    #{
        <<"lang">> := Lang,
        <<"text">> := Text,
        <<"created_at">> := Created
    } = Tweet
) ->
    [
        "<blockquote class=\"twitter-tweet\">",
        "<p lang=\"",
        Lang,
        "\">",
        Text,
        "</p>",
        "&mdash; ",
        Name,
        " (@",
        UserName,
        ")",
        "<a href=\"",
        get_tweet_link(UserName, Tweet),
        "\">",
        Created,
        "</a>",
        "</blockquote>",
        "<script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>"
    ].

get_tweet_link(UserName, #{<<"id">> := TweetId}) ->
    [get_profile_link(UserName), "/status/", TweetId].

format_tweet_title(#{<<"text">> := Text}) ->
    [Text].
