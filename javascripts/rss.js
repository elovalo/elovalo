define(['jquery', 'utils'], function($, utils) {
    function widget($parent, feeds, amount, postsPerFeed) {
        var parsedData = [];

        $.each(feeds, function(i, url) {
            parseRSS(url, function(data) {
                parsedData.push(data);

                if(parsedData.length == feeds.length) {
                    var entries = orderRSSEntries(parsedData, amount, postsPerFeed);

                    constructRSSUI($parent, entries);
                }
            });
        });
    }

    function parseRSS(url, callback) {
        $.ajax({
            url: document.location.protocol + '//ajax.googleapis.com/ajax/services/feed/load?v=1.0&num=10&callback=?&q=' + encodeURIComponent(url),
            dataType: 'json',
            success: function(data) {
                callback(data.responseData? data.responseData.feed: {author: null, entries: []});
            }
        });
    }

    function orderRSSEntries(data, amount, postsPerFeed) {
        postsPerFeed = postsPerFeed || 1;
        var entries = [];

        $.each(data, function(i, k) {
            var author = k.author;

            entries = entries.concat(k.entries.slice(0, postsPerFeed));

            $.each(entries, function(i, k) {
                k.author = k.author || author;
            });
        });

        $.each(entries, function(i, k) {
            k.publishedDate = new Date(k.publishedDate);
        });

        return utils.orderEntries(entries, amount);
    }

    function constructRSSUI($parent, entries) {
        var $dl = $('<dl>').appendTo($parent);

        $.each(entries, function(i, k) {
            $('<dt>').append('<span class="date">' + utils.ISODateString(k.publishedDate) + '</span>').
                append('<span class="author">' + k.author + '</span>').appendTo($dl);
            $('<dd>', {'class': 'news_item'}).attr('title', utils.htmlDecode(k.contentSnippet)).append('<a href="' + k.link + '"><span class="title">' + k.title + '</span></a>').appendTo($dl);
        });
    }

    return {
        widget: widget
    };
});
