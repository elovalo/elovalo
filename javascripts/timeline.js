define(['jquery', 'utils', 'storyjs-embed'], function($, utils) {
    function widget(id, url) {
        var parsedData = [];

        parseRSS(url, function(data) {
            constructTimeline(id, convert(data));
        });
    }

    function parseRSS(url, callback) {
        $.ajax({
            url: document.location.protocol + '//ajax.googleapis.com/ajax/services/feed/load?v=1.0&num=100000&callback=?&q=' + encodeURIComponent(url),
            dataType: 'json',
            success: function(data) {
                callback(data.responseData? data.responseData.feed: {author: null, entries: []});
            }
        });
    }

    function convert(data) {
        var dates = data.entries.map(function(v) {
            var date = new Date(v.publishedDate);
            var d = date.getFullYear() + ',' + (date.getMonth() + 1) + ',' + date.getDate();

            return {
                startDate: d,
                endData: d,
                headline: v.title,
                text: v.content
            };
        });

        return {
            timeline: {
                //headline: 'Elovalo',
                type: 'default',
                startDate: '2012',
                //text: "<p>Intro body text goes here, some HTML is ok</p>p>",
                date: dates
            }
        };
    }

    function constructTimeline(id, data) {
        createStoryJS({
            type: 'timeline',
            width: '100%',
            height: '600',
            source: data,
            embed_id: id,
            start_at_end: true,
            debug: true
        });
    }

    return {
        widget: widget
    };
});
