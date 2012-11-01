define(['jquery', 'utils', 'storyjs-embed'], function($, utils) {
    function widget(id, url) {
        var parsedData = [];

        parseRSS(url, function(data) {
            constructTimeline(id, convert(data));
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

    function convert(data) {
        console.log(data);

        data = {
            timeline: {
                headline: 'foobar',
                type: 'default',
                startDate: '2012',
                text: "<p>Intro body text goes here, some HTML is ok</p>p>",
                date: [
                    {
                        startDate: "2012,12,10",
                        endData: "2012,12,11",
                        headline: "Headline Goes Here",
                        text: "<p>Body text goes here, some HTML is OK</p>"
                    }
                ]
            }
        };

        console.log(data);

        return data;
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
