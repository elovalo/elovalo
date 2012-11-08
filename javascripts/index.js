require(['jquery', 'timeline', 'jquery.caro.min'], function($, timeline) {
    $(function() {
        var amount = 6;

        $('.details').caro({naviClass: 'navi', cycle: true});

        timeline.widget('timeline', 'https://www.facebook.com/feeds/page.php?format=rss20&id=272782769499260');
    });
});
