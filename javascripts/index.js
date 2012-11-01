require(['jquery', 'rss', 'timeline', 'jquery.caro.min'], function($, rss, timeline) {
    $(function() {
        var amount = 6;

        rss.widget($('.news'), ['https://www.facebook.com/feeds/page.php?format=rss20&id=272782769499260'], amount, amount);
        $('.details').caro({naviClass: 'navi', cycle: true});

        timeline.widget('timeline', 'https://www.facebook.com/feeds/page.php?format=rss20&id=272782769499260');
    });
});
