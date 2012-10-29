require(['jquery', 'rss', 'jquery.caro.min'], function($, rss) {
    $(function() {
        var amount = 6;

        rss.widget($('.news'), ['https://www.facebook.com/feeds/page.php?format=rss20&id=272782769499260'], amount, amount);
        $('.details').caro({naviClass: 'navi', cycle: true});
    });
});
