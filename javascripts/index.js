require(['jquery', 'rss'], function($, rss) {
    $(function() {
        rss.widget($('#news'), ['https://www.facebook.com/feeds/page.php?format=rss20&id=272782769499260'], 5, 5);
    });
});
