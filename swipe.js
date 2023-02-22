window.addEventListener('load', function () {
    document.querySelectorAll('.section-wrapper').forEach(function (x) {
        SwipeListener(x);
        var act = function (cur, other, otherClass) {
            if (other && other.classList.contains('section-wrapper')) {
                setTimeout(function () {
                    window.location.hash = other.querySelector('a').getAttribute('href');
                    cur.classList.remove('animate', 'animateLeft', 'animateRight');
                    other.classList.add('animate', otherClass);
                }, 500);
                setTimeout(function () {
                    other.classList.remove('animate', otherClass);
                }, 1000);
            }
        };
        x.addEventListener('swipe', function (e) {
            if (document.documentElement.classList.contains('highlight')) {
                var cur = e.target.closest('.section-wrapper');
                var other;
                var otherClass;
                if (e.detail.directions.right) {
                    other = cur.previousSibling;
                    if (other) {
                        cur.classList.add('animate', 'animateRight');
                        otherClass = 'animateFromLeft';
                    }
                }
                if (e.detail.directions.left) {
                    other = cur.nextSibling;
                    if (other) {
                        cur.classList.add('animate', 'animateLeft');
                        otherClass = 'animateFromRight';
                    }
                }
                act(cur, other, otherClass);
            }
        });
    });
});
