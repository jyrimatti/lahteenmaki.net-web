var swipeContainerClass = 'section-wrapper';
window.addEventListener('load', function () {
    document.querySelectorAll('.' + swipeContainerClass).forEach(function (x) {
        SwipeListener(x);
        var act = function (cur, other, otherClass) {
            if (other && other.classList.contains(swipeContainerClass)) {
                setTimeout(function () {
                    other.querySelector('a').click();
                    cur.classList.remove('animate', 'animateLeft', 'animateRight');
                    other.classList.add('animate', otherClass);
                }, 500);
                setTimeout(function () {
                    other.classList.remove('animate', otherClass);
                }, 1000);
            }
        };
        x.addEventListener('swipe', function (e) {
            if (window.location.hash) {
                var cur = e.target.closest('.' + swipeContainerClass);
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
