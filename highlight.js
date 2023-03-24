window.addEventListener("load", function () {
  // select correct highlighted box whenever hash changes
  var hashchange = function () {
    // 1. set correct radio button
    document.querySelectorAll('.carousel').forEach(function(x) {
      x.checked = x.parentNode.querySelector('a').getAttribute('href') == window.location.hash;
    });

    if (window.location.hash == "") {
      document.documentElement.classList.remove('highlight');
    } else {
      document.documentElement.classList.add('highlight');
      // 2. scroll to correct box
      document.querySelector('.content').scrollTo({left: document.querySelector(window.location.hash.replace('#', '.')).offsetLeft, behavior: 'smooth'});
    }
  };
  hashchange();
  window.addEventListener("hashchange", hashchange);

  // update hash while scrolling (== swiping)
  var scrollPos = 0;
  var content = document.querySelector('.content');
  content.addEventListener('scroll', function(ev) {
    scrollPos = ev.target.scrollLeft;
  });
  setInterval(function() {
    if (scrollPos == content.scrollLeft) {
      [...document.querySelectorAll('.section')].filter(function(e) { return e.getBoundingClientRect().left > 0 && e.getBoundingClientRect().left < 100; })
                                                .forEach(function(x) { window.location.hash = '#' + [...x.classList].filter(function(v) { return v != 'section'; }); });
    }
  }, 500);

  // remove hash if the highlighted box header is clicked
  document.body.addEventListener("click", function (event) {
    if (window.innerWidth > 850 && (event.target.classList.contains("container") || event.target.getAttribute("href") == window.location.hash)) {
      window.location.hash = "";
      event.preventDefault();
      return false;
    }
  });

  // change hash when carousel-radio changes
  document.querySelectorAll(".carousel").forEach(function (x) {
    x.addEventListener("input", function (event) {
      if (event.target.checked) {
        window.location.hash = event.target.parentElement
                                    .querySelector("a")
                                    .getAttribute("href");
      }
    });
  });

  // select first hash on small screens
  var higlightSomething = function() {
    if (window.innerWidth <= 850 && window.location.hash == '') {
      window.location.hash = document.querySelector('.section-wrapper a').getAttribute('href');
    }
  }
  higlightSomething();
  window.addEventListener('resize', higlightSomething);
});
