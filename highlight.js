window.addEventListener("load", function () {
  var scrollPos = 0;

  // select correct highlighted box whenever hash changes
  var hashchange = function () {
    // 1. set correct radio button
    document.querySelectorAll('.carousel').forEach(function(x) {
      x.checked = x.parentNode.querySelector('a').getAttribute('href') == window.location.hash;
    });

    if (window.location.hash == "") {
      scrollPos = 0;
      document.documentElement.classList.remove('highlight');
    } else {
      document.documentElement.classList.add('highlight');
      // 2. scroll to correct box
      //document.querySelector('.content').scrollTo({left: document.querySelector(window.location.hash.replace('#', '.')).offsetLeft, behavior: 'smooth'});
    }
  };
  hashchange();
  window.addEventListener("hashchange", hashchange);

  // update hash while scrolling (== swiping)
  var content = document.querySelector('.content');
  content.addEventListener('scroll', function(ev) {
    scrollPos = ev.target.scrollLeft;
  });
  setInterval(function() {
    if (scrollPos != 0 && scrollPos == content.scrollLeft) {
      [...document.querySelectorAll('.section-wrapper')].filter(function(e) {
          return e.getBoundingClientRect().left > -100 && e.getBoundingClientRect().left < 100;
      }).forEach(function(x) {
          window.location.hash = '#' + x.id;
          scrollPos = 0;
      });
    }
  }, 500);

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
