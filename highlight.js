window.addEventListener("load", function () {
  var hashchange = function () {
    document.querySelectorAll('.carousel').forEach(function(x) {
      x.checked = x.parentNode.querySelector('a').getAttribute('href') == window.location.hash;
    });

    if (window.location.hash == "") {
      document.documentElement.classList.remove('highlight');
    } else {
      document.documentElement.classList.add('highlight');
    }
  };
  hashchange();
  window.addEventListener("hashchange", hashchange);

  // remove highlighing if the highlighted box header is clicked
  document.body.addEventListener("click", function (event) {
    if (event.target.classList.contains("container") || event.target.getAttribute("href") == window.location.hash) {
      window.location.hash = "";
      event.preventDefault();
      return false;
    }
  });

  // change highlight when carousel-radio changes
  document.querySelectorAll(".carousel").forEach(function (x) {
    x.addEventListener("input", function (event) {
      if (event.target.checked) {
        window.location.hash = event.target.parentElement
                                    .querySelector("a")
                                    .getAttribute("href");
      }
    });
  });

  // highlight some item on small screens
  var higlightSomething = function() {
    if (window.innerWidth <= 850 && window.location.hash == '') {
      window.location.hash = document.querySelector('.section-wrapper a').getAttribute('href');
    }
  }
  higlightSomething();
  window.addEventListener('resize', higlightSomething);
});
