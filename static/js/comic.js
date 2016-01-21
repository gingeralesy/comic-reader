$(function() {
  var comic = $("input#comic-data").data();
  var page = $("input#page-data").data();
  var body = $("body");
  var reader = $("#reader");
       
  reader.creader(comic,page);
  body.keypress(function(event) {
    switch (event.key) {
    case "ArrowLeft":
      reader.creader.previousPage();
      break;
    case "ArrowRight":
      reader.creader.nextPage();
      break;
    }
  });
});
