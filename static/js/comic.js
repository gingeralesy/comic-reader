$(function() {
  var comicID = Number.parseInt($("#comic-id").val());
  var initialPage = Number.parseInt($("#page-number").val());
  var body = $("body");
  var reader = $("#reader");
       
  reader.creader("init",comicID,initialPage);
  body.keypress(function(event) {
    switch (event.key) {
    case "ArrowLeft":
      reader.creader("prevPage");
      break;
    case "ArrowRight":
      reader.creader("nextPage");
      break;
    }
  });
});
