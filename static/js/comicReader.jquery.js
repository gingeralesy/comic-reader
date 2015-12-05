(function($) {
  var CREADER = "creader";

  // --- Constructor ---
  var init = function(comicID,pageNum) {
    this.data(CREADER, {
      comic : {},
      pageCache : {},
      animating : false,
      curPage : pageNum
    });
    
    this.empty();
    $("<div>",{ 
      "class": "previous-page hidden"
    }).appendTo(this);
    $("<div>",{
      "class": "current-page"
    }).appendTo(this);
    $("<div>",{
      "class": "next-page hidden"
    }).appendTo(this);

    var readerEl = this;
    getComic(comicID,function (data) {
      readerEl.data(CREADER).comic = data;
    });

    getComicPage(comicID,pageNum,function(data) {
      readerEl.data(CREADER).pageCache["page-number"] = data;
      readerEl.find(".current-page").css("background-image", data["image-uri"]);
    });
    
    return this;
  };

  // --- Public API ---
  var currentPage = function() {
    return this.data(CREADER).curPage;
  };

  var nextPage = function() {
    if (isAnimating.apply(this))
      return;
    this.data(CREADER).curPage += 1;
    return this;
  };

  var prevPage = function() {
    if (isAnimating.apply(this))
      return;
    this.data(CREADER).curPage -= 1;
    return this;
  };

  var isAnimating = function() {
    return this.data(CREADER).animating;
  };

  // --- Private API ---
  // -- main private interface --
  var loadPage = function(pageNum) {
    var reader = this;
    var comicID = this.data(CREADER).comic.id;

    if (!reader.data(CREADER).pageCache[pageNum]) {
      getComicPage(comicID,pageNum,function (resp) {
        var data = resp.data;
        if (data) {
          reader.data(CREADER).pageCache[pageNum] = data;
        }
        console.log(data);
      }, function(jqXHR, textStatus, errorThrown) {
        alert("Failed to load page" + pageNum + ": " + textStatus + ": " + errorThrown);
      });
    }
  };
  
  // -- ajax call interface --
  var getComic = function(comicID,success,error,complete) {
    sendGetRequest("/api/comic", {
      "comic-id" : comicID
    }, success, error, complete);
  };
  
  var getComicPage = function(comicID,pageNum,success,error,complete) {
    sendGetRequest("/api/comic/page", {
      "comic-id" : comicID,
      "page-number" : pageNum
    }, success, error, complete);
  };
  
  var sendGetRequest = function(url,parameters,success,error,complete) {
    sendRequest(url,"GET",parameters,success,error,complete);
  };
  
  var sendRequest = function(url,method,message,success,error,complete) {
    $.ajax(url, {
      method : method,
      dataType : "json",
      data : message,
      success : success,
      error : error,
      complete : complete
    });
  };

  // --- jQuery plugin extension ---
  $.fn.creader = function() {
    var args = [];
    $.each(arguments, function (i,v) {
      // Need to do this because arguments is immutable
      args.push(v);
    });
    
    var action = args.shift();
    if (action === "init")
      return init.apply(this, args);
    if (action === "currentPage")
      return currentPage.apply(this);
    if (action === "nextPage")
      return nextPage.apply(this);
    if (action === "prevPage" || action === "previousPage")
      return prevPage.apply(this);
    if (action === "isAnimating")
      return isAnimating.apply(this);
    throw new Error("Invalid action: " + action);
  };
}(jQuery));
