(function($) {
  var CREADER = "creader";

  // --- Constructor ---
  var init = function(comicID, initialPage) {
    this.data(CREADER, {
      comicID : comicID,
      pageCache : {},
      animating : false,
      curPage : initialPage ? initialPage : 0
    });
    this.empty();
    $("<div>",{ 
      "class": "previous-page",
      id : comicID + "-prev"
    }).appendTo(this);
    $("<div>",{
      "class": "current-page",
      id : comicID + "-curr"
    }).appendTo(this);
    $("<div>",{
      "class": "next-page",
      id : comicID + "-next"
    }).appendTo(this);

    loadPage.apply(this, [initialPage]);
    
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
    var comicID = this.data(CREADER).comicID;

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
    // Need to do this because arguments is immutable
    var args = [];
    $.each(arguments, function (i,v) {
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
