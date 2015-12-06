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

    var readerEl = this;
    getComic(comicID,function (resp) {
      readerEl.data(CREADER).comic = resp.data;
      fetchAndSetPage.apply(readerEl,[pareNum,"current-page",true]);
      fetchAndSetPage.apply(readerEl,[pareNum + 1,"next-page"]);
      if (pageNum > 0)
        fetchAndSetPage.apply(readerEl,[pareNum - 1,"previous-page"]);
    });

    return this;
  };

  // --- Public API ---
  var currentPage = function() {
    return this.data(CREADER).curPage;
  };

  var nextPage = function() {
    if (isBusy.apply(this))
      return;
    this.data(CREADER).curPage += 1;
    return this;
  };

  var prevPage = function() {
    if (isBusy.apply(this))
      return;
    this.data(CREADER).curPage -= 1;
    return this;
  };

  var isBusy = function() {
    return this.data(CREADER).animating;
  };

  // --- Private API ---
  // -- main private interface --
  var fetchAndSetPage = function(pageNum,elementId,addTranscript) {
    var readerEl = this;
    if (!readerEl.data(CREADER).pageCache[pageNum]) {
      getComicPage(comicID,pageNum,function(resp) {
        if (resp && resp.data && resp.data.imageUri) {
          setPage.apply(this,[resp.data,elementId]);
          
          if (addTranscript && resp.data.transcript)
            setTranscript.apply(this,[resp.data.transcript]);
          
          readerEl.data(CREADER).pageCache[resp.data.pageNumber] = resp.data;
        }
      });
    } else {
      var page = readerEl.data(CREADER).pageCache[pageNum];
      if (page && page.imageUri) {
        setPage.apply(this,[page,elementId]);
        if (addTranscript && page.transcript)
          setTranscript.apply(this,[page.transcript]);
      }
    }
  };

  var setPage = function(page,elementId) {
    this.find("#" + elementId).remove();
    $("<img>",{
      id: elementId,
      "class": "comic-page",
      src: page.imageUri
    }).appendTo(this);
  };

  var setTranscript = function(transcript) {
    this.find("#transcript").remove();
    $("<div>",{
      id: "transcript",
      "class": "hidden comic-transcript",
      text : resp.data.transcript
    }).appendTo(this);
  };
  
  // -- ajax call interface --
  var getComic = function(comicID,success,error,complete) {
    sendRequest("/api/comic", "GET", {
      "comic-id" : comicID
    }, success, error, complete);
  };
  
  var getComicPage = function(comicID,pageNum,success,error,complete) {
    sendRequest("/api/comic/page", "GET", {
      "comic-id" : comicID,
      "page-number" : pageNum
    }, success, error, complete);
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
    if (action === "isBusy")
      return isBusy.apply(this);
    throw new Error("Invalid action: " + action);
  };
}(jQuery));
