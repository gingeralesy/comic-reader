(function($) {
  // --- Static constants ---
  var CREADER = "creader";
  var CUR_PAGE_ID = "current-page";
  var NEXT_PAGE_ID = "next-page";
  var PREV_PAGE_ID = "previous-page"

  // --- Constructor ---
  function ComicReader(element,comic,page) {
    element = $(element);
    if (element.length == 0)
      throwError("Missing parameter: element");
    if ($.isEmptyObject(comic))
      throwError("Missing parameter: comic");
    if ($.isEmptyObject(page))
      throwError("Missing parameter: page");

    var pageNum = parsePageNum(page["pageNumber"]);
    
    this.$el = element;
    this.comic = comic;
    this.pageCache[pageNum] = page;
    this.currentPage = pageNum;

    var reader = this;
    getComicPages(comic.id,parsePageNum(pageNum - 1),pageNum + 1,function(resp) {
      if (resp && $.isArray(resp.data)) {
        $.each(resp.data,function(i,page) {
          var pageNum = parsePageNum(page["pageNumber"]);

          if (pageNum == reader.currentPage) {
            setPage.apply(reader,[page,CUR_PAGE_ID]);
            if (page.transcript)
              setTranscript.apply(reader,[page.transcript]);
          } else if (pageNum == reader.currentPage - 1) {
            setPage.apply(reader,[page,PREV_PAGE_ID]);
          } else if (pageNum == reader.currentPage + 1) {
            setPage.apply(reader,[page,NEXT_PAGE_ID]);
          }
          
          reader.pageCache[pageNum] = page;
        });
      }
    });
  }
  // --- Public API ---
  ComicReader.prototype.$el = $();
  ComicReader.prototype.comic = {};
  ComicReader.prototype.pageCache = {};
  ComicReader.prototype.animating = false; // TODO: Find a way to make this read-only
  ComicReader.prototype.currentPage = 0; // TODO: Find a way to make this read-only
  ComicReader.prototype.nextPage = function() {
    if (this.isBusy())
      return;
    // TODO: actually change page
    this.currentPage += 1;
    return this;
  };
  ComicReader.prototype.previousPage = function() {
    if (isBusy.apply(this))
      return;
    // TODO: actually change page
    this.currentPage -= 1;
    return this;
  };
  ComicReader.prototype.isBusy = function() {
    return this.animating;
  };

  // --- Private API ---
  // -- main private interface --
  var parsePageNum = function(pageNum) {
    pageNum = parseInt(pageNum);
    if (isNaN(pageNum))
      throwError("Invalid page number: " + pageNum);
    return (0 < pageNum) ? pageNum : 0;
  };

  var setPage = function(page,elementId) {
    this.$el.find("#" + elementId).remove();
    $("<img>",{
      id: elementId,
      "class": "comic-page",
      src: page.imageUri
    }).appendTo(this.$el);
  };

  var setTranscript = function(transcript) {
    this.$el.find("#transcript").remove();
    if (transcript) {
      $("<div>",{
        id: "transcript",
        "class": "hidden comic-transcript",
        text : transcript
      }).append($("<h3>",{
        text : this.page.title || this.comic.title || "Comic Transcript"
      })).append($("<p>",{
        text : transcript
      })).appendTo(this.$el);
    }
  };
  
  // -- ajax call interface --
  var getComic = function(comicID,success,error,complete) {
    sendRequest("/api/comic", "GET", {
      "comic-id" : comicID
    }, success, error, complete);
  };
  
  var getComicPage = function(comicID,pageNum,success,error,complete) {
    getComicPages(comicID,pageNum,pageNum,success,error,complete);
  };
  
  var getComicPages = function(comicID,startPage,endPage,success,error,complete) {
    startPage = parsePageNum(startPage);
    endPage = parsePageNum(endPage);
    sendRequest("/api/comic/page", "GET", {
      "comic-id" : comicID,
      "start" : startPage,
      "end" : endPage
    }, success, error, complete);
  };
  
  var sendRequest = function(url,method,message,success,error,complete) {
    error = error || function(data) {
      if (data)
        printError("Response status " + String(data.status) + ": " +
                   String(data.message));
    };
    
    $.ajax(url, {
      method : method,
      dataType : "json",
      data : message,
      success : function(data) {
        if (!data || 400 <= data.status)
          return ((typeof error === "function") ? error(data) : undefined);
        return ((typeof success === "function") ? success(data) : undefined);
      },
      error : error,
      complete : complete
    });
  };

  var printError = function(message) {
    if (console && console.error)
      console.error(message);
  };

  var throwError = function(message) {
    printError(message);
    throw new Error(message);
  };

  // --- jQuery plugin extension ---
  $.fn.creader = function(comic,page) {
    this.creader = new ComicReader(this,comic,page);
  };
}(jQuery));
