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

    var pageNum = parsePageNum(page.pageNumber);
    
    this.$el = element;
    this.comic = comic;
    this._priv.pageCache[pageNum] = page;
    this._priv.curPage = pageNum;

    var reader = this;
    if (pageNum + 1 < comic.pageCount) {
      getComicPages(comic.id, parsePageNum(pageNum - 1),
                    Math.min(3,comic.pageCount), function(resp) {
        if (resp && $.isArray(resp.data)) {
          $.each(resp.data,function(i,page) {
            var pageNum = parsePageNum(page.pageNumber);

            if (pageNum == reader.currentPage) {
              setPage.apply(reader,[page,CUR_PAGE_ID]);
              if (page.transcript)
                setTranscript.apply(reader,[page.transcript]);
            } else if (pageNum == reader.currentPage - 1) {
              setPage.apply(reader,[page,PREV_PAGE_ID]);
            } else if (pageNum == reader.currentPage + 1) {
              setPage.apply(reader,[page,NEXT_PAGE_ID]);
            }
            
            reader._priv.pageCache[pageNum] = page;
          });
        }
      });
    }
  }
  // --- Public API ---
  ComicReader.prototype.$el = $();
  ComicReader.prototype.comic = {};
  ComicReader.prototype.currentPage = function() {
    return this._priv.curPage;
  };
  ComicReader.prototype.nextPage = function() {
    if (this.isBusy())
      return;
    // TODO: actually change page
    this._priv.curPage += 1;
    return this;
  };
  ComicReader.prototype.previousPage = function() {
    if (this.isBusy())
      return;
    // TODO: actually change page
    this._priv.curPage -= 1;
    return this;
  };
  ComicReader.prototype.isBusy = function() {
    return this._priv.animating;
  };

  // --- Private API ---
  ComicReader.prototype._priv = {};
  ComicReader.prototype._priv.animating = false;
  ComicReader.prototype._priv.curPage = 0;
  ComicReader.prototype._priv.pageCache = {};
  
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
    getComicPages(comicID,pageNum,1,success,error,complete);
  };
  
  var getComicPages = function(comicID,startPage,count,success,error,complete) {
    var startPage = parsePageNum(startPage);
    var count = parsePageNum(count);
    sendRequest("/api/comic/page", "GET", {
      "comic-id" : comicID,
      "start" : startPage,
      "count" : count
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
    if (!comic || !page)
      return this.data(CREADER);
    this.data(CREADER,new ComicReader(this,comic,page));
  };
}(jQuery));
