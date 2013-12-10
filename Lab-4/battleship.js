// jQuery can't be dollar bills in Haskell, this is its export
var js_jquery = $;

// Imported within Haskell's UI.hs to bind a click handler with callback
function js_click(obj, callback) {
   obj.click(function(evt) {
      A(
         callback,
         [
            // ... callbacks get the DOM Node's id to identify them
            [0, "#" + $(this).attr('id')],
            [0, getState()]
         , 0]
      );
   });
}

function js_unbind(obj) {
   obj.off();
}

// Config values used as defaults
config = {
   log: true,
   delimiter: "-",
   start: [0, 0],
   alignment: 'horizontal',
   sizes: {
      'aircraftcarrier': 5,
      'battleship': 4,
      'submarine': 3,
      'destroyer': 3,
      'patrolboat': 2
   }
}

// State is maintained within the object, always contains
// last selected boat also fetched by Haskell as String
state = {
   boatmodel: undefined,
   start: undefined,
   alignment: undefined
};

// Selects a boat from the legend (without coloring anything)
// just changes state within JS
selectBoat = function(idx) {
   var $this     = $(idx);
   var boatmodel = $this.attr('id');

   state.boatmodel = boatmodel;
   state.start     = config.start;
   state.alignment = config.alignment;

   console.log('State changed:', state);
};

// Bound by Haskell to get the current UI's state as String
getState = function() {
   var start = "";

   if (state.start !== undefined) {
      start = state.start.join("-");
   }

   return state.boatmodel + "|" + start + "|" + state.alignment;
}

// Adds a boat to the field by coloring cells, also sets state's start
addBoat = function(idx) {
   if (state.boatmodel === undefined) {
      alert("Please select a boat to position!");
      return false;
   }

   markHorizontally(false);

   var $this      = $(idx)
       , id       = $this.attr('id')
       , position = id.split(config.delimiter);

   state.start = [position[0]--, position[1]--];

   markHorizontally(true);

   console.log('State changed:', state);
}

// Colors/uncolors cells horitzonally based on the book flag
markHorizontally = function(book) {
   var boatmodel = state.boatmodel;

   var row  = state.start[0] + 1
       , cell = state.start[1] + 1;

   var length = cell + config.sizes[boatmodel];
   var $cells = $( $('tr').get( row ) ).find('td');

   $cells = $cells.slice(cell, length);

   if (book == true) {
      $cells.addClass('boat');
      $cells.data('model', boatmodel);
   } else {
      $cells.removeClass('boat')
      $cells.data('model', null);
   }
}

// Colors/uncolors cells vertically based on the book flag
markVertically = function(book) {
   var boatmodel = state.boatmodel;

   var $rows     = $('tr')
       , column = state.start[1] + 1
       , row  = state.start[0] + 1

   var length = row + config.sizes[boatmodel];

   $rows = $rows.slice(row, length);

   if (book == true) {
      $.each($rows, function(i, row) {
         var $row = $(row);
         var $cell = $( $row.find('td').get(column) );

         $cell.addClass('boat').data('model', boatmodel);
      });
   } else {
      $.each($rows, function(i, row) {
         var $row = $(row);
         var $cell = $( $row.find('td').get(column) );

         $cell.removeClass('boat').data('model', null);
      });
   }
}

// Flips a boat's alignment and colors cells accordingly
flipBoat = function() {
   if(state.alignment === 'horizontal') {
      state.alignment = 'vertical';
      markHorizontally(false);
      markVertically(true);
   } else {
      state.alignment = 'horizontal';
      markVertically(false);
      markHorizontally(true);
   }

   console.log('State changed:', state);
};

// Locks a boat by resetting the state to initial representation
lockBoat = function() {
   state.boatmodel = undefined;
   state.start     = undefined;
   state.alignment = undefined;
};

// Clears the field to start the game
startGame = function(idx) {
   var $tds = $('tbody td:not(.shead)');

   $tds.removeClass('boat');

   $('#legend').css('visibility', 'hidden');
   $('table').addClass('game-mode');
};

// Marks a hit on the field
markHit = function(idx) {
   var $this = $("#" + idx);

   $this.text('☠');
};

// Marks a hit on the field
markMiss = function(idx) {
   var $this = $("#" + idx);

   $this.text('☹');
};

// Though one: refreshes the browser window to restart game
resetGame = function(idx) {
   window.location.reload()
};

// Outputs a string within the game's message box
message = function(msg) {
   $msgBox = $("#msg-box");

   $msgBox.html(msg);
}

// Outputs a string within the game's debug box
debug = function(msg) {
   $debugBox = $("#debug-box");

   $debugBox.prepend("<br />" + msg);
}