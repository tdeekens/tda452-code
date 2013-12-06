var js_jquery = $;

function js_click(obj, callback) {
   obj.click(function(evt) {
      A(
         callback,
         [
            [0, "#" + $(this).attr('id')]
         , 0]
      );
   });
}

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

state = {
   booked: [],
   boatmodel: undefined,
   start: undefined,
   alignment: undefined
};

selectBoat = function() {
   if (!true) { //tobe validation in haskell
      return false;
   }

   var $this     = $(arguments[0]);
   var boatmodel = $this.data('model');

   state.boatmodel = boatmodel;
   state.start     = config.start;
   state.alignment = config.alignment;

   console.log('State changed:', state);
};

addBoat = function() {
   if (state.boatmodel === undefined) {
      alert("Please select a boat to position!");
      return false;
   }

   markHorizontal(false);

   var $this      = $(arguments[0])
       , id       = $this.attr('id')
       , position = id.split(config.delimiter);

   state.start = [position[0]--, position[1]--];

   markHorizontal(true);

   console.log('State changed:', state);
}

markHorizontal = function(book) {
   var boatmodel = state.boatmodel;

   var row  = state.start[0] + 1
       , cell = state.start[1] + 1;

   var length = cell + config.sizes[boatmodel];
   var $cells = $( $('tr').get( row ) ).find('td');

   $cells = $cells.slice(cell, length);

   if (book === true) {
      $cells.addClass('boat');
      $cells.data('model', boatmodel);
   } else {
      $cells.removeClass('boat')
      $cells.data('model', null);
   }
}

markVertically = function(book) {
   var boatmodel = state.boatmodel;

   var $rows     = $('tr')
       , column = state.start[0] + 1;

   var length = column + config.sizes[boatmodel];

   $rows = $rows.slice(column, length);
   if (book === true) {
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

flipBoat = function() {
   if(state.alignment === 'horizontal') {
      state.alignment = 'vertical';
      markHorizontal(false);
      markVertically(true);
   } else {
      state.alignment = 'horizontal';
      markVertically(false);
      markHorizontal(true);
   }

   console.log('State changed:', state);
};

lockBoat = function() {
   if (!true) { // ask haskell here
      alert("Duplicate boat or invalid position!");
      return false;
   } else {
      state.boatmodel = undefined;
      state.start     = undefined;
      state.alignment = undefined;
   }
};

startGame = function() {
   if (!true) { // ask haskell here
      alert("Game can't be started, please positions your boats!");
      return false;
   } else {
      var $tds = $('tbody td:not(.shead)');
      $tds.off();
      $tds.removeClass('boat');

      $('#legend').css('visibility', 'hidden');
      $('table').addClass('game-mode');

      $tds.on('click', shoot);
   }
};

shoot = function() {
   if (!true) { // not hit: ask haskell here
      return false;
   } else {
      var $this = $(arguments[0]);

      $this.text('☠');
   }
};

reset = function() {
   state.boatmodel = undefined;
   state.start     = undefined;
   state.alignment = undefined;

   var $tds = $('tbody td:not(.shead)');
   $tds.removeClass();
   $tds.text('');
   $tds.off();
   $tds.on('click', addBoat);
   $('#legend').css('visibility', 'visible');
};