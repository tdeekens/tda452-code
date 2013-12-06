window.battleship = {};

battleship.config = {
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

battleship.state = {
   booked: [],
   boatmodel: undefined,
   start: undefined,
   alignment: undefined
};

battleship.selectBoat = function() {
   if (!true) { //tobe validation in haskell
      return false;
   }

   var $this     = $(this);
   var boatmodel = $this.data('model');

   battleship.state.boatmodel = boatmodel;
   battleship.state.start     = battleship.config.start;
   battleship.state.alignment = battleship.config.alignment;

   console.log('State changed:', battleship.state);
};

battleship.addBoat = function() {
   if (battleship.state.boatmodel === undefined) {
      alert("Please select a boat to position!");
      return false;
   }

   battleship.markHorizontal(false);

   var $this    = $(this)
       , id       = $this.attr('id')
       , position = id.split(battleship.config.delimiter);

   battleship.state.start = [position[0]--, position[1]--];

   battleship.markHorizontal(true);

   console.log('State changed:', battleship.state);
}

battleship.markHorizontal = function(book) {
   var boatmodel = battleship.state.boatmodel;

   var row  = battleship.state.start[0] + 1
       , cell = battleship.state.start[1] + 1;

   var length = cell + battleship.config.sizes[boatmodel];
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

battleship.markVertically = function(book) {
   var boatmodel = battleship.state.boatmodel;

   var $rows     = $('tr')
       , column = battleship.state.start[0] + 1;

   var length = column + battleship.config.sizes[boatmodel];

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

battleship.flipBoat = function() {
   if(battleship.state.alignment === 'horizontal') {
      battleship.state.alignment = 'vertical';
      battleship.markHorizontal(false);
      battleship.markVertically(true);
   } else {
      battleship.state.alignment = 'horizontal';
      battleship.markVertically(false);
      battleship.markHorizontal(true);
   }

   console.log('State changed:', battleship.state);
};

battleship.lockBoat = function() {
   if (!true) { // ask haskell here
      alert("Duplicate boat or invalid position!");
      return false;
   } else {
      battleship.state.boatmodel = undefined;
      battleship.state.start     = undefined;
      battleship.state.alignment = undefined;
   }
};

battleship.startGame = function() {
   if (!true) { // ask haskell here
      alert("Game can't be started, please positions your boats!");
      return false;
   } else {
      var $tds = $('tbody td:not(.shead)');
      $tds.off();
      $tds.removeClass('boat');

      $('#legend').css('visibility', 'hidden');
      $('table').addClass('game-mode');

      $tds.on('click', battleship.shoot);
   }
};

battleship.shoot = function() {
   if (!true) { // not hit: ask haskell here
      return false;
   } else {
      var $this = $(this);

      $this.text('☠');
   }
};

battleship.reset = function() {
   battleship.state.boatmodel = undefined;
   battleship.state.start     = undefined;
   battleship.state.alignment = undefined;

   var $tds = $('tbody td:not(.shead)');
   $tds.removeClass();
   $tds.text('');
   $tds.off();
   $tds.on('click', battleship.addBoat);
   $('#legend').css('visibility', 'visible');
};

$(function() {
   $('.boat').on('click', battleship.selectBoat);
   $('button#flip').on('click', battleship.flipBoat);
   $('button#lock').on('click', battleship.lockBoat);
   $('button#start').on('click', battleship.startGame);
   $('button#reset').on('click', battleship.reset);

   $('tbody td:not(.shead)').on('click', battleship.addBoat);
});